program main
use, intrinsic :: iso_c_binding, only: c_char, c_int, c_funptr, c_ptr, c_associated, c_funloc, c_loc, c_f_pointer
use package_types, only: fpm_package_t, package_t
use download_helper, only: download
use fortran_pcre, only: pcre_free, PCRE_CASELESS, PCRE_NO_AUTO_CAPTURE
use pcre_helper, only: regex, match
use c_util, only: to_c_string, to_fortran_string
use os, only: remove, now, fileTime
use fhash, only: fhash_tbl_t, fhash_key, fhash_key_t
use json, only: parse_json
implicit none

! Remote registry file
character(len=*), parameter :: registry_url =&
    'https://github.com/fortran-lang/fpm-registry/raw/master/index.json'

! Local registry file: '%TEMP%/index.json'
character(len=:), allocatable :: registry_file
character(len=:,kind=c_char), allocatable :: registry_file_c

! Cache index.json for time_to_live seconds
integer, parameter :: time_to_live = 60*60

integer(kind=c_int) :: i
logical :: download_ok
type(c_funptr) :: cproc
type(fhash_tbl_t), target :: tbl

! Command line arguments
integer :: arg_count
character(len=64) :: arg, search_term, info_term

registry_file   = get_registry_file()
registry_file_c = to_c_string(registry_file)

!
! Download index.json from the internet only if
! the local copy is older than time_to_live seconds
!
if (abs(now() - fileTime(registry_file_c)) .gt. time_to_live) then
    !print *, 'Downloading registry ...'

    i = remove(registry_file_c)
    download_ok = download(registry_url, registry_file_c)

    if (.not. download_ok) then
        print *, 'Registry download failed.'
        stop
    end if
end if

cproc = c_funloc(callback)
i = parse_json(registry_file_c, cproc, c_loc(tbl))

arg_count = command_argument_count()
if (arg_count .ne. 2) then
call usage()
return
end if

arg = ''
call get_command_argument(1, arg)

if (arg .eq. 'search') then
    call get_command_argument(2, search_term)
    call table_search(tbl, trim(search_term))
else if (arg .eq. 'info') then
    call get_command_argument(2, info_term)
    call table_info(tbl, trim(info_term))
else
    call usage()
end if

contains

!
! is_windows()
! Returns true if the code is running on Windows operating system
!
logical function is_windows() result(r)
    implicit none
    character(len=255) :: dir
    integer :: length
    integer :: vstatus

    call get_environment_variable("OS", dir, length, vstatus)

    if (vstatus .eq. 0 .and. dir .eq. "Windows_NT") then
        r = .true.
    end if
end function

!
! get_registry_file()
! Resolves '%TEMP%/index.json'
! 
function get_registry_file() result(r)
    character(len=255) :: tempdir
    character(len=:), allocatable :: r
    integer :: length
    integer :: vstatus
    character(len=*), parameter :: f = "index.json"

    call get_environment_variable("TEMP", tempdir, length, vstatus)

    if (vstatus .eq. 0 .and. is_windows()) then
        r = trim(tempdir) // "\" // f
    else
        r = "/tmp/" // f
    end if
end function

subroutine usage()
    print 100, 'Usage: avpkg <search|info> <package>'
    print *
    print 100, 'Example: avpkg search molecular'
    print 100, 'Example: avpkg search "thermodynamics|mechanics"'
    print 100, 'Example: avpkg info weather'
    100 format(a)
end subroutine

subroutine table_get_package(tbl, k, v, r)
    type(fhash_tbl_t),  intent(in)  :: tbl
    class(fhash_key_t), intent(in)  :: k
    type(package_t),    intent(out) :: v
    logical, intent(out) :: r
    integer :: stat
    class(*), allocatable :: data

    r = .true.
    call tbl%get(k, data, stat)

    if (stat /= 0) then
        r = .false.
    end if

    select type(d=>data)
        type is (package_t)
        v = d
        class default
        ! Error handling: found wrong type
        ! print *, 'error'
    end select
end subroutine

subroutine table_search(tbl, pattern)
    type(fhash_tbl_t), intent(in), pointer :: tbl
    character(len=*), intent(in) :: pattern
    type(package_t) :: pkg
    logical :: r, r1, r2
    integer :: i
    integer :: num_buckets, num_items, num_collisions, max_depth
    type(c_ptr) :: re

    re = regex(pattern, ior(PCRE_CASELESS, PCRE_NO_AUTO_CAPTURE))
    call tbl%stats(num_buckets, num_items, num_collisions, max_depth)

    do i = 1, num_items
        call table_get_package(tbl, fhash_key(i), pkg, r)

        r1 = match(re, pkg%name)
        r2 = match(re, pkg%description)

        if (r1 .or. r2) then
            print 100, pkg%name, pkg%description, pkg%version
        end if

        100 format(a, ' : ', a, ': ', a)
    end do

    call pcre_free(re)
end subroutine

subroutine table_info(tbl, pattern)
    type(fhash_tbl_t), intent(in), pointer :: tbl
    character(len=*), intent(in) :: pattern
    type(package_t) :: pkg
    logical :: r, r1, r2
    integer :: i
    integer :: num_buckets, num_items, num_collisions, max_depth
    type(c_ptr) :: re

    re = regex(pattern, ior(PCRE_CASELESS, PCRE_NO_AUTO_CAPTURE))
    call tbl%stats(num_buckets, num_items, num_collisions, max_depth)

    do i = 1, num_items
        call table_get_package(tbl, fhash_key(i), pkg, r)
        r1 = match(re, pkg%name)
        r2 = match(re, pkg%description)

        if (r1 .or. r2) then
            print 100, 'name', pkg%name
            print 100, 'description', pkg%description
            print 100, 'version', pkg%version
            print 100, 'license', pkg%license
            print 100, 'author', pkg%author
            print 100, 'copyright', pkg%copyright
            print 100, 'git', pkg%git
            print 100, 'git-tag', pkg%git_tag
            print *

            100 format(a12, ': ', a)
        end if
    end do

    call pcre_free(re)
end subroutine

subroutine table_add(tbl, pkg)
    type(fhash_tbl_t), intent(in), pointer :: tbl
    type(package_t), intent(in) :: pkg
    integer :: num_buckets, num_items, num_collisions, max_depth

    call tbl%stats(num_buckets, num_items, num_collisions, max_depth)
    call tbl%set(fhash_key(num_items+1), value=pkg)
end subroutine

subroutine callback(pkg, fortran_ptr) bind(c)
    type(fpm_package_t), intent(in), value :: pkg
    type(c_ptr), intent(in), value :: fortran_ptr
    type(fhash_tbl_t), pointer :: tbl
    type(package_t) :: pkg_f

    call c_f_pointer(fortran_ptr, tbl)

    if (c_associated(pkg%name))        pkg_f%name = to_fortran_string(pkg%name)
    if (c_associated(pkg%version))     pkg_f%version = to_fortran_string(pkg%version)
    if (c_associated(pkg%description)) pkg_f%description = to_fortran_string(pkg%description)
    if (c_associated(pkg%license))     pkg_f%license = to_fortran_string(pkg%license)
    if (c_associated(pkg%author))      pkg_f%author = to_fortran_string(pkg%author)
    if (c_associated(pkg%copyright))   pkg_f%copyright = to_fortran_string(pkg%copyright)
    if (c_associated(pkg%git))         pkg_f%git = to_fortran_string(pkg%git)
    if (c_associated(pkg%git_tag))     pkg_f%git_tag = to_fortran_string(pkg%git_tag)

    call table_add(tbl, pkg_f)
end subroutine

end program main

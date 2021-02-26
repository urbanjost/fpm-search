program main
use, intrinsic :: iso_c_binding, only: c_char, c_int, c_funptr, c_ptr, c_associated, c_funloc, c_loc, c_f_pointer
use package_types, only: fpm_package_t, package_t
use download_helper, only: download
use fortran_pcre, only: pcre_free, PCRE_CASELESS, PCRE_NO_AUTO_CAPTURE
use pcre_helper, only: regex, match
use c_util, only: to_c_string, to_fortran_string, to_fortran_array
use os, only: remove, now, fileTime
use fhash, only: fhash_tbl_t, fhash_key, fhash_key_t
use json, only: parse_json
use M_CLI2, only : set_args, lget, arg=>unnamed, get_args
use M_strings, only: join
use stdlib_ascii, only: is_alphanum
use stdlib_error, only: check

implicit none
character(len=:),allocatable :: help_text(:), version_text(:)
integer :: loop

! Remote registry file
character(len=*), parameter :: registry_url =&
    'https://github.com/fortran-lang/fpm-registry/raw/master/index.json'

character(len=:), allocatable :: alternate_registry_url
character(len=:), allocatable :: remote_registry_url

! Local registry file: '%TEMP%/index.json'
character(len=:), allocatable :: registry_file
character(len=:,kind=c_char), allocatable :: registry_file_c

! Cache index.json for time_to_live seconds
integer, parameter :: time_to_live = 60*60

integer(kind=c_int) :: i
logical :: download_ok
type(c_funptr) :: cproc
type(fhash_tbl_t), target :: tbl
integer :: arg_count

call usage()
call set_args(' --toml:T F --registry "null" --force-download:F F', help_text, version_text)
arg_count = size(arg)

if (arg_count .eq. 0) then
   arg = ['.']
   arg_count = 1
end if

call get_args("registry", alternate_registry_url)

if (alternate_registry_url .eq. "null") then
    registry_file = get_registry_file(registry_url)
    remote_registry_url = registry_url
else
    registry_file = get_registry_file(alternate_registry_url)
    remote_registry_url = alternate_registry_url
end if

registry_file_c = to_c_string(registry_file)

!
! Download index.json from the internet only if
! the local copy is older than time_to_live seconds
!
if (lget('force-download') .or. (abs(now() - fileTime(registry_file_c)) .gt. time_to_live)) then
    print *, 'Downloading registry ... ', remote_registry_url

    i = remove(registry_file_c)
    download_ok = download(remote_registry_url, registry_file_c)
    call check(download_ok .eqv. .true., 'Registry download failed')
end if

cproc = c_funloc(callback)
i = parse_json(registry_file_c, cproc, c_loc(tbl))
call check(i .eq. 0, 'parse_json() failed')

if (lget('toml')) then
    select case(arg_count)
    case(1)
       call table_add(tbl, trim(arg(1)))
    case(2)
       call table_add(tbl, trim(arg(1)), trim(arg(2)))
    case default
       write(*,*)'wrong number of arguments for "--toml" mode'
       write(*,'(a)')help_text
    end select
else
    if (lget('verbose')) then
        do loop=1, arg_count
            call table_info(tbl, trim(arg(loop)))
        end do
    else
        do loop=1, arg_count
            call table_search(tbl, trim(arg(loop)))
        end do
    end if
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
! to_alpha_numeric(url)
! input:  "https://domain/path/%11/j.php?.json"
! output: "https___domain_path__11_j_php__json"
!
function to_alpha_numeric(url) result(r)
    character(len=*), intent(in) :: url
    character(len=len(url)) :: r
    integer :: i

    do i = 1, len(url)
        if (is_alphanum(url(i:i))) then
            r(i:i) = url(i:i)
        else
            r(i:i) = '_'
        end if
    end do
end function

!
! get_registry_file(url)
! Resolves '%TEMP%/alpha_numeric(url)'
!
function get_registry_file(url) result(r)
    character(len=*), intent(in) :: url
    character(len=255) :: tempdir
    character(len=:), allocatable :: r
    integer :: length
    integer :: vstatus
    character(len=:), allocatable :: f

    f = to_alpha_numeric(url)
    call get_environment_variable("TEMP", tempdir, length, vstatus)

    if (vstatus .eq. 0 .and. is_windows()) then
        r = trim(tempdir) // "\" // f
    else
        r = "/tmp/" // f
    end if
end function

subroutine usage()

version_text=[character(len=80) :: &
& 'PRODUCT:         fpm (Fortran Package Manager) utilities and examples', &
& 'PROGRAM:         fpm-search(1)                                       ', &
& 'VERSION:         0.8.0                                               ', &
& 'DESCRIPTION:     display available FPM packages                      ', &
& 'AUTHOR:          brocolis@eml.cc                                     ', &
& 'LICENSE:         ISC License                                         ', &
& 'COPYRIGHT:       2021 fpm-search contributors                        ', &
& 'HOME PAGE:       https://github.com/brocolis/fpm-search              ', &
& '']

help_text=[character(len=80) :: &
! '12345678901234567890123456789012345678901234567890123456789012345678901234567890', &
& 'NAME                                                                            ', &
& '   fpm-search(1) - display available FPM packages                               ', &
& 'SYNOPSIS                                                                        ', &
& '   syntax:                                                                      ', &
& '                                                                                ', &
& '    fpm-search SEARCH_STRING(s) [--verbose] [--registry URI] [--force-download] ', &
& '     or                                                                         ', &
& '    fpm-search --toml PACKAGE_NAME [TAG]                                        ', &
& 'DESCRIPTION                                                                     ', &
& '   Search for and display information describing fpm (Fortran Package Manager)  ', &
& '   packages registered in the fpm repository at                                 ', &
& '                                                                                ', &
& '      https://github.com/fortran-lang/fpm-registry                              ', &
& 'OPTIONS                                                                         ', &
& ' SEARCH MODE:                                                                   ', &
& '    SEARCH_STRING  A regular expression used to match package descriptions.     ', &
& '                   It is case-insensitive. The default is ".", causing all      ', &
& '                   registered packages to be displayed.                         ', &
& '    --verbose,-V   give more-detailed information about the packages matching   ', &
& '                   SEARCH_STRING.                                               ', &
& '                                                                                ', &
& ' TOML ENTRY MODE:                                                               ', &
& '    --toml,-T      instead of an fpm project description generate the line      ', &
& '                   needed to be added to the "fpm.toml" file in order to use    ', &
& '                   the specified external package in your fpm project.          ', &
& '    PACKAGE_NAME   when the --toml switch is supplied a string is required that ', &
& '                   in NOT treated as a Regular Expression but as a specific     ', &
& '                   case-sensitive fpm package name.                             ', &
& '    TAG            A git(1) tag name can optionally follow the PACKAGE_NAME     ', &
& '                   when using the --toml switch.                                ', &
& '                                                                                ', &
& ' DOCUMENTATION:                                                                 ', &
& '    --help,-h      display this help and exit                                   ', &
& '    --version,-v   output version information and exit                          ', &
& '                                                                                ', &
& 'EXAMPLE                                                                         ', &
& ' Sample commands:                                                               ', &
& '                                                                                ', &
& '  fpm-search molecular                                                          ', &
& '  fpm-search "thermodynamics|mechanics" # look for either string                ', &
& '  fpm-search weather --verbose                                                  ', &
& '  fpm-search "date|time"                                                        ', &
& '                                                                                ', &
& '  fpm-search M_color --toml                                                     ', &
& '  fpm-search --toml datetime v1.7.0                                             ', &
& '                                                                                ', &
& '  fpm-search     # list all package descriptions                                ', &
& '  fpm-search -V  # describe all packages in detail                              ', &
& '']
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
    integer :: num_buckets, num_items
    type(c_ptr) :: re

    re = regex(pattern, ior(PCRE_CASELESS, PCRE_NO_AUTO_CAPTURE))
    call tbl%stats(num_buckets, num_items)

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
    integer :: num_buckets, num_items
    type(c_ptr) :: re
    character(len=:), allocatable :: s

    re = regex(pattern, ior(PCRE_CASELESS, PCRE_NO_AUTO_CAPTURE))
    call tbl%stats(num_buckets, num_items)

    do i = 1, num_items
        call table_get_package(tbl, fhash_key(i), pkg, r)
        r1 = match(re, pkg%name)
        r2 = match(re, pkg%description)

        if (r1 .or. r2) then
            print 100, 'name', pkg%name
            print 100, 'description', pkg%description
            print 100, 'version', pkg%version
            print 100, 'license', pkg%license

            if (pkg%author_count .gt. 0) then
                s = join(pkg%author, ", ")
                s = s(1:len(s) - 2)
                print 100, 'author', s
            end if

            if (pkg%maintainer_count .gt. 0) then
                s = join(pkg%maintainer, ", ")
                s = s(1:len(s) - 2)
                print 100, 'maintainer', s
            end if

            if (pkg%categories_count .gt. 0) then
                s = join(pkg%categories, ", ")
                s = s(1:len(s) - 2)
                print 100, 'categories', s
            end if

            if (pkg%keywords_count .gt. 0) then
                s = join(pkg%keywords, ", ")
                s = s(1:len(s) - 2)
                print 100, 'keywords', s
            end if

            print 100, 'homepage', pkg%homepage
            print 100, 'copyright', pkg%copyright
            print 100, 'git', pkg%git
            print 100, 'git-tag', pkg%git_tag
            print *

            100 format(a12, ': ', a)
        end if
    end do

    call pcre_free(re)
end subroutine

subroutine table_add(tbl, name, tag)
    type(fhash_tbl_t), intent(in), pointer :: tbl
    character(len=*), intent(in) :: name
    character(len=*), intent(in), optional :: tag
    type(package_t) :: pkg
    logical :: r
    integer :: i
    integer :: num_buckets, num_items

    call tbl%stats(num_buckets, num_items)

    do i = 1, num_items
        call table_get_package(tbl, fhash_key(i), pkg, r)

        if (name .eq. pkg%name) then
            if (present(tag)) then
                if (tag .eq. pkg%git_tag) then
                    print 100, pkg%name, pkg%git, pkg%git_tag
                    return
                end if
            else
                print 200, pkg%name, pkg%git
                return
            end if
        end if

        100 format(a, ' = { git = "', a, '", tag="', a, '" }')
        200 format(a, ' = { git = "', a, '" }')
    end do

end subroutine

subroutine callback(pkg, fortran_ptr) bind(c)
    type(fpm_package_t), intent(in) :: pkg
    type(c_ptr), intent(in), value :: fortran_ptr
    type(fhash_tbl_t), pointer :: tbl
    type(package_t) :: pkg_f
    integer :: num_buckets, num_items

    call c_f_pointer(fortran_ptr, tbl)

    if (c_associated(pkg%name))        pkg_f%name = to_fortran_string(pkg%name)
    if (c_associated(pkg%version))     pkg_f%version = to_fortran_string(pkg%version)
    if (c_associated(pkg%description)) pkg_f%description = to_fortran_string(pkg%description)
    if (c_associated(pkg%license))     pkg_f%license = to_fortran_string(pkg%license)
    if (c_associated(pkg%author))      pkg_f%author = to_fortran_array(pkg%author_count, pkg%author)
    if (c_associated(pkg%maintainer))  pkg_f%maintainer = to_fortran_array(pkg%maintainer_count, pkg%maintainer)
    if (c_associated(pkg%categories))  pkg_f%categories = to_fortran_array(pkg%categories_count, pkg%categories)
    if (c_associated(pkg%keywords))    pkg_f%keywords = to_fortran_array(pkg%keywords_count, pkg%keywords)
    if (c_associated(pkg%homepage))    pkg_f%homepage = to_fortran_string(pkg%homepage)
    if (c_associated(pkg%copyright))   pkg_f%copyright = to_fortran_string(pkg%copyright)
    if (c_associated(pkg%git))         pkg_f%git = to_fortran_string(pkg%git)
    if (c_associated(pkg%git_tag))     pkg_f%git_tag = to_fortran_string(pkg%git_tag)

    pkg_f%author_count     = pkg%author_count
    pkg_f%maintainer_count = pkg%maintainer_count
    pkg_f%categories_count = pkg%categories_count
    pkg_f%keywords_count   = pkg%keywords_count

    call tbl%stats(num_buckets, num_items)
    call tbl%set(fhash_key(num_items+1), value=pkg_f)
end subroutine

end program main

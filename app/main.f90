program main
use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr
use package_types, only: package_t
use download_helper, only: download
use M_match, only: regex_pattern, getpat, match, YES, ERR
use c_util, only: to_c_string, to_fortran_string, to_fortran_array
use os, only: remove, now, fileTime
use fhash, only: fhash_tbl_t, fhash_key, fhash_key_t
use json, only: get_packages
use M_CLI2, only : set_args, lget, arg=>unnamed, get_args
use M_strings, only: join
use stdlib_ascii, only: is_alphanum
use stdlib_error, only: check
use config, only: config_t, registry_t, read_config_file

implicit none
character(len=:),allocatable :: help_text(:), version_text(:)
integer :: loop

! Remote registry file
character(len=*), parameter :: registry_url =&
    'https://github.com/fortran-lang/fpm-registry/raw/master/index.json'

character(len=:), allocatable :: alternate_registry_url
character(len=:), allocatable :: remote_registry_url

! Local registry file: '%TEMP%/index.json'
character(len=:), allocatable :: registry_file, registry_file_home, registry_file_etc
character(len=:,kind=c_char), allocatable :: registry_file_c

! Cache index.json for time_to_live seconds
integer, parameter :: time_to_live = 60*60

character(len=:), allocatable :: config_file_home, config_file_etc
type(config_t) :: cfg_home, cfg_etc
logical :: config_ok

integer(kind=c_int) :: i
logical :: download_ok
logical :: r
type(fhash_tbl_t) :: tbl
integer :: arg_count
integer :: num_items

call usage()
call set_args(' --toml:T F --registry "null" --force-download:F F', help_text, version_text)
arg_count = size(arg)

if (arg_count .eq. 0) then
   arg = ['^']
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
    download_ok = download(remote_registry_url, registry_file)
    call check(download_ok, 'Registry download failed')
end if

call get_packages(registry_file, tbl, r)
call check(r, 'get_packages() failed')
! call tbl%stats(num_items=num_items)
! print *, 'num_items:', num_items

!
! Download additional registries
!
call get_config_file_home(config_file_home, config_ok)
if (config_ok) then
    call read_config_file(config_file_home, cfg_home)
    call download_registries(cfg_home, time_to_live, registry_file_home, lget('force-download'))
    call get_packages(registry_file_home, tbl, r)
    call check(r, 'get_packages() failed')
end if

call get_config_file_etc(config_file_etc, config_ok)
if (config_ok) then
    call read_config_file(config_file_etc, cfg_etc)
    call download_registries(cfg_etc, time_to_live, registry_file_etc, lget('force-download'))
    call get_packages(registry_file_etc, tbl, r)
    call check(r, 'get_packages() failed')
end if

! call tbl%stats(num_items=num_items)
! print *, 'num_items:', num_items
! call dump_config(cfg_home)
! call dump_config(cfg_etc)

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

subroutine download_registries(cfg, time_to_live, r, force)
    type(config_t), intent(in) :: cfg
    integer, intent(in) :: time_to_live
    character(len=:), intent(inout), allocatable :: r
    logical, intent(in) :: force
    character(len=:,kind=c_char), allocatable :: registry_file_c
    integer :: n
    integer(kind=c_int) :: i
    logical :: download_ok

    if (.not. allocated(cfg%registry)) return

    do n = 1, size(cfg%registry)
        r = get_registry_file(cfg%registry(n)%url)
        registry_file_c = to_c_string(r)

        if (force .or. (abs(now() - fileTime(registry_file_c)) .gt. time_to_live)) then
            print *, 'Downloading registry ... ', cfg%registry(n)%url

            i = remove(registry_file_c)
            download_ok = download(cfg%registry(n)%url, r)
            call check(download_ok, 'Registry download failed')
        end if
    end do
end subroutine

!
! is_windows()
! Returns true if the code is running on Windows operating system
!
logical function is_windows() result(r)
    implicit none
    character(len=255) :: dir
    integer :: length
    integer :: vstatus

    r = .false.
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

!
! get_config_file_home
! Return value: '%HOMEPATH%/.fpm-search.conf' on Windows
!               '$HOME/.fpm-search.conf' on Linux
!
subroutine get_config_file_home(r, ok)
    logical, intent(out) :: ok
    character(len=255) :: homedir
    character(len=:), intent(out), allocatable :: r
    integer :: length
    integer :: vstatus
    character(len=*), parameter :: f = '.fpm-search.conf'

    if (is_windows()) then
        call get_environment_variable("HOMEPATH", homedir, length, vstatus)
        ok = vstatus .eq. 0

        if (.not. ok) return
        r = trim(homedir) // "\" // f
        inquire(file=r, exist=ok)
    else
        call get_environment_variable("HOME", homedir, length, vstatus)
        ok = vstatus .eq. 0

        if (.not. ok) return
        r = trim(homedir) // "/" // f
        inquire(file=r, exist=ok)
    end if
end subroutine

!
! get_config_file_etc
! Return value: Windows: .false.
!               Linux: .true. if '/etc/fpm-search.conf' exists
!
subroutine get_config_file_etc(r, ok)
    logical, intent(out) :: ok
    character(len=:), intent(out), allocatable :: r

    if (is_windows()) then
        ok = .false.
    else
        r = '/etc/fpm-search.conf'
        inquire(file=r, exist=ok)
    end if
end subroutine

subroutine usage()

version_text=[character(len=80) :: &
& 'PRODUCT:         fpm (Fortran Package Manager) utilities and examples', &
& 'PROGRAM:         fpm-search(1)                                       ', &
& 'VERSION:         0.12.0                                              ', &
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
& '                   It is case-sensitive. The default is ".", causing all        ', &
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
& '  fpm-search hash                                                               ', &
& '  fpm-search pixel --verbose                                                    ', &
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
    type(fhash_tbl_t), intent(in) :: tbl
    character(len=*), intent(in) :: pattern
    type(package_t) :: pkg
    logical :: r, r1, r2
    integer :: i
    integer :: num_buckets, num_items
    integer :: j
    type(regex_pattern) :: p

    j = getpat(pattern, p%pat)
    call check(j .ne. ERR, 'Illegal pattern for regex.')

    call tbl%stats(num_buckets, num_items)

    do i = 1, num_items
        call table_get_package(tbl, fhash_key(i), pkg, r)

        r1 = match(pkg%name//char(10), p%pat) .eq. YES
        r2 = match(pkg%description//char(10), p%pat) .eq. YES

        if (r1 .or. r2) then
            print 100, pkg%name, pkg%description
        end if

        100 format(a, ' : ', a)
    end do
end subroutine

subroutine table_info(tbl, pattern)
    type(fhash_tbl_t), intent(in) :: tbl
    character(len=*), intent(in) :: pattern
    type(package_t) :: pkg
    logical :: r, r1, r2
    integer :: i
    integer :: num_buckets, num_items
    character(len=:), allocatable :: s
    integer :: j
    type(regex_pattern) :: p

    j = getpat(pattern, p%pat)
    call check(j .ne. ERR, 'Illegal pattern for regex.')

    call tbl%stats(num_buckets, num_items)

    do i = 1, num_items
        call table_get_package(tbl, fhash_key(i), pkg, r)
        r1 = match(pkg%name//char(10), p%pat) .eq. YES
        r2 = match(pkg%description//char(10), p%pat) .eq. YES

        if (r1 .or. r2) then
            print 100, 'name', pkg%name
            print 100, 'description', pkg%description
            print 100, 'license', pkg%license

            if (pkg%version_count .gt. 0) then
                s = join(pkg%version, ", ")
                print 100, 'version', s
            end if

            if (pkg%author_count .gt. 0) then
                s = join(pkg%author, ", ")
                print 100, 'author', s
            end if

            if (pkg%maintainer_count .gt. 0) then
                s = join(pkg%maintainer, ", ")
                print 100, 'maintainer', s
            end if

            if (pkg%categories_count .gt. 0) then
                s = join(pkg%categories, ", ")
                print 100, 'categories', s
            end if

            if (pkg%keywords_count .gt. 0) then
                s = join(pkg%keywords, ", ")
                print 100, 'keywords', s
            end if

            if (pkg%dependencies_count .gt. 0) then
                s = join(pkg%dependencies, ", ")
                print 100, 'dependencies', s
            end if

            if (pkg%dev_dependencies_count .gt. 0) then
                s = join(pkg%dev_dependencies, ", ")
                print 100, 'dev-dependencies', s
            end if

            print 100, 'homepage', pkg%homepage
            print 100, 'copyright', pkg%copyright
            print 100, 'git', pkg%git
            print 100, 'git-tag', pkg%git_tag
            print *

            100 format(a16, ': ', a)
        end if
    end do
end subroutine

subroutine table_add(tbl, name, tag)
    type(fhash_tbl_t), intent(in) :: tbl
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

end program main

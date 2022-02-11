program main
use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
use package_types, only: package_t
use download_helper, only: download
use os, only: remove, now, fileTime
use fhash, only: fhash_tbl_t, fhash_key, fhash_key_t
use json, only: get_packages
use M_CLI2, only : set_args, lget, arg=>unnamed, get_args
use M_strings, only: join
use config, only: config_t, registry_t, read_config_file

implicit none
character(len=:),allocatable :: help_text(:), version_text(:)
integer :: loop

! Remote registry file
character(len=*), parameter :: registry_url =&
    'https://github.com/fortran-lang/fpm-registry/raw/HEAD/index.json'

character(len=:), allocatable :: alternate_registry_url
character(len=:), allocatable :: remote_registry_url

! Local registry file: '%TEMP%/index.json'
character(len=:), allocatable :: registry_file
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
integer :: n
integer :: ier

call usage()
call set_args(' --registry "null" --force-download:F F', help_text, version_text)
arg_count = size(arg)

if (arg_count .eq. 0) then
   arg = ['']
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
    i = remove(registry_file_c)
    download_ok = download(remote_registry_url, registry_file)
    call ck(download_ok, 'Registry download failed')
end if

call get_packages(registry_file, tbl, r)
call ck(r, 'get_packages() failed')
! call tbl%stats(num_items=num_items)
! print *, 'num_items:', num_items

!
! Download additional registries
!
call get_config_file_home(config_file_home, config_ok)
if (config_ok) then
    call read_config_file(config_file_home, cfg_home)
    call download_registries(cfg_home, time_to_live, lget('force-download'))
    do n = 1, size(cfg_home%registry)
        call get_packages(cfg_home%registry(n)%local_file, tbl, r)
        call ck(r, 'get_packages() failed')
    end do
end if

call get_config_file_etc(config_file_etc, config_ok)
if (config_ok) then
    call read_config_file(config_file_etc, cfg_etc)
    call download_registries(cfg_etc, time_to_live, lget('force-download'))
    do n = 1, size(cfg_etc%registry)
        call get_packages(cfg_etc%registry(n)%local_file, tbl, r)
        call ck(r, 'get_packages() failed')
    end do
end if

if (lget('verbose')) then
    do loop=1, arg_count
        call table_info(tbl, trim(arg(loop)))
    end do
else
    do loop=1, arg_count
        call table_search(tbl, trim(arg(loop)))
    end do
end if

contains

subroutine download_registries(cfg, time_to_live, force)
    type(config_t), intent(inout) :: cfg
    integer, intent(in) :: time_to_live
    logical, intent(in) :: force
    character(len=:,kind=c_char), allocatable :: registry_file_c
    integer :: n
    integer(kind=c_int) :: i
    logical :: download_ok

    if (.not. allocated(cfg%registry)) return

    do n = 1, size(cfg%registry)
        cfg%registry(n)%local_file = get_registry_file(cfg%registry(n)%url)
        registry_file_c = to_c_string(cfg%registry(n)%local_file)

        if (force .or. (abs(now() - fileTime(registry_file_c)) .gt. time_to_live)) then
            i = remove(registry_file_c)
            download_ok = download(cfg%registry(n)%url, cfg%registry(n)%local_file)
            call ck(download_ok, 'Registry download failed')
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
! is_alphanum(c)
! Original from https://github.com/fortran-lang/stdlib
!
pure logical function is_alphanum(c) result(r)
    character(len=1), intent(in) :: c
    r = (c >= '0' .and. c <= '9') .or. (c >= 'a' .and. c <= 'z') .or. (c >= 'A' .and. c <= 'Z')
end function

subroutine ck(condition, msg)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: msg

    if (.not. condition) then
        print *, 'ERROR:', msg
        stop
    end if
end subroutine

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
& 'VERSION:         0.18.0                                              ', &
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
& '                                                                                ', &
& 'SYNOPSIS                                                                        ', &
& '   syntax:                                                                      ', &
& '   fpm-search SEARCH_STRING [--verbose] [--registry URI] [--force-download]     ', &
& '                                                                                ', &
& 'DESCRIPTION                                                                     ', &
& '   Search for and display information describing fpm (Fortran Package Manager)  ', &
& '   packages registered in the fpm repository at                                 ', &
& '   https://github.com/fortran-lang/fpm-registry                                 ', &
& '                                                                                ', &
& 'OPTIONS                                                                         ', &
& '    SEARCH_STRING  A string used to match package descriptions.                 ', &
& '                   It is case-insensitive. The default is "", causing all       ', &
& '                   registered packages to be displayed.                         ', &
& '    --verbose,-V   give more-detailed information about the packages matching   ', &
& '                   SEARCH_STRING.                                               ', &
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
    call tbl%get_raw(k, data, stat)

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
    logical :: r, r0, r1, r2
    integer :: i
    integer :: num_buckets, num_items

    call tbl%stats(num_buckets, num_items)
    r0 = pattern .eq. ""
    r1 = .false.
    r2 = .false.

    do i = 1, num_items
        call table_get_package(tbl, fhash_key(i), pkg, r)

        if (.not. r0) then
            r1 = indexi(pkg%name, pattern) > 0
            r2 = indexi(pkg%description, pattern) > 0
        end if

        if (r0 .or. r1 .or. r2) then
            print 100, pkg%name, pkg%description
        end if

        100 format(a, ' : ', a)
    end do

end subroutine

subroutine table_info(tbl, pattern)
    type(fhash_tbl_t), intent(in) :: tbl
    character(len=*), intent(in) :: pattern
    type(package_t) :: pkg
    logical :: r, r0, r1, r2
    integer :: i
    integer :: num_buckets, num_items
    character(len=:), allocatable :: s

    call tbl%stats(num_buckets, num_items)
    r0 = pattern .eq. ""
    r1=.false.
    r2=.false.

    do i = 1, num_items
        call table_get_package(tbl, fhash_key(i), pkg, r)

        if (.not. r0) then
            r1 = indexi(pkg%name, pattern) > 0
            r2 = indexi(pkg%description, pattern) > 0
        end if

        if (r0 .or. r1 .or. r2) then
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

            if(allocated(pkg%homepage))print 100, 'homepage', pkg%homepage
            print 100, 'copyright', pkg%copyright
            print 100, 'git', pkg%git
            print 100, 'git-tag', pkg%git_tag

            if (pkg%git_tag .ne. 'null') then
                print 200, 'toml', pkg%name, pkg%git, pkg%git_tag
            else
                print 300, 'toml', pkg%name, pkg%git
            end if
            print *

            100 format(a16, ': ', a)
            200 format(a16, ': ', a, ' = { git = "', a, '", tag="', a, '" }')
            300 format(a16, ': ', a, ' = { git = "', a, '" }')
        end if
    end do

end subroutine

!
! Function indexi
!
! Find the position of substring within string ignoring case.
! If substring is not present in string, zero is returned.
! Status: experimental
!
integer function indexi(string, substring) result(r)
    character(len=*), intent(in) :: string
    character(len=*), intent(in) :: substring
    character(len=1) :: s
    character(len=1) :: t
    integer :: opos ! outer position
    integer :: ipos ! inner position

    r = 0
    opos = 1

    if (len(substring) .eq. 0 .or. len(substring) > len(string)) return

    do while (opos <= len(string))
        s = string(opos:opos)
        t = substring(1:1)
        ipos = 1

        do while (.true.)
            if (uchar(s) .ne. uchar(t)) then
                ipos = 0
                exit
            end if

            if ((opos+ipos) > len(string) .or. (ipos+1) > len(substring)) then
                exit
            end if

            s = string(opos+ipos:opos+ipos)
            t = substring(ipos+1:ipos+1)
            ipos = ipos + 1
        end do

        if (ipos .eq. len(substring)) then
            r = opos
            exit
        end if

        opos = opos + 1
    end do
end function

integer function uchar(c) result(r)
    character(len=1), intent(in) :: c
    integer :: uc

    uc = ichar(c)

    ! 'a' = 97, 'z' = 122
    if (uc >= 97 .and. uc <= 122) then
        r = uc - 32
    else
        r = uc
    end if
end function

function to_c_string(f) result(c)
    character(len=*), intent(in) :: f
    character(len=:,kind=c_char), allocatable :: c
    c = f(1:len_trim(f)) // c_null_char
end function

end program main

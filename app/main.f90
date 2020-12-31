module functions
    implicit none
    private
    public get_temp_file
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
    ! random_string(n)
    ! Generates a random_string with n ASCII characters from 'a' to 'z'
    !
    function random_string(n) result(str)
        implicit none
        integer, intent(in) :: n
        real :: r(n)
        character(len=n) :: str
        integer :: i
        integer :: ints(n)

        call random_number(r)
        ints = 97 + floor((122 - 97 + 1) * r)

        do i = 1, n
            str(i:i) = char(ints(i))
        end do
    end function

    !
    ! get_temp_file()
    ! Generates a random filename in the system temporary directory
    ! 
    function get_temp_file() result(tempfile)
        implicit none
        character(len=255) :: tempdir
        character(len=255) :: tempfile
        integer :: length
        integer :: vstatus
        character(len=10) :: s
        character(len=*), parameter :: extension = ".toml"

        s = random_string(10)
        call get_environment_variable("TEMP", tempdir, length, vstatus)

        if (vstatus .eq. 0 .and. is_windows()) then
            tempfile = trim(tempdir) // "\" // s // extension
        else
            tempfile = "/tmp/" // s // extension
        end if
    end function
end module functions

module download_helper
    implicit none
    private
    public download
    contains

    !
    ! download(url, local_file)
    !
    ! Example: download('https://example.com/file', '/tmp/local_file')
    !
    ! This function is a slightly modified version of the original code from
    ! https://github.com/interkosmos/fortran-curl
    !
    logical function download(url, local_file) result(ret)
        use, intrinsic :: iso_c_binding
        use :: curl
        use :: callback_download
        implicit none

        character(len=*), intent(in) :: url
        character(len=255), intent(in), target :: local_file
        character(len=*), parameter  :: DEFAULT_PROTOCOL = 'https'
        integer                      :: rc
        logical                      :: file_exists
        type(c_ptr)                  :: curl_ptr

        inquire(file=trim(local_file), exist=file_exists)

        if (file_exists) then
            print '(3a)', 'Local file "', trim(local_file), '" already exists'
            stop
        end if

        curl_ptr = curl_easy_init()

        if (.not. c_associated(curl_ptr)) then
            stop 'Error: curl_easy_init() failed'
        end if

        ! Set curl options.
        rc = curl_easy_setopt(curl_ptr, CURLOPT_DEFAULT_PROTOCOL, DEFAULT_PROTOCOL // c_null_char)
        rc = curl_easy_setopt(curl_ptr, CURLOPT_URL,              url // c_null_char)
        rc = curl_easy_setopt(curl_ptr, CURLOPT_FOLLOWLOCATION,   int( 1, kind=8))
        rc = curl_easy_setopt(curl_ptr, CURLOPT_TIMEOUT,          int(10, kind=8))
        rc = curl_easy_setopt(curl_ptr, CURLOPT_NOSIGNAL,         int( 1, kind=8))
        rc = curl_easy_setopt(curl_ptr, CURLOPT_CONNECTTIMEOUT,   int(10, kind=8))
        rc = curl_easy_setopt(curl_ptr, CURLOPT_WRITEFUNCTION,    c_funloc(response_callback))
        rc = curl_easy_setopt(curl_ptr, CURLOPT_WRITEDATA,        c_loc(local_file))

        ! print '(5a)', 'Saving "', url, '" to file "', trim(local_file), '" ...'

        ! Send request.
        if (curl_easy_perform(curl_ptr) /= CURLE_OK) then
            print '(a)', 'download() error: curl_easy_perform() failed'
        else
            ret = .true.
        end if

        call curl_easy_cleanup(curl_ptr)
    end function

end module download_helper

program main
    use M_strings, only: crop
    use functions, only: get_temp_file
    use download_helper, only: download
    use fhash, only: fhash_tbl_t, fhash_key, fhash_key_t
    implicit none

    character(len=*), parameter :: fpm_registry_url =&
    'https://raw.githubusercontent.com/fortran-lang/fpm-registry/master/registry.toml'

    logical :: r
    character(len=1024) :: line = ''
    character(len=:), allocatable :: strout
    character(len=:), allocatable :: package
    character(len=:), allocatable :: url
    integer :: ios
    integer :: i, u
    integer :: pos1, pos2
    character(len=255), allocatable :: local_file
    character(len=*), parameter     :: fpm_toml = "fpm.toml"
    logical                         :: file_exists, dependencies_section_exists

    type package_t
        character(:), allocatable :: name
        character(:), allocatable :: url
    end type package_t

    type(fhash_tbl_t) :: tbl
    type(package_t)   :: pkg
    integer           :: key

    local_file = get_temp_file()
    r = download(fpm_registry_url, local_file)

    if (.not. r) then
        stop
    end if

    key = 1
    open(newunit=u, file=local_file, status="old")

    do
        read(u, '(a)', iostat=ios) line
        if (ios .ne. 0) exit

        strout = crop(line)

        pos1 = index(strout, "[")
        pos2 = index(strout, "]")

        if (pos1 .eq. 1 .and.  pos2 .gt. 2) then
            package = strout(2:pos2-1)
        end if

        pos1 = index(strout, "{")
        pos2 = index(strout, "}")

        if (pos1 .gt. 1 .and.  pos2 .gt. 2) then
            url = strout(pos1:pos2)
            pkg%name = package
            pkg%url = url
            call tbl%set(fhash_key(key), value=pkg)
            key = key + 1
        end if
    end do

    close(u)

    ! Print the Package Table
    print 100, 'N', 'Package Name', 'Package URL'
    write(*,'(80("-"))')

    do i = 1, key-1
        call fhash_get_package(tbl, fhash_key(i), pkg)
        print 200, i, pkg%name, pkg%url
    end do

    100 format(a4, a20, '   ', a)
    200 format(i4, a20, ' = ', a)

    ! Check if fpm.toml exists
    inquire(file=fpm_toml, exist=file_exists)
    if (.not. file_exists) then
        print *, 'File "', fpm_toml, '" does not exist. Bye.'
        stop
    end if

    ! Check if [dependencies] section exists
    open(newunit=u, file=fpm_toml, status="old")
    do
        read(u, '(a)', iostat=ios) line
        if (ios .ne. 0) exit

        strout = crop(line)
        pos1 = index(strout, "[dependencies]")

        if (pos1 .ne. 0) then
            dependencies_section_exists = .true.
            exit
        end if
    end do
    close(u)

    if (dependencies_section_exists) then
        print *
        print *, '[dependencies] section found in "', fpm_toml, '". Bye.'
        stop
    end if

    ! Ask user
    write(*,'(80("-"))')
    print *, 'Which package do you want to add?'
    write (*,'(a)', advance="no") 'Enter the package number (press 0 to exit): '
    read *, i

    if (i .eq. 0) then
        stop
    end if

    if (i .lt. 0 .or. i .gt. key - 1) then
        print *, 'Error: number out of range'
        stop
    end if

    call fhash_get_package(tbl, fhash_key(i), pkg)

    ! Write changes to fpm.toml
    print *, 'Inserting Package ', pkg%name, ' ...'
    open(newunit=u, file=fpm_toml, position="append", status="old")
    write(u, *) '[dependencies]'
    write(u, *) pkg%name, ' = ', pkg%url
    close(u)
    print *, 'Done.'

    contains

    !
    ! Define custom getter for package_t type
    ! Based on original code from
    ! https://lkedward.github.io/fhash/page/2-derived-type-demo/index.html
    !
    subroutine fhash_get_package(tbl, k, package)
        type(fhash_tbl_t), intent(in) :: tbl
        class(fhash_key_t), intent(in) :: k
        type(package_t), intent(out) :: package
        integer :: stat
        class(*), allocatable :: data

        call tbl%get(k, data, stat)

        if (stat /= 0) then
            print *, 'Error key not found', stat! Error handling: key not found
            stop
        end if

        select type(d=>data)
            type is (package_t)
            package = d
            class default
            ! Error handling: found wrong type
            print *, 'error'
        end select
    end subroutine fhash_get_package

    ! FIXME
    ! remove(local_file)
end program main

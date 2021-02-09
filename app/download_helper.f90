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
        character(len=*), intent(in), target :: local_file
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
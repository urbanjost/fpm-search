! download.f90
!
! Downloads a remote file, using libcurl.
!
! Author:  Philipp Engel
! Licence: ISC
! Source: https://github.com/interkosmos/fortran-curl/blob/master/examples/download/download.f90
!
!
module callback_download
    use :: curl, only: c_f_str_ptr
    implicit none
    private
    public :: response_callback
contains
    ! static size_t callback(char *ptr, size_t size, size_t nmemb, void *data)
    function response_callback(ptr, size, nmemb, data) bind(c)
        !! Callback function for `CURLOPT_WRITEFUNCTION` that appends the
        !! response chunk in `ptr` to the given file with file name in pointer
        !! `data`.
        !!
        !! This callback function might be called several times by libcurl,
        !! passing in more chunks of the response.
        use, intrinsic :: iso_c_binding, only: c_associated, c_f_pointer, c_ptr, c_size_t
        type(c_ptr),            intent(in), value :: ptr               !! C pointer to a chunk of the response.
        integer(kind=c_size_t), intent(in), value :: size              !! Always 1.
        integer(kind=c_size_t), intent(in), value :: nmemb             !! Size of the response chunk.
        type(c_ptr),            intent(in), value :: data              !! C pointer to argument passed by caller.
        integer(kind=c_size_t)                    :: response_callback !! Function return value.
        character(len=255), pointer               :: file_name         !! File to store response to.
        character(len=:), allocatable             :: chunk             !! Response chunk.
        integer(kind=8)                           :: fu, rc

        response_callback = int(0, kind=c_size_t)

        if (.not. c_associated(ptr)) return
        if (.not. c_associated(data)) return

        call c_f_pointer(data, file_name)
        if (len_trim(file_name) == 0) return

        allocate (character(len=nmemb) :: chunk)
        call c_f_str_ptr(ptr, chunk)

        open (access   = 'stream', &
              action   = 'write', &
              file     = trim(file_name), &
              form     = 'unformatted', &
              iostat   = rc, &
              newunit  = fu, &
              position = 'append', &
              status   = 'unknown')
        if (rc /= 0) return
        write (fu) chunk
        close (fu)

        deallocate (chunk)
        response_callback = nmemb
    end function response_callback
end module callback_download

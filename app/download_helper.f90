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
    logical function download(url, local_file) result(ret)
        character(len=*), intent(in) :: url
        character(len=*), intent(in) :: local_file
        character(len=:), allocatable :: cmd
        integer :: i

        cmd = "curl -Ls "//char(34)//url//char(34)//" -o "//char(34)//local_file//char(34)
        call execute_command_line(cmd, exitstat=i)
        ret = i .eq. 0
    end function

end module download_helper
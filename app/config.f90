module config
implicit none
public :: config_t, registry_t, read_config_file

type registry_t
    character(len=:), allocatable :: label
    character(len=:), allocatable :: url
    character(len=:), allocatable :: local_file
    logical :: enable
end type registry_t

type config_t
    character(len=:), allocatable :: filename
    type(registry_t), allocatable :: registry(:)
end type config_t

contains

subroutine read_config_file(filename, cfg)
    character(len=*), intent(in) :: filename
    type(config_t), intent(out)  :: cfg

    character(len=1024) :: line = ''
    integer :: ios
    integer :: u
    integer :: n
    integer :: pos

    n = 0
    cfg%filename = filename

    !
    ! Discover the number n of label=URL
    !
    open(newunit=u, file=filename, status="old")
    do
        read(u, '(a)', iostat=ios) line
        if (ios .ne. 0) exit
        if (line(1:1) .eq. "#") cycle

        pos = index(line, "=")

        if (pos .gt. 1) then
            n = n + 1
        end if
    end do
    close(u)

    if (n .eq. 0) return

    ! Allocate n items
    allocate(cfg%registry(n))

    ! Fill
    n = 1
    open(newunit=u, file=filename, status="old")
    do
        read(u, '(a)', iostat=ios) line
        if (ios .ne. 0) exit
        if (line(1:1) .eq. "#") cycle

        pos = index(line, "=")

        if (pos .gt. 1) then
            cfg%registry(n)%label = trim(line(1:pos-1))
            cfg%registry(n)%url = trim(line(pos+1:))
            n = n + 1
        end if
    end do
    close(u)

end subroutine

subroutine dump_config(cfg)
    type(config_t), intent(in)  :: cfg
    integer :: u
    integer :: n

    if (.not. allocated(cfg%registry)) return
    print *, cfg%filename

    do n = 1, size(cfg%registry)
        print *, cfg%registry(n)%label
        print *, cfg%registry(n)%url
    end do

end subroutine
end module

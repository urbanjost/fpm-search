module c_util
use, intrinsic :: iso_c_binding, only: c_size_t, c_ptr, c_char, c_null_char, c_f_pointer
implicit none
public :: c_strlen, to_c_string, to_fortran_string

interface
    integer(c_size_t) function c_strlen(s) bind(c, name="strlen")
        import c_size_t, c_ptr
        type(c_ptr), intent(in), value :: s
    end function
end interface

contains

function to_c_string(f) result(c)
    character(len=*), intent(in) :: f
    character(len=:,kind=c_char), allocatable :: c
    integer :: f_len

    f_len = len_trim(f)
    c = f(1:f_len) // c_null_char
end function

!
! to_fortran_string
! Based on original version written by @ivanpribec
!
function to_fortran_string(cptr) result(str)
    character(len=:,kind=c_char), allocatable :: str
    type(c_ptr) :: cptr
    integer(c_size_t) :: n

    n = c_strlen(cptr)
    allocate(character(len=n,kind=c_char) :: str)

    block
      character(len=n,kind=c_char), pointer :: s
      call c_f_pointer(cptr, s) ! Recovers a view of the C string
      str = s                   ! Copies the string contents
    end block
end function

end module
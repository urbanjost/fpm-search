module json
use, intrinsic :: iso_c_binding, only: c_ptr, c_funptr, c_int, c_char
implicit none
interface

    function parse_json(file, func, fortran_ptr) result(r) bind(c, name="parse_json")
        import c_ptr, c_funptr, c_int, c_char
        integer(kind=c_int) :: r
        character(kind=c_char), intent(in) :: file(*)
        type(c_funptr), intent(in), value :: func
        type(c_ptr), intent(in), value :: fortran_ptr
    end function

end interface

end module

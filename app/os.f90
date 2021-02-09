module os
use, intrinsic :: iso_c_binding, only: c_int, c_char, c_int64_t
implicit none

interface
    function fileTime(f) result(r) bind(c, name="fileTime")
        import c_int64_t, c_char
        integer(kind=c_int64_t) :: r
        character(kind=c_char), intent(in) :: f(*)
    end function

    function now() result(r) bind(c, name="now")
        import c_int64_t
        integer(kind=c_int64_t) :: r
    end function

    function remove(f) result(r) bind(c, name="remove")
        import c_int, c_char
        integer(kind=c_int) :: r
        character(kind=c_char), intent(in) :: f(*)
    end function
end interface

end module
module pcre_helper
use, intrinsic :: iso_c_binding, only: c_ptr, c_char, c_int, c_null_ptr
use fortran_pcre, only: pcre_compile, pcre_exec, pcre_free
use c_util, only: to_c_string
implicit none

contains

function regex(pattern, options) result(r)
    type(c_ptr) :: r
    character(len=*), intent(in) :: pattern
    character(len=:,kind=c_char), allocatable :: pattern_c
    integer(kind=c_int), intent(in), optional :: options
    integer(kind=c_int) :: opts
    type(c_ptr) :: errptr
    integer(c_int), target :: erroffset

    opts = 0

    if (present(options)) then
        opts = options
    end if

    pattern_c = to_c_string(pattern)
    r = pcre_compile(pattern_c, opts, errptr, erroffset, c_null_ptr)
end function

function match(re, subject) result(r)
    logical :: r
    character(len=*), intent(in) :: subject
    character(len=:,kind=c_char), allocatable :: subject_c
    type(c_ptr), intent(in) :: re
    type(c_ptr) :: errptr
    integer(c_int), target :: erroffset
    integer, dimension(1:3) :: vec

    subject_c = to_c_string(subject)
    r = pcre_exec(re, c_null_ptr, subject_c, len_trim(subject_c)-1, 0, 0, vec, size(vec)) .gt. 0
end function

end module
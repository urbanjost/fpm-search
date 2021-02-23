module package_types
use, intrinsic :: iso_c_binding, only: c_ptr, c_char, c_int
implicit none
public :: fpm_package_t, package_t

type, bind(c) :: fpm_package_t
    type(c_ptr) name
    type(c_ptr) version
    type(c_ptr) license
    type(c_ptr) copyright
    type(c_ptr) git
    type(c_ptr) git_tag
    type(c_ptr) description
    type(c_ptr) homepage

    type(c_ptr) author
    integer(kind=c_int) author_count

    type(c_ptr) maintainer
    integer(kind=c_int) maintainer_count

    type(c_ptr) categories
    integer(kind=c_int) categories_count

    type(c_ptr) keywords
    integer(kind=c_int) keywords_count
end type

type package_t
    character(len=:, kind=c_char), allocatable :: name
    character(len=:, kind=c_char), allocatable :: version
    character(len=:, kind=c_char), allocatable :: license
    character(len=64, kind=c_char), allocatable :: author(:)
    character(len=64, kind=c_char), allocatable :: maintainer(:)
    character(len=64, kind=c_char), allocatable :: categories(:)
    character(len=64, kind=c_char), allocatable :: keywords(:)
    character(len=:, kind=c_char), allocatable :: copyright
    character(len=:, kind=c_char), allocatable :: git
    character(len=:, kind=c_char), allocatable :: git_tag
    character(len=:, kind=c_char), allocatable :: description
    character(len=:, kind=c_char), allocatable :: homepage

    integer(kind=c_int) author_count
    integer(kind=c_int) maintainer_count
    integer(kind=c_int) categories_count
    integer(kind=c_int) keywords_count
end type package_t

end module

module package_types
use, intrinsic :: iso_c_binding, only: c_ptr, c_char
implicit none
public :: fpm_package_t, package_t

type, bind(c) :: fpm_package_t
    type(c_ptr) name
    type(c_ptr) version
    type(c_ptr) license
    type(c_ptr) author
    type(c_ptr) maintainer
    type(c_ptr) copyright
    type(c_ptr) git
    type(c_ptr) git_tag
    type(c_ptr) description
end type

type package_t
    character(len=:, kind=c_char), allocatable :: name
    character(len=:, kind=c_char), allocatable :: version
    character(len=:, kind=c_char), allocatable :: license
    character(len=:, kind=c_char), allocatable :: author
    character(len=:, kind=c_char), allocatable :: maintainer
    character(len=:, kind=c_char), allocatable :: copyright
    character(len=:, kind=c_char), allocatable :: git
    character(len=:, kind=c_char), allocatable :: git_tag
    character(len=:, kind=c_char), allocatable :: description
end type package_t

end module

module package_types
implicit none
public :: package_t

type package_t
    character(len=:),  allocatable :: name
    character(len=12), allocatable :: version(:)
    character(len=:),  allocatable :: license
    character(len=64), allocatable :: author(:)
    character(len=200), allocatable :: maintainer(:)
    character(len=64), allocatable :: categories(:)
    character(len=64), allocatable :: keywords(:)
    character(len=64), allocatable :: dependencies(:)
    character(len=64), allocatable :: dev_dependencies(:)
    character(len=:),  allocatable :: copyright
    character(len=:),  allocatable :: git
    character(len=:),  allocatable :: git_tag
    character(len=:),  allocatable :: description
    character(len=:),  allocatable :: homepage

    integer :: version_count
    integer :: author_count
    integer :: maintainer_count
    integer :: categories_count
    integer :: keywords_count
    integer :: dependencies_count
    integer :: dev_dependencies_count
end type package_t

end module

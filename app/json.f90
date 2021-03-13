module json
use json_module, IK => json_IK
use json_kinds
use package_types
use fhash, only: fhash_tbl_t, fhash_key
use stdlib_error, only: check
implicit none
public :: get_packages

contains

subroutine get_packages(filename, tbl, ret)
    character(len=*), intent(in) :: filename
    type(fhash_tbl_t), intent(out) :: tbl
    logical, intent(out) :: ret
    type(json_core) :: json
    type(json_value), pointer :: f, packages, package, version
    character(kind=json_CK,len=:), allocatable :: str
    integer(kind=IK) :: i, j, latest_id
    integer(kind=IK) :: num_packages
    type(package_t)  :: pkg
    logical :: found

    ret = .false.

    call json%initialize()
    call json%load(filename, f)
    call json%get(f, 'packages', packages, found)

    call check(found, 'packages node not found.');
    num_packages = json%count(packages)

    do i = 1, num_packages
        latest_id = 0
        call json%get_child(packages, i, package)
        call json%info(package, n_children=pkg%version_count, name=str)

        pkg%name = str
        allocate(pkg%version(pkg%version_count))

        ! Collect versions
        do j = 1, pkg%version_count
            call json%get_child(package, j, version)
            call json%info(version, name=str)
            pkg%version(j) = str

            if (str .eq. 'latest') then
                latest_id = j
                call fill(json, version, pkg)
            end if

            if (j .eq. pkg%version_count .and. latest_id .eq. 0) then
                call fill(json, version, pkg)
            end if
        end do

        call tbl%set(fhash_key(i), value=pkg)

        deallocate(pkg%version)
        if (allocated(pkg%author)) deallocate(pkg%author)
        if (allocated(pkg%maintainer)) deallocate(pkg%maintainer)
        if (allocated(pkg%keywords)) deallocate(pkg%keywords)
        if (allocated(pkg%categories)) deallocate(pkg%categories)
        if (allocated(pkg%dependencies)) deallocate(pkg%dependencies)
        if (allocated(pkg%dev_dependencies)) deallocate(pkg%dev_dependencies)
    end do

    call json%destroy()
    ret = .not. json%failed()
end subroutine

subroutine fill(json, p, pkg)
    type(json_core), intent(inout) :: json
    type(json_value), intent(in), pointer :: p
    type(package_t), intent(inout) :: pkg
    type(json_value), pointer :: q
    character(kind=json_CK,len=:), allocatable :: str
    logical :: found

    call json%get(p, 'description', str, found)
    if (found) pkg%description = str

    call json%get(p, 'copyright', str, found)
    if (found) pkg%copyright = str

    call json%get(p, 'homepage', str, found)
    if (found) pkg%homepage = str

    call json%get(p, 'git', str, found)
    if (found) pkg%git = str

    call json%get(p, 'git-tag', str, found)
    if (found) pkg%git_tag = str

    call json%get(p, 'license', str, found)
    if (found) pkg%license = str

    call json%get(p, 'author', str, found)
    if (found) then
        pkg%author_count = 1
        allocate(pkg%author(1))
        pkg%author(1) = str
    else
        call json%get(p, 'author', q, found)
        if (found) call fill_authors(json, q, pkg)
    end if

    call json%get(p, 'maintainer', str, found)
    if (found) then
        pkg%maintainer_count = 1
        allocate(pkg%maintainer(1))
        pkg%maintainer(1) = str
    else
        call json%get(p, 'maintainer', q, found)
        if (found) call fill_maintainers(json, q, pkg)
    end if

    call json%get(p, 'keywords', q, found)
    if (found) call fill_keywords(json, q, pkg)

    call json%get(p, 'categories', q, found)
    if (found) call fill_categories(json, q, pkg)

    call json%get(p, 'dependencies', q, found)
    if (found) call fill_dependencies(json, q, pkg)

    call json%get(p, 'dev-dependencies', q, found)
    if (found) call fill_dev_dependencies(json, q, pkg)

    if (allocated(str)) deallocate(str)
end subroutine

subroutine fill_keywords(json, p, pkg)
    type(json_core), intent(inout) :: json
    type(json_value), intent(in), pointer :: p
    type(package_t), intent(inout) :: pkg
    type(json_value), pointer :: q
    integer(kind=IK) :: i
    character(kind=json_CK,len=:), allocatable :: str

    pkg%keywords_count = json%count(p)
    allocate(pkg%keywords(pkg%keywords_count))

    do i = 1, pkg%keywords_count
        call json%get_child(p, i, q)
        call json%info(q, name=str)
        pkg%keywords(i) = str
    end do

    if (allocated(str)) deallocate(str)
end subroutine

subroutine fill_categories(json, p, pkg)
    type(json_core), intent(inout) :: json
    type(json_value), intent(in), pointer :: p
    type(package_t), intent(inout) :: pkg
    type(json_value), pointer :: q
    integer(kind=IK) :: i
    character(kind=json_CK,len=:), allocatable :: str

    pkg%categories_count = json%count(p)
    allocate(pkg%categories(pkg%categories_count))

    do i = 1, pkg%categories_count
        call json%get_child(p, i, q)
        call json%info(q, name=str)
        pkg%categories(i) = str
    end do

    if (allocated(str)) deallocate(str)
end subroutine

subroutine fill_maintainers(json, p, pkg)
    type(json_core), intent(inout) :: json
    type(json_value), intent(in), pointer :: p
    type(package_t), intent(inout) :: pkg
    type(json_value), pointer :: q
    integer(kind=IK) :: i
    character(kind=json_CK,len=:), allocatable :: str

    pkg%maintainer_count = json%count(p)
    allocate(pkg%maintainer(pkg%maintainer_count))

    do i = 1, pkg%maintainer_count
        call json%get_child(p, i, q)
        call json%get(q, '@', str)
        pkg%maintainer(i) = str
    end do

    if (allocated(str)) deallocate(str)
end subroutine

subroutine fill_authors(json, p, pkg)
    type(json_core), intent(inout) :: json
    type(json_value), intent(in), pointer :: p
    type(package_t), intent(inout) :: pkg
    type(json_value), pointer :: q
    integer(kind=IK) :: i
    character(kind=json_CK,len=:), allocatable :: str

    pkg%author_count = json%count(p)
    allocate(pkg%author(pkg%author_count))

    do i = 1, pkg%author_count
        call json%get_child(p, i, q)
        call json%get(q, '@', str)
        pkg%author(i) = str
    end do

    if (allocated(str)) deallocate(str)
end subroutine

subroutine fill_dependencies(json, p, pkg)
    type(json_core), intent(inout) :: json
    type(json_value), intent(in), pointer :: p
    type(package_t), intent(inout) :: pkg
    type(json_value), pointer :: q
    integer(kind=IK) :: i
    character(kind=json_CK,len=:), allocatable :: str

    pkg%dependencies_count = json%count(p)
    allocate(pkg%dependencies(pkg%dependencies_count))

    do i = 1, pkg%dependencies_count
        call json%get_child(p, i, q)
        call json%info(q, name=str)
        pkg%dependencies(i) = str
    end do

    if (allocated(str)) deallocate(str)
end subroutine

subroutine fill_dev_dependencies(json, p, pkg)
    type(json_core), intent(inout) :: json
    type(json_value), intent(in), pointer :: p
    type(package_t), intent(inout) :: pkg
    type(json_value), pointer :: q
    integer(kind=IK) :: i
    character(kind=json_CK,len=:), allocatable :: str

    pkg%dev_dependencies_count = json%count(p)
    allocate(pkg%dev_dependencies(pkg%dev_dependencies_count))

    do i = 1, pkg%dev_dependencies_count
        call json%get_child(p, i, q)
        call json%info(q, name=str)
        pkg%dev_dependencies(i) = str
    end do

    if (allocated(str)) deallocate(str)
end subroutine

end module

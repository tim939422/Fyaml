program test_fyaml
    use fyaml
    use yaml_parser, only: yaml_node
    use yaml_types
    implicit none

    type(fyaml_doc) :: doc
    type(yaml_value) :: val, person
    logical :: success
    character(len=:), allocatable, dimension(:) :: keys  ! Remove initialization
    character(len=*), parameter :: source_dir = SOURCE_DIR
    character(len=*), parameter :: filename = source_dir //"/"//"example.yaml"

    success = .false.
    print '(A,1X,A)', 'Loading file:', trim(filename)
    call doc%load(filename, success)
    if (.not. success) then
      write(error_unit,*) 'Error: Failed to load YAML file'
      error stop
    end if

    ! Initialize keys as empty array
    allocate(character(len=1)::keys(0))

    ! Get person node
    person = doc%get("person")

    ! Get all person keys
    if (associated(person%node%children)) then
        if (allocated(keys)) deallocate(keys)
        keys = get_keys_from_node(person%node%children)
    endif
    print *, "Keys:", keys

    ! Get specific values
    val = person%get("name")
    print *, "Name:", val%get_str()

    val = person%get("age")
    print *, "Age:", val%get_int()

    val = person%get("skills")
    if (val%is_sequence()) then
        print *, "Skills:", get_sequence_as_strings(val%node)
    endif

contains
    function get_keys_from_node(node) result(keys)
        type(yaml_node), pointer, intent(in) :: node
        character(len=:), allocatable, dimension(:) :: keys
        type(yaml_node), pointer :: current
        integer :: count, i

        ! Count keys
        count = 0
        current => node
        do while (associated(current))
            count = count + 1
            current => current%next
        end do

        ! Allocate and fill keys
        allocate(character(len=32) :: keys(count))
        current => node
        i = 1
        do while (associated(current))
            keys(i) = current%key
            i = i + 1
            current => current%next
        end do
    end function

    function get_sequence_as_strings(node) result(items)
        type(yaml_node), pointer, intent(in) :: node
        character(len=:), allocatable, dimension(:) :: items
        type(yaml_node), pointer :: current
        integer :: count, i

        ! Count sequence items
        count = 0
        current => node%children
        do while (associated(current))
            count = count + 1
            current => current%next
        end do

        ! Allocate and fill items
        allocate(character(len=32) :: items(count))
        current => node%children
        i = 1
        do while (associated(current))
            items(i) = current%value
            i = i + 1
            current => current%next
        end do
    end function

end program test_fyaml

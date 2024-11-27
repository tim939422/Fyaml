module fyaml
    use yaml_parser
    use yaml_types
    implicit none

    private
    public :: fyaml_doc, yaml_value, yaml_dict, yaml_pair, yaml_wrapper

    ! Constants for value types
    integer, parameter :: TYPE_STRING = 1
    integer, parameter :: TYPE_REAL = 2
    integer, parameter :: TYPE_INTEGER = 3
    integer, parameter :: TYPE_BOOLEAN = 4
    integer, parameter :: TYPE_NULL = 5
    integer, parameter :: TYPE_SEQUENCE = 6
    integer, parameter :: TYPE_DICT = 7

    ! Value type definition
    type :: yaml_value
        character(len=:), allocatable :: str_val
        real :: real_val
        integer :: int_val
        logical :: bool_val
        logical :: is_null = .false.
        character(len=:), allocatable, dimension(:) :: sequence
        integer :: value_type = 0
        type(yaml_dict), pointer :: dict_val => null()
    contains
        procedure :: get => get_nested_value
    end type

    ! Dictionary entry type
    type :: yaml_pair
        character(len=:), allocatable :: key
        type(yaml_value) :: value
        type(yaml_dict), pointer :: nested => null()
        type(yaml_pair), pointer :: next => null()
    end type

    ! Dictionary type
    type :: yaml_dict
        type(yaml_pair), pointer :: first => null()
        integer :: count = 0
    contains
        procedure :: get => get_value
        procedure :: set => set_value
        procedure :: keys => get_keys
    end type

    ! Document type
    type :: fyaml_doc
        type(yaml_dict) :: root
    contains
        procedure :: load => load_yaml_doc
    end type

    ! Wrapper type
    type :: yaml_wrapper
        type(yaml_document) :: raw_doc
        type(fyaml_doc) :: doc
        type(yaml_dict) :: dict  ! Added missing member
    contains
        procedure :: load => load_yaml_wrapper
        procedure :: convert => convert_to_dict
    end type

contains
    ! Separate load procedures for each type
    subroutine load_yaml_doc(this, filename)
        class(fyaml_doc), intent(inout) :: this
        character(len=*), intent(in) :: filename
        type(yaml_document), allocatable :: parsed_docs(:)

        call parse_yaml(filename, parsed_docs)
        if (allocated(parsed_docs)) then
            call convert_node_to_dict(parsed_docs(1)%root, this%root)
            deallocate(parsed_docs)
        endif
    end subroutine

    subroutine load_yaml_wrapper(this, filename)
        class(yaml_wrapper), intent(inout) :: this
        character(len=*), intent(in) :: filename

        call this%doc%load(filename)
        call this%convert()
    end subroutine

    ! Added parse_sequence function
    function fyaml_parse_sequence(str) result(seq)
        character(len=*), intent(in) :: str
        character(len=:), allocatable, dimension(:) :: seq
        integer :: n, i, start, end

        n = count(transfer(str, 'a', len(str)) == ',') + 1
        allocate(character(len=32) :: seq(n))

        start = 2  ! Skip '['
        do i = 1, n
            end = index(str(start:), ',') - 1
            if (end < 0) end = len_trim(str) - 1  ! Last item
            seq(i) = trim(adjustl(str(start:start+end-1)))
            start = start + end + 2
        end do
    end function fyaml_parse_sequence

    recursive subroutine convert_node_to_dict(node, dict)
        type(yaml_node), pointer, intent(in) :: node
        type(yaml_dict), intent(inout) :: dict
        type(yaml_pair), pointer :: new_pair
        type(yaml_node), pointer :: current

        current => node
        do while (associated(current))
            allocate(new_pair)
            new_pair%key = current%key

            if (associated(current%children)) then
                allocate(new_pair%nested)
                new_pair%value%value_type = TYPE_DICT
                new_pair%value%dict_val => new_pair%nested
                call convert_node_to_dict(current%children, new_pair%nested)
            else
                ! Set value based on content
                if (is_sequence(current%value)) then
                    new_pair%value%value_type = TYPE_SEQUENCE
                    new_pair%value%sequence = fyaml_parse_sequence(current%value)
                else if (is_null(current%value)) then
                    new_pair%value%value_type = TYPE_NULL
                    new_pair%value%is_null = .true.
                else if (is_boolean(current%value)) then
                    new_pair%value%value_type = TYPE_BOOLEAN
                    new_pair%value%bool_val = current%value == 'true'  ! Direct comparison instead of parse_sequence
                else if (is_number(current%value)) then
                    if (index(current%value, '.') > 0) then
                        new_pair%value%value_type = TYPE_REAL
                        read(current%value, *) new_pair%value%real_val
                    else
                        new_pair%value%value_type = TYPE_INTEGER
                        read(current%value, *) new_pair%value%int_val
                    endif
                else
                    new_pair%value%value_type = TYPE_STRING
                    new_pair%value%str_val = current%value
                endif
            endif

            if (.not. associated(dict%first)) then
                dict%first => new_pair
            else
                new_pair%next => dict%first
                dict%first => new_pair
            endif
            dict%count = dict%count + 1

            current => current%next
        end do
    end subroutine

    subroutine set_value(this, key, value)
        class(yaml_dict), intent(inout) :: this
        character(len=*), intent(in) :: key
        type(yaml_value), intent(in) :: value
        type(yaml_pair), pointer :: current, new_pair

        ! Check if key exists
        current => this%first
        do while (associated(current))
            if (current%key == key) then
                ! Update existing value
                current%value = value
                return
            endif
            current => current%next
        end do

        ! Key doesn't exist, create new pair
        allocate(new_pair)
        new_pair%key = key
        new_pair%value = value

        ! Add to beginning of list
        new_pair%next => this%first
        this%first => new_pair
        this%count = this%count + 1
    end subroutine set_value

    ! Helper functions
    function is_sequence(str) result(res)
        character(len=*), intent(in) :: str
        logical :: res
        res = str(1:1) == '['
    end function

    function is_null(str) result(res)
        character(len=*), intent(in) :: str
        logical :: res
        res = trim(str) == 'null'
    end function

    function is_boolean(str) result(res)
        character(len=*), intent(in) :: str
        logical :: res
        res = trim(str) == 'true' .or. trim(str) == 'false'
    end function

    function is_number(str) result(res)
        character(len=*), intent(in) :: str
        logical :: res
        res = verify(trim(str), '0123456789.-') == 0
    end function

    function get_value(this, key) result(val)
        class(yaml_dict), intent(in) :: this
        character(len=*), intent(in) :: key
        type(yaml_value) :: val
        type(yaml_pair), pointer :: current

        current => this%first
        do while (associated(current))
            if (current%key == key) then
                val = current%value
                return
            endif
            current => current%next
        end do
    end function

    function get_keys(this) result(keys)
        class(yaml_dict), intent(in) :: this
        character(len=:), allocatable, dimension(:) :: keys
        type(yaml_pair), pointer :: current
        integer :: i

        allocate(character(len=32) :: keys(this%count))
        current => this%first
        i = 1
        do while (associated(current))
            keys(i) = current%key
            i = i + 1
            current => current%next
        end do
    end function

    subroutine convert_to_dict(this)
        class(yaml_wrapper), intent(inout) :: this

        ! Convert from raw yaml_document to yaml_dict structure
        if (associated(this%raw_doc%root)) then
            call convert_node_to_dict(this%raw_doc%root, this%dict)
        endif
    end subroutine convert_to_dict

    recursive function get_nested_value(self, key) result(val)
        class(yaml_value), intent(in) :: self
        character(len=*), intent(in) :: key
        type(yaml_value) :: val
        integer :: dot_pos
        character(len=:), allocatable :: first_key, rest_path

        dot_pos = index(key, ".")

        if (dot_pos > 0) then
            first_key = key(1:dot_pos-1)
            rest_path = key(dot_pos+1:)

            if (associated(self%dict_val)) then
                val = self%dict_val%get(first_key)
                if (.not. val%is_null .and. allocated(rest_path)) then
                    val = val%get(rest_path)
                endif
            else
                val%is_null = .true.
            endif
        else
            if (associated(self%dict_val)) then
                val = self%dict_val%get(key)
            else
                val%is_null = .true.
            endif
        endif
    end function
end module
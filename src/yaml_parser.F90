module yaml_parser
  use yaml_types
  implicit none
contains
  subroutine parse_yaml(filename, docs)
    character(len=*), intent(in) :: filename
    type(yaml_document), allocatable, intent(out) :: docs(:)
    character(len=256) :: line
    integer :: unit, ios, doc_count
    type(yaml_document), pointer :: current_doc

    ! Open the YAML file for reading
    open(unit=unit, file=filename, status='old', action='read', iostat=ios)
    if (ios /= 0) then
      print *, 'Error opening file: ', filename
      return
    end if

    doc_count = 0
    allocate(docs(0))

    ! Read the file line by line
    do
      read(unit, '(A)', iostat=ios) line
      if (ios /= 0) exit
      if (trim(line) == '') cycle  ! Skip empty lines

      ! Check for document start marker
      if (trim(line) == '---') then
        doc_count = doc_count + 1
        allocate(docs(doc_count))
        current_doc => docs(doc_count)
        call initialize_document(current_doc)
        cycle
      end if

      ! Check for document end marker
      if (trim(line) == '...') then
        current_doc => null()
        cycle
      end if

      ! Parse the line if within a document
      if (associated(current_doc)) then
        call parse_line(trim(line), current_doc)
      end if
    end do

    close(unit)
  end subroutine parse_yaml

  subroutine initialize_document(doc)
    type(yaml_document), intent(out) :: doc
    doc%root => null()
    doc%anchors => null()
  end subroutine initialize_document

  subroutine parse_line(line, doc)
    character(len=*), intent(in) :: line
    type(yaml_document), intent(inout) :: doc
    type(yaml_node), pointer :: new_node, current_node, parent_node
    integer :: pos, current_indent, previous_indent
    real :: r_value
    integer :: i_value
    logical :: l_value, is_real, is_int, is_logical

    ! Remove inline comments
    pos = index(line, '#')
    if (pos > 0) then
      line = trim(line(1:pos-1))
    end if

    ! Determine the indentation level
    current_indent = count_leading_spaces(line)

    ! Allocate a new node
    allocate(new_node)
    call initialize_node(new_node)

    ! Check if the line is part of a sequence
    if (line(1:1) == '-') then
      new_node%is_sequence = .true.
      new_node%key = ''
      new_node%value = trim(line(2:))
    else
      pos = index(line, ':')
      if (pos > 0) then
        new_node%key = trim(line(1:pos-1))
        new_node%value = trim(line(pos+1:))
      else
        new_node%key = trim(line)
        new_node%value = ''
      end if
    end if

    ! Check for anchors
    pos = index(new_node%key, '&')
    if (pos > 0) then
      new_node%anchor = trim(new_node%key(pos+1:))
      new_node%key = trim(new_node%key(1:pos-1))
      call add_anchor(doc, new_node)
    end if

    ! Check for aliases
    pos = index(new_node%value, '*')
    if (pos > 0) then
      call resolve_alias(doc, trim(new_node%value(pos+1:)), new_node)
      return
    end if

    ! Determine the type of the value
    call determine_value_type(new_node)

    ! Handle indentation to determine the structure
    current_node => doc%root
    parent_node => null()
    previous_indent = 0

    do while (associated(current_node))
      if (current_indent > previous_indent) then
        if (associated(current_node%children)) then
          parent_node => current_node
          current_node => current_node%children
        else
          current_node%children => new_node
          return
        end if
      else if (current_indent < previous_indent) then
        current_node => parent_node
        parent_node => null()
        previous_indent = previous_indent - 2
      else
        if (associated(current_node%next)) then
          current_node => current_node%next
        else
          current_node%next => new_node
          return
        end if
      end if
    end do

    if (.not. associated(doc%root)) then
      doc%root => new_node
    end if
  end subroutine parse_line

  subroutine initialize_node(node)
    type(yaml_node), intent(out) :: node
    node%key = ''
    node%value = ''
    node%children => null()
    node%next => null()
    node%is_sequence = .false.
    node%is_null = .false.
    node%is_boolean = .false.
    node%is_integer = .false.
    node%is_float = .false.
    node%is_string = .true.
    node%anchor = ''
  end subroutine initialize_node

  subroutine add_anchor(doc, node)
    type(yaml_document), intent(inout) :: doc
    type(yaml_node), intent(in) :: node
    integer :: n

    if (.not. associated(doc%anchors)) then
      allocate(doc%anchors(1))
      doc%anchors(1) => node
    else
      n = size(doc%anchors)
      allocate(doc%anchors(n+1))
      doc%anchors(n+1) => node
    end if
  end subroutine add_anchor

  subroutine resolve_alias(doc, alias, node)
    type(yaml_document), intent(in) :: doc
    character(len=*), intent(in) :: alias
    type(yaml_node), intent(out) :: node
    integer :: i

    do i = 1, size(doc%anchors)
      if (trim(doc%anchors(i)%anchor) == alias) then
        node = doc%anchors(i)
        return
      end if
    end do
    print *, 'Error: Alias not found: ', alias
  end subroutine resolve_alias

  subroutine determine_value_type(node)
    type(yaml_node), intent(inout) :: node
    real :: r_value
    integer :: i_value
    logical :: l_value, is_real, is_int, is_logical

    ! Check if the value is a null
    if (trim(node%value) == 'null' .or. trim(node%value) == '~') then
      node%is_null = .true.
      node%value = ''
      return
    end if

    ! Check if the value is a boolean
    if (trim(node%value) == 'true' .or. trim(node%value) == 'false') then
      node%is_boolean = .true.
      l_value = (trim(node%value) == 'true')
      write(node%value, '(L1)') l_value
      return
    end if

    ! Check if the value is a float
    read(node%value, *, iostat=is_real) r_value
    if (is_real == 0) then
      node%is_float = .true.
      write(node%value, '(F6.2)') r_value
      return
    end if

    ! Check if the value is an integer
    read(node%value, *, iostat=is_int) i_value
    if (is_int == 0) then
      node%is_integer = .true.
      write(node%value, '(I0)') i_value
      return
    end if

    ! Default to string
    node%is_string = .true.
  end subroutine determine_value_type

  integer function count_leading_spaces(line)
    character(len=*), intent(in) :: line
    integer :: i

    count_leading_spaces = 0
    do i = 1, len(line)
      if (line(i:i) /= ' ') exit
      count_leading_spaces = count_leading_spaces + 1
    end do
  end function count_leading_spaces

end module yaml_parser

module yaml_parser
  use yaml_types
  implicit none
contains
  subroutine parse_yaml(filename, doc)
    character(len=*), intent(in) :: filename
    type(yaml_document), intent(out) :: doc
    character(len=256) :: line
    integer :: unit, ios, current_indent, previous_indent
    type(yaml_node), pointer :: current_node, new_node, parent_node

    open(unit=unit, file=filename, status='old', action='read', iostat=ios)
    if (ios /= 0) then
      print *, 'Error opening file: ', filename
      return
    end if

    current_node => null()
    parent_node => null()
    previous_indent = 0

    do
      read(unit, '(A)', iostat=ios) line
      if (ios /= 0) exit
      if (trim(line) == '' .or. line(1:1) == '#') cycle

      current_indent = count_leading_spaces(line)
      allocate(new_node)
      call parse_line(trim(line), new_node, doc)

      if (current_indent > previous_indent) then
        if (associated(current_node)) then
          current_node%children => new_node
        else
          doc%root => new_node
        end if
        parent_node => current_node
      else if (current_indent < previous_indent) then
        do while (associated(parent_node) .and. current_indent < previous_indent)
          current_node => parent_node
          parent_node => parent_node%next
          previous_indent = previous_indent - 2
        end do
        current_node%next => new_node
      else
        if (associated(current_node)) then
          current_node%next => new_node
        else
          doc%root => new_node
        end if
      end if

      current_node => new_node
      previous_indent = current_indent
    end do

    close(unit)
  end subroutine parse_yaml

  subroutine parse_line(line, node, doc)
    character(len=*), intent(in) :: line
    type(yaml_node), intent(out) :: node
    type(yaml_document), intent(inout) :: doc
    integer :: pos
    real :: r_value
    integer :: i_value
    logical :: l_value, is_real, is_int, is_logical

    if (line(1:1) == '-') then
      node%is_sequence = .true.
      node%key = ''
      node%value = trim(line(2:))
    else
      pos = index(line, ':')
      if (pos > 0) then
        node%key = trim(line(1:pos-1))
        node%value = trim(line(pos+1:))
      else
        node%key = trim(line)
        node%value = ''
      end if
    end if

    ! Check for anchors
    pos = index(node%key, '&')
    if (pos > 0) then
      node%anchor = trim(node%key(pos+1:))
      node%key = trim(node%key(1:pos-1))
      call add_anchor(doc, node)
    end if

    ! Check for aliases
    pos = index(node%value, '*')
    if (pos > 0) then
      call resolve_alias(doc, trim(node%value(pos+1:)), node)
      return
    end if

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
  end subroutine parse_line

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

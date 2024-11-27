module yaml_parser
  use yaml_types
  implicit none
contains
  subroutine parse_yaml(filename, docs)
    character(len=*), intent(in) :: filename
    type(yaml_document), allocatable, target, intent(out) :: docs(:)  ! Add TARGET attribute
    type(yaml_document), allocatable, target :: tmp_docs(:)  ! Add declaration for tmp_docs
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
        call move_alloc(docs, tmp_docs)  ! Use move_alloc to resize
        allocate(docs(doc_count))
        if (doc_count > 1) then
          docs(1:doc_count-1) = tmp_docs
        end if
        current_doc => docs(doc_count)  ! Now valid pointer assignment
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

  ! Update initialize_document subroutine
  subroutine initialize_document(doc)
    implicit none
    type(yaml_document), intent(inout) :: doc
    integer :: alloc_stat
    integer, parameter :: INITIAL_ANCHOR_SIZE = 4  ! Start small

    if (.not. associated(doc%anchors)) then
        allocate(doc%anchors(INITIAL_ANCHOR_SIZE), stat=alloc_stat)
        if (alloc_stat /= 0) then
            print *, "Error: Failed to allocate anchors array"
            return
        endif
    endif
    doc%anchor_count = 0
  end subroutine

  subroutine parse_line(line, doc)
    character(len=*), intent(in) :: line
    type(yaml_document), intent(inout) :: doc
    type(yaml_node), pointer :: new_node, current_node, parent_node, alias_node
    type(yaml_error) :: err
    integer :: pos, current_indent, previous_indent
    real :: r_value
    integer :: i_value
    logical :: l_value, is_real, is_int, is_logical
    character(len=:), allocatable :: local_line  ! New local variable

    ! Create local copy of line for modification
    local_line = line

    ! Remove inline comments
    pos = index(local_line, '#')
    if (pos > 0) then
        local_line = trim(local_line(1:pos-1))
    end if

    ! Determine the indentation level
    current_indent = count_leading_spaces(local_line)

    ! Allocate a new node
    allocate(new_node)
    call initialize_node(new_node)

    ! Check if the line is part of a sequence
    if (local_line(1:1) == '-') then
        new_node%is_sequence = .true.
        new_node%key = ''
        new_node%value = trim(local_line(2:))
    else
        pos = index(local_line, ':')
        if (pos > 0) then
            new_node%key = trim(local_line(1:pos-1))
            new_node%value = trim(local_line(pos+1:))
        else
            new_node%key = trim(local_line)
            new_node%value = ''
        end if
    end if

    ! Check for flow form sequences and mappings
    if (index(line, '[') > 0 .or. index(line, '{') > 0) then
      call parse_flow_form(line, new_node)
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
        call resolve_alias(doc, trim(new_node%value(pos+1:)), alias_node, err)
        if (.not. err%has_error .and. associated(alias_node)) then
            new_node = alias_node
            deallocate(alias_node)
        else
            print *, trim(err%message)
        end if
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

  subroutine parse_flow_form(line, node)
      character(len=*), intent(in) :: line
      type(yaml_node), pointer, intent(inout) :: node  ! Changed to pointer
      integer :: pos, start, end
      character(len=:), allocatable :: content

      ! Handle flow form sequences
      if (index(line, '[') > 0) then
          start = index(line, '[')
          end = index(line, ']')
          if (end > start) then
              content = trim(line(start+1:end-1))
              ! Node is now a pointer, can be passed directly
              call parse_sequence(content, node)
          end if
      end if

      ! Handle flow form mappings
      if (index(line, '{') > 0) then
          start = index(line, '{')
          end = index(line, '}')
          if (end > start) then
              content = trim(line(start+1:end-1))
              call parse_mapping(content, node)
          end if
      end if
  end subroutine parse_flow_form

  subroutine parse_sequence(content, node)
      ! Modified parameter declarations
      character(len=:), allocatable, intent(inout) :: content
      type(yaml_node), pointer, intent(inout) :: node

      ! Local variables
      character(len=:), allocatable :: item
      character(len=:), allocatable :: local_content
      integer :: pos

      ! Copy input content to local variable for manipulation
      local_content = content

      ! Split the content by commas to get individual items
      do
        pos = index(local_content, ',')
        if (pos > 0) then
          item = trim(local_content(1:pos-1))
          local_content = trim(local_content(pos+1:))
        else
          item = trim(local_content)
          local_content = ''
        end if

        ! Create a new node for each item
        allocate(node%children)
        call initialize_node(node%children)
        node%children%value = item
        node => node%children

        if (len(local_content) == 0) exit
      end do

      ! Update the original content
      content = local_content

  end subroutine parse_sequence

  subroutine parse_mapping(content, node)
    character(len=*), intent(inout) :: content
    type(yaml_node), pointer, intent(inout) :: node
    character(len=:), allocatable :: key, value
    character(len=:), allocatable :: local_content, pair
    integer :: pos, content_len

    ! Safely initialize local content
    content_len = len_trim(content)
    if (content_len == 0) return

    allocate(character(len=content_len) :: local_content)
    local_content = trim(content)

    ! Process key-value pairs
    do while (len_trim(local_content) > 0)
        ! Get next pair
        pos = index(local_content, ',')
        if (pos > 0) then
            if (allocated(pair)) deallocate(pair)
            allocate(character(len=pos-1) :: pair)
            pair = trim(local_content(1:pos-1))
            local_content = trim(local_content(pos+1:))
        else
            if (allocated(pair)) deallocate(pair)
            allocate(character(len=len_trim(local_content)) :: pair)
            pair = trim(local_content)
            local_content = ''
        end if

        ! Process key-value pair
        pos = index(pair, ':')
        if (pos > 0) then
            if (allocated(key)) deallocate(key)
            if (allocated(value)) deallocate(value)

            allocate(character(len=pos-1) :: key)
            allocate(character(len=len_trim(pair)-pos) :: value)

            key = trim(pair(1:pos-1))
            value = trim(pair(pos+1:))

            ! Create new node
            allocate(node%children)
            call initialize_node(node%children)
            node%children%key = key
            node%children%value = value
            node => node%children
        end if
    end do

    ! Clean up
    if (allocated(local_content)) deallocate(local_content)
    if (allocated(key)) deallocate(key)
    if (allocated(value)) deallocate(value)
    if (allocated(pair)) deallocate(pair)

end subroutine parse_mapping

  subroutine initialize_node(node)
    type(yaml_node), pointer, intent(inout) :: node
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

  subroutine grow_anchors(doc)
      type(yaml_document), intent(inout) :: doc
      type(yaml_node), pointer :: temp(:)
      integer :: old_size, new_size, alloc_stat, i

      ! Get current size
      old_size = size(doc%anchors)
      new_size = old_size * 2

      ! Allocate new array
      allocate(temp(new_size), stat=alloc_stat)
      if (alloc_stat /= 0) then
          print *, "Error: Failed to grow anchors array"
          return
      endif

      ! Copy existing elements
      temp(1:old_size) = doc%anchors(1:old_size)

      ! Deallocate old array and reassign
      deallocate(doc%anchors)
      doc%anchors => temp
  end subroutine grow_anchors

! Modified add_anchor subroutine:
  subroutine add_anchor(doc, node, error)
    type(yaml_document), intent(inout) :: doc
    type(yaml_node), target, intent(in) :: node
    type(yaml_error), intent(out), optional :: error
    integer :: anchor_idx, alloc_stat

    ! Initialize anchors array if needed
    if (.not. associated(doc%anchors)) then
        allocate(doc%anchors(4), stat=alloc_stat)
        if (alloc_stat /= 0) then
            if (present(error)) then
                error%has_error = .true.
                error%message = 'Failed to allocate anchors array'
            end if
            return
        endif
        doc%anchor_count = 0
    end if

    ! Grow array if needed
    if (doc%anchor_count >= size(doc%anchors)) then
        call grow_anchors(doc)
    end if

    ! Add new anchor with bounds check
    doc%anchor_count = doc%anchor_count + 1
    anchor_idx = doc%anchor_count

    if (anchor_idx <= size(doc%anchors)) then
        doc%anchors(anchor_idx) = node
    else
        if (present(error)) then
            error%has_error = .true.
            error%message = 'Anchor array bounds exceeded'
        end if
    end if
end subroutine add_anchor

! Modified resolve_alias subroutine:
subroutine resolve_alias(doc, alias, node, error)
    type(yaml_document), intent(in) :: doc
    character(len=*), intent(in) :: alias
    type(yaml_node), pointer, intent(inout) :: node  ! Changed to pointer
    type(yaml_error), intent(out), optional :: error
    integer :: i
    logical :: found
    type(yaml_node), pointer :: temp_node

    ! Initialize
    found = .false.

    ! Safety checks
    if (.not. associated(doc%anchors)) then
        if (present(error)) then
            error%has_error = .true.
            error%message = 'Anchor array not initialized'
        end if
        nullify(node)  ! Ensure node is nullified on error
        return
    end if

    ! Replace strict check with more permissive handling
    if (doc%anchor_count <= 0) then
        ! Only error if we're specifically looking for an anchor
        if (associated(doc%anchors)) then
            if (present(error)) then
                error%has_error = .true.
                error%message = 'No anchors defined'
            end if
            nullify(node)
            return
        end if
        ! Otherwise continue parsing without error
        return
    end if

    ! Search for alias with bounds checking
    do i = 1, min(doc%anchor_count, size(doc%anchors))
        if (.not. associated(doc%anchors(i)%children)) cycle
        if (trim(doc%anchors(i)%anchor) == trim(alias)) then
            ! Allocate new node and copy data
            allocate(node)
            node = doc%anchors(i)
            found = .true.
            exit
        end if
    end do

    ! Handle not found case
    if (.not. found) then
        if (present(error)) then
            error%has_error = .true.
            error%message = 'Alias not found: '//trim(alias)
        end if
        ! Initialize new node
        allocate(node)
        call initialize_node(node)
    end if

end subroutine resolve_alias

  subroutine determine_value_type(node)
    type(yaml_node), intent(inout) :: node
    real :: r_value
    integer :: i_value
    logical :: l_value
    logical :: is_real, is_int
    integer :: rc
    character(len=32) :: temp_str  ! Buffer for numeric conversions

    ! Early exit for empty values
    if (len_trim(node%value) == 0) then
        node%is_null = .true.
        return
    end if

    ! Check for null values
    if (to_lower(trim(node%value)) == 'null' .or. &
        trim(node%value) == '~' .or. &
        to_lower(trim(node%value)) == 'nan') then
        node%is_null = .true.
        node%value = ''
        return
    end if

    ! Check for boolean values
    if (trim(node%value) == 'true' .or. trim(node%value) == 'false') then
        node%is_boolean = .true.
        l_value = (trim(node%value) == 'true')
        write(temp_str, '(L1)', iostat=rc) l_value
        if (rc == 0) node%value = trim(temp_str)
        return
    end if

    ! Check for float values
    if (is_real_string(trim(node%value))) then
        read(node%value, *, iostat=rc) r_value
        if (rc == 0) then
            node%is_float = .true.
            write(temp_str, '(G14.6)', iostat=rc) r_value
            if (rc == 0) node%value = trim(temp_str)
            return
        end if
    end if

    ! Check for integer values
    if (is_int_string(trim(node%value))) then
        read(node%value, *, iostat=rc) i_value
        if (rc == 0) then
            node%is_integer = .true.
            write(temp_str, '(I0)', iostat=rc) i_value
            if (rc == 0) node%value = trim(temp_str)
            return
        end if
    end if

    ! Default to string
    node%is_string = .true.
end subroutine determine_value_type

  integer function count_leading_spaces(line)
    implicit none

    character(len=*), intent(in) :: line
    integer :: i

    count_leading_spaces = 0
    do i = 1, len(line)
      if (line(i:i) /= ' ') exit
      count_leading_spaces = count_leading_spaces + 1
    end do
  end function count_leading_spaces

  function is_real_string(str) result(is_real)
    implicit none
    character(len=*), intent(in) :: str
    logical :: is_real
    real :: r_value
    integer :: iostat

    read(str, *, iostat=iostat) r_value
    if (iostat == 0) then
      is_real = .true.
    else
      is_real = .false.
    end if
  end function is_real_string

  function is_int_string(str) result(is_int)
    implicit none
    character(len=*), intent(in) :: str
    logical :: is_int
    integer :: i_value
    integer :: iostat

    read(str, *, iostat=iostat) i_value
    if (iostat == 0) then
      is_int = .true.
    else
      is_int = .false.
    end if
  end function is_int_string

  function to_lower(str) result(lower_str)
    implicit none
    character(len=*), intent(in) :: str
    character(len=len(str)) :: lower_str
    integer :: i

    lower_str = str
    do i = 1, len(str)
      if (iachar(str(i:i)) >= iachar('A') .and. iachar(str(i:i)) <= iachar('Z')) then
        lower_str(i:i) = achar(iachar(str(i:i)) + 32)
      end if
    end do
  end function to_lower

end module yaml_parser

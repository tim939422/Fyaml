program test_yaml_parser
  use yaml_types
  use yaml_parser
  implicit none
  type(yaml_document) :: doc
  type(yaml_node), pointer :: node

  ! Parse the test YAML file
  call parse_yaml('tests/test_example.yaml', doc)

  ! Run tests
  call run_tests(doc)

contains

  subroutine run_tests(doc)
    type(yaml_document), intent(in) :: doc
    type(yaml_node), pointer :: node

    ! Test 1: Check root node
    node => doc%root
    if (.not. associated(node)) then
      print *, 'Test 1 Failed: Root node is not associated'
    else
      print *, 'Test 1 Passed: Root node is associated'
    end if

    ! Test 2: Check person name
    node => find_node(doc%root, 'person')
    if (associated(node)) node => find_node(node%children, 'name')
    if (associated(node) .and. trim(node%value) == 'Jane Doe') then
      print *, 'Test 2 Passed: Person name is Jane Doe'
    else
      print *, 'Test 2 Failed: Person name is not Jane Doe'
    end if

    ! Test 3: Check integer value
    node => find_node(doc%root, 'int_value')
    if (associated(node) .and. trim(node%value) == '42') then
      print *, 'Test 3 Passed: Integer value is 42'
    else
      print *, 'Test 3 Failed: Integer value is not 42'
    end if

    ! Test 4: Check float value
    node => find_node(doc%root, 'float_value')
    if (associated(node) .and. trim(node%value) == '3.14') then
      print *, 'Test 4 Passed: Float value is 3.14'
    else
      print *, 'Test 4 Failed: Float value is not 3.14'
    end if

    ! Test 5: Check boolean true value
    node => find_node(doc%root, 'bool_true')
    if (associated(node) .and. trim(node%value) == 'T') then
      print *, 'Test 5 Passed: Boolean true value is T'
    else
      print *, 'Test 5 Failed: Boolean true value is not T'
    end if

    ! Test 6: Check boolean false value
    node => find_node(doc%root, 'bool_false')
    if (associated(node) .and. trim(node%value) == 'F') then
      print *, 'Test 6 Passed: Boolean false value is F'
    else
      print *, 'Test 6 Failed: Boolean false value is not F'
    end if

    ! Test 7: Check null value
    node => find_node(doc%root, 'null_value')
    if (associated(node) .and. node%is_null) then
      print *, 'Test 7 Passed: Null value is correctly identified'
    else
      print *, 'Test 7 Failed: Null value is not correctly identified'
    end if

    ! Test 8: Check alias reference
    node => find_node(doc%root, 'alias_reference')
    if (associated(node) .and. trim(node%value) == 'Jane Doe') then
      print *, 'Test 8 Passed: Alias reference is correctly resolved'
    else
      print *, 'Test 8 Failed: Alias reference is not correctly resolved'
    end if

    ! Test 9: Check nested sequence
    node => find_node(doc%root, 'nested_sequence')
    if (associated(node) .and. associated(node%children)) then
      if (trim(node%children%value) == 'item1' .and. trim(node%children%next%value) == 'item2' .and. trim(node%children%next%next%value) == 'item3') then
        print *, 'Test 9 Passed: Nested sequence is correctly parsed'
      else
        print *, 'Test 9 Failed: Nested sequence is not correctly parsed'
      end if
    else
      print *, 'Test 9 Failed: Nested sequence is not correctly parsed'
    end if

    ! Test 10: Check nested mapping
    node => find_node(doc%root, 'nested_mapping')
    if (associated(node) .and. associated(node%children)) then
      if (trim(node%children%key) == 'key1' .and. trim(node%children%value) == 'value1' .and. trim(node%children%next%key) == 'key2' .and. trim(node%children%next%value) == 'value2') then
        print *, 'Test 10 Passed: Nested mapping is correctly parsed'
      else
        print *, 'Test 10 Failed: Nested mapping is not correctly parsed'
      end if
    else
      print *, 'Test 10 Failed: Nested mapping is not correctly parsed'
    end if

  end subroutine run_tests

  function find_node(node, key) result(found_node)
    type(yaml_node), pointer :: node
    character(len=*), intent(in) :: key
    type(yaml_node), pointer :: found_node

    found_node => null()
    do while (associated(node))
      if (trim(node%key) == key) then
        found_node => node
        return
      end if
      node => node%next
    end do
  end function find_node

end program test_yaml_parser

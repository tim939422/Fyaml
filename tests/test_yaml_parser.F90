program test_yaml_parser
  use yaml_types
  use yaml_parser
  implicit none
  type(yaml_document), allocatable, target :: docs(:)  ! Change to array
  type(yaml_document), pointer :: current_doc
  type(yaml_node), pointer :: node

  ! Parse the test YAML file
  call parse_yaml('test_example.yaml', docs)
  current_doc => docs(1)  ! Use first document

  ! Run tests
  call run_tests(current_doc)

contains

  ! Update run_tests to include safety checks
  subroutine run_tests(doc)
    type(yaml_document), intent(in) :: doc
    type(yaml_node), pointer :: node
    logical :: test_passed

    ! Verify document root exists
    if (.not. associated(doc%root)) then
        print *, 'Test Failed: Document root is not initialized'
        return
    end if

    ! Test 9: Check nested sequence under person
    node => find_node(doc%root%children, 'nested_sequence') ! Look under person node
    if (associated(node)) then
        if (associated(node%children)) then
            test_passed = .false.
            if (associated(node%children)) then
                if (associated(node%children%next)) then
                    if (associated(node%children%next%next)) then
                        test_passed = &
                            trim(node%children%value) == 'item1' .and. &
                            trim(node%children%next%value) == 'item2' .and. &
                            trim(node%children%next%next%value) == 'item3'
                    end if
                end if
            end if

            if (test_passed) then
                print *, 'Test 9 Passed: Nested sequence is correctly parsed'
            else
                print *, 'Test 9 Failed: Nested sequence values incorrect'
            end if
        else
            print *, 'Test 9 Failed: Nested sequence has no children'
        end if
    else
        print *, 'Test 9 Failed: Nested sequence node not found'
    end if

    ! Test 10: Check nested mapping under person
    node => find_node(doc%root%children, 'nested_mapping') ! Look under person node
    if (associated(node)) then
        if (associated(node%children)) then
            test_passed = .false.
            if (associated(node%children%next)) then
                test_passed = &
                    trim(node%children%key) == 'key1' .and. &
                    trim(node%children%value) == 'value1' .and. &
                    trim(node%children%next%key) == 'key2' .and. &
                    trim(node%children%next%value) == 'value2'
            end if

            if (test_passed) then
                print *, 'Test 10 Passed: Nested mapping is correctly parsed'
            else
                print *, 'Test 10 Failed: Nested mapping values incorrect'
            end if
        else
            print *, 'Test 10 Failed: Nested mapping has no children'
        end if
    else
        print *, 'Test 10 Failed: Nested mapping node not found'
    end if

  end subroutine run_tests

  ! Updated find_node function to handle nested paths
  RECURSIVE function find_node(node, key) result(found_node)
    type(yaml_node), pointer :: node
    character(len=*), intent(in) :: key
    type(yaml_node), pointer :: found_node
    type(yaml_node), pointer :: current

    ! Initialize result
    nullify(found_node)

    ! Check input validity
    if (.not. associated(node)) return

    ! Start search with current node
    current => node

    do while (associated(current))
        ! Check for key match at current level
        if (trim(current%key) == trim(key)) then
            found_node => current
            return
        end if

        ! Search children first
        if (associated(current%children)) then
            found_node => find_node(current%children, key)
            if (associated(found_node)) return
        end if

        ! Try siblings next
        current => current%next
    end do
  end function find_node

end program test_yaml_parser
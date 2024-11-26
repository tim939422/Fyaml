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

    ! Additional tests can be added here...

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


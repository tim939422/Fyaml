program test_fyaml
  use fyaml
  use test_utils
  implicit none

  call test_basic_loading()
  call test_basic_types()
  call test_sequences()
  call test_nested_structures()
  call test_dict_operations()

  print *, 'All tests passed!'

contains
  subroutine test_basic_loading()
    type(fyaml_doc) :: doc

    call doc%load('test_example.yaml')
    print *, 'Basic loading test passed'
  end subroutine

end program test_fyaml
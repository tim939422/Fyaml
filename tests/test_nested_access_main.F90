program test_nested_access_main
  ! use test_fyaml
  use test_utils, only: test_nested_access, ERR_SUCCESS
  implicit none
  integer :: status
  status = test_nested_access()
  if (status /= ERR_SUCCESS) error stop
end program test_nested_access_main

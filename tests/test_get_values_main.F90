program test_get_values_main
  ! use test_fyaml
  use test_utils, only: test_get_values, ERR_SUCCESS
  implicit none
  integer :: status
  status = test_get_values()
  if (status /= ERR_SUCCESS) error stop
end program test_get_values_main

program test_get_value_main
  use test_utils, only: test_get_value, ERR_SUCCESS
  implicit none
  integer :: status
  status = test_get_value()
  if (status /= ERR_SUCCESS) error stop
end program test_get_value_main

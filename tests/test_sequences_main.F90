program test_sequences_main
  ! use test_fyaml
  use test_utils, only: test_sequences, ERR_SUCCESS
  implicit none
  integer :: status
  status = test_sequences()
  if (status /= ERR_SUCCESS) error stop
end program test_sequences_main

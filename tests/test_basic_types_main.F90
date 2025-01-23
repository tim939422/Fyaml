program test_basic_types_main
    use test_utils, only: test_basic_types, ERR_SUCCESS
    implicit none
    integer :: status
    status = test_basic_types()
    if (status /= ERR_SUCCESS) error stop
end program test_basic_types_main

program test_basic_loading_main
    use test_utils, only: test_basic_loading, ERR_SUCCESS
    implicit none
    integer :: status
    status = test_basic_loading()
    if (status /= ERR_SUCCESS) error stop
end program test_basic_loading_main

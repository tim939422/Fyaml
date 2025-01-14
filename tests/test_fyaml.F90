module test_fyaml
    use test_utils
    use fyaml
    use iso_fortran_env, only: error_unit
    implicit none

    contains

    subroutine run_all_tests(status)
        integer, intent(inout) :: status

        print *, '========================================'
        print *, "Running Basic Loading Test..."
        print *, " "
        status = test_basic_loading()
        if (status /= ERR_SUCCESS) then
            write(error_unit,*) "Basic Loading Test Failed with status:", status
        else
            print *, "Basic Loading Test Passed!!!!!!!"
        endif

        print *, " "
        print *, '========================================'
        print *, "Running Basic Types Test..."
        print *, " "
        status = test_basic_types()
        if (status /= ERR_SUCCESS) then
            write(error_unit,*) "Basic Types Test Failed with status:", status
        else
            print *, "Basic Types Test Passed!!!!!!!"
        endif

        print *, " "
        print *, '========================================'
        print *, "Running Sequences Test..."
        print *, " "
        status = test_sequences()
        if (status /= ERR_SUCCESS) then
            write(error_unit,*) "Sequences Test Failed with status:", status
        else
            print *, "Sequences Test Passed!!!!!!!"
        endif

        print *, " "
        print *, '========================================'
        print *, "Running Nested Access Test..."
        print *, " "
        status = test_nested_access()
        if (status /= ERR_SUCCESS) then
            write(error_unit,*) "Nested Access Test Failed with status:", status
        else
            print *, "Nested Access Test Passed!!!!!!!"
        endif

        print *, " "
        print *, '========================================'
        print *, "Running Get Value Test..."
        print *, " "
        status = test_get_value()
        if (status /= ERR_SUCCESS) then
            write(error_unit,*) "Get Value Test Failed with status:", status
        else
            print *, "Get Value Test Passed!!!!!!!"
        endif

        print *, " "
        print *, '========================================'
        print *, "Running Get Values Test..."
        print *, " "
        status = test_get_values()
        if (status /= ERR_SUCCESS) then
            write(error_unit,*) "Get Values Test Failed with status:", status
        else
            print *, "Get Values Test Passed!!!!!!!"
        endif

        print *, " "
        print *, '========================================'
        print *, "Running Multi Document Test..."
        print *, " "
        status = test_multiple_docs()
        if (status /= ERR_SUCCESS) then
            write(error_unit,*) "Multi Document Test Failed with status:", status
        else
            print *, "Multi Document Test Passed!!!!!!!"
        endif

        print *, " "
        print *, '========================================'
        print *, "All tests completed."
        print *, '========================================'
    end subroutine run_all_tests

end module test_fyaml

program test_fyaml_main
    use test_fyaml
    implicit none

    integer :: status

    call run_all_tests(status)
    if (status /= 0) error stop
end program test_fyaml_main

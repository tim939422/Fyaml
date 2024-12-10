module test_utils
    use fyaml
    use iso_fortran_env, only: error_unit
    implicit none

    ! Error codes and debug levels
    integer, parameter :: ERR_SUCCESS = 0
    integer, parameter :: ERR_ALLOC = 1
    integer, parameter :: ERR_ASSERT = 2
    integer, parameter :: DEBUG = 1

    interface assert_equal
        module procedure assert_equal_int
        module procedure assert_equal_real
        module procedure assert_equal_logical
        module procedure assert_equal_string
    end interface

contains
    subroutine assert_equal_int(expected, actual, message, status)
        integer, intent(in) :: expected, actual
        character(len=*), intent(in) :: message
        integer, intent(out) :: status
        if (expected /= actual) then
            print *, 'FAILED: ', message
            print *, 'Expected: ', expected, ' Got: ', actual
            status = ERR_ASSERT
        else
            status = ERR_SUCCESS
        end if
    end subroutine

    subroutine assert_equal_real(expected, actual, message, tolerance, status)
        real, intent(in) :: expected, actual
        character(len=*), intent(in) :: message
        real, intent(in), optional :: tolerance
        real :: tol
        integer, intent(out) :: status

        tol = 1.0e-6
        if (present(tolerance)) tol = tolerance
        if (abs(expected - actual) > tol) then
            print *, 'FAILED: ', message
            print *, 'Expected: ', expected, ' Got: ', actual
            status = ERR_ASSERT
        else
            status = ERR_SUCCESS
        end if
    end subroutine

    subroutine assert_equal_logical(expected, actual, message, status)
        logical, intent(in) :: expected, actual
        character(len=*), intent(in) :: message
        integer, intent(out) :: status
        if (.not. (expected .eqv. actual)) then
            print *, 'FAILED: ', message
            print *, 'Expected: ', expected, ' Got: ', actual
            status = ERR_ASSERT
        else
            status = ERR_SUCCESS
        end if
    end subroutine

    subroutine assert_equal_string(expected, actual, message, status)
        character(len=*), intent(in) :: expected, actual, message
        character(len=:), allocatable :: exp_val, act_val
        integer :: alloc_status
        integer, intent(out) :: status

        call allocate_string_value(exp_val, expected, alloc_status)
        if (alloc_status /= 0) then
            print *, 'FAILED: Memory allocation error for expected value'
            status = ERR_ALLOC
            return
        end if

        call allocate_string_value(act_val, actual, alloc_status)
        if (alloc_status /= 0) then
            print *, 'FAILED: Memory allocation error for actual value'
            if (allocated(exp_val)) deallocate(exp_val)
            status = ERR_ALLOC
            return
        end if

        if (exp_val /= act_val) then
            print *, 'FAILED: ', message
            print *, 'Expected: ', trim(exp_val), ' Got: ', trim(act_val)
            status = ERR_ASSERT
        else
            status = ERR_SUCCESS
        end if

        if (allocated(exp_val)) deallocate(exp_val)
        if (allocated(act_val)) deallocate(act_val)
    end subroutine

    ! Helper for safe string allocation
    subroutine safe_allocate_string(str, length, status)
        character(len=:), allocatable, intent(out) :: str
        integer, intent(in) :: length
        integer, intent(out) :: status

        if (allocated(str)) deallocate(str)
        allocate(character(len=length) :: str, stat=status)
        if (status /= 0) then
            write(error_unit,*) "Failed to allocate string of length", length
        end if
    end subroutine

    ! Allocation helper
    subroutine allocate_string_value(val, str, status)
        character(len=:), allocatable, intent(out) :: val
        character(len=*), intent(in) :: str
        integer, intent(out) :: status

        if (allocated(val)) deallocate(val)
        allocate(character(len=len_trim(str)) :: val, stat=status)
        if (status == 0) then
            val = trim(str)
        end if
    end subroutine

    ! Test routines returning status
    integer function test_basic_loading()
        type(fyaml_doc) :: doc
        character(len=100) :: filename = "test_example.yaml"
        logical :: success

        test_basic_loading = ERR_SUCCESS
        write(*,*) 'Loading file:', trim(filename)
        call doc%load(filename, success)
        if (.not. success) then
            write(error_unit,*) 'Error loading YAML document.'
            test_basic_loading = ERR_ALLOC
            return
        endif
        write(*,*) 'YAML document loaded successfully.'
    end function

    integer function test_basic_types()
        type(fyaml_doc) :: doc
        type(yaml_value) :: val
        type(yaml_dict) :: company
        character(len=:), allocatable :: key
        integer :: status

        test_basic_types = ERR_SUCCESS
        call safe_allocate_string(key, 20, status)
        if (status /= 0) then
            write(error_unit,*) "Failed to allocate key string"
            test_basic_types = ERR_ALLOC
            return
        endif

        call doc%load("test_example.yaml")

        key = "company"
        val = doc%root%get(key)
        if (.not. associated(val%dict_val)) then
            write(error_unit,*) "Failed to get company dictionary"
            test_basic_types = ERR_ASSERT
            return
        endif
        company = val%dict_val
        ! Assuming keys() returns an integer for size
        call assert_equal(4, size(company%keys()), "Number of keys in company", status)
        if (status /= ERR_SUCCESS) then
            test_basic_types = status
            return
        endif

        ! Additional type assertions...
        ! Example:
        key = "name"
        val = company%get(key)
        call assert_equal_string("Example Corp", val%str_val, "String value test", status)
        if (status /= ERR_SUCCESS) then
            test_basic_types = status
            return
        endif

        if (allocated(key)) deallocate(key)

    end function test_basic_types

    integer function test_sequences()
        type(fyaml_doc) :: doc
        type(yaml_value) :: val
        character(len=:), allocatable :: key
        integer :: status

        test_sequences = ERR_SUCCESS
        call safe_allocate_string(key, 20, status)
        if (status /= 0) then
            test_sequences = ERR_ALLOC
            return
        endif

        call doc%load("test_example.yaml")

        key = "person"
        val = doc%root%get(key)
        if (.not. associated(val%dict_val)) then
            test_sequences = ERR_ASSERT
            return
        endif

        key = "skills"
        val = val%get(key)
        if (.not. allocated(val%sequence)) then
            test_sequences = ERR_ASSERT
            return
        endif

        call assert_equal(3, size(val%sequence), "Sequence size test", status)
        if (status /= ERR_SUCCESS) then
            test_sequences = status
            return
        endif

        call assert_equal_string("R", val%sequence(1), "Sequence item 1 test", status)
        if (status /= ERR_SUCCESS) then
            test_sequences = status
            return
        endif

        call assert_equal_string("SQL", val%sequence(2), "Sequence item 2 test", status)
        if (status /= ERR_SUCCESS) then
            test_sequences = status
            return
        endif

        call assert_equal_string("Python", val%sequence(3), "Sequence item 3 test", status)
        if (status /= ERR_SUCCESS) then
            test_sequences = status
            return
        endif

        if (allocated(key)) deallocate(key)
    end function

    ! Additional test functions...

end module test_utils

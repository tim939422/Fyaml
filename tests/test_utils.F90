module test_utils
    use fyaml
    implicit none

    interface assert_equal
      module procedure assert_equal_int
      module procedure assert_equal_real
      module procedure assert_equal_logical
      module procedure assert_equal_string
    end interface

  contains
    subroutine assert_equal_int(expected, actual, message)
      integer, intent(in) :: expected, actual
      character(len=*), intent(in) :: message
      if (expected /= actual) then
        print *, 'FAILED: ', message
        print *, 'Expected: ', expected, ' Got: ', actual
        error stop
      end if
    end subroutine

    subroutine assert_equal_real(expected, actual, message, tolerance)
      real, intent(in) :: expected, actual
      character(len=*), intent(in) :: message
      real, intent(in), optional :: tolerance
      real :: tol
      tol = 1.0e-6
      if (present(tolerance)) tol = tolerance
      if (abs(expected - actual) > tol) then
        print *, 'FAILED: ', message
        print *, 'Expected: ', expected, ' Got: ', actual
        error stop
      end if
    end subroutine

    subroutine assert_equal_logical(expected, actual, message)
      logical, intent(in) :: expected, actual
      character(len=*), intent(in) :: message
      if (expected .neqv. actual) then
        print *, 'FAILED: ', message
        print *, 'Expected: ', expected, ' Got: ', actual
        error stop
      end if
    end subroutine

    subroutine assert_equal_string(expected, actual, message)
      character(len=*), intent(in) :: expected, actual, message
      if (trim(expected) /= trim(actual)) then
        print *, 'FAILED: ', message
        print *, 'Expected: ', trim(expected), ' Got: ', trim(actual)
        error stop
      end if
    end subroutine

    subroutine test_basic_types()
      type(fyaml_doc) :: doc
      type(yaml_value) :: val
      character(len=20) :: key  ! Single key variable with sufficient length

      call doc%load("test_example.yaml")

      key = "person"
      val = doc%root%get(key)

      key = "name"
      val = val%get(key)
      call assert_equal("Jane Doe", val%str_val, "String value test")

      key = "person"
      val = doc%root%get(key)

      key = "age"
      val = val%get(key)
      call assert_equal(25, val%int_val, "Integer value test")

      key = "person"
      val = doc%root%get(key)

      key = "height"
      val = val%get(key)
      call assert_equal(5.7, val%real_val, "Real value test")

      key = "person"
      val = doc%root%get(key)

      key = "married"
      val = val%get(key)
      call assert_equal(.false., val%bool_val, "Boolean value test")

      key = "person"
      val = doc%root%get(key)

      key = "null_value"
      val = val%get(key)
      call assert_equal(.true., val%is_null, "Null value test")
    end subroutine

    subroutine test_sequences()
      type(fyaml_doc) :: doc
      type(yaml_value) :: val
      character(len=20) :: key

      call doc%load("test_example.yaml")

      key = "person"
      val = doc%root%get(key)

      key = "skills"
      val = val%get(key)
      call assert_equal(3, size(val%sequence), "Sequence size test")
      call assert_equal("R", val%sequence(1), "Sequence item 1 test")
      call assert_equal("SQL", val%sequence(2), "Sequence item 2 test")
      call assert_equal("Python", val%sequence(3), "Sequence item 3 test")
    end subroutine

    subroutine test_nested_structures()
        type(fyaml_doc) :: doc
        type(yaml_value) :: val
        type(yaml_dict), pointer :: nested
        character(len=:), allocatable :: key

        call doc%load("test_example.yaml")

        ! Get nested person structure - use get() method
        val = doc%root%get("person")
        ! Access nested values through val
        if (associated(val%dict_val)) then
            nested => val%dict_val
        end if

        ! Test nested values
        val = doc%root%get("person.name")
        call assert_equal("Jane Doe", val%str_val, "Nested string test")

        val = doc%root%get("person.age")
        call assert_equal(25, val%int_val, "Nested integer test")
    end subroutine

    subroutine test_dict_operations()
      type(fyaml_doc) :: doc
      type(yaml_dict), pointer :: dict
      type(yaml_value) :: val
      character(len=:), allocatable, dimension(:) :: keys
      character(len=20) :: key

      call doc%load("test_example.yaml")

      ! Get person dictionary first
      val = doc%root%get("person")
      if (associated(val%dict_val)) then
          ! Now we can get keys from the dictionary
          dict => val%dict_val
          keys = dict%keys()

          ! Test dictionary operations
          call assert_equal(4, size(keys), "Number of keys in person")
          ! Additional assertions for specific keys can be added here
      end if
    end subroutine

end module test_utils


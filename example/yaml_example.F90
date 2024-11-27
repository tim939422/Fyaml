program test_fyaml
    use fyaml
    implicit none

    type(fyaml_doc) :: doc
    type(yaml_value) :: val, person
    logical :: success
    character(len=:), allocatable, dimension(:) :: keys
    character(len=*), parameter :: filename = "example.yaml"

    success = .false.
    write(*,*) 'Loading file:', filename
    call doc%load(filename, success)
    if (.not. success) then
      write(error_unit,*) 'Error: Failed to load YAML file'
      error stop
    end if

    ! Get all person keys
    person = doc%root%get("person")
    keys = val%dict_val%keys()
    print *, "Keys:", keys

    ! Get specific values
    val = person%get("name")
    print *, "Name:", val%str_val

    val = person%get("age")
    print *, "Age:", val%int_val

    val = person%get("skills")
    print *, "Skills:", val%sequence

end program test_fyaml

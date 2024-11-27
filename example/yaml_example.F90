program test_fyaml
      use fyaml
      implicit none

      type(yaml_document) :: doc
      type(yaml_value) :: val
      character(len=:), allocatable, dimension(:) :: keys

      call doc%load("example.yaml")

      ! Get all person keys
      keys = doc%root%get("person")%keys()

      ! Get specific values
      val = doc%root%get("person")%get("name")
      print *, "Name:", val%str_val

      val = doc%root%get("person")%get("age")
      print *, "Age:", val%int_val

      val = doc%root%get("person")%get("skills")
      print *, "Skills:", val%sequence
  end program test_fyaml

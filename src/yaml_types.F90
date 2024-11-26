module yaml_types
  implicit none
  type :: yaml_node
    character(len=:), allocatable :: key
    character(len=:), allocatable :: value
    type(yaml_node), pointer :: children => null()
    type(yaml_node), pointer :: next => null()
    logical :: is_sequence = .false.
    logical :: is_null = .false.
    logical :: is_boolean = .false.
    logical :: is_integer = .false.
    logical :: is_float = .false.
    logical :: is_string = .true.
    character(len=:), allocatable :: anchor
  end type yaml_node

  type :: yaml_document
    type(yaml_node), pointer :: root => null()
    type(yaml_node), pointer :: anchors(:) => null()
  end type yaml_document
end module yaml_types

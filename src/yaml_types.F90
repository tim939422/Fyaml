module yaml_types
  implicit none
  type :: yaml_node
    character(len=:), allocatable :: key
    character(len=:), allocatable :: value
    type(yaml_node), pointer :: children => null()
    type(yaml_node), pointer :: next => null()
    logical :: is_sequence = .false.
  end type yaml_node

  type :: yaml_document
    type(yaml_node), pointer :: root => null()
  end type yaml_document
end module yaml_types


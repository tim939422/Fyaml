module yaml_types
  implicit none

  ! Define a type to represent a YAML node
  type :: yaml_node
    character(len=:), allocatable :: key       ! Key of the YAML node
    character(len=:), allocatable :: value     ! Value of the YAML node
    type(yaml_node), pointer :: children => null()  ! Pointer to child nodes (for nested structures)
    type(yaml_node), pointer :: next => null()      ! Pointer to the next node (for sequences)
    logical :: is_sequence = .false.                ! Flag to indicate if the node is part of a sequence
    logical :: is_null = .false.                    ! Flag to indicate if the node represents a null value
    logical :: is_boolean = .false.                 ! Flag to indicate if the node represents a boolean value
    logical :: is_integer = .false.                 ! Flag to indicate if the node represents an integer value
    logical :: is_float = .false.                   ! Flag to indicate if the node represents a float value
    logical :: is_string = .true.                   ! Flag to indicate if the node represents a string value
    character(len=:), allocatable :: anchor         ! Anchor name for the node (if any)
  end type yaml_node

  ! Define a type to represent a YAML document
  type :: yaml_document
    type(yaml_node), pointer :: root => null()      ! Pointer to the root node of the document
    type(yaml_node), pointer :: anchors(:) => null()! Array of pointers to nodes with anchors
  end type yaml_document

end module yaml_types


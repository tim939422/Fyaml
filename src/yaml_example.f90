program yaml_example
  use yaml_types
  use yaml_parser
  implicit none
  type(yaml_document) :: doc
  type(yaml_node), pointer :: node

  call parse_yaml('example.yaml', doc)

  call print_yaml(doc%root, 0)
end program yaml_example

subroutine print_yaml(node, indent)
  use yaml_types
  implicit none
  type(yaml_node), pointer :: node
  integer, intent(in) :: indent
  integer :: i

  if (.not. associated(node)) return

  do while (associated(node))
    do i = 1, indent
      write(*, '(A)', advance='no') '  '
    end do
    if (node%is_sequence) then
      write(*, '(A)', advance='no') '- '
    else
      write(*, '(A, A)', advance='no') node%key, ': '
    end if
    if (associated(node%children)) then
      write(*, '(A)') ''
      call print_yaml(node%children, indent + 2)
    else
      if (node%is_null) then
        write(*, '(A)') 'null'
      else if (node%is_boolean) then
        write(*, '(A)') trim(node%value)
      else if (node%is_integer) then
        write(*, '(A)') trim(node%value)
      else if (node%is_float) then
        write(*, '(A)') trim(node%value)
      else
        write(*, '(A)') node%value
      end if
    end if
    node => node%next
  end do
end subroutine print_yaml


program yaml_example
  use yaml_types
  use yaml_parser
  implicit none
  type(yaml_document), allocatable :: docs(:)
  integer :: i, lines_read

  ! Read and parse the YAML file
  call read_yaml('example.yaml', docs)
  lines_read = 0

  ! Print the parsed YAML content for each document
  do i = 1, size(docs)
    print *, 'Document ', i
    call print_yaml(docs(i)%root, 0, lines_read)
    print *, 'Total lines processed in document ', i, ': ', lines_read
  end do

contains

  RECURSIVE subroutine print_yaml(node, indent, line_count)
    use yaml_types
    implicit none
    type(yaml_node), pointer :: node
    integer, intent(in) :: indent
    integer, intent(inout) :: line_count
    integer :: i

    if (.not. associated(node)) return

    do while (associated(node))
      line_count = line_count + 1

      ! Print indentation
      do i = 1, indent
        write(*, '(A)', advance='no') '  '
      end do

      ! Print node content
      if (node%is_sequence) then
        write(*, '(A)', advance='no') '- '
      else
        write(*, '(A, A)', advance='no') trim(node%key), ': '
      end if

      ! Handle children or values
      if (associated(node%children)) then
        write(*, '(A)') ''
        call print_yaml(node%children, indent + 2, line_count)
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
          write(*, '(A)') trim(node%value)
        end if
      end if

      node => node%next
    end do
  end subroutine print_yaml
end program yaml_example


add_compile_options(${_fyaml_compiler_options})

# add_library(testing test_yaml_parser.F90)
# target_link_libraries(testing PRIVATE Fyaml)
# set_target_properties(testing PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include)

enable_testing()

add_executable(test_yaml_parser test_yaml_parser.F90)
target_link_libraries(test_yaml_parser PRIVATE yaml_parser)
set_target_properties(test_yaml_parser PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include)

add_test(
  NAME test_yaml_parser
  COMMAND test_yaml_parser
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

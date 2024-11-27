add_compile_options(${_fyaml_compiler_options})

# add_library(testing test_yaml_parser.F90)
# target_link_libraries(testing PRIVATE Fyaml)
# set_target_properties(testing PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include)

add_executable(yaml_example yaml_example.F90)
target_link_libraries(yaml_example PRIVATE yaml_parser)
set_target_properties(yaml_example PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include)

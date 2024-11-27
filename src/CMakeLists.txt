set(_srcs
    yaml_types.F90
    yaml_parser.F90)

add_library(yaml_parser ${_srcs})
set_target_properties(yaml_parser PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include)
target_link_libraries(yaml_parser PRIVATE ${_fyaml_compiler_options})
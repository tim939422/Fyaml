# YAML Parser in Fortran

This project is a simple YAML parser written in Fortran. It supports basic YAML features including nested structures, sequences, various data types (strings, integers, floats, booleans, nulls), comments, anchors, and aliases.

## Features

- Parse YAML files with nested mappings and sequences
- Handle various data types: strings, integers, floats, booleans, nulls
- Support for comments
- Support for anchors and aliases

## Directory Structure

.
├── CMakeLists.txt
├── README.md
└── src
    ├── yaml_types.f90
    ├── yaml_parser.f90
    └── yaml_example.f90

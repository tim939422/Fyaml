# YAML Parser in Fortran

This project is a simple YAML parser written in Fortran. It supports basic YAML features including nested structures, sequences, various data types (strings, integers, floats, booleans, nulls), comments, anchors, and aliases.

## Features

- Parse YAML files with nested mappings and sequences
- Handle various data types: strings, integers, floats, booleans, nulls
- Support for comments
- Support for anchors and aliases

## Directory Structure
```
.
├── CMakeLists.txt
├── README.md
└── src
    ├── yaml_types.f90
    ├── yaml_parser.f90
    └── yaml_example.f90
```


## Usage

### Example YAML File

Create a file named `example.yaml` with the following content:

```yaml
# This is a comment
person:
  name: John Doe          # The name of the person
  age: 30                 # The age of the person
  height: 5.9             # The height of the person in feet
  married: true           # Boolean indicating if the person is married
  birthdate: 1985-05-15   # The birthdate of the person
  address:                # Nested structure for the address
    street: 123 Main St   # The street address
    city: Anytown         # The city
    state: CA             # The state
  occupation: Software Developer  # The occupation of the person
  skills:                 # Sequence of skills
    - Fortran             # Skill 1
    - Python              # Skill 2
    - C++                 # Skill 3
  null_value: null        # Example of a null value

```


---
project: Fyaml
summary: A modern lightweight Fortran YAML parser
version: 0.1.0
year: 2024
author: Barry Baker and Zach Moon
author_pic: media/FYAML.png
src_dir: ./src
output_dir: ./doc
page_dir: pages
media_dir: media
project_github: https://github.com/bbakernoaa/Fyaml
project_website: https://github.com
predocmark: >
docmark: !
source: false
graph: true
search: true
preprocessor: gfortran -E
display: public
         protected
proc_internals: true
sort: permission-alpha
print_creation_date: true
creation_date: "%Y-%m-%d %H:%M %z"
doc_license: gfdl
max_frontpage_items: 5
coloured_edges: true
exclude_dir: ./tests
extra_mods: iso_fortran_env:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fFORTRAN_005fENV.html
            tomlf:https://toml-f.github.io/toml-f
            M_CLI2:https://github.com/urbanjost/M_CLI2
---

{!README.md!}

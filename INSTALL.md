# Prerequisites
Make sure you have the following installed:
* To compile pwc
1. ocamlc - OCaml compiler, version 4.02.0 or higher 
(parser.mly uses List.sort_uniq, to compile with lower versions of ocaml,
change it to List.sort)
2. ocamlyacc - OCaml parser generator
3. ocamllex - Ocaml lexer generator
<TODO>: comment on how to obtain those
* To run tests
1. Julia -
2. Python 

# Installation 
1. Retrieve the repository from github:
<TODO>

2. Create the .depend file in the root directory of the project
```
touch .depend
```

3. At this point one should be able to compile pwc
```
make
```

4. <TODO>: set globals

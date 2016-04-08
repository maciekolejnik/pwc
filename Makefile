# Programs

OCAMLC    = ocamlc
OCAMLOPT   = ocamlopt
OCAMLDEP  = ocamldep
OCAMLLEX  = ocamllex
OCAMLYACC = ocamlyacc
INCLUDES =
OCAMLFLAGS = $(INCLUDES)
OCAMLOPTFLAGS = $(INCLUDES)

# Specific targets

PPOBJECTS = global.cmo \
	declaration.cmo expression.cmo statement.cmo \
	label.cmo block.cmo flow.cmo \
	parser.cmo lexer.cmo

SYNTAXFILES = expression.ml declaration.ml statement.ml

pwc: $(PPOBJECTS) pwc.cmo
	$(OCAMLC) -g -o pwc str.cma $(PPOBJECTS) pwc.cmo

# Common stuff

.SUFFIXES: .ml .cmo .mli .cmi .cmx .mll .mly

pwc.cmo: pwc.ml block.ml flow.ml label.ml global.ml \
         $(SYNTAXFILES) parser.mly lexer.mll
	$(OCAMLC) $(OCAMLFLAG) -g -c pwc.ml 
label.cmo: label.ml global.ml $(SYNTAXFILES) 
	$(OCAMLC) $(OCAMLFLAG) -g -c label.ml
block.cmo: block.ml global.ml label.ml $(SYNTAXFILES)
	$(OCAMLC) $(OCAMLFLAG) -g -c block.ml 
flow.cmo: flow.ml global.ml label.ml $(SYNTAXFILES)
	$(OCAMLC) $(OCAMLFLAG) -g -c flow.ml 
statement.cmo: statement.ml global.ml declaration.ml expression.ml
	$(OCAMLC) $(OCAMLFLAG) -g -c statement.ml
parser.cmo: parser.ml
	$(OCAMLC) $(OCAMLFLAG) -c parser.mli
	$(OCAMLC) $(OCAMLFLAG) -c parser.ml
parser.ml: parser.mly global.ml $(SYNTAXFILES) 
	$(OCAMLYACC) parser.mly 
lexer.cmo: lexer.ml statement.ml
	$(OCAMLC) $(OCAMLFLAG) -c lexer.ml
lexer.ml: lexer.mll global.ml $(SYNTAXFILES)
	$(OCAMLLEX) lexer.mll
expression.cmo: expression.ml global.ml
	$(OCAMLC) $(OCAMLFLAG) -g -c expression.ml
declaration.cmo: declaration.ml global.ml
	$(OCAMLC) $(OCAMLFLAG) -g -c declaration.ml
.ml.cmo: global.ml 
	$(OCAMLC) $(OCAMLFLAG) -g -c $<
.mli.cmi:
	$(OCAMLC) $(OCAMLFLAG) -c $<
.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<
.mll.ml: 
	$(OCAMLLEX) $<
.mly.ml:
	$(OCAMLYACC) $<

# Start again

clean:
	rm -f pwc
	rm -f *.cmo *.cmi lexer.ml parser.ml parser.mli

realclean:
	rm -f pwc
	rm -f *.cmo *.cmi lexer.ml parser.ml parser.mli
	rm -f .depend
	touch .depend

# Generate new dependencies

depend: lexer.ml parser.ml parser.mli
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

include .depend

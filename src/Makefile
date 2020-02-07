OCAMLC = ocamlfind c
OCAMLOPT = ocamlfind opt
OCAMLDEP = ocamlfind dep
SOURCES = Ast.ml PParser.ml Unify.ml PEnv.ml SLD.ml Driver.ml
CAMLP5 = -syntax camlp5o -package ostap.syntax -package GT.syntax.all
#-pp "camlp5o -I `ocamlfind -query ostap.syntax` -I `ocamlfind -query GT.syntax` pa_ostap.cmo pa_gt.cmo -L `ocamlfind -query GT.syntax`"
PXFLAGS = $(CAMLP5)
BFLAGS = -rectypes -package GT -package ostap -linkpkg
#-I `ocamlfind -query ostap` -I `ocamlfind -query GT` -I `ocamlfind -query re` -I `ocamlfind -query re.emacs` -I `ocamlfind -query re.str`
OFLAGS = $(BFLAGS)

all: .depend MicroProlog.opt MicroProlog.byte


.depend: $(SOURCES)
	$(OCAMLDEP) $(PXFLAGS) *.ml > .depend

MicroProlog.opt: $(SOURCES:.ml=.cmx)
	$(OCAMLOPT) -o MicroProlog.opt $(OFLAGS) $(SOURCES:.ml=.cmx)

MicroProlog.byte: $(SOURCES:.ml=.cmo)
	$(OCAMLC) -o MicroProlog.byte $(BFLAGS) $(SOURCES:.ml=.cmo)

clean:
	rm -Rf *.cmi *.cmo *.cmx *.annot *.o

-include .depend
# generic rules

###############
%.cmi: %.mli
	$(OCAMLC) -c $(BFLAGS) $(PXFLAGS) $<

# Note: cmi <- mli should go first
%.cmi: %.ml
	$(OCAMLC) -c $(BFLAGS) $(PXFLAGS) $<

%.cmo: %.ml
	$(OCAMLC) -c $(BFLAGS) $(PXFLAGS) $<

%.o: %.ml
	$(OCAMLOPT) -c $(OFLAGS) $(STATIC) $(PXFLAGS) $<

%.cmx: %.ml
	$(OCAMLOPT) -c $(OFLAGS) $(STATIC) $(PXFLAGS) $<


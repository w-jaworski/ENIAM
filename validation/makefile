OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
INCLUDES=-I +xml-light -I +xlib -I +zarith -I +zip -I +bz2 -I +eniam
OCAMLFLAGS=$(INCLUDES) -g
OCAMLOPTFLAGS=$(INCLUDES) unix.cmxa xml-light.cmxa str.cmxa zarith.cmxa zip.cmxa bz2.cmxa xlib.cmxa \
  eniam-fuzzyAnalyzer.cmxa eniam-morphology.cmxa eniam-subsyntax.cmxa eniam-lcg-parser.cmxa \
	eniam-lcg-lexicon.cmxa \
	eniam-valence.cmxa eniam-semantics.cmxa eniam-exec.cmxa

SOURCES1=schema.ml
SOURCES2=selector.ml

all: schema selector validator vereniam

schema: $(SOURCES1)
	ocamlopt -o schema $(OCAMLOPTFLAGS) $^
	
selector: $(SOURCES2)
	ocamlopt -o selector $(OCAMLOPTFLAGS) $^
	
validator: validator.ml
	$(OCAMLOPT) -o validator $(OCAMLOPTFLAGS) $^

vereniam: vereniam.ml
	$(OCAMLOPT) -o vereniam $(OCAMLOPTFLAGS) $^

clean:
	rm -f *~ *.cm[aoix] *.o *.so *.cmxa *.a schema selector validator vereniam

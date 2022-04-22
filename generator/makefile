OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
INCLUDES=-I +xml-light -I +xlib -I +zip -I +bz2 -I +eniam
OCAMLFLAGS=$(INCLUDES) -g
OCAMLOPTFLAGS=$(INCLUDES) unix.cmxa xml-light.cmxa str.cmxa nums.cmxa zip.cmxa bz2.cmxa xlib.cmxa \
  eniam-morphology.cmxa eniam-subsyntax.cmxa #eniam-generator.cmxa
INSTALLDIR=`ocamlc -where`/eniam

SOURCES= canonicalParser.ml generator.ml

all: eniam-generator.cma eniam-generator.cmxa
#all: eniam-generator.cmxa

install: all
	mkdir -p $(INSTALLDIR)
	cp eniam-generator.cmxa eniam-generator.a eniam-generator.cma $(INSTALLDIR)
	cp canonicalParser.cmi generator.cmi $(INSTALLDIR)
	cp canonicalParser.cmx generator.cmx $(INSTALLDIR)
#	mkdir -p /usr/share/eniam/generator
#	cp resources/*  /usr/share/eniam/generator

install-local: all
	mkdir -p $(INSTALLDIR)
	cp eniam-generator.cmxa eniam-generator.a eniam-generator.cma $(INSTALLDIR)
	cp canonicalParser.cmi generator.cmi $(INSTALLDIR)
	cp canonicalParser.cmx generator.cmx $(INSTALLDIR)
#	mkdir -p /usr/local/share/eniam/generator
#	cp resources/*  /usr/local/share/eniam/generator

eniam-generator.cma: $(SOURCES)
	ocamlc -linkall -a -o eniam-generator.cma $(OCAMLFLAGS) $^

eniam-generator.cmxa: $(SOURCES)
	ocamlopt -linkall -a -o eniam-generator.cmxa $(INCLUDES) $^

test: $(SOURCES) test.ml
	$(OCAMLOPT) -o test $(OCAMLOPTFLAGS) $(SOURCES) test.ml

inflection: inflection.ml
	$(OCAMLOPT) -o inflection $(OCAMLOPTFLAGS) eniam-generator.cmxa inflection.ml

.SUFFIXES: .mll .mly .ml .mli .cmo .cmi .cmx

.mll.ml:
	ocamllex $<

.mly.mli:
	ocamlyacc $<

.mly.ml:
	ocamlyacc $<

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFALGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

clean:
	rm -f *~ *.cm[aoix] *.o *.so *.cmxa *.a test inflection

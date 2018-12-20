OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
INCLUDES=-I +xml-light -I +xlib -I +zip -I +bz2 -I +eniam
OCAMLFLAGS=$(INCLUDES) -g
OCAMLOPTFLAGS=$(INCLUDES) unix.cmxa xml-light.cmxa str.cmxa nums.cmxa zip.cmxa bz2.cmxa xlib.cmxa \
  eniam-morphology.cmxa eniam-tokenizer.cmxa eniam-subsyntax.cmxa
INSTALLDIR=`ocamlc -where`/eniam

SOURCES= subsyntaxTypes.ml subsyntaxStringOf.ml subsyntaxHTMLof.ml subsyntaxXMLof.ml subsyntaxGraphOf.ml paths.ml MWE.ml sentences.ml concraft.ml subsyntax.ml

#all: eniam-subsyntax.cma eniam-subsyntax.cmxa
all: eniam-subsyntax.cma eniam-subsyntax.cmxa

install: all
	mkdir -p $(INSTALLDIR)
	cp eniam-subsyntax.cmxa eniam-subsyntax.a eniam-subsyntax.cma $(INSTALLDIR)
	cp subsyntaxTypes.cmi subsyntaxStringOf.cmi subsyntaxHTMLof.cmi subsyntaxXMLof.cmi subsyntaxGraphOf.cmi paths.cmi MWE.cmi sentences.cmi subsyntax.cmi $(INSTALLDIR)
	cp subsyntaxTypes.cmx subsyntaxStringOf.cmx subsyntaxHTMLof.cmx subsyntaxXMLof.cmx subsyntaxGraphOf.cmx paths.cmx MWE.cmx sentences.cmx subsyntax.cmx $(INSTALLDIR)
	mkdir -p /usr/share/eniam/subsyntax
	cp resources/*  /usr/share/eniam/subsyntax

install-local: all
	mkdir -p $(INSTALLDIR)
	cp eniam-subsyntax.cmxa eniam-subsyntax.a eniam-subsyntax.cma $(INSTALLDIR)
	cp subsyntaxTypes.cmi subsyntaxStringOf.cmi subsyntaxHTMLof.cmi subsyntaxXMLof.cmi subsyntaxGraphOf.cmi paths.cmi MWE.cmi sentences.cmi subsyntax.cmi $(INSTALLDIR)
	cp subsyntaxTypes.cmx subsyntaxStringOf.cmx subsyntaxHTMLof.cmx subsyntaxXMLof.cmx subsyntaxGraphOf.cmx paths.cmx MWE.cmx sentences.cmx subsyntax.cmx $(INSTALLDIR)
	mkdir -p /usr/local/share/eniam/subsyntax
	cp resources/*  /usr/local/share/eniam/subsyntax

eniam-subsyntax.cma: $(SOURCES)
	ocamlc -linkall -a -o eniam-subsyntax.cma $(OCAMLFLAGS) $^

eniam-subsyntax.cmxa: $(SOURCES)
	ocamlopt -linkall -a -o eniam-subsyntax.cmxa $(INCLUDES) $^

test: test.ml
	$(OCAMLOPT) -o test $(OCAMLOPTFLAGS) test.ml

interface: interface.ml
	$(OCAMLOPT) -o subsyntax $(OCAMLOPTFLAGS) interface.ml

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
	rm -f *~ *.cm[aoix] *.o *.so *.cmxa *.a test subsyntax

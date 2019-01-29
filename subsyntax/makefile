OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
INCLUDES=-I +xml-light -I +xlib -I +zip -I +bz2 -I +eniam
OCAMLFLAGS=$(INCLUDES) -g
OCAMLOPTFLAGS=$(INCLUDES) unix.cmxa xml-light.cmxa str.cmxa nums.cmxa zip.cmxa bz2.cmxa xlib.cmxa \
  eniam-morphology.cmxa eniam-subsyntax.cmxa
INSTALLDIR=`ocamlc -where`/eniam

SOURCES= subsyntaxTypes.ml tagset.ml url.ml tokenizer.ml subsyntaxStringOf.ml lemmatization.ml patterns.ml dataLoader.ml subsyntaxHTMLof.ml subsyntaxXMLof.ml subsyntaxGraphOf.ml MWE.ml coordination.ml sentences.ml concraft.ml subsyntax.ml markedHTMLof.ml

all: eniam-subsyntax.cma eniam-subsyntax.cmxa
#all: eniam-subsyntax.cmxa

install: all
	mkdir -p $(INSTALLDIR)
	cp eniam-subsyntax.cmxa eniam-subsyntax.a eniam-subsyntax.cma $(INSTALLDIR)
	cp subsyntaxTypes.cmi tagset.cmi url.cmi tokenizer.cmi lemmatization.cmi patterns.cmi subsyntaxStringOf.cmi subsyntaxHTMLof.cmi subsyntaxXMLof.cmi subsyntaxGraphOf.cmi dataLoader.cmi MWE.cmi coordination.cmi sentences.cmi subsyntax.cmi markedHTMLof.cmi $(INSTALLDIR)
	cp subsyntaxTypes.cmx tagset.cmx url.cmx tokenizer.cmx lemmatization.cmx patterns.cmx subsyntaxStringOf.cmx subsyntaxHTMLof.cmx subsyntaxXMLof.cmx subsyntaxGraphOf.cmx dataLoader.cmx MWE.cmx coordination.cmx sentences.cmx subsyntax.cmx markedHTMLof.cmx $(INSTALLDIR)
	mkdir -p /usr/share/eniam/subsyntax
	cp resources/*  /usr/share/eniam/subsyntax

install-local: all
	mkdir -p $(INSTALLDIR)
	cp eniam-subsyntax.cmxa eniam-subsyntax.a eniam-subsyntax.cma $(INSTALLDIR)
	cp subsyntaxTypes.cmi tagset.cmi url.cmi tokenizer.cmi lemmatization.cmi patterns.cmi subsyntaxStringOf.cmi subsyntaxHTMLof.cmi subsyntaxXMLof.cmi subsyntaxGraphOf.cmi dataLoader.cmi MWE.cmi coordination.cmi sentences.cmi subsyntax.cmi markedHTMLof.cmi $(INSTALLDIR)
	cp subsyntaxTypes.cmx tagset.cmx url.cmx tokenizer.cmx lemmatization.cmx patterns.cmx subsyntaxStringOf.cmx subsyntaxHTMLof.cmx subsyntaxXMLof.cmx subsyntaxGraphOf.cmx dataLoader.cmx MWE.cmx coordination.cmx sentences.cmx subsyntax.cmx markedHTMLof.cmx $(INSTALLDIR)
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

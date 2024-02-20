OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
INCLUDES=-I +xml-light -I +xlib -I +zarith -I +zip -I +bz2 -I +eniam
OCAMLFLAGS=$(INCLUDES) -g
OCAMLOPTFLAGS=$(INCLUDES) unix.cmxa xml-light.cmxa str.cmxa zarith.cmxa zip.cmxa bz2.cmxa xlib.cmxa \
  eniam-fuzzyAnalyzer.cmxa eniam-morphology.cmxa eniam-subsyntax.cmxa eniam-lcg-parser.cmxa \
	eniam-lcg-lexicon.cmxa \
	eniam-valence.cmxa eniam-semantics.cmxa eniam-exec.cmxa
INSTALLDIR=`ocamlc -where`/eniam

SOURCES= execTypes.ml visualization.ml exec.ml selectSent.ml execXMLof.ml domExec.ml exec2.ml

all: eniam-exec.cma eniam-exec.cmxa

install: all
	mkdir -p $(INSTALLDIR)
	cp eniam-exec.cmxa eniam-exec.a eniam-exec.cma $(INSTALLDIR)
	cp execTypes.cmi exec.cmi domExec.cmi selectSent.cmi execXMLof.cmi visualization.cmi exec2.cmi $(INSTALLDIR)
	cp execTypes.cmx exec.cmx domExec.cmx selectSent.cmx execXMLof.cmx visualization.cmx exec2.cmx $(INSTALLDIR)

install-local: all
	mkdir -p $(INSTALLDIR)
	cp eniam-exec.cmxa eniam-exec.a eniam-exec.cma $(INSTALLDIR)
	cp execTypes.cmi exec.cmi domExec.cmi selectSent.cmi execXMLof.cmi visualization.cmi exec2.cmi $(INSTALLDIR)
	cp execTypes.cmx exec.cmx domExec.cmx selectSent.cmx execXMLof.cmx visualization.cmx exec2.cmx $(INSTALLDIR)

eniam-exec.cma: $(SOURCES)
	ocamlc -linkall -a -o eniam-exec.cma $(OCAMLFLAGS) $^

eniam-exec.cmxa: $(SOURCES)
	ocamlopt -linkall -a -o eniam-exec.cmxa $(INCLUDES) $^

eniam: eniam.ml
	$(OCAMLOPT) -o eniam $(OCAMLOPTFLAGS) $^

distriparser: distriparser.ml
	$(OCAMLOPT) -o distriparser $(OCAMLOPTFLAGS) $^

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
	rm -f *~ *.cm[aoix] *.o *.so *.cmxa *.a parser test mergertest semparser parser3.cgi parser.api

OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
INCLUDES=-I +xml-light -I +xlib -I +zarith -I +zip -I +bz2 -I +eniam
OCAMLFLAGS=$(INCLUDES) -g
OCAMLOPTFLAGS=$(INCLUDES) unix.cmxa xml-light.cmxa str.cmxa zarith.cmxa zip.cmxa bz2.cmxa xlib.cmxa \
  eniam-morphology.cmxa eniam-subsyntax.cmxa \
	eniam-lcg-parser.cmxa eniam-lcg-lexicon.cmxa \
	eniam-valence.cmxa #eniam-semantics.cmxa
INSTALLDIR=`ocamlc -where`/eniam

SOURCES= semTypes.ml disambiguation.ml coreference.ml semStringOf.ml semXMLof.ml semGraph.ml semLatexOf.ml semGraphOf.ml disamb.ml domSemantics.ml inferenceRulesParser.ml inference.ml json.ml 

all: eniam-semantics.cma eniam-semantics.cmxa

install: all
	mkdir -p $(INSTALLDIR)
	cp eniam-semantics.cmxa eniam-semantics.a eniam-semantics.cma $(INSTALLDIR)
	cp semTypes.cmi disambiguation.cmi coreference.cmi semStringOf.cmi semGraph.cmi semXMLof.cmi semLatexOf.cmi semGraphOf.cmi disamb.cmi domSemantics.cmi inferenceRulesParser.cmi inference.cmi json.cmi $(INSTALLDIR)
	cp semTypes.cmx disambiguation.cmx coreference.cmx semStringOf.cmx semGraph.cmx semXMLof.cmx semLatexOf.cmx semGraphOf.cmx disamb.cmx domSemantics.cmx inferenceRulesParser.cmx inference.cmx json.cmx $(INSTALLDIR)
#	mkdir -p /usr/share/eniam/semantics
#	cp resources/*  /usr/share/eniam/semantics

install-local: all
	mkdir -p $(INSTALLDIR)
	cp eniam-semantics.cmxa eniam-semantics.a eniam-semantics.cma $(INSTALLDIR)
	cp semTypes.cmi disambiguation.cmi coreference.cmi semStringOf.cmi semGraph.cmi semXMLof.cmi semLatexOf.cmi semGraphOf.cmi disamb.cmi domSemantics.cmi inferenceRulesParser.cmi inference.cmi json.cmi $(INSTALLDIR)
	cp semTypes.cmx disambiguation.cmx coreference.cmx semStringOf.cmx semGraph.cmx semXMLof.cmx semLatexOf.cmx semGraphOf.cmx disamb.cmx domSemantics.cmx inferenceRulesParser.cmx inference.cmx json.cmx $(INSTALLDIR)
#	mkdir -p /usr/local/share/eniam/semantics
#	cp resources/*  /usr/local/share/eniam/semantics

eniam-semantics.cma: $(SOURCES)
	ocamlc -linkall -a -o eniam-semantics.cma $(OCAMLFLAGS) $^

eniam-semantics.cmxa: $(SOURCES)
	ocamlopt -linkall -a -o eniam-semantics.cmxa $(INCLUDES) $^

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
	rm -f *~ *.cm[aoix] *.o *.so *.cmxa *.a

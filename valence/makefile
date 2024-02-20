OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
INCLUDES=-I +xml-light -I +xlib -I +zarith -I +zip -I +bz2 -I +eniam
OCAMLFLAGS=$(INCLUDES) -g
OCAMLOPTFLAGS=$(INCLUDES) unix.cmxa xml-light.cmxa str.cmxa zarith.cmxa zip.cmxa bz2.cmxa xlib.cmxa \
  eniam-morphology.cmxa eniam-tokenizer.cmxa eniam-subsyntax.cmxa eniam-lcg-parser.cmxa eniam-lcg-lexicon.cmxa eniam-lexSemantics.cmxa eniam-valence.cmxa
INSTALLDIR=`ocamlc -where`/eniam

SOURCES= walTypes.ml entries.ml lexSemanticsTypes.ml valParser.ml walReduce.ml walStringOf.ml valence.ml walRenderer.ml lexSemantics.ml domainLexSemantics.ml

all: eniam-valence.cma eniam-valence.cmxa

install: all
	mkdir -p $(INSTALLDIR)
	cp eniam-valence.cmxa eniam-valence.a eniam-valence.cma $(INSTALLDIR)
	cp walTypes.cmi entries.cmi lexSemanticsTypes.cmi valParser.cmi walReduce.cmi walStringOf.cmi valence.cmi walRenderer.cmi lexSemantics.cmi domainLexSemantics.cmi $(INSTALLDIR)
	cp walTypes.cmx entries.cmx lexSemanticsTypes.cmx valParser.cmx walReduce.cmx walStringOf.cmx valence.cmx walRenderer.cmx lexSemantics.cmx domainLexSemantics.cmx $(INSTALLDIR)
#	mkdir -p /usr/share/eniam/valence
#	cp resources/*  /usr/share/eniam/valence

install-local: all
	mkdir -p $(INSTALLDIR)
	cp eniam-valence.cmxa eniam-valence.a eniam-valence.cma $(INSTALLDIR)
	cp walTypes.cmi entries.cmi lexSemanticsTypes.cmi valParser.cmi walReduce.cmi walStringOf.cmi valence.cmi walRenderer.cmi lexSemantics.cmi domainLexSemantics.cmi $(INSTALLDIR)
	cp walTypes.cmx entries.cmx lexSemanticsTypes.cmx valParser.cmx walReduce.cmx walStringOf.cmx valence.cmx walRenderer.cmx lexSemantics.cmx domainLexSemantics.cmx $(INSTALLDIR)
#	mkdir -p /usr/local/share/eniam/valence
#	cp resources/*  /usr/local/share/eniam/valence

eniam-valence.cma: $(SOURCES)
	ocamlc -linkall -a -o eniam-valence.cma $(OCAMLFLAGS) $^

eniam-valence.cmxa: $(SOURCES)
	ocamlopt -linkall -a -o eniam-valence.cmxa $(INCLUDES) $^


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
	rm -f *~ *.cm[aoix] *.o *.so *.cmxa *.a test inttest

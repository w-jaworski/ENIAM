OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
INCLUDES=-I +xml-light -I +xlib -I +zip -I +bz2 -I +eniam -I /home/yacheu/Dropbox/Selidor/chaty
OCAMLFLAGS=$(INCLUDES) -g
OCAMLOPTFLAGS=$(INCLUDES) unix.cmxa xml-light.cmxa str.cmxa nums.cmxa zip.cmxa bz2.cmxa xlib.cmxa #eniam-morphology.cmxa
INSTALLDIR=`ocamlc -where`/eniam

SOURCES= ENIAMfuzzyDetector.ml

all: eniam-fuzzyAnalyzer.cma eniam-fuzzyAnalyzer.cmxa

install: all
	mkdir -p $(INSTALLDIR)
	cp eniam-fuzzyAnalyzer.cmxa eniam-fuzzyAnalyzer.a eniam-fuzzyAnalyzer.cma $(INSTALLDIR)
	cp ENIAMfuzzyDetector.cmi $(INSTALLDIR)
	cp ENIAMfuzzyDetector.cmx $(INSTALLDIR)
	mkdir -p /usr/share/eniam/fuzzyAnalyzer
	cp resources/*  /usr/share/eniam/fuzzyAnalyzer

install-local: all
	mkdir -p $(INSTALLDIR)
	cp eniam-fuzzyAnalyzer.cmxa eniam-fuzzyAnalyzer.a eniam-fuzzyAnalyzer.cma $(INSTALLDIR)
	cp ENIAMfuzzyDetector.cmi $(INSTALLDIR)
	cp ENIAMfuzzyDetector.cmx $(INSTALLDIR)
	mkdir -p /usr/local/share/eniam/fuzzyAnalyzer
	cp resources/*  /usr/local/share/eniam/fuzzyAnalyzer

eniam-fuzzyAnalyzer.cma: $(SOURCES)
	ocamlc -linkall -a -o eniam-fuzzyAnalyzer.cma $(OCAMLFLAGS) $^

eniam-fuzzyAnalyzer.cmxa: $(SOURCES)
	ocamlopt -linkall -a -o eniam-fuzzyAnalyzer.cmxa $(INCLUDES) $^

interface: interface.ml $(SOURCES)
	$(OCAMLOPT) -o corrector $(OCAMLOPTFLAGS) $(SOURCES) interface.ml

analyze: /home/yacheu/Dropbox/Selidor/chaty/chatDriver.ml /home/yacheu/Dropbox/Selidor/chaty/chatDriver2.ml $(SOURCES) analyze.ml
	$(OCAMLOPT) -o analyze $(OCAMLOPTFLAGS) /home/yacheu/Dropbox/Selidor/chaty/chatDriver.ml /home/yacheu/Dropbox/Selidor/chaty/chatDriver2.ml $(SOURCES) analyze.ml


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
	rm -f *~ *.cm[aoix] *.o *.so *.cmxa *.a analyze corrector

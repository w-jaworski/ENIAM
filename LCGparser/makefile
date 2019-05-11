OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
INCLUDES=-I +xml-light -I +xlib -I +zip -I +bz2 -I +eniam
OCAMLFLAGS=$(INCLUDES) -g
OCAMLOPTFLAGS=$(INCLUDES) unix.cmxa xml-light.cmxa str.cmxa nums.cmxa zip.cmxa bz2.cmxa xlib.cmxa eniam-lcg-parser.cmxa
INSTALLDIR=`ocamlc -where`/eniam

SOURCES= LCGtypes.ml LCGstringOf.ml LCGrules.ml LCGrenderer.ml LCGchart.ml LCGlatexOf.ml LCGreductions.ml LCGgraphOf.ml LCG_XMLof.ml LCG_JSONof.ml

all: eniam-lcg-parser.cma eniam-lcg-parser.cmxa

install: all
	mkdir -p $(INSTALLDIR)
	cp eniam-lcg-parser.cmxa eniam-lcg-parser.a eniam-lcg-parser.cma $(INSTALLDIR)
	cp LCGtypes.cmi LCGstringOf.cmi LCGrules.cmi LCGrenderer.cmi LCGchart.cmi LCGlatexOf.cmi LCGreductions.cmi LCGgraphOf.cmi LCG_XMLof.cmi LCG_JSONof.cmi $(INSTALLDIR)
	cp LCGtypes.cmx LCGstringOf.cmx LCGrules.cmx LCGrenderer.cmx LCGchart.cmx LCGlatexOf.cmx LCGreductions.cmx LCGgraphOf.cmx LCG_XMLof.cmx LCG_JSONof.cmx $(INSTALLDIR)

install-local: all
	mkdir -p $(INSTALLDIR)
	cp eniam-lcg-parser.cmxa eniam-lcg-parser.a eniam-lcg-parser.cma $(INSTALLDIR)
	cp LCGtypes.cmi LCGstringOf.cmi LCGrules.cmi LCGrenderer.cmi LCGchart.cmi LCGlatexOf.cmi LCGreductions.cmi LCGgraphOf.cmi LCG_XMLof.cmi LCG_JSONof.cmi $(INSTALLDIR)
	cp LCGtypes.cmx LCGstringOf.cmx LCGrules.cmx LCGrenderer.cmx LCGchart.cmx LCGlatexOf.cmx LCGreductions.cmx LCGgraphOf.cmx LCG_XMLof.cmx LCG_JSONof.cmx $(INSTALLDIR)

eniam-lcg-parser.cma: $(SOURCES)
	ocamlc -linkall -a -o eniam-lcg-parser.cma $(OCAMLFLAGS) $^

eniam-lcg-parser.cmxa: $(SOURCES)
	ocamlopt -linkall -a -o eniam-lcg-parser.cmxa $(INCLUDES) $^

test: test.ml
	mkdir -p results
	$(OCAMLOPT) -o test $(OCAMLOPTFLAGS) test.ml

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
	rm -f *~ *.cm[aoix] *.o *.so *.cmxa *.a test

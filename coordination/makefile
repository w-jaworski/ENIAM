OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
INCLUDES=-I +xml-light -I +xlib -I +zip -I +bz2 -I +eniam
OCAMLFLAGS=$(INCLUDES) -g
OCAMLOPTFLAGS=$(INCLUDES) unix.cmxa xml-light.cmxa str.cmxa nums.cmxa zip.cmxa bz2.cmxa xlib.cmxa \
  eniam-morphology.cmxa eniam-tokenizer.cmxa eniam-subsyntax.cmxa eniam-coordination.cmxa
INSTALLDIR=`ocamlc -where`/eniam

SOURCES= coordination.ml

all: eniam-coordination.cma eniam-coordination.cmxa

install: all
	mkdir -p $(INSTALLDIR)
	cp eniam-coordination.cmxa eniam-coordination.a eniam-coordination.cma $(INSTALLDIR)
	cp coordination.cmi $(INSTALLDIR)
	cp coordination.cmx $(INSTALLDIR)
#	mkdir -p /usr/share/eniam/coordination
#	cp resources/*  /usr/share/eniam/coordination

install-local: all
	mkdir -p $(INSTALLDIR)
	cp eniam-coordination.cmxa eniam-coordination.a eniam-coordination.cma $(INSTALLDIR)
	cp coordination.cmi $(INSTALLDIR)
	cp coordination.cmx $(INSTALLDIR)
#	mkdir -p /usr/local/share/eniam/coordination
#	cp resources/*  /usr/local/share/eniam/coordination

eniam-coordination.cma: $(SOURCES)
	ocamlc -linkall -a -o eniam-coordination.cma $(OCAMLFLAGS) $^

eniam-coordination.cmxa: $(SOURCES)
	ocamlopt -linkall -a -o eniam-coordination.cmxa $(INCLUDES) $^

#test: test.ml
#	$(OCAMLOPT) -o test $(OCAMLOPTFLAGS) test.ml

interface: interface.ml
	$(OCAMLOPT) -o coordination $(OCAMLOPTFLAGS) interface.ml

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
	rm -f *~ *.cm[aoix] *.o *.so *.cmxa *.a test coordination

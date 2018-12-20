OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
INCLUDES=-I +xml-light -I +xlib -I +zip -I +bz2 -I +eniam
OCAMLFLAGS=$(INCLUDES) -g
OCAMLOPTFLAGS=$(INCLUDES) unix.cmxa xml-light.cmxa str.cmxa nums.cmxa zip.cmxa bz2.cmxa xlib.cmxa eniam-morphology.cmxa eniam-tokenizer.cmxa
INSTALLDIR=`ocamlc -where`/eniam

SOURCES= tokenizerTypes.ml tagset.ml url.ml tokens.ml lemmatization.ml acronyms.ml patterns.ml tokenizer.ml

all: eniam-tokenizer.cma eniam-tokenizer.cmxa

install: all
	mkdir -p $(INSTALLDIR)
	cp eniam-tokenizer.cmxa eniam-tokenizer.a eniam-tokenizer.cma $(INSTALLDIR)
	cp tokenizerTypes.cmi tagset.cmi url.cmi tokens.cmi lemmatization.cmi acronyms.cmi patterns.cmi tokenizer.cmi $(INSTALLDIR)
	cp tokenizerTypes.cmx tagset.cmx url.cmx tokens.cmx lemmatization.cmx acronyms.cmx patterns.cmx tokenizer.cmx $(INSTALLDIR)
	mkdir -p /usr/share/eniam/tokenizer
	cp resources/*  /usr/share/eniam/tokenizer

install-local: all
	mkdir -p $(INSTALLDIR)
	cp eniam-tokenizer.cmxa eniam-tokenizer.a eniam-tokenizer.cma $(INSTALLDIR)
	cp tokenizerTypes.cmi tagset.cmi url.cmi tokens.cmi lemmatization.cmi acronyms.cmi patterns.cmi tokenizer.cmi $(INSTALLDIR)
	cp tokenizerTypes.cmx tagset.cmx url.cmx tokens.cmx lemmatization.cmx acronyms.cmx patterns.cmx tokenizer.cmx $(INSTALLDIR)
	mkdir -p /usr/local/share/eniam/tokenizer
	cp resources/mte_20151215.tab  /usr/local/share/eniam/tokenizer/mte_20151215.tab
	cp resources/top-level-domains.tab  /usr/local/share/eniam/tokenizer/top-level-domains.tab
	cp resources/README  /usr/local/share/eniam/tokenizer/README
#	ln -s /usr/local/share/eniam/tokenizer/mte_20151215.tab /usr/local/share/eniam/tokenizer/mte.tab

eniam-tokenizer.cma: $(SOURCES)
	ocamlc -linkall -a -o eniam-tokenizer.cma $(OCAMLFLAGS) $^

eniam-tokenizer.cmxa: $(SOURCES)
	ocamlopt -linkall -a -o eniam-tokenizer.cmxa $(INCLUDES) $^

test: test.ml
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

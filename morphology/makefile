OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
INCLUDES=-I +xml-light -I +xlib -I +zip -I +bz2 -I +eniam
OCAMLFLAGS=$(INCLUDES) -g
OCAMLOPTFLAGS=$(INCLUDES) unix.cmxa xml-light.cmxa str.cmxa nums.cmxa zip.cmxa bz2.cmxa xlib.cmxa
OCAMLOPTFLAGS2=$(OCAMLOPTFLAGS) eniam-morphology.cmxa
INSTALLDIR=`ocamlc -where`/eniam

MODS=morphologyTypes.ml morphologyRules.ml fonetics.ml stem.ml ruleGenerator.ml dict.ml inflexion.ml
SOURCES= morphologyTypes.ml morphologyRules.ml inflexion.ml

analyze: $(MODS) analyze.ml
	mkdir -p results
	$(OCAMLOPT) -o analyze $(OCAMLOPTFLAGS) $(MODS) analyze.ml

generate: $(MODS) generate.ml
	mkdir -p resources
	$(OCAMLOPT) -o generate $(OCAMLOPTFLAGS) $(MODS) generate.ml

all: eniam-morphology.cma eniam-morphology.cmxa

install: all
	mkdir -p $(INSTALLDIR)
	cp eniam-morphology.cmxa eniam-morphology.a eniam-morphology.cma $(INSTALLDIR)
	cp morphologyTypes.cmi morphologyRules.cmi inflexion.cmi $(INSTALLDIR)
	cp morphologyTypes.cmx morphologyRules.cmx inflexion.cmx $(INSTALLDIR)
	mkdir -p /usr/share/eniam/morphology
	cp resources/*  /usr/share/eniam/morphology

install-local: all
	mkdir -p $(INSTALLDIR)
	cp eniam-morphology.cmxa eniam-morphology.a eniam-morphology.cma $(INSTALLDIR)
	cp morphologyTypes.cmi morphologyRules.cmi inflexion.cmi $(INSTALLDIR)
	cp morphologyTypes.cmx morphologyRules.cmx inflexion.cmx $(INSTALLDIR)
	mkdir -p /usr/local/share/eniam/morphology
	cp resources/*  /usr/local/share/eniam/morphology

eniam-morphology.cma: $(SOURCES)
	ocamlc -linkall -a -o eniam-morphology.cma $(OCAMLFLAGS) $^

eniam-morphology.cmxa: $(SOURCES)
	ocamlopt -linkall -a -o eniam-morphology.cmxa $(INCLUDES) $^

test: test.ml
	$(OCAMLOPT) -o test $(OCAMLOPTFLAGS2) test.ml

interface: interface.ml
	$(OCAMLOPT) -o morphology $(OCAMLOPTFLAGS2) interface.ml

api: apiInterface.ml
	$(OCAMLOPT) -o morphology.api $(OCAMLOPTFLAGS2) $^

web: webInterface.ml webInterface2.ml
	$(OCAMLOPT) -o morphology.cgi $(OCAMLOPTFLAGS2) webInterface.ml
	$(OCAMLOPT) -o morphology2.cgi $(OCAMLOPTFLAGS2) webInterface2.ml

freq_test: freq_test.ml
	$(OCAMLOPT) -o freq_test $(OCAMLOPTFLAGS) freq_test.ml

suffix_tree: suffix_tree.ml
	$(OCAMLOPT) -o suffix_tree $(OCAMLOPTFLAGS) suffix_tree.ml

# freq_list_interps: freqUtils.ml freqListInterps.ml
# 	$(OCAMLOPT) -o freq_list_interps $(OCAMLOPTFLAGS) inflexion.cmxa freqUtils.ml freqListInterps.ml
#
# freq_stats: freqUtils.ml freqStats.ml
# 	$(OCAMLOPT) -o freq_stats $(OCAMLOPTFLAGS) inflexion.cmxa freqUtils.ml freqStats.ml

# freq_probs: freqUtils.ml freqProbs.ml
# 	$(OCAMLOPT) -o freq_probs $(OCAMLOPTFLAGS) inflexion.cmxa freqUtils.ml freqProbs.ml

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
	rm -f *~ *.cm[oixa] *.cmxa *.o *.a analyze generate suffix_tree freq_list_interps morphology morphology.cgi morphology2.cgi test

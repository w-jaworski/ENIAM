OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
INCLUDES=-I +xml-light -I +xlib -I +zarith -I +zip -I +bz2 -I +lablgtk2 -I +eniam
OCAMLFLAGS=$(INCLUDES)
OCAMLOPTFLAGS=$(INCLUDES) unix.cmxa threads.cmxa lablgtk.cmxa xml-light.cmxa str.cmxa zarith.cmxa zip.cmxa bz2.cmxa xlib.cmxa \
  eniam-morphology.cmxa eniam-subsyntax.cmxa

LEX_CREATOR= progress.ml types.ml verse.ml project.ml subcorpus_viewer.ml verse_viewer.ml project_viewer.ml main.ml 

VERSE_WORKER= types.ml verse_worker.ml 

all: 
	$(OCAMLOPT) -o lexcreator -thread $(OCAMLOPTFLAGS) gtkThread.cmx gtkInit.cmx $(LEX_CREATOR) 
	$(OCAMLOPT) -o verse_worker -thread $(OCAMLOPTFLAGS) $(VERSE_WORKER) 

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
	rm -f *~ *.cm[oix] *.o lexcreator verse_worker

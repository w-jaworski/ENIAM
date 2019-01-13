(*
 *  LCGlexicon is a library that provides LCG lexicon form Polish
 *  Copyright (C) 2016 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
 *  Copyright (C) 2016 Institute of Computer Science Polish Academy of Sciences
 *
 *  This library is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

open LCGlexiconTypes
open LCGtypes
open Xstd

let rules = LCGlexicon.make_rules false LCGlexiconTypes.rules_filename
(* let rules = LCGlexicon.make_rules false "resources/lexicon-pl.dic" *)

let examples = [
(*  "kot",[
    1, 0, 1, "","<sentence>","interp",    [],false;
    2, 1, 2, "","<clause>","interp",      [],false;
    3, 2, 3, "Ala","Ala","subst",         [["sg"];["nom"];["f"]],true;
    4, 3, 4, "ma","mieć","fin",           [["sg"];["ter"];["imperf"]],false;
    5, 4, 5, "kota","kot","subst",        [["sg"];["gen";"acc"];["m1";"m2"]],false;
    6, 5, 6, "","</clause>","interp",     [],false;
    7, 6, 7, ".","</sentence>","interp",  [],false;
  ],7;
  "kota",[
    1, 0, 1, "","<sentence>","interp",    [],false;
    2, 1, 2, "","<clause>","interp",      [],false;
    3, 2, 3, "Ala","Ala","subst",         [["sg"];["nom"];["f"]],true;
    4, 2, 3, "Ala","Al","subst",          [["sg"];["gen";"acc"];["m1"]],true;
    5, 3, 4, "ma","mieć","fin",           [["sg"];["ter"];["imperf"]],false;
    6, 4, 5, "kota","kot","subst",        [["sg"];["gen";"acc"];["m1";"m2"]],false;
    7, 4, 5, "kota","kota","subst",       [["sg"];["nom"];["f"]],false;
    8, 5, 6, "","</clause>","interp",     [],false;
    9, 6, 7, ".","</sentence>","interp",  [],false;
  ],7;
  "jaki",[
    1, 0, 1, "","<sentence>","interp",    [],false;
    2, 1, 2, "","<clause>","interp",      [],false;
    3, 2, 3,  "Jakiego","jaki","adj",     [["sg"];["gen";"acc"];["m1";"m2"];["pos"]],false;
    4, 3, 4, "kota","kot","subst",        [["sg"];["gen";"acc"];["m1";"m2"]],false;
    5, 4, 5, "Ala","Ala","subst",         [["sg"];["nom"];["f"]],true;
    6, 5, 6, "ma","mieć","fin",           [["sg"];["ter"];["imperf"]],false;
    7, 6, 7, "?","?","interp",            [],false;
    8, 7, 8, "","</clause>","interp",     [],false;
    9, 8, 9, ".","</sentence>","interp",  [],false;
  ],9;*)
  "kot_i_pies",[
    1, 0, 1, "","<sentence>","interp",    [],false;
    2, 1, 2, "","<clause>","interp",      [],false;
    3, 2, 3, "Ala","Ala","subst",         [["sg"];["nom"];["f"]],true;
    4, 3, 4, "ma","mieć","fin",           [["sg"];["ter"];["imperf"]],false;
    5, 4, 5, "kota","kot","subst",        [["sg"];["gen";"acc"];["m1";"m2"]],false;
    6, 5, 6, "i","i","conj",              [],false;
    7, 6, 7, "psa","pies","subst",        [["sg"];["gen";"acc"];["m2"]],false;
    8, 7, 8, "","</clause>","interp",     [],false;
    9, 8, 9, ".","</sentence>","interp",  [],false;
    ],9;
(*"kotx",[
    1, 0, 1, "","<sentence>","interp",    [],false;
    2, 1, 2, "","<clause>","interp",      [],false;
    3, 2, 3, "Ala","Ala","subst",         [["sg"];["nom"];["f"]],true;
    4, 3, 4, "ma","mieć","fin",           [["sg"];["ter"];["imperf"]],false;
    5, 4, 5, "„","„","interp",            [],false;
    6, 5, 6, "kota","kot","subst",        [["sg"];["gen";"acc"];["m1";"m2"]],false;
    7, 6, 7, "”","”","interp",            [],false;
    8, 7, 8, "","</clause>","interp",     [],false;
    9, 8, 9, ".","</sentence>","interp",  [],false;
],9;*)
]

let valence = [
  [Lemma,Eq,["Ala";"Al"];Pos,Eq,["subst"]],[],[];
  [Lemma,Eq,["mieć"];Pos,Eq,["fin"];Negation,Eq,["aff"];Mood,Eq,["indicative"]],[],[Both,Plus[One;Tensor[Atom "np";AVar "number";Atom "nom";AVar "gender";AVar "person"]];
                                                     Both,Plus[One;Tensor[Atom "np";Top;Atom "acc";Top;Top]]];
  (* [Lemma,Eq,["mieć"];Pos,Eq,["fin"];Negation,Eq,["neg"];Mood,Eq,["indicative"]],[],[Both,Plus[One;Tensor[Atom "np";AVar "number";Atom "nom";AVar "gender";AVar "person"]];
                                                     Both,Plus[One;Tensor[Atom "np";Top;Atom "gen";Top;Top]]]; *)
  [Lemma,Eq,["kota"];Pos,Eq,["subst"]],[],[];
  [Lemma,Eq,["kot"];Pos,Eq,["subst"]],[],[Both,Plus[One;Tensor[Atom "adjp";AVar "number";AVar "case";AVar "gender"]]];
  [Lemma,Eq,["pies"];Pos,Eq,["subst"]],[],[];
]

let create_chart valence tokens last =
  LCGrenderer.reset_variable_numbers ();
  let chart = LCGchart.make last in
  let chart = Xlist.fold tokens chart (fun chart (id,lnode,rnode,orth,lemma,pos,interp,proper) ->
      LCGrenderer.reset_variable_names ();
      LCGrenderer.add_variable_numbers ();
      let cats = CategoriesPL.clarify_categories proper "X" ["X"] (lemma,pos,interp) in
      let l = LCGlexicon.create_entries rules id orth cats valence [] in
      LCGchart.add_inc_list chart lnode rnode l 0) in
  chart

let create_text_fragments tokens last =
  let text_fragments = Array.make last IntMap.empty in
  Xlist.iter tokens (fun (id,lnode,rnode,orth,lemma,pos,interp,proper) ->
    text_fragments.(lnode) <- IntMap.add text_fragments.(lnode) rnode orth);
  Int.iter_down 0 (last - 1) (fun i ->
    let map = IntMap.fold text_fragments.(i) text_fragments.(i) (fun map j orth ->
      if j = last then map else
      IntMap.fold text_fragments.(j) map (fun map k orth2 ->
        IntMap.add map k (orth ^ " " ^ orth2))) in
    text_fragments.(i) <- map);
  text_fragments

let test_example valence (name,tokens,last) =
  LCGreductions.reset_variant_label ();
  let chart = create_chart valence tokens last in
  let text_fragments = create_text_fragments tokens last in
  LCGlatexOf.print_chart "results/" (name^"1_chart") "a1" text_fragments chart;
  let chart,references = LCGchart.lazify chart in
  LCGlatexOf.print_chart "results/" (name^"2_chart") "a4" text_fragments chart;
  LCGlatexOf.print_references "results/" (name^"2_references") "a4" references;
  let chart = LCGchart.parse LCGrules.application_rules chart references 30. Sys.time in (* uwaga: niejawna zmiana imperatywna w references *)
  LCGlatexOf.print_chart "results/" (name^"3_chart") "a4" text_fragments chart;
  LCGlatexOf.print_references "results/" (name^"3_references") "a4" references;
  if LCGchart.is_parsed chart then (
    let term = LCGchart.get_parsed_term chart in
    Xlatex.latex_file_out "results/" (name^"4_term") "a4" false (fun file ->
        Printf.fprintf file "\\[%s\\]\n" (LCGlatexOf.linear_term 0 term));
    Xlatex.latex_compile_and_clean "results/" (name^"4_term");
    let dependency_tree = LCGreductions.reduce term references in
    LCGlatexOf.print_dependency_tree "results/" (name^"4_dependency_tree") "a0" dependency_tree;
    if LCGreductions.is_reduced_dependency_tree dependency_tree then (
      LCGreductions.assign_labels dependency_tree; (* uwaga: niejawna zmiana imperatywna w dependency_tree *)
      LCGlatexOf.print_dependency_tree "results/" (name^"5_dependency_tree") "a4" dependency_tree;
      LCGreductions.remove_cuts dependency_tree; (* uwaga: niejawna zmiana imperatywna w dependency_tree *)
      LCGlatexOf.print_dependency_tree "results/" (name^"6_dependency_tree") "a4" dependency_tree;
      LCGgraphOf.print_dependency_tree "results/" (name^"6_dependency_tree") dependency_tree;
      LCGgraphOf.print_simplified_dependency_tree "results/" (name^"6_simple_dependency_tree") dependency_tree;
      ())
    else print_endline "not reduced")
  else print_endline "not parsed"

let _ =
  CategoriesPL.initialize ();
  Xlist.iter examples (test_example valence)

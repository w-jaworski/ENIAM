(*
 *  LCGparser, a parser for Logical Categorial Grammar formalism
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

open LCGtypes
open Xstd

type entry =
    Basic of grammar_symbol
  | Raised of grammar_symbol

let examples = [
(*  "kot",[
    0, 1, "Ala","Ala","subst",   Basic(Tensor[Atom "np"; Atom "nom"]);
    1, 2, "ma","mieć","fin",     Basic(ImpSet(Tensor[Atom "ip"],[Both,Tensor[Atom "np"; Atom "nom"];Both,Tensor[Atom "np"; Atom "acc"]]));
    (* 1, 2, "ma","mieć","fin",     Basic(Imp(Imp(Tensor[Atom "ip"],Backward,Tensor[Atom "np"; Atom "nom"]),Forward,Tensor[Atom "np"; Atom "nom"])); *)
    2, 3, "kota","kot","subst",  Basic(Tensor[Atom "np"; Atom "acc"]);
    3, 4, ".",".","interp",      Basic(Imp(Tensor[Atom "<root>"],Backward,Tensor[Atom "ip"]));
  ],4;*)

  (* "rudy",[
    0, 1, "Ala","Ala","subst",   Basic(Tensor[Atom "np"; Atom "nom"]);
    1, 2, "ma","mieć","fin",     Basic(ImpSet(Tensor[Atom "ip"],[Both,Tensor[Atom "np"; Atom "nom"];Both,Tensor[Atom "np"; Atom "acc"]]));
    2, 3, "rudego","rudy","adj", Basic(WithVar("case",With[Atom "gen"; Atom "acc"],"A",Tensor[Atom "adjp"; AVar "case"]));
    3, 4, "kota","kot","subst",  Basic(WithVar("case",With[Atom "gen"; Atom "acc"],"B",ImpSet(Tensor[Atom "np"; AVar "case"],[Backward,Maybe(Tensor[Atom "adjp"; AVar "case"])])));
    (* 3, 4, "kota","kot","subst",  Basic(WithVar("case",With[Atom "gen"; Atom "acc"],"B",ImpSet(Tensor[Atom "np"; AVar "case"],[Backward,Tensor[Atom "adjp"; AVar "case"]]))); *)
    4, 5, ".",".","interp",      Basic(Imp(Tensor[Atom "<root>"],Backward,Tensor[Atom "ip"]));
    ],5; *)

(*  "jaki",[
    0, 1, "Jakiego","jaki","adj",Raised(WithVar("case",With[Atom "gen"; Atom "acc"],"A",ImpSet(ImpSet(Tensor[Atom "cp"; Atom "int"; Atom "jaki"],
                                                                                             [Forward,Imp(Tensor[Atom "ip"],Forward,Tensor[Atom "np"; AVar "case"])]),
                                                                                             [Forward,Imp(Tensor[Atom "np"; AVar "case"],Backward,Tensor[Atom "adjp"; AVar "case"])])));
    1, 2, "kota","kot","subst",  Basic(WithVar("case",With[Atom "gen"; Atom "acc"],"B",ImpSet(Tensor[Atom "np"; AVar "case"],[Backward,Maybe(Tensor[Atom "adjp"; AVar "case"])])));
    (* 1, 2, "kota","kot","subst",  Basic(WithVar("case",With[Atom "gen"; Atom "acc"],"B",Imp(Tensor[Atom "np"; AVar "case"],Backward,Maybe(Tensor[Atom "adjp"; AVar "case"])))); *)
    (* 1, 2, "kota","kot","subst",  Basic(WithVar("case",With[Atom "gen"; Atom "acc"],"B",ImpSet(Tensor[Atom "np"; AVar "case"],[Backward,Tensor[Atom "adjp"; AVar "case"]]))); *)
    2, 3, "Ala","Ala","subst",   Basic(Tensor[Atom "np"; Atom "nom"]);
    3, 4, "ma","mieć","fin",     Basic(ImpSet(Tensor[Atom "ip"],[Both,Tensor[Atom "np"; Atom "nom"];Both,Tensor[Atom "np"; Atom "acc"]]));
    4, 5, "?","?","interp",      Basic(Imp(Tensor[Atom "<root>"],Backward,Tensor[Atom "cp";Atom "int";Top]));
  ],5;

  "ocean",[
    0, 1, "Wpłynąłem","wpłynąć","praet",   Basic(Imp(Tensor[Atom "ip"],Forward,Tensor[Atom "prepnp"; Atom "acc"]));
    1, 2, "na","na","prep",     Basic(Imp(Tensor[Atom "prepnp";Atom "acc"],Forward,Tensor[Atom "np"; Atom "acc"]));
    2, 3, "suchego","suchy","adj", Basic(Tensor[Atom "adjp"; Atom "gen"]);
    3, 4, "przestwór","przestwór","subst",  Basic(Imp(Tensor[Atom "np"; Atom "acc"],Forward,Tensor[Atom "np"; Atom "gen"]));
    4, 5, "oceanu","ocean","subst",  Basic(Imp(Tensor[Atom "np"; Atom "gen"],Backward,Plus[One;Tensor[Atom "adjp"; Atom "gen"];Tensor[Atom "x"]]));
    5, 6, ".",".","interp",      Basic(Imp(Tensor[Atom "<root>"],Backward,Tensor[Atom "ip"]));
    ],6;

  "krowa",[
    0, 1, "krowa","krowa","subst",  Basic(Tensor[Atom "np"; Atom "nom"]);
    ],1;

  "krowa_koń",[
    0, 1, "krowa","krowa","subst",  Basic(Tensor[Atom "np"; Atom "nom"]);
    1, 2, "koń","koń","subst",  Basic(Tensor[Atom "np"; Atom "nom"]);
    ],2;*)

  (* "krokodyl",[
    0, 1, "krokodyl","krokodyl","subst",  Basic(Imp(Tensor[Atom "np"; Atom "nom"],Forward,Tensor[Atom "np"; Atom "gen"]));
    ],1; *)

  (* "coord",[
    0, 1, "Ala","Ala","subst",   Basic(Tensor[Atom "np"; Atom "nom"]);
    1, 2, "ma","mieć","fin",     Basic(ImpSet(Tensor[Atom "ip"],[Both,Tensor[Atom "np"; Atom "nom"];Both,Tensor[Atom "np"; Atom "acc"]]));
    2, 3, "rudego","rudy","adj", Basic(WithVar("case",With[Atom "gen"; Atom "acc"],"A",Tensor[Atom "adjp"; AVar "case"]));
    3, 4, "słonia","słoń","subst",  Basic(WithVar("case",With[Atom "gen"; Atom "acc"],"B",ImpSet(Tensor[Atom "np"; AVar "case"],[Backward,Maybe(Tensor[Atom "adjp"; AVar "case"])])));
    (* 3, 4, "kota","kot","subst",  Basic(WithVar("case",With[Atom "gen"; Atom "acc"],"B",ImpSet(Tensor[Atom "np"; AVar "case"],[Backward,Tensor[Atom "adjp"; AVar "case"]]))); *)
    4, 5, ",",",","conj",  Basic(Preconj);
    5, 6, "kota","kot","subst",  Basic(WithVar("case",With[Atom "gen"; Atom "acc"],"C",ImpSet(Tensor[Atom "np"; AVar "case"],[Backward,Maybe(Tensor[Atom "adjp"; AVar "case"])])));
    6, 7, "i","i","conj",  Basic(Conj(WithVar("case",With[Atom "gen"; Atom "acc"; Atom "loc"],"D",Imp(Tensor[Atom "np"; AVar "case"],Both,Tensor[Atom "np"; AVar "case"]))));
    7, 8, "psa","pies","subst",  Basic(WithVar("case",With[Atom "gen"; Atom "acc"],"D",ImpSet(Tensor[Atom "np"; AVar "case"],[Backward,Maybe(Tensor[Atom "adjp"; AVar "case"])])));
    (* 3, 4, "psa","pies","subst",  Basic(WithVar("case",With[Atom "gen"; Atom "acc"],"C",ImpSet(Tensor[Atom "np"; AVar "case"],[Backward,Tensor[Atom "adjp"; AVar "case"]]))); *)
    8, 9, ".",".","interp",      Basic(Imp(Tensor[Atom "<root>"],Backward,Tensor[Atom "ip"]));
    ],9; *)

  "unlike_coord",[
    0, 1, "Al","Al","subst",   Basic(Tensor[Atom "np"; Atom "nom"]);
    1, 2, "jest","być","fin",     Basic(ImpSet(Tensor[Atom "ip"],[Both,Tensor[Atom "np"; Atom "nom"];Both,Tensor[Atom "np"; Atom "inst"]]));
    2, 3, "młody","młody","adj", Basic(WithVar("case",With[Atom "nom"; Atom "voc"],"A",Tensor[Atom "adjp"; AVar "case"]));
    3, 4, "i","i","conj",  Basic(Conj(Imp(Tensor[Atom "np"; Atom "inst"],Both,Plus[Tensor[Atom "np"; Atom "inst"];Tensor[Atom "adjp"; Atom "nom"]])));
    (* 3, 4, "kota","kot","subst",  Basic(WithVar("case",With[Atom "gen"; Atom "acc"],"B",ImpSet(Tensor[Atom "np"; AVar "case"],[Backward,Tensor[Atom "adjp"; AVar "case"]]))); *)
    4, 5, "republikaniniem","republikanin","subst",  Basic(WithVar("case",With[Atom "inst"],"C",ImpSet(Tensor[Atom "np"; AVar "case"],[Backward,Maybe(Tensor[Atom "adjp"; AVar "case"])])));
    5, 6, ".",".","interp",      Basic(Imp(Tensor[Atom "<root>"],Backward,Tensor[Atom "ip"]));
    ],6;

]

let create_chart tokens last =
  LCGrenderer.reset_variable_numbers ();
  let chart = LCGchart.make last in
  let chart = Xlist.fold tokens chart (fun chart (lnode,rnode,orth,lemma,pos,entry) ->
      LCGrenderer.reset_variable_names ();
      LCGrenderer.add_variable_numbers ();
      let syntax,semantics = match entry with
        Basic syntax ->
          let node = {LCGrenderer.empty_node with
                      orth=orth; lemma=lemma; pos=pos;
                      symbol=LCGrenderer.make_symbol syntax} in
          let semantics = LCGrenderer.make_term node syntax in
          LCGrenderer.simplify (syntax,semantics)
      | Raised syntax ->
          let node = {LCGrenderer.empty_node with
                      orth=orth; lemma=lemma; pos=pos;
                      symbol=LCGrenderer.make_raised_symbol syntax} in
          let outer_node = {LCGrenderer.empty_node with
                            orth=""; lemma=lemma; pos="";
                            symbol=LCGrenderer.make_symbol syntax} in
          let semantics = LCGrenderer.make_raised_term node outer_node syntax in
          LCGrenderer.simplify (syntax,semantics) in
      let lf = if lnode = 0 then true else false in
      let rf = if rnode = last then true else false in
      LCGchart.add chart lnode rnode (Bracket(lf,rf,syntax),semantics) 0) in
  chart

let create_text_fragments tokens last =
  let text_fragments = Array.make last IntMap.empty in
  Xlist.iter tokens (fun (lnode,rnode,orth,lemma,pos,entry) ->
    text_fragments.(lnode) <- IntMap.add text_fragments.(lnode) rnode orth);
  Int.iter_down 0 (last - 1) (fun i ->
    let map = IntMap.fold text_fragments.(i) text_fragments.(i) (fun map j orth ->
      if j = last then map else
      IntMap.fold text_fragments.(j) map (fun map k orth2 ->
        IntMap.add map k (orth ^ " " ^ orth2))) in
    text_fragments.(i) <- map);
  text_fragments

let test_example rules (name,tokens,last) =
  LCGreductions.reset_variant_label ();
  let chart = create_chart tokens last in
  let text_fragments = create_text_fragments tokens last in
  LCGlatexOf.print_chart "results/" (name^"1_chart") "a3" text_fragments chart;
  let chart,references = LCGchart.lazify chart in
  LCGlatexOf.print_chart "results/" (name^"2_chart") "a4" text_fragments chart;
  LCGlatexOf.print_references "results/" (name^"2_references") "a4" references;
  let chart = LCGchart.parse rules chart references 30. Sys.time in (* uwaga: niejawna zmiana imperatywna w references *)
  LCGlatexOf.print_chart "results/" (name^"3_chart") "a4" text_fragments chart;
  LCGlatexOf.print_references "results/" (name^"3_references") "a4" references;
  let chart = if not (LCGchart.is_parsed chart) then LCGchart.merge chart references else chart in
  LCGlatexOf.print_chart "results/" (name^"3b_chart") "a4" text_fragments chart;
  LCGlatexOf.print_references "results/" (name^"3b_references") "a4" references;
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

let rules = LCGrules.application_rules @ LCGrules.cross_composition_rules

let _ =
  Xlist.iter examples (test_example rules)

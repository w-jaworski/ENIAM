(*
 *  ENIAMexec implements ENIAM processing stream
 *  Copyright (C) 2017-2018 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
 *  Copyright (C) 2017-2018 Institute of Informatics, University of Warsaw
 *  Copyright (C) 2017-2018 SELIDOR - T. Puza, Ł. Wasilewski Sp.J.
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

open ExecTypes
open Xstd
open SemTypes


let eniam_semantic_processing verbosity tokens lex_sems (result : eniam_parse_result) =
(*  let tree,result =
    try
      let tree = SemValence.assign_frames tokens lex_sems result.dependency_tree6b in (* FIXME: tu potrzebne jest wstawianie pro *)
      let result = if verbosity < 2 then result else {result with dependency_tree7=tree} in
      tree,result
    with e -> [| |],{result with status=SemValenceError; msg=string_of_exn e} in
  if result.status = SemValenceError then result else
  let tree,result =
    try
      let tree = SemValence.reduce_tree tokens lex_sems tree in (* to nie jest potrzebne - to jest manipulacja agf i cięcie drzewa na części *)
      let result = if verbosity < 2 then result else {result with dependency_tree8=tree} in
      tree,result
    with e -> ExtArray.make 0 Dot,{result with status=SemValenceError; msg=string_of_exn e} in
  if result.status = SemValenceError then result else*)
  let tree = ExtArray.of_array result.dependency_tree6b LCGtypes.Dot in
  let result = {result with dependency_tree8 = tree} in
(*   print_endline "eniam_semantic_processing 1"; *)
  let result =
    try
      (* SemValence.transfer_attributes tree; (* niejawna zmiana imperatywna w tree *) *) (* FIXME: to jest potrzebne? *)
      result
    with e -> {result with status=SemValenceError; msg=Exec.string_of_exn e} in
  if result.status = SemValenceError then result else
  let tree,result =
    try
  (* print_endline "eniam_semantic_processing 2"; *)
      (* Disambiguation.selprefs tree; (* niejawna zmiana imperatywna w tree *) *)
      let tree = Disambiguation.merge tree in
      (* let tree = Disambiguation.random_tree tokens lex_sems tree in *) (* FIXME: tokens lex_sems nie są potrzebne *)
      let tree = LCGreductions.reshape_dependency_tree(*ExtArray.to_array*) tree in
      LexSemantics.create_tokens_for_artificial_nodes tokens lex_sems tree;
      Coreference.resolve tree;
      let result = if verbosity = 0 then result else {result with dependency_tree9=tree} in
      tree,result
    with e -> [| |],{result with status=SemValenceError; msg=Exec.string_of_exn e} in
  if result.status = SemValenceError then result else
  let graph,result =
    try
  (* print_endline "eniam_semantic_processing 3"; *)
      let graph = DomSemantics.translate tokens lex_sems tree in (* FIXME: pro nie mają id *)
      let result = if verbosity = 0 then result else {result with semantic_graph10=graph} in
      let graph = SemGraph.make_tree graph in
(*       let graph = expand_compound_concepts graph in *)
      let result = if verbosity = 0 then result else {result with semantic_graph11=graph} in
      graph,result
    with e -> SemTypes.Dot,{result with status=SemGraphError; msg=Exec.string_of_exn e} in
  if result.status = SemGraphError then result else
  let r = ref [] in
  (try SemGraph.validate_translation r graph with e -> r := Exec.string_of_exn e :: !r);
  if !r <> [] then {result with status = SemGraphError; msg=String.concat "<BR>" !r} else
  let graph,result =
    try
  (* print_endline "eniam_semantic_processing 4"; *)
      let graph = SemGraph.reduce_tree graph in
      let result = (*if verbosity = 0 then result else*) {result with semantic_graph11=graph} in
      graph,result
    with e -> SemTypes.Dot,{result with status=SemGraphError; msg=Exec.string_of_exn e} in
  if result.status = SemGraphError then result else
  let r = ref [] in
  (try SemGraph.validate_reduction r graph with e -> r := Exec.string_of_exn e :: !r);
  if !r <> [] then {result with status = SemGraphError; msg=String.concat "<BR>" !r} else
  let graph,result =
    try
  (* print_endline "eniam_semantic_processing 5"; *)
      let graph = SemGraph.greater_simplify graph in
(*    let graph = SemGraph.manage_quantification graph in  *)
      let graph = SemGraph.simplify_gender graph in
      let graph = SemGraph.manage_variant_labels graph in
      let result = (*if verbosity = 0 then result else*) {result with semantic_graph11=graph; semantic_graph12=graph} in
      graph,result
    with e -> SemTypes.Dot,{result with status=SemGraphError; msg=Exec.string_of_exn e} in
  if result.status = SemGraphError then result else
  {result with status = if result.status = PartialParsed then PartialSemParsed else SemParsed}

let semantic_processing verbosity tokens lex_sems text =
  map_text Struct (fun mode -> function
      ENIAMSentence result ->
        if result.status <> Parsed && result.status <> PartialParsed then ENIAMSentence result else
        ENIAMSentence (eniam_semantic_processing verbosity tokens lex_sems result)
    | t -> t) text

let eniam_semantic_processing2 verbosity tokens lex_sems result =
  let graph = result.semantic_graph11 in
  let graph,result =
    try
      let graph = SemGraph.simplify_tree graph in
(*      let graph = merge_apoz graph in
      let graph = SemGraph.simplify_tree graph in
      let graph = shift_parent_relations graph in
      let graph = SemGraph.simplify_tree graph in
      let graph = merge_apoz graph in
      let graph = SemGraph.simplify_tree graph in*)
      let result = if verbosity = 0 then result else {result with semantic_graph12=graph} in
      graph,result
    with e -> SemTypes.Dot,{result with status=SemGraphError2; msg=Exec.string_of_exn e} in
  if result.status = SemGraphError2 then result else
  let r = ref [] in
(*   print_endline "eniam_semantic_processing2 1"; *)
(*   (try validate_ontology r ontology graph with e -> r := Exec.string_of_exn e :: !r); *)
(*   print_endline "eniam_semantic_processing2 2"; *)
  if !r <> [] then
    {result with status = SemNotValidated; msg=String.concat "<BR>" !r}
  else {result with status = if result.status = PartialSemParsed then PartialSemParsed else SemParsed}

let semantic_processing2 verbosity tokens lex_sems text =
  map_text Struct (fun mode -> function
      ENIAMSentence result ->
        if result.status <> SemParsed && result.status <> PartialSemParsed then ENIAMSentence result else
        ENIAMSentence (eniam_semantic_processing2 verbosity tokens lex_sems result)
    | t -> t) text

exception NotParsed
    
let merge_graph_eniam_sentence (result : eniam_parse_result) =
  match result.status with
    SemParsed | PartialSemParsed -> result.semantic_graph12
  | _ -> raise NotParsed

let rec merge_graph_sentence = function
    RawSentence s -> Concept{empty_concept with cat="RawSentence"; sense=s}
  | StructSentence(paths,last) -> raise NotParsed
  | DepSentence paths -> raise NotParsed
  | ENIAMSentence result -> merge_graph_eniam_sentence result
  | QuotedSentences sentences -> Concept{empty_concept with cat="QuotedSentences"; contents=Tuple(Xlist.rev_map sentences (fun p -> merge_graph_sentence p.sentence))}
  | AltSentence l -> Concept{empty_concept with cat="AltSentence"; relations=Tuple(Xlist.rev_map l (fun (m,t) -> Relation(Visualization.string_of_mode m,"",merge_graph_sentence t)))}
  | ErrorSentence s -> raise NotParsed

let rec merge_graph_paragraph = function
    RawParagraph s -> Concept{empty_concept with cat="RawParagraph"; sense=s}
  | StructParagraph sentences -> Concept{empty_concept with cat="StructParagraph"; contents=Tuple(Xlist.rev_map sentences (fun p -> merge_graph_sentence p.sentence))}
  | AltParagraph l -> Concept{empty_concept with cat="AltParagraph"; relations=Tuple(Xlist.rev_map l (fun (m,t) -> Relation(Visualization.string_of_mode m,"",merge_graph_paragraph t)))}
  | ErrorParagraph s -> raise NotParsed

let rec merge_graph_text = function
    RawText s -> Concept{empty_concept with cat="RawText"; sense=s}
  | StructText paragraphs -> Concept{empty_concept with cat="StructText"; contents=Tuple(Xlist.rev_map paragraphs merge_graph_paragraph)}
  | JSONtext _ -> raise NotParsed
  | AltText l -> Concept{empty_concept with cat="AltText"; relations=Tuple(Xlist.rev_map l (fun (m,t) -> Relation(Visualization.string_of_mode m,"",merge_graph_text t)))}
  | ErrorText s -> raise NotParsed

let merge_graph t = 
  try
    let graph = merge_graph_text t in
    StructText[StructParagraph[{id=""; beg=0; len=0; next=0; file_prefix=""; sentence=ENIAMSentence{empty_eniam_parse_result with status=SemParsed; semantic_graph12=graph}}]]
  with NotParsed -> t

    

    

(*
 *  ENIAMvalence is a library that assigns tokens with lexicosemantic information.
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

open SubsyntaxTypes
open LexSemanticsTypes
open WalTypes
open LexSemantics
open Xstd

(* WalReduce *)

let user_select_entries lexemes =
    let tests = WalReduce.create_tests StringMap.empty StringMap.empty IntMap.empty lexemes in
    let valence = WalReduce.reduce_entries lexemes (fst !ValParser.valence) in
    Entries.map valence (fun _ _ (selectors,sense,cat,(*snode,*)schema) ->
      selectors,sense,cat,(*snode,*)WalReduce.reduce_schema2 tests schema)

(* Adjuncts *)
      
open WalTypes
open Xstd

open LCGlexiconTypes

let nie_vebs = StringSet.of_list ["fin";"bedzie";"praet";"winien";"impt";
                                  "imps";"pred";"inf";"pcon";"pant"]

let imp_aux = StringSet.of_list ["niech";"niechaj";"niechże";"niechajże"]

let rec check_selector_lex_constraints lexemes pos = function
    [] -> true
  | {sel=Negation;rel=Eq;values=["neg"]} :: selectors ->
    if not (StringSet.mem lexemes "nie") && (StringSet.mem nie_vebs pos) then false
    else check_selector_lex_constraints lexemes pos selectors
  | {sel=Mood;rel=Eq;values=["conditional"]} :: selectors ->
    if not (StringSet.mem lexemes "by") && (pos = "praet" || pos = "winien") then false
    else check_selector_lex_constraints lexemes pos selectors
  | {sel=Mood;rel=Eq;values=["imperative"]} :: selectors ->
    if StringSet.is_empty (StringSet.intersection lexemes imp_aux) && pos = "fin" then false
    else check_selector_lex_constraints lexemes pos selectors
  | _  :: selectors -> check_selector_lex_constraints lexemes pos selectors


(* ENIAMlexSemantics *)

let load_cats_map map filename =
  File.fold_tab filename map (fun map -> function
    lemma :: cat :: _ -> StringMap.add_inc map lemma [cat] (fun l -> cat :: l)
  | l -> failwith ("load_cats_map: " ^ String.concat "\t" l))

let coerced_map = ref StringMap.empty

let find_coercions cat =
  cat :: (try StringMap.find !coerced_map cat with Not_found -> [])

let proper_cats_map = ref StringMap.empty

let find_proper_cats cats =
  StringSet.to_list (Xlist.fold cats StringSet.empty (fun cats cat ->
    let found =
      try StringMap.find !proper_cats_map cat
      with Not_found -> (*failwith*)(*print_endline ("find_proper_cats: " ^ cat);*) [cat] in
    Xlist.fold found cats (fun cats cat ->
      if cat = "" then cats else StringSet.add cats cat)))

let split_schema schema =
  Xlist.fold schema ([],[],[]) (fun (local_schema,schema,distant_schema) p ->
    match p.WalTypes.range with
      WalTypes.Local -> p :: local_schema, schema, distant_schema
    | WalTypes.Middle -> local_schema, p :: schema, distant_schema
    | WalTypes.Distant ->  local_schema, schema, p :: distant_schema)

let assign_valence2 tokens lex_sems group =
  let lexemes = Xlist.fold group StringSet.empty (fun lexemes id ->
      let lemma = Tokenizer.get_lemma (ExtArray.get tokens id).token in
      StringSet.add lexemes lemma) in
  let connected = user_select_entries lexemes in
  Xlist.iter group (fun id ->
    let token_cat = Tokenizer.get_cat (ExtArray.get tokens id).token in
    let lemma, pos = match (ExtArray.get tokens id).token with
        Lemma(lemma,pos,_,_) -> lemma, pos
      | t -> failwith ("assign_valence2: unknown token " ^ SubsyntaxStringOf.string_of_token t) in
    let pos2 = Tagset.simplify_pos pos in
      (* Printf.printf "assign_valence2: Lemma lemma=%s pos=%s pos2=%s%!\n" lemma pos pos2; *)
      let connected2 = Entries.find connected pos2 lemma in
      let connected = if connected2 = [] then Entries.find connected pos2 "" else connected2 in
      (* if connected = [] then print_endline ("no valence information for: " ^ lemma ^ " " ^ pos ^ " " ^ pos2); *)
      let cat_selector_flag = true in
      (* Printf.printf "D %s |connected|=%d\n" lemma (Xlist.size connected); *)
      let connected = List.flatten (Xlist.map connected (fun (sel,sense,cat,(*snode,*)schema1) ->
        if cat <> token_cat then [] else
          (* print_endline ("E " ^ lemma ^ " " ^ sense); *)
          (*List.flatten*) (Xlist.rev_map (Valence.transform_entry pos lemma NegationUndef PredFalse AspectUndef schema1) (fun (selectors,schema) ->
            (* print_endline ("A " ^ WalStringOf.schema schema); *)
              (* Xlist.rev_map (ENIAMvalence2.get_aroles schema1 lemma pos) (fun (sel,arole,arole_attr,arev) -> *)(
                  {empty_frame with
                    selectors=sel @ (if cat_selector_flag then [
                      {sel=Cat;rel=Eq;values=[cat]}(*;
                      LCGlexiconTypes.SNode,LCGlexiconTypes.Eq,snode*)] else []) @ selectors;
                    senses=[sense, ["X",1], 0.]; cats=[cat,find_coercions cat]; positions=schema; (*snode=snode*)}))))) in
      let connected = Xlist.fold connected [] (fun connected frame -> (* UWAGA: zakładam, że selektory obejmują wszystkie przypadki, czyli że connected nie może stać się listą pustą *)
          if check_selector_lex_constraints lexemes pos frame.selectors then frame :: connected else connected) in
      (* Printf.printf "G %s |connected|=%d\n" lemma (Xlist.size connected); *)
      let schemata = Xlist.rev_map connected (fun frame ->
          let local_schema,schema,distant_schema = split_schema frame.positions in
          frame.selectors,frame.cats,(*frame.snode,*)
          WalRenderer.render_schema_cat lemma pos local_schema,
          WalRenderer.render_schema_cat lemma pos schema,
          WalRenderer.render_schema_cat lemma pos distant_schema) in
      (* let schemata = if schemata = [] then [[],["X",["X"]],[]] else schemata in *)
      let connected = Xlist.rev_map connected (fun frame ->
            (* print_endline ("B " ^ WalStringOf.schema frame.positions); *)
          {frame with
            positions = (*find_selprefs*) (WalRenderer.render_connected_schema_cat lemma pos (WalReduce.set_necessary pos frame.positions))}) in
      (* Printf.printf "H %s |connected|=%d\n" lemma (Xlist.size connected); *)
      let connected = Xlist.rev_map connected mark_nosem in
      ExtArray.set lex_sems id {(ExtArray.get lex_sems id) with
                                schemata=schemata; (*lex_entries=entries;*) frames=connected})

let prepare_pro_valence connected =
       let connected = List.flatten (Xlist.map connected (fun (poss,pro_lemma,sel,sense,cat,(*snode,*)schema1) ->
(*          Printf.printf "prepare_pro_valence: pro_lemma=%s sense=%s cat=%s\n" pro_lemma sense cat; *)
          (*List.flatten*) (Xlist.rev_map (Valence.transform_entry "pro" pro_lemma NegationUndef PredFalse AspectUndef schema1) (fun (selectors,schema) ->
            (* print_endline ("A " ^ WalStringOf.schema schema); *)
              (* Xlist.rev_map (ENIAMvalence2.get_aroles schema1 lemma pos) (fun (sel,arole,arole_attr,arev) -> *)(
                  {empty_frame with
                    selectors=sel @ [
                      {sel=Lemma;rel=Eq;values=[pro_lemma]};
                      (*{sel=Pos2;rel=Eq;values=[poss]};*)
                      {sel=Cat;rel=Eq;values=[cat]}] @ selectors;
                    senses=[sense, ["X",1], 0.]; cats=[cat,find_coercions cat]; positions=schema; (*snode=snode*)}))))) in
      (* Printf.printf "G %s |connected|=%d\n" lemma (Xlist.size connected); *)
      let schemata = Xlist.rev_map connected (fun frame ->
          let local_schema,schema,distant_schema = split_schema frame.positions in
          frame.selectors,frame.cats,(*frame.snode,*)
          WalRenderer.render_schema_cat "pro" "pro" local_schema,
          WalRenderer.render_schema_cat "pro" "pro" schema,
          WalRenderer.render_schema_cat "pro" "pro" distant_schema) in
      schemata
                               
let assign2 tokens text =
(*   print_endline "assign2 1"; *)
  let lex_sems = ExtArray.make (ExtArray.size tokens) empty_lex_sem in
(*   print_endline "assign2 2"; *)
  let _ = ExtArray.add lex_sems empty_lex_sem in
(*   print_endline "assign2 3"; *)
  Int.iter 1 (ExtArray.size tokens - 1) (fun i ->
    ignore (ExtArray.add lex_sems empty_lex_sem));
(*   print_endline "assign2 4"; *)
  let groups = split_tokens_into_groups (ExtArray.size tokens) text in
(*   print_endline "assign2 5"; *)
  remove_unused_tokens tokens groups;
(*   print_endline "assign2 6"; *)
  Xlist.iter groups (fun group -> assign_valence2 tokens lex_sems group); (* FIXME: trzeba by tu dodać usuwanie powtarzających się wpisów *)
(*   print_endline "assign2 7"; *)
  lex_sems

let catch_assign2 tokens text =
  try
    assign2 tokens text,""
  with e ->
(*     print_endline "catch_assign2"; *)
    ExtArray.make 0 empty_lex_sem,
    Printexc.to_string e

let initialize2 () =
  (* Subsyntax.initialize (); *)
  ValParser.initialize ();
  CategoriesPL.initialize ();
  coerced_map := load_cats_map StringMap.empty coercions_filename;
  coerced_map := Xlist.fold !SubsyntaxTypes.theories !coerced_map (fun map theory ->
    load_cats_map map (SubsyntaxTypes.theories_path ^ theory ^ "/coercions.tab"));
  coerced_map := Xlist.fold !SubsyntaxTypes.user_theories !coerced_map (fun map theory ->
    load_cats_map map (SubsyntaxTypes.user_theories_path ^ theory ^ "/coercions.tab"));
(*   proper_cats_map := load_cats_map StringMap.empty proper_cats_filename; *)
  ()

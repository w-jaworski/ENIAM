(*
 *  ENIAMsubsyntax: tokenization, lemmatization, MWE and sentence detecion for Polish
 *  Copyright (C) 2018-2019 Tomasz Garbus <tomasz.garbus1 atSPAMfree gmail dot com>
 *  Copyright (C) 2018-2019 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
 *  Copyright (C) 2018-2019 LekSeek Sp. z o.o. sp. k.
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
open SubsyntaxStringOf
open Patterns
open MWE
open Xstd
open Xset

let coordination_filename = "data/coordination.tab"

let crules = ref (StringMap.empty : (pat list * prod) list StringMap.t)
let crules2 = ref (StringMap.empty : (pat list * prod) list StringMap.t)

let initialize () =
(* Wczytuję reguły z pliku. *)
  let dict,dict2 = File.catch_no_file (load_mwe_dict2 coordination_filename) (StringMap.empty,StringMap.empty) in
  crules := dict;
  crules2 := dict2;
  ()

(*
 Znajduje wszystkie takie pary tokenów o lemacie |lemma_name|, że jeden zawiera
 się w drugim tzn.
  (t1.beg > t2.beg and t1.end <= t2.end) or (t1.beg >= t2.beg and t1.end < t2.end)
 i usuwa ten krótszy. Wykonuje rekurencyjnie dopóki nie zostanie 0 takich par.
*)
let rec remove_contained_tokens (tokens : token_env list) (lemma_name : string) : token_env list =
  let is_contained_in_other (token : token_env) (all_tokens : token_env list) =
    (*
    true wtw |token| ma rzeczywiście lemat |lemma_name| i istnieje jakiś
    inny token w liście |all_tokens|, który również ma lemat |lemma_name| i zawiera
    |token|. Innymi słowy, |token| jest redundantny i/lub zawiera niepełną informację
    o substancji lub liście substancji.
    *)
    match token.token with
      Lemma(x,_,_,_) when x = lemma_name -> (
        let contains (container : token_env) (contained : token_env) =
        (*
        true wtw oba argumenty mają lemat |lemma_name| i |container| "zawiera"
        |contained|.
        *)
          match container.token, contained.token with
          Lemma(x,_,_,_), Lemma(y,_,_,_) when (x = lemma_name && x = y) ->
            (container.beg < contained.beg && container.next >= contained.next)
            ||
            (container.beg <= contained.beg && container.next > contained.next)
          | _ -> false
        in
        let containing = Xlist.filter all_tokens (fun t -> contains t token) in
        match containing with
         [] -> false
        | _ -> true
       )
    | _ -> false
  in
  let after_removal = Xlist.filter tokens (fun token -> not (is_contained_in_other token tokens)) in
  if Xlist.size after_removal == Xlist.size tokens
    then after_removal
    else remove_contained_tokens after_removal lemma_name

type coord_marker = Coord1 | Coord1Comp | Coord2 | Coord2Comp

let left_marker_to_string marker = match marker with
  Coord1 -> "<coord1>"
  | Coord1Comp -> "<coord1comp>"
  | Coord2 -> "<coord2>"
  | Coord2Comp -> "<coord2comp>"

let right_marker_to_string marker = match marker with
  Coord1 -> "</coord1>"
  | Coord1Comp -> "</coord1comp>"
  | Coord2 -> "</coord2>"
  | Coord2Comp -> "</coord2comp>"

let rec left_markers_to_strings markers = match markers with
  [] -> []
  | [x] -> [left_marker_to_string x]
  | x :: l -> (left_marker_to_string x) :: (left_markers_to_strings l)

let right_markers_to_strings markers =
  let rec right_markers_to_strings_rec rev_markers = match rev_markers with
    [] -> []
    | [x] -> [right_marker_to_string x]
    | x :: l -> (right_marker_to_string x) :: (right_markers_to_strings_rec l)
  in
  right_markers_to_strings_rec (List.rev markers)

let visited = ref ([] : (token_env * coord_marker list * coord_marker list) list)

let rec mark_coordinations_rec left_markers right_markers markers token =
  if [] != (Xlist.filter !visited (fun e -> e = (token, left_markers, right_markers))) then markers else
  (
    visited := (token, left_markers, right_markers) :: !visited;
    match token.token with
    Lemma("DONE", _, _, _) ->
      Xlist.fold token.args markers (mark_coordinations_rec [] [])
    | Lemma("SubstanceList", _, _, _) -> (match token.args with
      [] -> failwith "SubstanceList without arguments"
      | [x] -> mark_coordinations_rec left_markers right_markers markers x
      | x :: k ->
        let markers = mark_coordinations_rec (Coord1 :: Coord1Comp :: left_markers) [Coord1Comp] markers x in
        let y = List.hd (List.rev k) in
        let markers = mark_coordinations_rec [Coord1Comp] (Coord1 :: Coord1Comp :: right_markers) markers y in
        let k = List.rev (List.tl (List.rev k)) in
        let markers = Xlist.fold k markers (mark_coordinations_rec [Coord1Comp] [Coord1Comp]) in
        markers
      )
    | Lemma("Substance", _, _, _) -> (match token.args with
      [] -> failwith "Substance without arguments"
      | [x] -> mark_coordinations_rec left_markers right_markers markers x
      | x :: k ->
        let markers = mark_coordinations_rec left_markers [] markers x in
        let y = List.hd (List.rev k) in
        let markers = mark_coordinations_rec [] right_markers markers y in
        let k = List.rev (List.tl (List.rev k)) in
        let markers = Xlist.fold k markers (mark_coordinations_rec [] []) in
        markers
      )
    | Lemma("NumberMeasureList", _, _, _) -> (match token.args with
      [] -> failwith "NumberMeasureList without arguments"
      | [x] -> mark_coordinations_rec left_markers right_markers markers x
      | x :: k ->
        let markers = mark_coordinations_rec (Coord2 :: Coord2Comp :: left_markers) [Coord2Comp] markers x in
        let y = List.hd (List.rev k) in
        let markers = mark_coordinations_rec [Coord2Comp] (Coord2 :: Coord2Comp :: right_markers) markers y in
        let k = List.rev (List.tl (List.rev k)) in
        let markers = Xlist.fold k markers (mark_coordinations_rec [Coord2Comp] [Coord2Comp]) in
        markers
      )
    | Lemma("NumberMeasure", _, _, _) -> (match token.args with
      [] -> failwith "NumberMeasure without arguments"
      | [x] -> mark_coordinations_rec left_markers right_markers markers x
      | x :: k ->
        let markers = mark_coordinations_rec left_markers [] markers x in
        let y = List.hd (List.rev k) in
        let markers = mark_coordinations_rec [] right_markers markers y in
        let k = List.rev (List.tl (List.rev k)) in
        let markers = Xlist.fold k markers (mark_coordinations_rec [] []) in
        markers
      )
    | _ ->
      let markers = IntMap.add_inc markers token.beg IntMap.empty (fun f -> f) in
      let beg_dict = IntMap.find markers token.beg in
      let token_markers = (token, (left_markers_to_strings left_markers), (right_markers_to_strings right_markers)) in
      let beg_dict = IntMap.add_inc beg_dict token.next [token_markers] (fun l -> token_markers :: l) in
      let markers = IntMap.add markers token.beg beg_dict in
      (if left_markers != [] || right_markers != [] then
        print_endline ((String.concat "" (left_markers_to_strings left_markers)) ^ (token.orth) ^ (String.concat "" (right_markers_to_strings right_markers)))
      else ());
      markers
  )

let mark_coordinations (tokens : token_env list) =
  let is_done token =
    match token.token with
    Lemma("DONE", _, _, _) -> true
    | _ -> false
  in
  let done_tokens = Xlist.filter tokens is_done in
  let markers = Xlist.fold done_tokens (IntMap.empty) (mark_coordinations_rec [] [])
  in markers

let leave_shorter_tokens tokens =
	let tokens = IntMap.fold tokens [] (fun tokens _ map ->
		IntMap.fold map tokens (fun tokens _ l ->
			TokenEnvSet.fold l tokens (fun tokens t ->
				t :: tokens))) in
	let tokens = remove_contained_tokens tokens "NumberMeasureList" in
	let tokens = remove_contained_tokens tokens "Substance" in
  let tokens = remove_contained_tokens tokens "SubstanceList" in
  let tokens = remove_contained_tokens tokens "DONE" in
	let paths = Xstd.IntMap.empty in
  let tokens = Xlist.fold tokens paths add_token2 in
	tokens

let disambiguate tokens =
(*   print_endline (SubsyntaxStringOf.token_list tokens); *)
(*   Printf.printf "disambiguate: |crules|=%d |crules2|=%d\n%!" (StringMap.size !crules) (StringMap.size !crules2); *)
(* Zamieniam listę tokenów na format akceptowany przez apply_rule (czyli mapę
  map TokenEnvSetów). *)
	let tokens = Xlist.fold tokens [] (fun tokens t -> match t.token with
		Lemma(_,"depr",_,"Measure") -> t :: tokens
		| Lemma(_,_,_,"Measure") -> tokens
		| _ -> t :: tokens) in
  let paths = Xstd.IntMap.empty in
  let tokens = Xlist.fold tokens paths add_token2 in
(* Aplikuję reguły. *)
  let rules = select_rules tokens !crules !crules2 in
  let tokens = Xlist.fold rules tokens apply_rule in

  let rules = select_rules tokens !crules !crules2 in
  let tokens = Xlist.fold rules tokens apply_rule in

	let rules = select_rules tokens !crules !crules2 in
  let tokens = Xlist.fold rules tokens apply_rule in

	let rules = select_rules tokens !crules !crules2 in
  let tokens = Xlist.fold rules tokens apply_rule in

	let rules = select_rules tokens !crules !crules2 in
  let tokens = Xlist.fold rules tokens apply_rule in

	let rules = select_rules tokens !crules !crules2 in
  let tokens = Xlist.fold rules tokens apply_rule in
(* Konwertuję |tokens| z powrotem do listy token_env. *)
  let tokens = IntMap.fold tokens [] (fun tokens _ map ->
    IntMap.fold map tokens (fun tokens _ l ->
      TokenEnvSet.fold l tokens (fun tokens t ->
        t :: tokens))) in
  let tokens = fst (Patterns.sort (tokens,0)) in
(*
 Usuwam te tokeny, które mają lemat Substance i zawierają się w innym tokenie
 z lematem Substance (zawieranie rozumiemy w sensie odpowiednich nierówności
 między atrybutami beg i next).
*)
  let tokens = remove_contained_tokens tokens "NumberMeasureList" in
  let tokens = remove_contained_tokens tokens "Substance" in
  let tokens = remove_contained_tokens tokens "SubstanceList" in
  let tokens = remove_contained_tokens tokens "DONE" in
  let markers = mark_coordinations tokens in
  let tokens = insert_tokens markers tokens in
(*  print_endline (SubsyntaxStringOf.token_list tokens);
  print_endline "XXXXXXXXXXXXXXXXXXXXXXXXX";*)
  tokens

let catch_disambiguate tokens =
  try
    let tokens = disambiguate tokens in tokens,"" (* MWE.disambiguate ? *)
  with e -> [], Printexc.to_string e

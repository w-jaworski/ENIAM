(*
 *  ENIAMvalence is a library that assigns tokens with lexicosemantic information.
 *  Copyright (C) 2016-2017 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
 *  Copyright (C) 2016-2017 Institute of Computer Science Polish Academy of Sciences
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
open Xstd


let rec find a l i =
  if a.(i) = max_int then (
    a.(i) <- i;
    i) else
  if a.(i) = i then (
    Xlist.iter l (fun j -> a.(j) <- i);
    i) else
  find a (i :: l) a.(i)

let union a i j =
  if i = j then i else
  let x = min i j in
  let y = max i j in
  a.(y) <- x;
  x

let rec split_tokens_into_groups_sentence a = function
    RawSentence s -> ()
  | StructSentence([],_) -> ()
  | StructSentence((id,_,_) :: paths,_) ->
      ignore (Xlist.fold paths (find a [] id) (fun m (id,_,_) ->
        union a m (find a [] id)))
  | DepSentence paths_list  ->
      Xlist.iter paths_list (fun paths ->
        if Array.length paths = 0 then () else
        let id,_,_ = paths.(0) in
        (* Printf.printf "id=%s i=%d\n%!" id 0; *)
        ignore (Int.fold 1 (Array.length paths - 1) (find a [] id) (fun m i ->
          let id,_,_ = paths.(i) in
          (* Printf.printf "id=%s i=%d\n%!" id i; *)
          union a m (find a [] id))))
  | QuotedSentences sentences ->
      Xlist.iter sentences (fun p ->
        split_tokens_into_groups_sentence a p.sentence)
  | AltSentence l -> Xlist.iter l (fun (mode,sentence) ->
        split_tokens_into_groups_sentence a sentence)
  | ErrorSentence s -> ()

let rec split_tokens_into_groups_paragraph a = function
    RawParagraph s -> ()
  | StructParagraph(_,sentences) ->
      Xlist.iter sentences (fun p -> split_tokens_into_groups_sentence a p.sentence)
  | AltParagraph l -> Xlist.iter l (fun (mode,paragraph) ->
      split_tokens_into_groups_paragraph a paragraph)
  | ErrorParagraph s -> ()

let rec split_tokens_into_groups_text a = function
    RawText s -> ()
  | StructText paragraphs ->
      Xlist.iter paragraphs (split_tokens_into_groups_paragraph a)
  | AltText l -> Xlist.iter l (fun (mode,text) ->
      split_tokens_into_groups_text a text)
  | ErrorText s -> ()

let split_tokens_into_groups size text =
  let a = Array.make size max_int in
  split_tokens_into_groups_text a text;
  Int.iter 0 (Array.length a - 1) (fun i ->
    if a.(i) <> max_int then a.(i) <- a.(a.(i)));
  let map = Int.fold 0 (Array.length a - 1) IntMap.empty (fun map i ->
    if a.(i) = max_int then map else
    IntMap.add_inc map a.(i) [i] (fun l -> i :: l)) in
  IntMap.fold map [] (fun l _ v -> v :: l)

let mark_nosem frame =
  {frame with positions = List.rev (Xlist.rev_map frame.positions (fun p ->
    if p.mode=["lemma"] || p.role="Lemma" then
      if p.gf <> ARG then failwith "mark_nosem" else
      {p with gf=NOSEM}
    else p))}

let remove_unused_tokens tokens groups =
  let set = Xlist.fold groups IntSet.empty (fun set group ->
    Xlist.fold group set IntSet.add) in
  Int.iter 1 (ExtArray.size tokens - 1) (fun i ->
    if IntSet.mem set i then () else
    ExtArray.set tokens i SubsyntaxTypes.empty_token_env)

open LCGtypes

let rec create_tokens_for_artificial_nodes_rec tokens lex_sems = function
    Node t ->
        let t = if t.id = 0 then (
          let id = ExtArray.add tokens empty_token_env in
          let lex_sem = {empty_lex_sem with frames=[{empty_frame with senses=[t.lemma, [t.lemma,0], unknown_sense_weight]}]} in
          let id2 = ExtArray.add lex_sems lex_sem in
          if id <> id2 then failwith "create_tokens_for_artificial_nodes_rec: tokens inconsistent with lex_sems" else
          let t = if t.symbol = Dot then
            {t with symbol = match t.pos with
                "<root>" -> Tuple[Val "<root>"]
              | "<merge>" -> Tuple[Val "<merge>"]
              | "<raw>" -> Tuple[Val "<raw>"]
              | "pro" -> Tuple[Val "pro"]
              | s -> failwith ("create_tokens_for_artificial_nodes_rec: " ^ s)} else t in
          {t with id=id}) else t in
        Node{t with args = create_tokens_for_artificial_nodes_rec tokens lex_sems t.args}
  | Tuple l ->
      Tuple(List.rev (Xlist.rev_map l (create_tokens_for_artificial_nodes_rec tokens lex_sems)))
  | Variant(e,l) ->
      Variant(e,List.rev (Xlist.rev_map l (fun (i,t) ->
        i, create_tokens_for_artificial_nodes_rec tokens lex_sems t)))
  | Dot -> Dot
  | Ref i -> Ref i
  | t -> failwith ("create_tokens_for_artificial_nodes_rec: " ^ LCGstringOf.linear_term 0 t)

let create_tokens_for_artificial_nodes tokens lex_sems dependency_tree =
  (* print_endline "create_tokens_for_artificial_nodes"; *)
  Int.iter 0 (Array.length dependency_tree - 1) (fun i ->
    dependency_tree.(i) <- create_tokens_for_artificial_nodes_rec tokens lex_sems dependency_tree.(i))


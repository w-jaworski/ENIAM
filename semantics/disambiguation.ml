(*
 *  ENIAMsemantics implements semantic processing for ENIAM
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

open LCGtypes
open Xstd

let rec get_attr pat = function
    [] -> raise Not_found
  | (s,v) :: l ->
      if s = pat then v
      else get_attr pat l

let merge_variant e l =
  let set = Xlist.fold l TermSet.empty (fun set (_,t) -> TermSet.add set t) in
  if TermSet.size set = 1 then TermSet.max_elt set else
  Variant(e,l)

let merge_nodes result_tree t_map e l =
  let l = Xlist.rev_map l (function
      i, Ref id -> i, (match ExtArray.get result_tree id with Node t -> t | _ -> raise Not_found)
    | _ -> raise Not_found) in
  let _,h = List.hd l in
  let h_cat = try get_attr "CAT" h.attrs with Not_found -> Dot in
  let h_coerced = try get_attr "COERCED" h.attrs with Not_found -> Dot in
  Xlist.iter (List.tl l) (fun (_,t) ->
    let t_cat = try get_attr "CAT" t.attrs with Not_found -> Dot in
    let t_coerced = try get_attr "COERCED" t.attrs with Not_found -> Dot in
    if h.orth <> t.orth || h.lemma <> t.lemma || h.pos <> t.pos || h.weight <> t.weight ||
      h.id <> t.id || h.symbol <> t.symbol || h_cat <> t_cat || h_coerced <> t_coerced ||
      h.arg_symbol <> t.arg_symbol || h.arg_dir <> t.arg_dir then raise Not_found else ());
  let args = Xlist.fold l [] (fun l (i,t) -> (i,t.args) :: l) in
  let attrs = Xlist.fold l StringMap.empty (fun map (i,t) ->
    Xlist.fold t.attrs map (fun map (k,v) ->
      StringMap.add_inc map k [i,v] (fun l -> (i,v) :: l))) in
  let args = merge_variant e args in
  let attrs = StringMap.fold attrs [] (fun l k v ->
    (k,merge_variant e v) :: l) in
  let t = Node{h with args=args; attrs=attrs} in
      let s = LCGstringOf.linear_term 0 t in
      if StringMap.mem !t_map s then Ref(StringMap.find !t_map s) else (
      let id = ExtArray.add result_tree t in
      t_map := StringMap.add !t_map s id;
      Ref id)

let rec merge_rec tree result_tree id_map t_map = function
    Ref i ->
      if IntMap.mem !id_map i then Ref(IntMap.find !id_map i) else
      let t = merge_rec tree result_tree id_map t_map (ExtArray.get tree i) in
      let s = LCGstringOf.linear_term 0 t in
      if StringMap.mem !t_map s then Ref(StringMap.find !t_map s) else (
      let id = ExtArray.add result_tree t in
      id_map := IntMap.add !id_map i id;
      t_map := StringMap.add !t_map s id;
      Ref id)
  | Node t -> Node{t with args=merge_rec tree result_tree id_map t_map t.args}
  | Variant(e,l) ->
      let map = Xlist.fold l StringMap.empty (fun map (i,t) ->
        let t = merge_rec tree result_tree id_map t_map t in
        StringMap.add map (LCGstringOf.linear_term 0 t) t) in
      let _,l = StringMap.fold map (1,[]) (fun (i,l) _ t -> i+1, (string_of_int i,t) :: l) in
      (match l with
        [_,t] -> t
      | _ -> (try merge_nodes result_tree t_map e l with Not_found -> Variant(e,List.rev l)))
  | Tuple l ->
      let l = Xlist.rev_map l (merge_rec tree result_tree id_map t_map) in
      Tuple(List.rev l)
  | Dot -> Dot
  | t -> failwith ("merge: " ^ LCGstringOf.linear_term 0 t)

let merge tree =
  let result_tree = ExtArray.make (ExtArray.size tree / 4) Dot in
  let _ = ExtArray.add result_tree Dot in
  let t = merge_rec tree result_tree (ref IntMap.empty) (ref StringMap.empty) (ExtArray.get tree 0) in
  ExtArray.set result_tree 0 t;
  result_tree

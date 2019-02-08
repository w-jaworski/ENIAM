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

open Xstd
open LCGtypes

let transitive_closure l =
  let map = Xlist.fold l IntMap.empty (fun map set ->
    IntSet.fold set map (fun map x ->
      IntMap.add_inc map x set (fun set2 -> IntSet.union set set2))) in
  let f = ref true in
  let r = ref map in
  while !f do
    f := false;
    r := IntMap.fold (!r) (!r) (fun map k set ->
      let set2 = IntSet.fold set set (fun set2 v ->
        IntSet.union set2 (IntMap.find map v)) in
      if IntSet.size set2 > IntSet.size set then f := true;
      IntMap.add map k set2)
  done;
  let map = IntMap.fold (!r) IntMap.empty (fun map _ set ->
    IntMap.add map (IntSet.min_elt set) set) in
  IntMap.fold map [] (fun l _ set -> set :: l)

let rec resolve_rec (cr,ce,co) = function
    Node t ->
      let cr,ce,co = Xlist.fold t.attrs (cr,ce,co) (fun (cr,ce,co) -> function
          "controller",Val c -> StringMap.add_inc cr c (IntSet.singleton t.id) (fun set -> IntSet.add set t.id),ce,co
        | "controllee",Val c -> cr,StringMap.add_inc ce c (IntSet.singleton t.id) (fun set -> IntSet.add set t.id),co
        | "coref",Val c -> cr,ce,StringMap.add_inc co c (IntSet.singleton t.id) (fun set -> IntSet.add set t.id)
        | "controller",_ -> failwith "resolve_rec"
        | "controllee",_ -> failwith "resolve_rec"
        | "coref",_ -> failwith "resolve_rec"
        | _ -> cr,ce,co) in
      resolve_rec (cr,ce,co) t.args (* FIXME: czy to wywołanie jest potrzebne ? *)
  | Tuple l -> Xlist.fold l (cr,ce,co) resolve_rec
  | Variant(e,l) -> Xlist.fold l (cr,ce,co) (fun (cr,ce,co) (i,t) -> resolve_rec (cr,ce,co) t)
  | Dot -> cr,ce,co
  | Ref i -> cr,ce,co
  | t -> failwith ("resolve_rec: " ^ LCGstringOf.linear_term 0 t)

let rec set_coref use_label co = function
    Node t ->
      let t = if IntMap.mem co t.id then {t with attrs=((if IntSet.mem use_label t.id then "label" else "def-label"),Val(IntMap.find co t.id)) :: t.attrs} else t in
      Node{t with args = set_coref use_label co t.args}
  | Tuple l -> Tuple(List.rev (Xlist.rev_map l (set_coref use_label co)))
  | Variant(e,l) ->
      Variant(e,List.rev (Xlist.rev_map l (fun (i,t) ->
        i, set_coref use_label co t)))
  | Dot -> Dot
  | Ref i -> Ref i
  | t -> failwith ("set_coref: " ^ LCGstringOf.linear_term 0 t)

let resolve dependency_tree =
  let cr,ce,co = Int.fold 0 (Array.length dependency_tree - 1) (StringMap.empty,StringMap.empty,StringMap.empty) (fun (cr,ce,co) i ->
    resolve_rec (cr,ce,co) dependency_tree.(i)) in
  let use_label = StringMap.fold co IntSet.empty (fun use_label _ set -> IntSet.union use_label set) in
  let co = StringMap.fold co [] (fun co c set -> (IntSet.union set (try StringMap.find cr c with Not_found -> failwith "resolve")) :: co) in
  let co,_ = Xlist.fold (transitive_closure co) (IntMap.empty,1) (fun (co,i) set ->
    let label = string_of_int i in
    IntSet.fold set co (fun co id ->
      IntMap.add co id label), i+1) in
  Int.iter 0 (Array.length dependency_tree - 1) (fun i ->
    dependency_tree.(i) <- set_coref use_label co dependency_tree.(i))

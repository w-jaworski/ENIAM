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

type node = {
  orth: string;
  lemma: string;
  pos: string;
  weight: float;
  id: int;
  symbol: linear_term;
  arg_symbol: linear_term;
  arg_dir: string;
  (*agf: WalTypes.gf;
  amorf: WalTypes.morf;
  arole: string;
  arole_attr: string;
  sense: string;
  hipero: StringSet.t;
  sense_weight: float;
    position: WalTypes.schema_field;*)
  attrs: (string * linear_term) list;
  args: linear_term;
  gf: string;
  role: string;
  role_attr: string;
  coord_arg: int;
  selprefs: linear_term;
(*   sense: linear_term; *)
  arole: string;
  arole_attr: string;
  arev: bool;
  sem_args: linear_term;
  n_label: string;
  n_def_label: string;
  snode: string;
  (*cat: linear_term;
  coerced: linear_term;*)
}

(*and concept =
  {c_sense: linear_term; c_gsense: linear_term; c_orth: linear_term; c_name: linear_term; (*c_visible_var: bool;*)
    c_quant: linear_term; c_local_quant: bool; c_label: string; c_def_label: string;
   (*c_modalities: (string * type_term) list;*)
   c_relations: linear_term; c_variable: (string * string);
   c_pos: int; c_cat: linear_term}

and context =
  {cx_sense: linear_term; cx_contents: linear_term; cx_label: string; cx_def_label: string;
    cx_relations: linear_term; cx_variable: (string * string); cx_pos: int; cx_cat: linear_term}*)

and concept=
  {cat: string; sense: string; contents: linear_term; atrs: (string * linear_term) list;
   relations: linear_term; label: string; def_label: string}
    
and linear_term =
    Tuple of linear_term list
  | Variant of string * (string * linear_term) list (* etykieta * indeks * term *)
  | Dot
  | Val of string
  | Node of node
  | Concept of concept
(*   | Context of context *)
  | Relation of string * string * linear_term (* role * role_attr * concept *)
  | RevRelation of string * string * linear_term (* role * role_attr * concept *)
  | SingleRelation of linear_term
  (* | TripleRelation of string * string * linear_term * linear_term (* role * role_attr * concept *) *)
  | AddRelation of linear_term * string * string * linear_term (* nadrzędnik * role * role_attr * podrzędnik *)
  | AddParentRelation of linear_term * linear_term (* relacja * podrzędnik *)
  | AddSingleRelation of linear_term * linear_term (* role * podrzędnik *)
  | RemoveRelation of string * string * linear_term
  | SetContextName of string * linear_term (* sense * concept *)
  | CreateContext of concept * linear_term (* context * args *)
  (* | MakeTripleRelation of string * string * linear_term (* role * role_attr * concept *) *)
  | ManageCoordination of node * linear_term
  | Ref of int

let empty_concept =
  {cat=""; sense=""; contents=Dot; relations=Dot; label=""; def_label=""; atrs=[]}

let sem_lexicon_filename = WalTypes.resource_path ^ "/semantics/lexicon-pl.dic"

let user_ontology_flag = ref false

let rec compare_linear_term = function
    Concept c,Concept d ->
      if c.sense = d.sense && c.label = d.label &&
        c.def_label = d.def_label && c.cat = d.cat &&
        compare_linear_term (c.contents,d.contents) = 0 &&
        compare_linear_term (c.relations,d.relations) = 0 then 0 else compare c d
(*    Concept c,Concept d ->
      if c.c_sense = d.c_sense && c.c_name = d.c_name && c.c_label = d.c_label &&
        c.c_def_label = d.c_def_label && c.c_cat = d.c_cat && c.c_local_quant = d.c_local_quant &&
        compare_linear_term (c.c_quant,d.c_quant) = 0 &&
        compare_linear_term (c.c_relations,d.c_relations) = 0 then 0 else compare c d
  | Context c,Context d ->
      if c.cx_sense = d.cx_sense && c.cx_cat = d.cx_cat &&
        compare_linear_term (c.cx_contents,d.cx_contents) = 0 &&
        compare_linear_term (c.cx_relations,d.cx_relations) = 0 then 0 else compare c d*)
  | (Relation(r1,a1,t1) as s),(Relation(r2,a2,t2) as t) ->
      if r1 = r2 && a1 = a2 then compare_linear_term (t1,t2) else compare s t
  | (RevRelation(r1,a1,t1) as s),(RevRelation(r2,a2,t2) as t) ->
      if r1 = r2 && a1 = a2 then compare_linear_term (t1,t2) else compare s t
  | SingleRelation s,SingleRelation t -> compare_linear_term (s,t)
  | (Tuple l1 as s),(Tuple l2 as t) ->
      if Xlist.size l1 = Xlist.size l2 then
        if Xlist.fold2 l1 l2 true (fun b t1 t2 ->
          b && compare_linear_term (t1,t2) = 0) then 0
        else compare s t
      else compare s t
  | (Variant(e1,l1) as s),(Variant(e2,l2) as t) ->
      if e1 = e2 && Xlist.size l1 = Xlist.size l2 then
        if Xlist.fold2 l1 l2 true (fun b (i1,t1) (i2,t2) ->
          b && i1 = i2 && compare_linear_term (t1,t2) = 0) then 0
        else compare s t
      else compare s t
  | s,t -> compare s t


module OrderedTerm = struct

  type t = linear_term

  let compare s t = compare_linear_term (s,t)

end

module TermMap = Xmap.Make(OrderedTerm)
module TermSet = Xset.Make(OrderedTerm)

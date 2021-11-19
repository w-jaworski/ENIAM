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

open SemTypes

let rec linear_term c = function
  | Tuple l ->
    let s = String.concat "⊗" (Xlist.map l (linear_term 2)) in
    if c > 1 then "(" ^ s ^ ")" else s
  | Variant(e,l) -> "〈" ^ String.concat "," (Xlist.map l (fun (i,t) -> e^i^": "^linear_term 0 t)) ^ "〉"
  | Dot -> "∙"
  | Val s -> s
  | Node t ->
    "[" ^
    (String.concat "; " (Xlist.map (
      ["ORTH",Val t.orth;"LEMMA",Val t.lemma;"POS",Val t.pos;"ID",Val (string_of_int t.id)] @ 
      (if t.n_label = "" then [] else ["LABEL",Val t.n_label]) @
      (if t.n_def_label = "" then [] else ["DEF-LABEL",Val t.n_def_label]) @
      (if t.weight = 0. then [] else ["WEIGHT",Val (string_of_float t.weight)]) @
      ["SYMBOL",t.symbol;"ARG_SYMBOL",t.arg_symbol;"ARG_DIR",Val t.arg_dir;"ARGS",t.args] @ t.attrs) (fun (e,t) ->
         e ^ ": " ^ (linear_term 0 t)))) ^ "]"
  | Ref i -> "ref " ^ string_of_int i
  | Concept c ->
    "[" ^
    (String.concat "; " (Xlist.map (
      ["SENSE",Val c.sense;(*"NAME",c.name;*)"CAT",Val c.cat] @ 
      (if c.label = "" then [] else ["LABEL",Val c.label]) @
      (if c.def_label = "" then [] else ["DEF-LABEL",Val c.def_label]) @
         (*"VARIABLE",Val (fst c.variable ^ "_" ^ snd c.variable);"POS",Val (string_of_int c.pos);
         "QUANT",c.quant;"LOCAL-QUANT",if c.local_quant then Val "+" else Val "-";*)
      ["RELATIONS",c.relations] @ 
      (if c.contents = Dot then [] else ["CONTENTS",c.contents])) (fun (e,t) ->
        e ^ ": " ^ (linear_term 0 t)))) ^ "]"
(*  | Context c ->
    "[" ^
    (String.concat "; " (Xlist.map ([
         "SENSE",c.cx_sense;"CAT",c.cx_cat;"LABEL",Val c.cx_label;"DEF-LABEL",Val c.cx_def_label;
         "VARIABLE",Val (fst c.cx_variable ^ "_" ^ snd c.cx_variable);"POS",Val (string_of_int c.cx_pos);
         "RELATIONS",c.cx_relations;"CONTENTS",c.cx_contents]) (fun (e,t) ->
        e ^ ": " ^ (linear_term 0 t)))) ^ "]"*)
  | Relation(r,"",c) -> "relation(" ^ r ^ "," ^ linear_term 0 c ^ ")"
  | Relation(r,a,c) -> "relation(" ^ r ^ "," ^ a ^ "," ^ linear_term 0 c ^ ")"
  | RevRelation(r,"",c) -> "revrelation(" ^ r ^ "," ^ linear_term 0 c ^ ")"
  | RevRelation(r,a,c) -> "revrelation(" ^ r ^ "," ^ a ^ "," ^ linear_term 0 c ^ ")"
  | SingleRelation r -> "singlerelation(" ^ linear_term 0 r ^ ")"
  (* | TripleRelation(r,a,c,t) -> "triplerelation(" ^ r ^ "," ^ a ^ "," ^ linear_term 0 c ^ "," ^ linear_term 0 t ^ ")" *)
  | AddRelation(t,r,a,s) -> "addrelation(" ^ linear_term 0 t ^ "," ^ r ^ "," ^ a ^ "," ^ linear_term 0 s ^ ")"
  | AddParentRelation(t,s) -> "addparentrelation(" ^ linear_term 0 t ^ "," ^ linear_term 0 s ^ ")"
  | AddSingleRelation(r,s) -> "addsinglerelation(" ^ linear_term 0 r ^ "," ^ linear_term 0 s ^ ")"
  | RemoveRelation(r,a,t) -> "removerelation(" ^ r ^ "," ^ a ^ "," ^ linear_term 0 t ^ ")"
  | SetContextName(s,t) -> "setcontextname(" ^ s ^ "," ^ linear_term 0 t ^ ")"
  | CreateContext(s,t) -> "createcontext(" ^ linear_term 0 (Concept s) ^ "," ^ linear_term 0 t ^ ")"
  (* | MakeTripleRelation(r,a,c) -> "maketriplerelation(" ^ r ^ "," ^ a ^ "," ^ linear_term 0 c ^ ")" *)
  | ManageCoordination(t,r) -> "managecoordination(" ^ linear_term 0 r ^ ")" (* FIXME: t *)

let escape_string s = s
  
let rec split_relations single_rels = function
  | Tuple l -> 
      let single_rels,l = Xlist.fold l (single_rels,[]) (fun (single_rels,l) t -> 
        let single_rels, t = split_relations single_rels t in
        if t = Dot then single_rels, l else single_rels, t :: l) in
      if l = [] then single_rels, Dot else
      single_rels, Tuple l
  | SingleRelation r -> (SingleRelation r) :: single_rels, Dot
  | t -> single_rels, t

  
let rec linear_term_formatted spaces = function
  | Tuple l -> 
      let l = List.rev (Xlist.fold l [] (fun l -> function Dot -> l | t -> t :: l)) in
      String.concat ("\n" ^ spaces) (Xlist.map l (linear_term_formatted spaces))
  | Variant(e,l) -> "〈" ^ String.concat (",\n"^spaces ^ "  ") (Xlist.map l (fun (i,t) -> e^i^": "^linear_term_formatted (spaces ^ "  ") t)) ^ "〉"
  | Dot -> "∙"
  | Val s -> s
  | Node t ->
    "[" ^
    (String.concat "; " (Xlist.map (["ORTH",Val t.orth;"LEMMA",Val t.lemma;"POS",Val t.pos;"ID",Val (string_of_int t.id);"LABEL",Val t.n_label;"DEF-LABEL",Val t.n_def_label;
                                     "WEIGHT",Val (string_of_float t.weight);"SYMBOL",t.symbol;
                                     "ARG_SYMBOL",t.arg_symbol;"ARG_DIR",Val t.arg_dir] @ t.attrs) (fun (e,t) ->
         e ^ ": " ^ (linear_term_formatted spaces t)))) ^ "]\n" ^
      linear_term_formatted (spaces ^ "  ") t.args          
  | Ref i -> "ref " ^ string_of_int i
  | Concept t ->
      let single_rels, relations = split_relations [] t.relations in
      let s = 
        String.concat " " (
            (if t.label="" then [] else ["?" ^ t.label]) @
            (if t.def_label="" then [] else ["*" ^ t.def_label]) @
            (if t.cat = "" then [] else [escape_string t.cat]) @
            (if t.sense = "" then [] else ["„" ^ escape_string t.sense ^ "”"]) @
            (if single_rels = [] then [] else (Xlist.map single_rels (linear_term_formatted "")))) in
      if relations = Dot then
        if t.contents = Dot then s else
        s ^ " [\n" ^ spaces ^ "  " ^ linear_term_formatted (spaces ^ "  ") t.contents ^ "]"
      else
        s ^ "\n" ^ spaces ^ "  " ^ linear_term_formatted (spaces ^ "  ") relations ^
        (if t.contents = Dot then "" else
        " [\n" ^ spaces ^ "  " ^ linear_term_formatted (spaces ^ "  ") t.contents ^ "]")
  | Relation(r,"",c) -> r ^ ": " ^ linear_term_formatted spaces c
  | RevRelation(r,"",c) -> "REV " ^ r ^ ": " ^ linear_term_formatted spaces c
  | SingleRelation r -> linear_term_formatted spaces r
  (* | TripleRelation(r,a,c,t) -> "triplerelation(" ^ r ^ "," ^ a ^ "," ^ linear_term_formatted (spaces ^ "  ") c ^ "," ^ linear_term_formatted (spaces ^ "  ") t ^ ")" *)
(*  | AddRelation(t,r,a,s) -> "addrelation(\n" ^ linear_term_formatted (spaces ^ "  ") t ^ "," ^ r ^ "," ^ a ^ ",\n" ^ linear_term_formatted (spaces ^ "  ") s ^ ")"
  | AddParentRelation(t,s) -> "addparentrelation(\n" ^ linear_term_formatted (spaces ^ "  ") t ^ ",\n" ^ linear_term_formatted (spaces ^ "  ") s ^ ")"
  | AddSingleRelation(r,s) -> "addsinglerelation(\n" ^ linear_term_formatted (spaces ^ "  ") r ^ ",\n" ^ linear_term_formatted (spaces ^ "  ") s ^ ")"
  | RemoveRelation(r,a,t) -> "removerelation(" ^ r ^ "," ^ a ^ ",\n" ^ linear_term_formatted (spaces ^ "  ") t ^ ")"
  | SetContextName(s,t) -> "setcontextname(" ^ s ^ ",\n" ^ linear_term_formatted (spaces ^ "  ") t ^ ")"
  | CreateContext(s,t) -> "createcontext(\n" ^ linear_term_formatted (spaces ^ "  ") (Concept s) ^ ",\n" ^ linear_term_formatted (spaces ^ "  ") t ^ ")"
  (* | MakeTripleRelation(r,a,c) -> "maketriplerelation(" ^ r ^ "," ^ a ^ "," ^ linear_term_formatted (spaces ^ "  ") c ^ ")" *)
  | ManageCoordination(t,r) -> "managecoordination(\n" ^ linear_term_formatted (spaces ^ "  ") r ^ ")" (* FIXME: t *)*)
  | _ -> failwith "linear_term_formatted"
  
 

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
    (String.concat "; " (Xlist.map (["ORTH",Val t.orth;"LEMMA",Val t.lemma;"POS",Val t.pos;"ID",Val (string_of_int t.id);"LABEL",Val t.n_label;"DEF-LABEL",Val t.n_def_label;
                                     "WEIGHT",Val (string_of_float t.weight);"SYMBOL",t.symbol;
                                     "ARG_SYMBOL",t.arg_symbol;"ARG_DIR",Val t.arg_dir;"ARGS",t.args] @ t.attrs) (fun (e,t) ->
         e ^ ": " ^ (linear_term 0 t)))) ^ "]"
  | Ref i -> "ref " ^ string_of_int i
  | Concept c ->
    "[" ^
    (String.concat "; " (Xlist.map ([
         "SENSE",Val c.sense;(*"NAME",c.name;*)"CAT",Val c.cat;"LABEL",Val c.label;"DEF-LABEL",Val c.def_label;
         (*"VARIABLE",Val (fst c.variable ^ "_" ^ snd c.variable);"POS",Val (string_of_int c.pos);
         "QUANT",c.quant;"LOCAL-QUANT",if c.local_quant then Val "+" else Val "-";*)"RELATIONS",c.relations;"CONTENTS",c.contents]) (fun (e,t) ->
        e ^ ": " ^ (linear_term 0 t)))) ^ "]"
(*  | Context c ->
    "[" ^
    (String.concat "; " (Xlist.map ([
         "SENSE",c.cx_sense;"CAT",c.cx_cat;"LABEL",Val c.cx_label;"DEF-LABEL",Val c.cx_def_label;
         "VARIABLE",Val (fst c.cx_variable ^ "_" ^ snd c.cx_variable);"POS",Val (string_of_int c.cx_pos);
         "RELATIONS",c.cx_relations;"CONTENTS",c.cx_contents]) (fun (e,t) ->
        e ^ ": " ^ (linear_term 0 t)))) ^ "]"*)
  | Relation(r,a,c) -> "relation(" ^ r ^ "," ^ a ^ "," ^ linear_term 0 c ^ ")"
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

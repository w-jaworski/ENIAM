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
open Xstd
open Printf

(*let rec first_digit_index v n i =
  if i = n then n else
  if String.get v i >= '0' && String.get v i <= '9' then i
  else first_digit_index v n (i+1)

let variable v =
  let n = String.length v in
  let i = first_digit_index v n 0 in
  if n = i then v else
    String.sub v 0 i ^ "_{" ^ String.sub v i (n-i) ^ "}"*)

let rec linear_term c = function
    Tuple l ->
    let l = Xlist.map l (linear_term 2) in
    let n = Xlist.fold l 0 (fun n s -> String.length s + n) in
    let s =
      if n < 100 then String.concat "\\otimes " l else
        let s = String.concat "\\otimes\\\\ {}" l in
        "\\begin{array}{l}" ^ s ^ "\\end{array}" in
    if c > 1 then "(" ^ s ^ ")" else s
  | Variant(e,l) -> "\\langle " ^ String.concat ","(*"$,\\\\$"*) (Xlist.map l (fun (i,t) -> e^i^": "^linear_term 0 t)) ^ "\\rangle"
  | Dot -> "\\bullet"
  | Val s -> "\\text{" ^ Xlatex.escape_string s ^ "}"
  | Node t ->
    "{\\left[\\begin{array}{ll}" ^
    (String.concat "\\\\ " (Xlist.map (["ORTH",Val t.orth;"LEMMA",Val t.lemma;"POS",Val t.pos;"ID",Val (string_of_int t.id);"LABEL",Val t.n_label;"DEF-LABEL",Val t.n_def_label;
                                        "WEIGHT",Val (string_of_float t.weight);"SYMBOL",t.symbol;
                                        "ARG_SYMBOL",t.arg_symbol;"ARG_DIR",Val t.arg_dir;
                                        "GF",Val t.gf; "ROLE", Val t.role; "ROLE-ATTR", Val t.role_attr;
                                        "SELPREFS",t.selprefs; (*"SENSE",t.sense;*)
                                        "AROLE", Val t.role; "AROLE-ATTR", Val t.role_attr; "AREV", Val (if t.arev then "+" else "-");
                                        "SEM-ARGS",t.sem_args; "ARGS",t.args] @ t.attrs) (fun (e,t) ->
         "\\text{" ^ (Xlatex.escape_string e) ^ "} & " ^ (linear_term 0 t)))) ^ "\\end{array}\\right]}"
  | Ref i -> "{\\bf ref}\\; " ^ string_of_int i
  (* | Choice choices -> "{\\bf choice}(" ^ String.concat ";" (StringMap.fold choices [] (fun l ei t -> (sprintf "%s: %s" ei (linear_term 0 t)) :: l)) ^ ")"*)
  | Concept c ->
    "{\\left[\\begin{array}{ll}" ^
    (String.concat "\\\\ " (Xlist.map ([
         "SENSE",Val c.sense;(*"NAME",c.c_name;*)"CAT",Val c.cat;"LABEL",Val c.label;"DEF-LABEL",Val c.def_label;
         (*"VARIABLE",Val (fst c.c_variable ^ "_" ^ snd c.c_variable);"POS",Val (string_of_int c.c_pos);
         "QUANT",c.c_quant;"LOCAL-QUANT",if c.c_local_quant then Val "+" else Val "-";*)"RELATIONS",c.relations;"CONTENTS",c.contents]) (fun (e,t) ->
        "\\text{" ^ (Xlatex.escape_string e) ^ "} & " ^ (linear_term 0 t)))) ^ "\\end{array}\\right]}"
(*  | Concept c ->
    "{\\left[\\begin{array}{ll}" ^
    (String.concat "\\\\ " (Xlist.map ([
         "SENSE",c.c_sense;"NAME",c.c_name;"CAT",c.c_cat;"LABEL",Val c.c_label;"DEF-LABEL",Val c.c_def_label;
         "VARIABLE",Val (fst c.c_variable ^ "_" ^ snd c.c_variable);"POS",Val (string_of_int c.c_pos);
         "QUANT",c.c_quant;"LOCAL-QUANT",if c.c_local_quant then Val "+" else Val "-";"RELATIONS",c.c_relations]) (fun (e,t) ->
        "\\text{" ^ (Xlatex.escape_string e) ^ "} & " ^ (linear_term 0 t)))) ^ "\\end{array}\\right]}"
  | Context c ->
    "{\\left[\\begin{array}{ll}" ^
    (String.concat "\\\\ " (Xlist.map ([
         "SENSE",c.cx_sense;"CAT",c.cx_cat;"LABEL",Val c.cx_label;"DEF-LABEL",Val c.cx_def_label;
         "VARIABLE",Val (fst c.cx_variable ^ "_" ^ snd c.cx_variable);"POS",Val (string_of_int c.cx_pos);
         "RELATIONS",c.cx_relations;"CONTENTS",c.cx_contents]) (fun (e,t) ->
        "\\text{" ^ (Xlatex.escape_string e) ^ "} & " ^ (linear_term 0 t)))) ^ "\\end{array}\\right]}"*)
  | Relation(r,a,c) -> "{\\bf relation}(" ^ (*linear_term 0*) r ^ "," ^ (*linear_term 0*) a ^ "," ^ linear_term 0 c ^ ")"
  | RevRelation(r,a,c) -> "{\\bf revrelation}(" ^ (*linear_term 0*) r ^ "," ^ (*linear_term 0*) a ^ "," ^ linear_term 0 c ^ ")"
  | SingleRelation r -> "{\\bf singlerelation}(" ^ linear_term 0 r ^ ")"
  (* | TripleRelation(r,a,c,t) -> "{\\bf triplerelation}(" ^ (*linear_term 0*) r ^ "," ^ (*linear_term 0*) a ^ "," ^ linear_term 0 c ^ "," ^ linear_term 0 t ^ ")" *)
  | AddRelation(t,r,a,s) -> "{\\bf addrelation}(" ^ linear_term 0 t ^ "," ^ r ^ "," ^ a ^ "," ^ linear_term 0 s ^ ")"
  | AddParentRelation(r,s) -> "{\\bf addparentrelation}(" ^ linear_term 0 r ^ "," ^ linear_term 0 s ^ ")"
  | AddSingleRelation(r,s) -> "{\\bf addsinglerelation}(" ^ linear_term 0 r ^ "," ^ linear_term 0 s ^ ")"
  | RemoveRelation(r,a,t) -> "{\\bf removerelation}(" ^ r ^ "," ^ a ^ "," ^ linear_term 0 t ^ ")"
  | SetContextName(s,t) -> "{\\bf setcontextname}(" ^ s ^ "," ^ linear_term 0 t ^ ")"
  | CreateContext(s,t) -> "{\\bf createcontext}(" ^ linear_term 0 (Concept s) ^ "," ^ linear_term 0 t ^ ")"
  (* | MakeTripleRelation(r,a,c) -> "{\\bf maketriplerelation}(" ^ (*linear_term 0*) r ^ "," ^ (*linear_term 0*) a ^ "," ^ linear_term 0 c ^ ")" *)
  | ManageCoordination(t,r) -> "{\\bf managecoordination}(" ^ linear_term 0 r ^ ")" (* FIXME: t *)

let print_semantic_graph path name page dependency_tree =
  Xlatex.latex_file_out path name page false (fun file ->
      Int.iter 0 (Array.length dependency_tree - 1) (fun i ->
          if dependency_tree.(i) <> Dot then Printf.fprintf file "{\\bf %d} $%s$\\\\\n" i (linear_term 0 dependency_tree.(i))));
  Xlatex.latex_compile_and_clean path name

(* let print_references path name page references =
  Xlatex.latex_file_out path name page false (fun file ->
      Int.iter 0 (ExtArray.size references - 1) (fun i ->
          if ExtArray.get references i <> Dot then Printf.fprintf file "{\\bf %d} $%s$\\\\\n" i (linear_term 0 (ExtArray.get references i))));
  Xlatex.latex_compile_and_clean path name *)

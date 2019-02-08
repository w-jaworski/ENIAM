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
open Printf

let escape_string s =
  Int.fold 0 (String.length s - 1) "" (fun t i ->
      match String.sub s i 1 with
        "<" -> t ^ "〈"
      | ">" -> t ^ "〉"
      | "\"" -> t ^ "\\\""
      | c -> t ^ c)

let string_of_node t =
  let l = [
    "ORTH",Val t.orth;"LEMMA",Val t.lemma;"POS",Val t.pos;"ID",Val (string_of_int t.id);"LABEL",Val t.n_label;"DEF-LABEL",Val t.n_def_label;"WEIGHT",Val (string_of_float t.weight);
    "SYMBOL",t.symbol;"ARG_SYMBOL",t.arg_symbol;"ARG_DIR",Val t.arg_dir;
    "GF",Val t.gf;"ROLE",Val t.role;"ROLE_ATTR",Val t.role_attr;"SELPREFS",t.selprefs;(*"SENSE",t.sense;*)
    "AROLE",Val t.arole;"AROLE_ATTR",Val t.role_attr;"AREV",Val (string_of_bool t.arev);"SEM_ARGS",t.sem_args] @ t.attrs in
  "{ " ^ String.concat " | " (Xlist.map l (fun (e,t) -> "{ " ^ e ^ " | " ^ escape_string (SemStringOf.linear_term 0 t) ^ " }")) ^ " }"

let rec string_of_quant_rec quant = function
    Tuple l -> Xlist.fold l quant string_of_quant_rec
  | Variant(e,l) -> (SemStringOf.linear_term 0 (Variant(e,l))) :: quant
  | Dot -> quant
  | Val s -> s :: quant
  | _ -> failwith "string_of_quant_rec"

let string_of_quant t =
  let l = string_of_quant_rec [] t in
  let s = String.concat " " l in
  if s = "" then "" else "<I>" ^ s ^ "</I> "

(*
let single_rel_id_count = ref 0

let get_single_rel_id () =
  let id = !single_rel_id_count in
  incr single_rel_id_count;
  "s" ^ string_of_int id

let print_edge file label upper id =
  if upper <> "" then
    if label = "" then fprintf file "  %s -> %s\n" upper id
    else fprintf file "  %s -> %s  [label=\"%s\"]\n" upper id label

let rec print_dependency_tree_rec file edge upper id = function
    Node t ->
    fprintf file "  %s [label=\"%s\"]\n" id (string_of_node t);
    print_edge file edge upper id;
    print_dependency_tree_rec2 file "" id t.args
  | Variant(e,l) ->
    fprintf file "  %s [shape=diamond,label=\"%s\"]\n" id e;
    print_edge file edge upper id;
    Xlist.iter l (fun (i,t) -> print_dependency_tree_rec2 file i id t)
  | Val s ->
    fprintf file "  %s [shape=box,label=\"%s\"]\n" id s;
    print_edge file edge upper id
  (* | SetAttr(a,s,t) ->
    fprintf file "  %s [shape=box,label=\"SetAttr(%s,%s)\"]\n" id a (LCGstringOf.linear_term 0 s);
    print_edge file edge upper id;
    print_dependency_tree_rec2 file "" id t *)
  | Dot -> ()
  (*          fprintf file "  %s [shape=box,label=\"Dot\"]\n" id;
              print_edge file edge upper id*)
  | Ref i -> print_edge file edge upper ("x" ^ string_of_int i)
  | t -> failwith ("print_dependency_tree_rec: " ^ SemStringOf.linear_term 0 t)

and print_dependency_tree_rec2 file edge upper = function
    Tuple l -> Xlist.iter l (print_dependency_tree_rec2 file edge upper)
  | t -> print_dependency_tree_rec file edge upper (get_single_rel_id ()) t

let print_semantic_graph path name dependency_tree =
  single_rel_id_count := 0;
  File.file_out (path ^ name ^ ".gv") (fun file ->
      fprintf file "digraph G {\n  node [shape=record]\n";
      Int.iter 0 (Array.length dependency_tree - 1) (fun i -> print_dependency_tree_rec file "" "" ("x" ^ string_of_int i) dependency_tree.(i));
      fprintf file "}\n");
  Sys.chdir path;
  ignore (Sys.command ("dot -Tpng " ^ name ^ ".gv -o " ^ name ^ ".png"));
  ignore (Sys.command ("rm " ^ name ^ ".gv"));
  String.iter (function '/' -> Sys.chdir ".." | _ -> ()) path
*)

let id_counter = ref 0

let print_edge2 file edge_rev edge_label edge_style edge_head edge_tail upper id =
  let edge_head,edge_tail,upper,id = if edge_rev then edge_tail,edge_head,id,upper else edge_head,edge_tail,upper,id in
  let l =
    (if edge_label = "" then [] else ["label=\"" ^ edge_label ^ "\""]) @
    (if edge_style = "" then [] else ["style=\"" ^ edge_style ^ "\""]) @
    (if edge_head = "" then [] else ["ltail=\"" ^ edge_head ^ "\""]) @
    (if edge_tail = "" then [] else ["lhead=\"" ^ edge_tail ^ "\""]) in
  if upper <> 0 then
    if l = [] then fprintf file "  %d -> %d\n" upper id
    else fprintf file "  %d -> %d  [%s]\n" upper id (String.concat "," l)

(*let rec print_graph2_rec file edge_rev edge_label edge_head upper = function
    Node t ->
          let id = !id_counter in
          incr id_counter;
          fprintf file "  %d [label=\"%s\"]\n" id (string_of_node t);
          print_edge2 file edge_rev edge_label edge_head "" upper id;
          print_graph2_rec file false "" "" id t.args
  | Concept t ->
          let id = !id_counter in
          incr id_counter;
          fprintf file "  %d [shape=box,label=<%s%s %s>]\n" id
            (string_of_quant t.c_quant)
            (escape_string (SemStringOf.linear_term 0 t.c_sense))
            (if t.c_name=Dot then "" else "„" ^ SemStringOf.linear_term 0 t.c_name ^ "”"); (* FIXME *)
          print_edge2 file edge_rev edge_label edge_head "" upper id;
          print_graph2_rec file false "" "" id t.c_relations
  | Context t ->
          let id = !id_counter in
          incr id_counter;
          if t.cx_sense = Dot then fprintf file "  subgraph cluster%d {\nlabel=\"\"\n" id
          else fprintf file "  subgraph cluster%d {\nlabel=\"%s\"\n" id (SemStringOf.linear_term 0 t.cx_sense);
          print_graph2_rec file false "" "" 0 t.cx_contents;
          fprintf file "  }\n";
          print_edge2 file edge_rev edge_label edge_head ("cluster" ^ string_of_int id) upper (id+1);
          print_graph2_rec file false "" ("cluster" ^ string_of_int id) (id+1) t.cx_relations;
  | Relation(role,role_attr,t) ->
          let id = !id_counter in
          incr id_counter;
          fprintf file "  %d [shape=circle,label=\"%s\\n%s\"]\n" id role role_attr;
          print_edge2 file false edge_label edge_head "" upper id;
          print_graph2_rec file false "" "" id t
  | RevRelation(role,role_attr,t) ->
          let id = !id_counter in
          incr id_counter;
          fprintf file "  %d [shape=circle,label=\"%s\\n%s\"]\n" id role role_attr;
          print_edge2 file true edge_label edge_head "" upper id;
          print_graph2_rec file true "" "" id t
  | SingleRelation(role) ->
          let id = !id_counter in
          incr id_counter;
          fprintf file "  %d [shape=circle,label=\"%s\"]\n" id role;
          print_edge2 file false edge_label edge_head "" upper id
  | AddRelation(t,role,role_attr,s) ->
          let id = !id_counter in
          incr id_counter;
          fprintf file "  %d [shape=circle,label=\"AddRelation\\n%s\\n%s\"]\n" id role role_attr;
          print_edge2 file edge_rev edge_label edge_head "" upper id;
          print_graph2_rec file false "" "" id t;
          print_graph2_rec file false "" "" id s
  | RemoveRelation t ->
          let id = !id_counter in
          incr id_counter;
          fprintf file "  %d [shape=circle,label=\"RemoveRelation\"]\n" id;
          print_edge2 file edge_rev edge_label edge_head "" upper id;
          print_graph2_rec file false "" "" id t
  | SetContextName(s,t) ->
          let id = !id_counter in
          incr id_counter;
          fprintf file "  %d [shape=circle,label=\"SetContextName\\n%s\"]\n" id s;
          print_edge2 file edge_rev edge_label edge_head "" upper id;
          print_graph2_rec file false "" "" id t;
  | Tuple l -> Xlist.iter l (print_graph2_rec file edge_rev edge_label edge_head upper)
  | Variant(e,l) ->
          let id = !id_counter in
          incr id_counter;
          fprintf file "  %d [shape=diamond,label=\"%s\"]\n" id e;
          print_edge2 file edge_rev edge_label edge_head "" upper id;
          Xlist.iter l (fun (i,t) -> print_graph2_rec file edge_rev  i "" id t)
  | Val s ->
          let id = !id_counter in
          incr id_counter;
          fprintf file "  %d [shape=box,label=\"%s\"]\n" id s;
          print_edge2 file edge_rev edge_label edge_head "" upper id
  | Ref i -> print_edge2 file edge_rev edge_label edge_head "" upper i
  | Dot -> ()
  (* | t -> failwith ("print_graph_rec: " ^ SemStringOf.linear_term 0 t) *)

let print_semantic_graph path name query t =
(*   print_endline  *)
  id_counter := 1;
  File.file_out (path ^ name ^ ".gv") (fun file ->
    fprintf file "digraph G {\n  compound=true\n  node [shape=record]\n";
    Int.iter 0 (Array.length t - 1) (fun i -> print_graph2_rec file false "" "" i t.(i));
    fprintf file "label=\"%s\"\n  }\n" query);
  Sys.chdir path;
  ignore (Sys.command ("dot -Tpng " ^ name ^ ".gv -o " ^ name ^ ".png"));
  ignore (Sys.command ("rm " ^ name ^ ".gv"));
  String.iter (function '/' -> Sys.chdir ".." | _ -> ()) path*)

let rec print_graph2_rec file edge_rev edge_label edge_style edge_head upper = function
    Node t ->
          let id = !id_counter in
          incr id_counter;
          fprintf file "  %d [label=\"%s\"]\n" id (string_of_node t);
          print_edge2 file edge_rev edge_label edge_style edge_head "" upper id;
          let _ = print_graph2_rec file false "" "" "" id t.args in
          id
  | Concept({contents=Dot} as t) ->
          let id = !id_counter in
          incr id_counter;
          fprintf file "  %d [shape=box,label=<%s%s%s%s>]\n" id
            (if t.label="" then "" else "?" ^ t.label ^ " ")
            (if t.def_label="" then "" else "*" ^ t.def_label ^ " ")
            (escape_string t.cat ^ " ")
            (escape_string t.sense);
          print_edge2 file edge_rev edge_label edge_style edge_head "" upper id;
          let _ = print_graph2_rec file false "" "" "" id t.relations in
          id
  | Concept t ->
          let id = !id_counter in
          incr id_counter;
          fprintf file "  subgraph cluster%d {\nlabel=\"%s%s%s%s\"\n" id
            (if t.label="" then "" else "?" ^ t.label ^ " ")
            (if t.def_label="" then "" else "*" ^ t.def_label ^ " ")
            (escape_string t.cat ^ " ")
            (escape_string t.sense);
          let iid = print_graph2_rec file false "" "" "" 0 t.contents in
          fprintf file "  }\n";
          print_edge2 file edge_rev edge_label edge_style edge_head ("cluster" ^ string_of_int id) upper iid;
          let _ = print_graph2_rec file false "" "" ("cluster" ^ string_of_int id) iid t.relations in
          iid
(*  | Concept t ->
          let id = !id_counter in
          incr id_counter;
          fprintf file "  %d [shape=box,label=<%s%s%s%s%s %s>]\n" id
            (string_of_quant t.c_quant)
            (if t.c_label="" then "" else "?" ^ t.c_label ^ " ")
            (if t.c_def_label="" then "" else "*" ^ t.c_def_label ^ " ")
            (if t.c_cat=Dot then "" else escape_string (SemStringOf.linear_term 0 t.c_cat ^ " "))
            (escape_string (SemStringOf.linear_term 0 t.c_sense))
            (if t.c_name=Dot then "" else "„" ^ SemStringOf.linear_term 0 t.c_name ^ "”"); (* FIXME *)
          print_edge2 file edge_rev edge_label edge_style edge_head "" upper id;
          let _ = print_graph2_rec file false "" "" "" id t.c_relations in
          id
  | Context t ->
          let id = !id_counter in
          incr id_counter;
          fprintf file "  subgraph cluster%d {\nlabel=\"%s%s%s%s\"\n" id
            (if t.cx_label="" then "" else "?" ^ t.cx_label ^ " ")
            (if t.cx_def_label="" then "" else "*" ^ t.cx_def_label ^ " ")
            (if t.cx_cat=Dot then "" else escape_string (SemStringOf.linear_term 0 t.cx_cat ^ " "))
            (if t.cx_sense = Dot then "" else escape_string (SemStringOf.linear_term 0 t.cx_sense));
          let iid = print_graph2_rec file false "" "" "" 0 t.cx_contents in
          fprintf file "  }\n";
          print_edge2 file edge_rev edge_label edge_style edge_head ("cluster" ^ string_of_int id) upper iid;
          let _ = print_graph2_rec file false "" "" ("cluster" ^ string_of_int id) iid t.cx_relations in
          iid*)
  | Relation(role,role_attr,t) ->
          let id = !id_counter in
          incr id_counter;
          fprintf file "  %d [shape=circle,label=\"%s\\n%s\"]\n" id role role_attr;
          print_edge2 file false edge_label edge_style edge_head "" upper id;
          let _ = print_graph2_rec file false "" "" "" id t in
          id
  | RevRelation(role,role_attr,t) ->
          let id = !id_counter in
          incr id_counter;
          fprintf file "  %d [shape=circle,label=\"%s\\n%s\"]\n" id role role_attr;
          print_edge2 file true edge_label edge_style edge_head "" upper id;
          let _ = print_graph2_rec file true "" "" "" id t in
          id
  | SingleRelation(role) ->
          let id = !id_counter in
          incr id_counter;
          fprintf file "  %d [shape=circle,label=\"%s\"]\n" id (SemStringOf.linear_term 0 role);
          let _ = print_edge2 file false edge_label edge_style edge_head "" upper id in
          id
  (* | TripleRelation(role,role_attr,s,t) ->
          let id = !id_counter in
          incr id_counter;
          fprintf file "  %d [shape=circle,label=\"%s\\n%s\"]\n" id role role_attr;
          print_edge2 file false edge_label edge_style edge_head "" upper id;
          print_graph2_rec file false "" "dotted" "" id s;
          print_graph2_rec file false "" "" "" id t *)
  | AddRelation(t,role,role_attr,s) ->
          let id = !id_counter in
          incr id_counter;
          fprintf file "  %d [shape=circle,label=\"AddRelation\\n%s\\n%s\"]\n" id role role_attr;
          print_edge2 file edge_rev edge_label edge_style edge_head "" upper id;
          let _ = print_graph2_rec file false "" "" "" id t in
          let _ = print_graph2_rec file false "" "" "" id s in
          id
  | AddParentRelation(t,s) ->
          let id = !id_counter in
          incr id_counter;
          fprintf file "  %d [shape=circle,label=\"AddParentRelation\"]\n" id;
          print_edge2 file edge_rev edge_label edge_style edge_head "" upper id;
          let _ = print_graph2_rec file false "" "" "" id t in
          let _ = print_graph2_rec file false "" "" "" id s in
          id
  | AddSingleRelation(role,t) ->
          let id = !id_counter in
          incr id_counter;
          fprintf file "  %d [shape=circle,label=\"AddSingleRelation\\n%s\"]\n" id (SemStringOf.linear_term 0 role);
          print_edge2 file edge_rev edge_label edge_style edge_head "" upper id;
          let _ = print_graph2_rec file false "" "" "" id t in
          id
  | RemoveRelation(role,role_attr,t) ->
          let id = !id_counter in
          incr id_counter;
          fprintf file "  %d [shape=circle,label=\"RemoveRelation\\n%s\\n%s\"]\n" id role role_attr;
          print_edge2 file edge_rev edge_label edge_style edge_head "" upper id;
          let _ = print_graph2_rec file false "" "" "" id t in
          id
  | SetContextName(s,t) ->
          let id = !id_counter in
          incr id_counter;
          fprintf file "  %d [shape=circle,label=\"SetContextName\\n%s\"]\n" id s;
          print_edge2 file edge_rev edge_label edge_style edge_head "" upper id;
          let _ = print_graph2_rec file false "" "" "" id t in
          id
  | CreateContext(s,t) ->
          let id = !id_counter in
          incr id_counter;
          fprintf file "  %d [shape=circle,label=\"CreateContext\\n%s\"]\n" id s.sense;
          print_edge2 file edge_rev edge_label edge_style edge_head "" upper id;
          let _ = print_graph2_rec file false "" "" "" id t in
          id
  (* | MakeTripleRelation(role,role_attr,t) ->
          let id = !id_counter in
          incr id_counter;
          fprintf file "  %d [shape=circle,label=\"MakeTripleRelation\\n%s\\n%s\"]\n" id role role_attr;
          print_edge2 file false edge_label edge_style edge_head "" upper id;
          print_graph2_rec file false "" "" "" id t *)
  | ManageCoordination(n,t) ->
          let id = !id_counter in
          incr id_counter;
          fprintf file "  %d [shape=circle,label=\"ManageCoordination\"]\n" id;
          print_edge2 file false edge_label edge_style edge_head "" upper id;
          let _ = print_graph2_rec file false "" "" "" id t in
          id
  | Tuple l -> Xlist.fold l 0 (fun _ t -> print_graph2_rec file edge_rev edge_label edge_style edge_head upper t)
  | Variant(e,l) ->
          let id = !id_counter in
          incr id_counter;
          fprintf file "  %d [shape=diamond,label=\"%s\"]\n" id e;
          print_edge2 file edge_rev edge_label edge_style edge_head "" upper id;
          Xlist.iter l (fun (i,t) -> ignore(print_graph2_rec file edge_rev  i "" "" id t));
          id
  | Val s ->
          let id = !id_counter in
          incr id_counter;
          fprintf file "  %d [shape=box,label=\"%s\"]\n" id s;
          let _ = print_edge2 file edge_rev edge_label edge_style edge_head "" upper id in
          id
  | Dot -> 0
  | t -> failwith ("print_graph_rec: " ^ SemStringOf.linear_term 0 t)

let print_semantic_graph2 path name query t =
  let dir = Sys.getcwd () in
(*   print_endline  *)
  id_counter := 1;
  File.file_out (path ^ name ^ ".gv") (fun file ->
    fprintf file "digraph G {\n  compound=true\n  node [shape=record]\n";
    ignore (print_graph2_rec file false "" "" "" 0 t);
    fprintf file "label=\"%s\"\n  }\n" query);
  Sys.chdir path;
  ignore (Sys.command ("dot -Tpng " ^ name ^ ".gv -o " ^ name ^ ".png"));
  (* ignore (Sys.command ("rm " ^ name ^ ".gv")); *)
  Sys.chdir dir

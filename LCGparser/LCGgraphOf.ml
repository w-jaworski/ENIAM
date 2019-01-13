(*
 *  LCGparser, a parser for Logical Categorial Grammar formalism
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
    "ORTH",Val t.orth;"LEMMA",Val t.lemma;"POS",Val t.pos;"ID",Val (string_of_int t.id);"WEIGHT",Val (string_of_float t.weight);
    "SYMBOL",t.symbol;"ARG_SYMBOL",t.arg_symbol;"ARG_DIR",Val t.arg_dir] @ t.attrs in
  "{ " ^ String.concat " | " (Xlist.map l (fun (e,t) -> "{ " ^ e ^ " | " ^ escape_string (LCGstringOf.linear_term 0 t) ^ " }")) ^ " }"


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
  | SetAttr(a,s,t) ->
    fprintf file "  %s [shape=box,label=\"SetAttr(%s,%s)\"]\n" id a (LCGstringOf.linear_term 0 s);
    print_edge file edge upper id;
    print_dependency_tree_rec2 file "" id t
  | Dot -> ()
  (*          fprintf file "  %s [shape=box,label=\"Dot\"]\n" id;
              print_edge file edge upper id*)
  | Ref i -> print_edge file edge upper ("x" ^ string_of_int i)
  | t -> failwith ("print_dependency_tree_rec: " ^ LCGstringOf.linear_term 0 t)

and print_dependency_tree_rec2 file edge upper = function
    Tuple l -> Xlist.iter (List.rev l) (print_dependency_tree_rec2 file edge upper)
  | t -> print_dependency_tree_rec file edge upper (get_single_rel_id ()) t

let print_dependency_tree path name dependency_tree =
  let dir = Sys.getcwd () in
  single_rel_id_count := 0;
  File.file_out (path ^ name ^ ".gv") (fun file ->
      fprintf file "digraph G {\n  node [shape=record]\n";
      Int.iter 0 (Array.length dependency_tree - 1) (fun i -> print_dependency_tree_rec file "" "" ("x" ^ string_of_int i) dependency_tree.(i));
      fprintf file "}\n");
  Sys.chdir path;
  ignore (Sys.command ("dot -Tpng " ^ name ^ ".gv -o " ^ name ^ ".png"));
  ignore (Sys.command ("rm " ^ name ^ ".gv"));
  Sys.chdir dir

let rec print_simplified_dependency_tree_rec2 file edge upper = function
    Tuple l -> Xlist.iter (List.rev l) (print_simplified_dependency_tree_rec2 file edge upper)
  | Variant(e,l) ->
    fprintf file "  %s [shape=diamond]\n" e;
    print_edge file edge upper e;
    Xlist.iter l (fun (i,t) -> print_simplified_dependency_tree_rec2 file i e t)
  | Dot -> ()
  | Ref i -> print_edge file edge upper ("x" ^ string_of_int i)
  | SetAttr(a,s,t) -> ()
    (* fprintf file "  %s [shape=box,label=\"SetAttr(%s,%s)\"]\n" id a (LCGstringOf.linear_term 0 s);
    print_edge file edge upper id; *)
    (* print_simplified_dependency_tree_rec2 file "" id t *)
  | Node t -> ()
  | t -> failwith ("print_simplified_dependency_tree_rec2: " ^ LCGstringOf.linear_term 0 t)

let rec print_simplified_dependency_tree_rec file edge upper id = function
    Node t ->
    (* fprintf file "  %s [label=\"%s\\n%s:%s\\n%s\\n%f\"]\n" id (escape_string t.orth) (escape_string t.lemma) t.pos (escape_string (LCGstringOf.linear_term 0 t.symbol)) t.weight; *)
    fprintf file "  %s [label=\"%s\\n%s:%s\\n%s\"]\n" id (escape_string t.orth) (escape_string t.lemma) t.pos (escape_string (LCGstringOf.linear_term 0 t.symbol));
    print_edge file edge upper id;
    print_simplified_dependency_tree_rec2 file "" id t.args
  | Variant(e,l) ->
    fprintf file "  %s [shape=diamond,label=\"%s\"]\n" id e;
    print_edge file edge upper id;
    Xlist.iter l (fun (i,t) -> print_simplified_dependency_tree_rec file i id (id ^ "y" ^ i) t)
  (* | SetAttr(a,s,t) ->
    fprintf file "  %s [shape=box,label=\"SetAttr(%s,%s)\"]\n" id a (LCGstringOf.linear_term 0 s);
    print_edge file edge upper id; *)
    (* print_simplified_dependency_tree_rec2 file "" id t *)
  | Dot -> ()
  | t -> failwith ("print_simplified_dependency_tree_rec: " ^ LCGstringOf.linear_term 0 t)

let print_simplified_dependency_tree path name dependency_tree =
  let dir = Sys.getcwd () in
  File.file_out (path ^ name ^ ".gv") (fun file ->
      fprintf file "digraph G {\n  node [shape=box]\n";
      Int.iter 0 (Array.length dependency_tree - 1) (fun i -> print_simplified_dependency_tree_rec file "" "" ("x" ^ string_of_int i) dependency_tree.(i));
      fprintf file "}\n");
  Sys.chdir path;
  ignore (Sys.command ("dot -Tpng " ^ name ^ ".gv -o " ^ name ^ ".png"));
  ignore (Sys.command ("rm " ^ name ^ ".gv"));
  Sys.chdir dir

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
open Xstd
open Printf

let type_term s = "\\text{" ^ s ^ "}"

let rec first_digit_index v n i =
  if i = n then n else
  if String.get v i >= '0' && String.get v i <= '9' then i
  else first_digit_index v n (i+1)

let variable v =
  let n = String.length v in
  let i = first_digit_index v n 0 in
  if n = i then v else
    String.sub v 0 i ^ "_{" ^ String.sub v i (n-i) ^ "}"

let pred_name = function
    "name_token" -> "name\\_token"
  | s -> s

let rec linear_term c = function
    Var v -> variable v
  | Tuple l ->
    let l = Xlist.map l (linear_term 2) in
    let n = Xlist.fold l 0 (fun n s -> String.length s + n) in
    let s =
      if n < 100 then String.concat "\\otimes " l else
        let s = String.concat "\\otimes\\\\ {}" l in
        "\\begin{array}{l}" ^ s ^ "\\end{array}" in
    if c > 1 then "(" ^ s ^ ")" else s
  (*   | LetIn(l,s,t) -> "{\\bf let } " ^ String.concat "\\otimes " (Xlist.map l variable) ^ " = " ^ (linear_term 0 s) ^ " \\text{ {\\bf in }} " ^ (linear_term 0 t) *)
  | Variant(e,l) -> "\\langle " ^ String.concat ","(*"$,\\\\$"*) (Xlist.map l (fun (i,t) -> e^i^": "^linear_term 0 t)) ^ "\\rangle"
  | VariantVar(v,t) -> "\\langle " ^ linear_term 0 t ^ "\\rangle_\\text{" ^ v ^ "}"
  | Proj(n,t) -> "\\pi_" ^ (string_of_int n) ^ (linear_term c t)
  | ProjVar(v,t) -> "\\pi_{" ^ variable v ^ "}" ^ (linear_term c t)
  | SubstVar v -> variable v
  | Subst(s,v,t) -> "{\\bf subst}(" ^ (linear_term 0 s) ^ "," ^ variable v ^ "," ^ (linear_term 0 t) ^ ")"
  | Inj(n,t) -> "{\\bf inj}_{" ^ (string_of_int n) ^ "}" ^ (linear_term c t)
  | Case(t,l) -> "{\\bf case}\\; " ^ (linear_term 0 t) ^ " \\;{\\bf of}\\; " ^
                 (String.concat " | " (Xlist.map l (fun (v,t) -> variable v ^ " \\to " ^ (linear_term 0 t))))
  | Lambda(v,t) -> "\\lambda " ^ variable v ^ "." ^ (linear_term c t)
  | LambdaSet(l,t) -> "\\lambda " ^ (String.concat "," (Xlist.map l variable)) ^ "." ^ (linear_term c t)
  | LambdaRot(n,t) -> "{\\bf rot}_{" ^ (string_of_int n) ^ "}" ^ (linear_term c t)
  | App(s,t) -> "\\big((" ^ (linear_term 0 s) ^ ")(" ^ (linear_term 0 t) ^ ")\\big)"
  | Dot -> "\\bullet"
  | Val s -> "\\text{" ^ Xlatex.escape_string s ^ "}"
  | SetAttr(e,s,t) -> "{\\bf setattr}(\\text{{\\sc " ^ Xlatex.escape_string e ^ "}}," ^ linear_term 0 s ^ "," ^ linear_term 0 t ^ ")"
  | Fix(s,t) -> "{\\bf fix}(" ^ linear_term 0 s ^ "," ^ linear_term 0 t ^ ")"
  | Empty t -> "{\\bf empty}(" ^ linear_term 0 t ^ ")"
  | Apply t -> "{\\bf apply}(" ^ linear_term 0 t ^ ")"
  | Insert(s,t) -> "{\\bf insert}(" ^ linear_term 0 s ^ "," ^ linear_term 0 t ^ ")"
  | Node t ->
    "{\\left[\\begin{array}{ll}" ^
    (String.concat "\\\\ " (Xlist.map (["ORTH",Val t.orth;"LEMMA",Val t.lemma;"POS",Val t.pos;"ID",Val (string_of_int t.id);
                                        "WEIGHT",Val (string_of_float t.weight);"SYMBOL",t.symbol;
                                        "ARG_SYMBOL",t.arg_symbol;"ARG_DIR",Val t.arg_dir;"ARGS",t.args] @ t.attrs) (fun (e,t) ->
         "\\text{" ^ (Xlatex.escape_string e) ^ "} & " ^ (linear_term 0 t)))) ^ "\\end{array}\\right]}"
  (* | Morf m -> "\\text{" ^ Xlatex.escape_string (WalStringOf.morf m) ^ "}"
  | Gf s -> "\\text{" ^ Xlatex.escape_string (WalStringOf.gf s) ^ "}" *)
  | Coord(l,t,a) -> "[" ^ String.concat "; " (Xlist.map l (linear_term 0))  ^ "]_{" ^ linear_term 0 t ^ "," ^ linear_term 0 a ^ "}"
  | AddCoord(s,t) -> "{\\bf add}(" ^ linear_term 0 s ^ "," ^ linear_term 0 t ^ ")"
  | MapCoord(s,t) -> "{\\bf map}(" ^ linear_term 0 s ^ "," ^ linear_term 0 t ^ ")"
  | ConcatCoord(s,t) -> "{\\bf concat}(" ^ linear_term 0 s ^ "," ^ linear_term 0 t ^ ")"
  | Ref i -> "{\\bf ref}\\; " ^ string_of_int i
  | Cut t -> "{\\bf cut}(" ^ linear_term 0 t ^ ")"
  (* | Choice(e,i,t) -> "{\\bf choice}(" ^ e ^ String.concat "" i ^ "," ^ linear_term 0 t ^ ")" *)
  (* | Choice choices -> "{\\bf choice}(" ^ String.concat ";" (StringMap.fold choices [] (fun l ei t -> (sprintf "%s: %s" ei (linear_term 0 t)) :: l)) ^ ")"
  | Concept c ->
    "{\\left[\\begin{array}{ll}" ^
    (String.concat "\\\\ " (Xlist.map ([
         "SENSE",c.c_sense;"NAME",c.c_name;
         "VARIABLE",Val (fst c.c_variable ^ "_" ^ snd c.c_variable);"POS",Val (string_of_int c.c_pos);
         "QUANT",c.c_quant;"LOCAL-QUANT",if c.c_local_quant then Val "+" else Val "-";"RELATIONS",c.c_relations]) (fun (e,t) ->
        "\\text{" ^ (Xlatex.escape_string e) ^ "} & " ^ (linear_term 0 t)))) ^ "\\end{array}\\right]}"
  | Context c ->
    "{\\left[\\begin{array}{ll}" ^
    (String.concat "\\\\ " (Xlist.map ([
         "SENSE",c.cx_sense;
         "VARIABLE",Val (fst c.cx_variable ^ "_" ^ snd c.cx_variable);"POS",Val (string_of_int c.cx_pos);
         "RELATIONS",c.cx_relations;"CONTENTS",c.cx_contents]) (fun (e,t) ->
        "\\text{" ^ (Xlatex.escape_string e) ^ "} & " ^ (linear_term 0 t)))) ^ "\\end{array}\\right]}"
  | Relation(r,a,c) -> "{\\bf relation}(" ^ linear_term 0 r ^ "," ^ linear_term 0 a ^ "," ^ linear_term 0 c ^ ")"
  | RevRelation(r,a,c) -> "{\\bf revrelation}(" ^ linear_term 0 r ^ "," ^ linear_term 0 a ^ "," ^ linear_term 0 c ^ ")"
  | SingleRelation r -> "{\\bf singlerelation}(" ^ linear_term 0 r ^ ")"
  | AddRelation(t,r,a,s) -> "{\\bf addrelation}(" ^ linear_term 0 t ^ "," ^ r ^ "," ^ a ^ "," ^ linear_term 0 s ^ ")"
  | RemoveRelation r -> "{\\bf removerelation}(" ^ linear_term 0 r ^ ")"
  | SetContextName(s,t) -> "{\\bf setcontextname}(" ^ s ^ "," ^ linear_term 0 t ^ ")" *)

let rec linear_term_simple c = function
    Var v -> variable v
  | Tuple l ->
    let s = String.concat "\\otimes " (Xlist.map l (linear_term_simple 2)) in
    if c > 1 then "(" ^ s ^ ")" else s
  (*   | LetIn(l,s,t) -> "\\;$\\\\\n${\\bf let } " ^ String.concat "\\otimes " (Xlist.map l variable) ^ " = " ^ (linear_term_simple 0 s) ^ " \\text{ {\\bf in }} " ^ (linear_term_simple 0 t) *)
  (*   | Triple(t1,t2,t3) -> "\\{" ^ linear_term_simple 0 t1 ^ "," ^ linear_term_simple 0 t2 ^ "," ^ linear_term_simple 0 t3 ^ "\\}" *)
  | Variant(e,l) -> "\\langle " ^ String.concat "," (Xlist.map l (fun (i,t) -> e^i^": "^linear_term_simple 0 t)) ^ "\\rangle"
  | VariantVar(v,t) -> "\\langle " ^ linear_term_simple 0 t ^ "\\rangle_\\text{" ^ v ^ "}"
  | Proj(n,t) -> "\\pi_" ^ (string_of_int n) ^ (linear_term_simple c t)
  | ProjVar(v,t) -> "\\pi_{" ^ variable v ^ "}" ^ (linear_term_simple c t)
  | SubstVar v -> variable v
  | Subst(s,v,t) -> "{\\bf subst}(" ^ (linear_term_simple 0 s) ^ "," ^ variable v ^ "," ^ (linear_term_simple 0 t) ^ ")"
  | Inj(n,t) -> "{\\bf inj}_" ^ (string_of_int n) ^ (linear_term_simple c t)
  | Case(t,l) -> "{\\bf case}\\; " ^ (linear_term_simple 0 t) ^ " \\;{\\bf of}\\; " ^
                 (String.concat " | " (Xlist.map l (fun (v,t) -> variable v ^ " \\to " ^ (linear_term_simple 0 t))))
  | Lambda(v,t) -> "\\lambda " ^ variable v ^ "." ^ (linear_term_simple c t)
  | LambdaSet(l,t) -> "\\lambda " ^ (String.concat "," (Xlist.map l variable)) ^ "." ^ (linear_term_simple c t)
  | LambdaRot(n,t) -> "{\\bf rot}_{" ^ (string_of_int n) ^ "}" ^ (linear_term_simple c t)
  | App(s,t) -> "\\big((" ^ (linear_term_simple 0 s) ^ ")(" ^ (linear_term_simple 0 t) ^ ")\\big)"
  | Dot -> "\\bullet"
  | Val s -> "\\text{" ^ Xlatex.escape_string s ^ "}"
  | SetAttr(e,s,t) -> "{\\bf setattr}(\\text{{\\sc " ^ Xlatex.escape_string e ^ "}}," ^ linear_term_simple 0 s ^ "," ^ linear_term_simple 0 t ^ ")"
  | Fix(s,t) -> "{\\bf fix}(" ^ linear_term_simple 0 s ^ "," ^ linear_term_simple 0 t ^ ")"
  | Empty t -> "{\\bf empty}(" ^ linear_term_simple 0 t ^ ")"
  | Apply t -> "{\\bf apply}(" ^ linear_term_simple 0 t ^ ")"
  | Insert(s,t) -> "{\\bf insert}(" ^ linear_term_simple 0 s ^ "," ^ linear_term_simple 0 t ^ ")"
  | Node _ -> "node"
  (* | Morf m -> "\\text{" ^ Xlatex.escape_string (WalStringOf.morf m) ^ "}"
  | Gf s -> "\\text{" ^ Xlatex.escape_string (WalStringOf.gf s) ^ "}" *)
  | Coord(l,t,a) -> "[" ^ String.concat "; " (Xlist.map l (linear_term 0))  ^ "]_{" ^ linear_term 0 t ^ "," ^ linear_term 0 a ^ "}"
  | AddCoord(s,t) -> "{\\bf add}(" ^ linear_term 0 s ^ "," ^ linear_term 0 t ^ ")"
  | MapCoord(s,t) -> "{\\bf map}(" ^ linear_term 0 s ^ "," ^ linear_term 0 t ^ ")"
  | ConcatCoord(s,t) -> "{\\bf concat}(" ^ linear_term 0 s ^ "," ^ linear_term 0 t ^ ")"
  | Ref i -> "{\\bf ref}\\; " ^ string_of_int i
  | Cut t -> "{\\bf cut}(" ^ linear_term_simple 0 t ^ ")"
  (* | Choice(e,i,t) -> "{\\bf choice}(" ^ e ^ String.concat "" i ^ "," ^ linear_term_simple 0 t ^ ")" *)
  (* | Choice choices -> "{\\bf choice}(" ^ String.concat ";" (StringMap.fold choices [] (fun l ei t -> (sprintf "%s: %s" ei (linear_term_simple 0 t)) :: l)) ^ ")"
  | Concept c ->
    "{\\left[\\begin{array}{ll}" ^
    (String.concat "\\\\ " (Xlist.map ([
         "SENSE",c.c_sense;"NAME",c.c_name;
         "VARIABLE",Val (fst c.c_variable ^ "_" ^ snd c.c_variable);"POS",Val (string_of_int c.c_pos);
         "QUANT",c.c_quant;"LOCAL-QUANT",if c.c_local_quant then Val "+" else Val "-";"RELATIONS",c.c_relations]) (fun (e,t) ->
        "\\text{" ^ (Xlatex.escape_string e) ^ "} & " ^ (linear_term_simple 0 t)))) ^ "\\end{array}\\right]}"
  | Context c ->
    "{\\left[\\begin{array}{ll}" ^
    (String.concat "\\\\ " (Xlist.map ([
         "SENSE",c.cx_sense;
         "VARIABLE",Val (fst c.cx_variable ^ "_" ^ snd c.cx_variable);"POS",Val (string_of_int c.cx_pos);
         "RELATIONS",c.cx_relations;"CONTENTS",c.cx_contents]) (fun (e,t) ->
        "\\text{" ^ (Xlatex.escape_string e) ^ "} & " ^ (linear_term_simple 0 t)))) ^ "\\end{array}\\right]}"
  | Relation(r,a,c) -> "{\\bf relation}(" ^ linear_term_simple 0 r ^ "," ^ linear_term_simple 0 a ^ "," ^ linear_term_simple 0 c ^ ")"
  | RevRelation(r,a,c) -> "{\\bf revrelation}(" ^ linear_term_simple 0 r ^ "," ^ linear_term_simple 0 a ^ "," ^ linear_term_simple 0 c ^ ")"
  | SingleRelation r -> "{\\bf singlerelation}(" ^ linear_term_simple 0 r ^ ")"
  | AddRelation(t,r,a,s) -> "{\\bf addrelation}(" ^ linear_term_simple 0 t ^ "," ^ r ^ "," ^ a ^ "," ^ linear_term_simple 0 s ^ ")"
  | RemoveRelation r -> "{\\bf removerelation}(" ^ linear_term_simple 0 r ^ ")"
  | SetContextName(s,t) -> "{\\bf setcontextname}(" ^ s ^ "," ^ linear_term_simple 0 t ^ ")" *)

let direction = function
    Forward -> "/"
  | Backward  -> "\\backslash"
  | Both -> "|"

let atom = function
    "m1" -> "\\text{m}_1"
  | "m2" -> "\\text{m}_2"
  | "m3" -> "\\text{m}_3"
  | "n1" -> "\\text{n}_1"
  | "n2" -> "\\text{n}_2"
  | "f" -> "\\text{f}"
  | "p1" -> "\\text{p}_1"
  | "p2" -> "\\text{p}_2"
  | "p3" -> "\\text{p}_3"
  | s -> "\\text{" ^ Xlatex.escape_string s ^ "}"

let rec internal_grammar_symbol c = function
    Atom x -> atom x
  | AVar x -> " " ^ x
  | With l ->
    let s = String.concat "\\with" (Xlist.map l (internal_grammar_symbol 2)) in
    if c > 1 then "(" ^ s ^ ")" else s
  | Zero -> "0"
  | Top -> "\\top"

let rec grammar_symbol c = function
    Tensor l ->
    let s = String.concat "\\otimes" (Xlist.map l (internal_grammar_symbol 2)) in
    if c > 1 then "(" ^ s ^ ")" else s
  | Plus l ->
    let s = String.concat "\\oplus" (Xlist.map l (grammar_symbol 2)) in
    if c > 1 then "(" ^ s ^ ")" else s
  | StarWith l ->
    let s = String.concat "\\with" (Xlist.map l (grammar_symbol 2)) in
    if c > 1 then "(" ^ s ^ ")" else s
  | Imp(s,d,t) -> "(" ^ (grammar_symbol 2 s) ^ "\\\\ \\hspace{1cm}" ^ direction d ^ (grammar_symbol 2 t) ^ ")"
  | One -> "1"
  | ImpSet(s,l) ->
    let s = (grammar_symbol 1 s) ^ "\\{" ^ String.concat "\n," (Xlist.map l (fun (d,a) -> "\\\\ \\hspace{1cm}" ^ direction d ^ grammar_symbol 1 a)) ^ "\\}" in
    if c > 0 then "(" ^ s ^ ")" else s
  | WithVar(v,s,e,t) -> "\\bigwith_{" ^ e ^ ":" ^ v ^ ":=" ^ (internal_grammar_symbol 2 s) ^ "} \\\\ " ^ (grammar_symbol 2 t)
  (* | Star(s,t) -> grammar_symbol 2 s ^ "^\\star" ^ grammar_symbol 2 t *)
  | Star(s,t) -> "\\star" ^ grammar_symbol 2 t
  | Conj s -> "{\\bf conj}(" ^ grammar_symbol 2 s ^ ")"
  | Preconj -> "{\\bf preconj}"
  | Bracket(lf,rf,s) -> "\\langle " ^ (if lf then "\\langle " else "") ^ (grammar_symbol 0 s) ^ "\\rangle" ^ (if rf then "\\rangle " else "")
  | BracketSet d -> "{\\bf BracketSet}(" ^ direction d ^ ")"
  | Maybe s -> "?" ^ grammar_symbol 2 s

let chart page text_fragments g =
  let layers = LCGchart.fold g IntMap.empty (fun layers (symbol,node1,node2,cost,sem,layer) ->
      let nodes = try IntMap.find layers layer with Not_found -> IntMap.empty in
      let content = node2, cost, grammar_symbol 0 symbol, linear_term 0 sem in
      (*     let nodes = IntMap.add_inc nodes node1 (node2,[content]) (fun (n,l) -> if n <> node2 then failwith "to_latex" else n, content :: l) in *)
      let nodes = IntMap.add_inc nodes node1 [content] (fun l -> content :: l) in
      IntMap.add layers layer nodes) in
  let n = match page with "a4" -> "10" | "a1" -> "40" | _ -> "20" in
  "\\begin{longtable}{|l|l|l|l|p{" ^ n ^ "cm}|}\n\\hline\n" ^
  String.concat "" (List.rev (IntMap.fold layers [] (fun l layer nodes ->
      IntMap.fold nodes l (fun l node1 contents ->
          Xlist.fold contents l (fun l (node2,cost,symbol,sem) ->
              let s = try Xlatex.escape_string (IntMap.find text_fragments.(node1) node2) with Not_found -> failwith (Printf.sprintf "chart: text_fragment not found %d-%d" node1 node2) in
              (Printf.sprintf "%d & %d--%d %d & %s & $\\begin{array}{l}%s\\end{array}$ & $%s$\\\\\n\\hline\n" layer node1 node2 cost s symbol sem) :: l))))) ^
  "\\end{longtable}"

let chart2 page text_fragments g =
  let n = match page with "a4" -> "4" | "a1" -> "10" | _ -> "6" in
  "\\begin{longtable}{|l|p{" ^ n ^ "cm}|l|}\n\\hline\n" ^
  String.concat "" (List.rev (LCGchart.fold g [] (fun l (symbol,node1,node2,cost,sem,layer) ->
      let s = try Xlatex.escape_string (IntMap.find text_fragments.(node1) node2) with Not_found -> failwith (Printf.sprintf "chart: text_fragment not found %d-%d" node1 node2) in
      (Printf.sprintf "%d--%d %d & %s & $\\begin{array}{l}%s\\end{array}$\\\\\n\\hline\n" node1 node2 cost s (grammar_symbol 0 symbol)) :: l))) ^
  "\\end{longtable}"

let print_chart path name page text_fragments g =
  Xlatex.latex_file_out path name page false (fun file ->
      Printf.fprintf file "%s\n" (chart page text_fragments g));
  Xlatex.latex_compile_and_clean path name

let print_chart2 path name page text_fragments g =
  Xlatex.latex_file_out path name page false (fun file ->
      Printf.fprintf file "%s\n" (chart2 page text_fragments g));
  Xlatex.latex_compile_and_clean path name

let table_entries_of_symbol_term_list l =
  String.concat "" (Xlist.rev_map l (fun (symbol,sem) ->
      let symbol = grammar_symbol 0 symbol in
      let sem = linear_term 0 sem in
      Printf.sprintf "$\\begin{array}{l}%s\\end{array}$ & $%s$\\\\\n\\hline\n" symbol sem))

let parsed_dep_chart l =
  if l = [] then "empty" else
    "\\begin{longtable}{|l|p{20cm}|}\n\\hline\n" ^
    table_entries_of_symbol_term_list l ^
    "\\end{longtable}"

let not_parsed_dep_chart (id,left,l,right) =
  Printf.sprintf "conll\\_id=%d\\\\" id ^
  "\\begin{longtable}{|l|p{20cm}|}\n\\hline\n" ^
  String.concat "\\hline\n" (Xlist.map left table_entries_of_symbol_term_list) ^
  "\\hline\n\\hline\n\\hline\n" ^
  table_entries_of_symbol_term_list l ^
  "\\hline\n\\hline\n\\hline\n" ^
  String.concat "\\hline\n" (Xlist.map right table_entries_of_symbol_term_list) ^
  "\\end{longtable}"

let print_not_parsed_dep_chart path name page x =
  Xlatex.latex_file_out path name page false (fun file ->
      Printf.fprintf file "%s\n" (not_parsed_dep_chart x));
  Xlatex.latex_compile_and_clean path name

let rec dep_chart_rec (DepNode(id,left,l,right)) =
  (* printf "dep_chart_rec: %d\n" id; *)
  String.concat "" (Xlist.map left dep_chart_rec) ^
  String.concat "" (Xlist.rev_map l (fun (symbol,sem) ->
      let symbol = grammar_symbol 0 symbol in
      let sem = linear_term 0 sem in
      Printf.sprintf "%d & $\\begin{array}{l}%s\\end{array}$ & $%s$\\\\\n\\hline\n" id symbol sem)) ^
  String.concat "" (Xlist.map right dep_chart_rec)

let dep_chart graph =
  "\\begin{longtable}{|l|l|p{20cm}|}\n\\hline\n" ^
  dep_chart_rec graph ^
  "\\end{longtable}"

let print_dep_chart path name page g =
  Xlatex.latex_file_out path name page false (fun file ->
      Printf.fprintf file "%s\n" (dep_chart (*page*) g));
  Xlatex.latex_compile_and_clean path name

let print_dependency_tree path name page dependency_tree =
  Xlatex.latex_file_out path name page false (fun file ->
      Int.iter 0 (Array.length dependency_tree - 1) (fun i ->
          if dependency_tree.(i) <> Dot then Printf.fprintf file "{\\bf %d} $%s$\\\\\n" i (linear_term 0 dependency_tree.(i))));
  Xlatex.latex_compile_and_clean path name

let print_references path name page references =
  Xlatex.latex_file_out path name page false (fun file ->
      Int.iter 0 (ExtArray.size references - 1) (fun i ->
          if ExtArray.get references i <> Dot then Printf.fprintf file "{\\bf %d} $%s$\\\\\n" i (linear_term 0 (ExtArray.get references i))));
  Xlatex.latex_compile_and_clean path name

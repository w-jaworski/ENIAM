(*
 *  LCGlexicon is a library that provides LCG lexicon form Polish
 *  Copyright (C) 2016 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
 *  Copyright (C) 2016 Institute of Computer Science Polish Academy of Sciences
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
open LCGlexiconTypes
open CategoriesPL

let rec add_quantifiers t = function
    [] -> t
  | (cat,s) :: l ->  add_quantifiers (WithVar(string_of_selector cat,s,"",t)) l

let rec add_quantifiers_simple t = function
    [] -> t
  | (cat,s) :: l ->
    if LCGrenderer.count_avar (string_of_selector cat) t = 0 then add_quantifiers_simple t l
    else add_quantifiers_simple (WithVar(string_of_selector cat,s,"",t)) l

(* FIXME: kopia z LCGlatexOf *)
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
  | "n" -> "\\text{n}"
  | "f" -> "\\text{f}"
  | "p1" -> "\\text{p}_1"
  | "p2" -> "\\text{p}_2"
  | "p3" -> "\\text{p}_3"
  | s -> "\\text{" ^ Xlatex.escape_string s ^ "}"

let rec latex_of_internal_grammar_symbol c = function
    Atom x -> atom x
  | AVar x -> " " ^ x
  | With l ->
    let s = String.concat "\\with" (Xlist.map l (latex_of_internal_grammar_symbol 2)) in
    if c > 1 then "(" ^ s ^ ")" else s
  | Zero -> "0"
  | Top -> "\\top"

(* argument schema oznacza schemat walencyjny dodawany do reguły dla danego leksemu na podstawie
   schematu walenycjnego tego leksemu *)
(* "..." jako restrykcja kwantyfikatora oznacza, że dozwolone wartości zmiennej są wyznaczone
   przez interpretację morfosyntaktyczną formy, przykładowo dla formy "zielonemu" będą miały postać ..... *)
(* tensor wiąże silniej niż plus i imp *)

let quant_newline = function
    WithVar _ -> ""
  | _ -> "\\\\ \\hspace{1cm}"

let rec latex_of_grammar_symbol c = function
    Tensor l ->
    let s = String.concat "\\bullet" (Xlist.map l (latex_of_internal_grammar_symbol 2)) in
    (*if c > 1 then "(" ^ s ^ ")" else*) s
  | Plus l ->
    let s = String.concat "\\oplus" (Xlist.map l (latex_of_grammar_symbol 2)) in
    if c > 1 then "(" ^ s ^ ")" else s
  | Imp(s,d,t) -> "(" ^ (latex_of_grammar_symbol 2 s) ^ direction d ^ (latex_of_grammar_symbol 2 t) ^ ")"
  | One -> "1"
  | ImpSet(s,l) ->
    let s = (latex_of_grammar_symbol 1 s) ^ "\\{" ^ String.concat "\n," (Xlist.map l (fun (d,a) ->
        if a = Tensor[AVar "schema"] then "schema" else direction d ^ latex_of_grammar_symbol 1 a)) ^ "\\}" in
    if c > 0 then "(" ^ s ^ ")" else s
  | WithVar(v,Top,e,t) -> "\\bigwith_{" ^ v ^ ":=\\dots} " ^ (quant_newline t) ^ (latex_of_grammar_symbol 2 t)
  | WithVar(v,s,e,t) -> "\\bigwith_{" ^ v ^ ":=" ^ (latex_of_internal_grammar_symbol 2 s) ^ "} " ^ (quant_newline t) ^ (latex_of_grammar_symbol 2 t)
  | Star s -> latex_of_grammar_symbol 2 s ^ "^\\star"
  | Bracket(lf,rf,s) -> "\\langle " ^ (if lf then "\\langle " else "") ^ (latex_of_grammar_symbol 0 s) ^ "\\rangle" ^ (if rf then "\\rangle " else "")
  | BracketSet d -> "{\\bf BracketSet}(" ^ direction d ^ ")"
  | Maybe s -> "?" ^ latex_of_grammar_symbol 2 s

let print_latex_lexicon lexicon =
  Printf.printf "lexicon size: %d\n" (Xlist.size lexicon);
  Xlatex.latex_file_out "results/" "lexicon" "a0" false (fun file ->
      Xlist.iter lexicon (fun (selectors,(bracket,quant,syntax),semantics) ->
          let syntax = add_quantifiers_simple syntax (List.rev quant) in
          Printf.fprintf file "%s\\\\\n$\\begin{array}{l}%s\\end{array}$\\\\\\;\\\\\\;\\\\\n" (Xlatex.escape_string (string_of_selectors (List.rev selectors))) (latex_of_grammar_symbol 0 syntax)));
  Xlatex.latex_compile_and_clean "results/" "lexicon"


let _ =
  if Array.length Sys.argv < 2 then
    print_endline "missing argument\nUsage: print_lexicon <input-file>"
  else
    let lexicon = LCGlexiconParser.load_lexicon Sys.argv.(1) in
    let lexicon = List.rev (Xlist.rev_map lexicon LCGlexicon.assign_quantifiers) in
    print_latex_lexicon lexicon

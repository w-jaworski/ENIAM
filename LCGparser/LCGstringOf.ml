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

let direction = function
    Forward -> "/"
  | Backward  -> "\\\\"
  | Both -> "|"

let rec linear_term c = function
    Var v -> v
  | Tuple l ->
    let s = String.concat "⊗" (Xlist.map l (linear_term 2)) in
    if c > 1 then "(" ^ s ^ ")" else s
  (*   | LetIn(l,s,t) -> "let " ^ String.concat "⊗" l ^ " = " ^ (linear_term 0 s) ^ " in " ^ (linear_term 0 t) *)
  | Variant(e,l) -> "〈" ^ String.concat "," (Xlist.map l (fun (i,t) -> e^i^": "^linear_term 0 t)) ^ "〉"
  | VariantVar(v,t) -> "〈" ^ linear_term 0 t ^ "〉_" ^ v
  | Proj(n,t) -> "π_" ^ (string_of_int n) ^ (linear_term c t)
  | ProjVar(v,t) -> "π_[" ^ v ^ "]" ^ (linear_term c t)
  | SubstVar v -> v
  | Subst(s,v,t) -> "subst(" ^ (linear_term 0 s) ^ "," ^ v ^ "," ^ (linear_term 0 t) ^ ")"
  | Inj(n,t) -> "inj_" ^ (string_of_int n) ^ (linear_term c t)
  | Case(t,l) -> "case " ^ (linear_term 0 t) ^ " of " ^
                 (String.concat " | " (Xlist.map l (fun (v,t) -> v ^ " -> " ^ (linear_term 0 t))))
  | Lambda(v,t) -> "λ" ^ v ^ "." ^ (linear_term c t)
  | LambdaSet(l,t) -> "λ" ^ (String.concat "," l) ^ "." ^ (linear_term c t)
  | LambdaRot(n,t) -> "rot_" ^ (string_of_int n) ^ (linear_term c t)
  | App(s,t) -> "(" ^ (linear_term 0 s) ^ ")(" ^ (linear_term 0 t) ^ ")"
  | Dot -> "∙"
  | Val s -> s
  | SetAttr(e,s,t) -> "setattr(" ^ e ^ "," ^ linear_term 0 s ^ "," ^ linear_term 0 t ^ ")"
  | Fix(s,t) -> "fix(" ^ linear_term 0 s ^ "," ^ linear_term 0 t ^ ")"
  | Empty t -> "empty(" ^ linear_term 0 t ^ ")"
  | Apply t -> "apply(" ^ linear_term 0 t ^ ")"
  | Insert(s,t) -> "insert(" ^ linear_term 0 s ^ "," ^ linear_term 0 t ^ ")"
  (* | Choice(e,i,t) -> "choice(" ^ e ^ String.concat "" i ^ "," ^ linear_term 0 t ^ ")" *)
  | Node t ->
    "[" ^
    (String.concat "; " (Xlist.map (["ORTH",Val t.orth;"LEMMA",Val t.lemma;"POS",Val t.pos;"ID",Val (string_of_int t.id);
                                     "WEIGHT",Val (string_of_float t.weight);"SYMBOL",t.symbol;
                                     "ARG_SYMBOL",t.arg_symbol;"ARG_DIR",Val t.arg_dir;"ARGS",t.args] @ t.attrs) (fun (e,t) ->
         e ^ ": " ^ (linear_term 0 t)))) ^ "]"
  | Coord(l,t,a) -> "[" ^ String.concat "; " (Xlist.map l (linear_term 0))  ^ "]_" ^ linear_term 0 t  ^ "_" ^ linear_term 0 a
  | AddCoord(s,t) -> "add(" ^ linear_term 0 s ^ "," ^ linear_term 0 t ^ ")"
  | MapCoord(s,t) -> "map(" ^ linear_term 0 s ^ "," ^ linear_term 0 t ^ ")"
  | ConcatCoord(s,t) -> "concat(" ^ linear_term 0 s ^ "," ^ linear_term 0 t ^ ")"
  | Ref i -> "ref " ^ string_of_int i
  | Cut t -> "cut(" ^ linear_term 0 t ^ ")"

let rec internal_grammar_symbol c = function
    Atom x -> x
  | AVar x -> x
  | With l ->
    let s = String.concat "&" (Xlist.map l (internal_grammar_symbol 2)) in
    if c > 1 then "(" ^ s ^ ")" else s
  | Zero -> "0"
  | Top -> "⊤"

let rec grammar_symbol c = function
    Tensor l ->
    let s = String.concat "⊗" (Xlist.map l (internal_grammar_symbol 2)) in
    if c > 1 then "(" ^ s ^ ")" else s
  | Plus l ->
    let s = String.concat "⊕" (Xlist.map l (grammar_symbol 2)) in
    if c > 1 then "(" ^ s ^ ")" else s
  | Imp(s,d,t) -> (grammar_symbol 2 s) ^ direction d ^ (grammar_symbol 2 t)
  | One -> "1"
  | ImpSet(s,l) ->
    let s = (grammar_symbol 1 s) ^ "{" ^ String.concat "," (Xlist.map l (fun (d,a) -> direction d ^ grammar_symbol 1 a)) ^ "}" in
    if c > 0 then "(" ^ s ^ ")" else s
  | WithVar(v,s,e,t) -> "&_" ^ e ^ ": " ^ v ^ ":=" ^ (internal_grammar_symbol 2 s) ^ " " ^ (grammar_symbol 2 t)
  | Star(s,t) -> (grammar_symbol 2 s) ^ "^*" ^ grammar_symbol 2 t
  | StarWith l ->
    let s = String.concat "&" (Xlist.map l (grammar_symbol 2)) in
    if c > 1 then "(" ^ s ^ ")" else s
  | Conj s -> "Conj(" ^ grammar_symbol 2 s ^ ")"
  | Preconj -> "Preconj"
  | Bracket(lf,rf,s) -> "⟨" ^ (if lf then "⟨" else "") ^ (grammar_symbol 0 s) ^ "⟩" ^ (if rf then "⟩" else "")
  | BracketSet d -> "BracketSet(" ^ direction d ^ ")"
  | Maybe s -> "?" ^ grammar_symbol 2 s


let rec internal_grammar_symbol_prime = function
    Atom x -> "Atom(" ^ x ^ ")"
  | AVar x -> "AVar(" ^ x ^ ")"
  | With l -> "With[" ^ (String.concat ";" (Xlist.map l (internal_grammar_symbol_prime))) ^ "]"
  | Zero -> "Zero"
  | Top -> "Top"

let rec grammar_symbol_prime = function
    Tensor l -> "Tensor[" ^ (String.concat ";" (Xlist.map l (internal_grammar_symbol_prime))) ^ "]"
  | Plus l -> "Plus[" ^ (String.concat ";" (Xlist.map l (grammar_symbol_prime))) ^ "]"
  | StarWith l -> "StarWith[" ^ (String.concat ";" (Xlist.map l (grammar_symbol_prime))) ^ "]"
  | Imp(s,d,t) -> "Imp(" ^ (grammar_symbol_prime s) ^ "," ^ direction d ^ "," ^ (grammar_symbol_prime t) ^ ")"
  | One -> "One"
  | ImpSet(s,l) -> "ImpSet(" ^ (grammar_symbol_prime s) ^ ",[" ^ String.concat ";" (Xlist.map l (fun (d,a) -> direction d ^ grammar_symbol_prime a)) ^ "])"
  | WithVar(v,s,e,t) -> "WithVar(" ^ v ^ "," ^ (internal_grammar_symbol_prime s) ^ "," ^ e ^ "," ^ (grammar_symbol_prime t) ^ ")"
  | Star(s,t) -> "Star(" ^ grammar_symbol_prime s ^ "," ^ grammar_symbol 2 t ^ ")"
  | Conj s -> "Conj(" ^ grammar_symbol 2 s ^ ")"
  | Preconj -> "Preconj"
  | Bracket(lf,rf,s) -> "Bracket(" ^ string_of_bool lf ^ "," ^ string_of_bool rf ^ "," ^ (grammar_symbol_prime s) ^ ")"
  | BracketSet d -> "BracketSet(" ^ direction d ^ ")"
  | Maybe s -> "Maybe(" ^ grammar_symbol_prime s ^ ")"

let symbol_sem_list l =
  String.concat "\n  " (Xlist.map l (fun (symbol,sem) ->
      grammar_symbol 0 symbol ^ ": " ^ linear_term 0 sem))

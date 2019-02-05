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

open Xstd

type direction = Forward | Backward | Both

type linear_variable = string

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
  meaning: string;
  hipero: StringSet.t;
  meaning_weight: float;
    position: WalTypes.schema_field;*)
  attrs: (string * linear_term) list;
  args: linear_term}

(*and concept =
  {c_sense: linear_term; c_name: linear_term; (*c_visible_var: bool;*) c_quant: linear_term; c_local_quant: bool;
   (*c_modalities: (string * type_term) list;*)
   c_relations: linear_term; c_variable: (string * string); c_pos: int}

and context =
  {cx_sense: linear_term; cx_contents: linear_term; cx_relations: linear_term; cx_variable: (string * string); cx_pos: int}*)

and linear_term =
    Var of linear_variable
  | Tuple of linear_term list
  (*   | LetIn of linear_variable list * linear_term * linear_term *)
  | Variant of string * (string * linear_term) list (* etykieta * indeks * term *)
  | VariantVar of string * linear_term
  | Proj of int * linear_term
  | ProjVar of string * linear_term
  | SubstVar of string
  | Subst of linear_term * string * linear_term
  | Inj of int * linear_term
  | Case of linear_term * (linear_variable * linear_term) list
  | Lambda of linear_variable * linear_term
  | LambdaSet of linear_variable list * linear_term
  | LambdaRot of int * linear_term
  | App of linear_term * linear_term
  | Dot
  | SetAttr of string * linear_term * linear_term
  | Val of string
  | Fix of linear_term * linear_term
  | Empty of linear_term
  | Apply of linear_term
  | Insert of linear_term * linear_term
  | Node of node
(*  | Morf of WalTypes.morf
  | Gf of WalTypes.gf*)
  (* | Choice of (*linear_term StringMap.t*) string * string list * linear_term (* etykieta * indeksy * term *) *)
(*  | Concept of concept
  | Context of context
  | Relation of linear_term * linear_term * linear_term (* role * role_attr * concept *)
  | RevRelation of linear_term * linear_term * linear_term (* role * role_attr * concept *)
  | SingleRelation of linear_term
  | AddRelation of linear_term * string * string * linear_term (* nadrządnik * role * role_attr * podrzędnik *)
  | RemoveRelation of linear_term
    | SetContextName of string * linear_term*)
  | Coord of linear_term list * linear_term * linear_term
  | AddCoord of linear_term * linear_term
  | MapCoord of linear_term * linear_term
  | ConcatCoord of linear_term * linear_term
  | Ref of int
  | Cut of linear_term

type internal_grammar_symbol =
    Atom of string
  | AVar of string
  | With of internal_grammar_symbol list
  | Zero
  | Top

type grammar_symbol =
    Tensor of internal_grammar_symbol list
  | Plus of grammar_symbol list
  | Imp of grammar_symbol * direction * grammar_symbol
  | One
  | ImpSet of grammar_symbol * (direction * grammar_symbol) list
  | WithVar of string * internal_grammar_symbol * string * grammar_symbol  (* zmienna * wartości * etykieta * term *)
  | StarWith of grammar_symbol list
  | Star of grammar_symbol * grammar_symbol (* argument * funktor spójnika *)
  | Conj of grammar_symbol (* funktor spójnika *)
  | Preconj
  | Bracket of bool * bool * grammar_symbol
  | BracketSet of direction
  | Maybe of grammar_symbol

module OrderedSymbol = struct

  type t = grammar_symbol

  let compare = compare

end

module SymbolMap = Xmap.Make(OrderedSymbol)

module OrderedTerm = struct

  type t = linear_term

  let compare = compare

end

module TermMap = Xmap.Make(OrderedTerm)
module TermSet = Xset.Make(OrderedTerm)

module OrderedSymbolTerm = struct

  type t = grammar_symbol * linear_term

  let compare = compare

end

module SymbolTermSet = Xset.Make(OrderedSymbolTerm)

type chart = (SymbolTermSet.key list * int) array array array

exception Timeout of float
exception SemTooBig

type dep_tree =
    DepNode of int * dep_tree list * (grammar_symbol * linear_term) list * dep_tree list (* conll_id * left_nodes * ... * right_nodes *)

exception NotDepParsed of
    int *
    (grammar_symbol * linear_term) list list *
    (grammar_symbol * linear_term) list *
    (grammar_symbol * linear_term) list list

let no_nodes = ref 10000000

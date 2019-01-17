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

open Xstd

type categories = {lemma: string; pos: string; pos2: string;
                   cat: string; coerced: string list; roles: string list; snode: string list; phrase: string list;
                   numbers: string list; cases: string list; genders: string list; persons: string list;
                   grads: string list; praeps: string list; acms: string list;
                   aspects: string list; negations: string list; moods: string list; tenses: string list;
                   nsyn: string list; nsem: string list; modes: string list; (*psem: string list;*)
                   pt: string; col: string list;
                  }

type selector =
    Lemma | IncludeLemmata | (*NewLemma |*) Pos | Pos2 | Cat | Coerced | Role | SNode | Phrase |
    Number | Case | Gender | Person | Grad | Praep |
    Acm | Aspect | Negation | Mood | Tense | Nsyn | Nsem | Ctype | Mode | (*Psem |*) Pt | Col |
    Icat | Inumber | Igender | Iperson | Nperson | Ncat | Plemma |
    Unumber | Ucase | Ugender | Uperson | Amode |
    Irole | Prole | Nrole | Inode | Pnode | Nnode | Pcat

module OrderedSelector = struct
  type t = selector
  let compare = compare
end

module SelectorMap=Xmap.Make(OrderedSelector)
module SelectorSet=Xset.Make(OrderedSelector)

type rule =
    Bracket
  | Quant of (selector * LCGtypes.internal_grammar_symbol) list
  | Coord
  | PreCoord
  | NoCoord
  | Cost of int
  | Raised of selector list
  | Syntax of LCGtypes.grammar_symbol
  | Sem of string

type rule_sem =
    BasicSem of selector list
  | RaisedSem of selector list * selector list
  | TermSem of selector list * string
  | QuotSem of selector list
  | InclusionSem of selector list
  | ConjSem of selector list

type selector_relation = Eq | Neq (*| StrictEq*)

(* x="s" oznacza, że żeby reguła została użyta token musi mieć "s" jako jedną z wartości atrybutu x, reguła zostanie wykonana dla x z usuniętymi pozostałymi wartościami *)
(* x!="s" oznacza, że żeby reguła została użyta token musi mieć jako jedną z wartości atrybutu x symbol inny od "s", reguła zostanie wykonana dla x z usuniętą wartością "s" *)
(* x=="s" oznacza, że żeby reguła została użyta token musi mieć "s" jako jednyną z wartość atrybutu x *)

(* wzajemne zależności między kategoriami (np między case i person w subst) są rozstrzygane w Categories *)

(* Basic oznacza że kwantyfikacja i term są generowane zgodnie ze standardowymi regułami:
   - kwantyfikacja przebiega po wszystkich zdefiniowanych kategoriariach i wartościach wziętych z cats
   - typ jest zadany bezpośrednio
   - term tworzy wierzchołek w strukturze zależnościowej etykietowany wszystkimi zdefiniowanymi kategoriami

   Quant oznacza że typ i term są generowane zgodnie ze standardowymi regułami:
   - kwantyfikacja jest zadana bezpośrednio
   - typ jest zadany bezpośrednio
   - term tworzy wierzchołek w strukturze zależnościowej etykietowany wszystkimi zdefiniowanymi kategoriami

*)

let empty_cats = {lemma=""; pos=""; pos2=""; cat="C"; coerced=[]; roles=[]; snode=[]; phrase=[];
                  numbers=[]; cases=[]; genders=[]; persons=[];
                  grads=[]; praeps=[]; acms=[]; aspects=[]; negations=[]; moods=[]; tenses=[];
                  nsyn=[]; nsem=[]; modes=[]; (*psem=[];*) pt=""; col=[];
                 }

type entry = {
  selectors: (selector * selector_relation * string list) list;
  rule: rule list; syntax: LCGtypes.grammar_symbol;
  semantics: rule_sem; cats: categories; weight: float;
  bracket: bool; coord: rule;
  quant: (selector * LCGtypes.internal_grammar_symbol) list;
  cost: int}

let empty_entry = {selectors=[]; rule=[]; syntax=LCGtypes.One;
  semantics=BasicSem []; cats=empty_cats; weight=0.;
  bracket=false; coord=NoCoord; quant=[]; cost=0}

(* let default_category_flag = ref true *)

let resource_path =
  try Sys.getenv "ENIAM_RESOURCE_PATH"
  with Not_found ->
    if Sys.file_exists "/usr/share/eniam" then "/usr/share/eniam" else
    if Sys.file_exists "/usr/local/share/eniam" then "/usr/local/share/eniam" else
    if Sys.file_exists "resources" then "resources" else
    failwith "resource directory does not exists"

let data_path =
  try Sys.getenv "ENIAM_USER_DATA_PATH"
  with Not_found -> "data"

let rules_filename = resource_path ^ "/LCGlexicon/lexicon-pl.dic"
let user_lexicon_filename = data_path ^ "/lexicon.dic"
let user_cats_filename = data_path ^ "/senses.tab"
let user_coerced_filename = data_path ^ "/coercions.tab"

let subst_uncountable_lexemes_filename = resource_path ^ "/LCGlexicon/subst_uncountable.dat"
let subst_uncountable_lexemes_filename2 = resource_path ^ "/LCGlexicon/subst_uncountable_stare.dat"
let subst_container_lexemes_filename = resource_path ^ "/LCGlexicon/subst_container.dat"
let subst_numeral_lexemes_filename = resource_path ^ "/LCGlexicon/subst_numeral.dat"
let subst_time_lexemes_filename = resource_path ^ "/LCGlexicon/subst_time.dat"

let adv_modes_filename = resource_path ^ "/Walenty/adv_modes.tab"
let num_nsems_filename = resource_path ^ "/LCGlexicon/num.tab"

let rules = ref (StringMap.empty : (entry list StringMap.t * entry list) StringMap.t)
let dep_rules = ref (StringMap.empty : (entry list StringMap.t * entry list) StringMap.t)
(* let theories_paths = ref ([] : string list) *)

(*
 *  ENIAMvalence is a library that assigns tokens with lexicosemantic information.
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

type opinion = Pewny | Potoczny | Watpliwy | Archaiczny | Zly | Wulgarny | Nieokreslony
  | Metaforyczny | Dziedzinowy | Sporadyczny
type negation = Negation | Aff | NegationUndef (*| NegationNA*)
type pred = PredTrue | PredFalse | PredUndef (*| PredNA*)
type aspect = Aspect of string | AspectUndef (*| AspectNA*)
type case = Case of string | Str | Part | CaseAgr | CaseUndef (*| AllUAgr | CaseUAgr*) | GenAgr | NomAgr | VocAgr | AllAgr
type comp = Comp of string | Zeby | Gdy | CompUndef
type comp_type = Int | Rel | CompTypeUndef (*| CompTypeAgr*)
type number = Number of string | NumberUndef | NumberAgr
type gender = Gender of string | GenderUndef | GenderAgr | Genders of string list
type grad = Grad of string | GradUndef | GradAgr
(* type psem = Psem | Pnosem *)
(* type refl = (*ReflEmpty |*) ReflTrue | ReflFalse | ReflUndef *)
(* type acm = Acm of string | AcmUndef *)

(*type mood = (*Mood of*) string (*| MoodUndef*)
type tense = string
type aux = NoAux | PastAux | FutAux | ImpAux

  type nsem = Common of string | Time*)

type gf = SUBJ | OBJ | ARG | ADJUNCT | CORE | NOSEM

type pos =
    SUBST of number * case
  | PPRON12 of number * case
  | PPRON3 of number * case
  | SIEBIE of case
  | PREP of case
  | NUM of case * gender
  | ADJ of number * case * gender * grad
  | ADV of grad
  | GER of number * case * gender * aspect * negation
  | PACT of number * case * gender * aspect * negation
  | PPAS of number * case * gender * aspect * negation
  | INF of aspect * negation
  | QUB
  | COMPAR of case
  | COMP of comp_type
  | PERS of (*number * gender * aspect * person * *)negation
  | FIXED

type phrase =
    NP of case
  | NPA of case
  | PrepNP of (*psem **) string * case
  | PrepFixed of (*psem **) string
  | AdjP of case
  | AdjA
  | PrepAdjP of string * case
  (* | NumP of case
  | PrepNumP of string * case *)
  | ComprepNP of string
  | ComparP of (*psem **) string * case
  | CP of comp_type * comp
  | NCP of case * comp_type * comp
  | PrepNCP of (*psem **) string * case * comp_type * comp
  | InfP of aspect
  | PadvP
  | AdvP of string
  | XP
  | IP
  | ColonP
  | SymbolP
  | FixedP of string
  (* | Num of case * acm *)
  | Head
  | Or
  (* | Refl
  | Recip *)
  | Qub
  | AdMod of grad
  | Inclusion
  | Pro
  | ProNG
  | Null
  (* | GerP of case
  | PrepGerP of string * case
  | PpasP of case
  | PrepPpasP of string * case
  | PactP of case *)
  | SimpleLexArg of string * pos
  | LexArg of int * string * pos
  | E of phrase
  | MorfId of int
  | LCG of LCGtypes.grammar_symbol

type restr = Natr | Ratr | Ratrs | Ratr1 | Atr | Atr1 | NoRestr

type sel_prefs =
    SynsetId of int
  | Predef of string
  | SynsetName of string
  | RelationRole of string * string * string (* relacja * rola * atrybut roli *)

type necessary = Req | Opt | Pro | ProNG | Multi

type direction = Both_ | Forward_ | Backward_

type range = Local | Distant | Middle

type position = {psn_id: int; gf: gf; role: string; role_attr: string; node: string; range: range;
                 sel_prefs: sel_prefs list; cat_prefs: string list;
                 mode: string list; cr: string list; ce: string list; morfs: phrase list;
                 dir: direction; is_necessary: necessary}

let empty_position =
  {psn_id=(-1); gf=ARG; role=""; role_attr=""; mode=[]; node="concept"; range=Middle;
   sel_prefs=[]; cat_prefs=["X"]; cr=[]; ce=[]; dir=Both_; morfs=[]; is_necessary=Opt}

type sense = {mng_id: int;
                name: string;
                variant: string;
                plwnluid: int;
                gloss: string}

let empty_sense = {mng_id = (-1);
                     name = "";
                     variant = "";
                     plwnluid = (-1);
                     gloss = ""}

(* type frame_atrs =
    EmptyAtrs of sense list
  | DefaultAtrs of sense list * refl * opinion * negation * pred * aspect
  | ComprepAtrs of string
  | NounAtrs of sense list * string * nsem (** string list*)
  | AdjAtrs of sense list * case * string (** string * string list*)
  | PersAtrs of sense list * string * negation * mood * tense * aux * aspect
  | GerAtrs of sense list * string * negation * aspect
  | NonPersAtrs of sense list * string * string * string * negation * aspect *)

(* type schema = {sch_id: int; opinion: opinion; reflexiveMark: refl; aspect: aspect;
               negativity: negation; predicativity: pred; positions: position list; text_rep: string} *)

type lex_entry =
    (* Frame of frame_atrs * position list *)
  (* | LexFrame of string * pos * restr * position list
  | ComprepFrame of string * pos * restr * position list *)
  | SimpleLexEntry of string * string
  | LexEntry of int * string * string * restr * position list
  | ComprepNPEntry of string * restr * position list
(*  | FrameR of frame_atrs * (string * string * string list * string list * morf list) list
  | LexFrameR of string * pos * restr * (string * string * string list * string list * morf list) list
  | ComprepFrameR of string * pos * restr * (string * string * string list * string list * morf list) list *)


(* module OrderedEntry = struct
  type  t = entry2
  let compare = compare
end

module EntrySet = Xset.Make(OrderedEntry) *)

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

(*let phrases_filename = resource_path ^ "/Walenty/phrases.tab"
let entries_filename = resource_path ^ "/Walenty/entries.tab"
let schemata_filename = resource_path ^ "/Walenty/schemata.tab"
let connected_filename = resource_path ^ "/Walenty/connected.tab"
let senses_filename = resource_path ^ "/Walenty/meanings.tab"*)

let user_valence_filename = data_path ^ "/valence.dic"

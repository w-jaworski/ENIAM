(*
 *  ENIAMtokenizer, a tokenizer for Polish
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

(* Długość pojedynczego znaku w tekście *)
let factor = 100

type token =
    SmallLetter of string * string 		(* uppercase * lowercase *)
  | CapLetter of string * string	(* uppercase * lowercase *)
  | AllSmall of string * string * string			(* lowercase * firstcap * lowercase *)
  | AllCap of string * string * string	(* uppercase * firstcap * lowercase *)
  | FirstCap of string * string * string	(* uppercase * firstcap * lowercase *)
  | SomeCap of string * string * string			(* uppercase * orig * lowercase *)
  | Ideogram of string * string		(* value * mode *)
  | Interp of string			(* orth *)
  | Symbol of string			(* orth *)
  | Other of string			(* orth *)
  | Lemma of string * string * string list list list * string	(* lemma * pos * interp * cat *)
  | Tokens of string * int list (*cat * token id list *)

type letter_size = SL | CL | AS | FC | AC | SC

type attr =
    FC | CS | MaybeCS | HasAglSuffix | AglSuffix | MWE | LemmNotVal | TokNotFound | NotValProper | LemmLowercase | Roman | Capitalics
  | SentBeg | SentEnd | SentBegEnd
  | BrevLemma of string
  | Disamb of string * string * string list list

(* Tekst reprezentuję jako zbiór obiektów typu token_record zawierających
   informacje o poszczególnych tokenach *)
type token_env = {
  orth: string;		(* sekwencja znaków pierwotnego tekstu składająca się na token *)
  corr_orth: string; (* sekwencja znaków pierwotnego tekstu składająca się na token z poprawionymi błędami *)
  beg: int; 		(* pozycja początkowa tokenu względem początku akapitu *)
  len: int; 		(* długość tokenu *)
  next: int; 		(* pozycja początkowa następnego tokenu względem początku akapitu *)
  token: token; 	(* treść tokenu *)
  attrs: attr list;	(* dodatkowe atrybuty *)
  weight: float;
  lemma_frequency: float;
  morf_frequency: float;
  tagger_output: (string * float * bool * bool) list;
  args: token_env list;
  }

(* Tokeny umieszczone są w strukturze danych umożliwiającej efektywne wyszukiwanie ich sekwencji,
   struktura danych sama z siebie nie wnosi informacji *)
type tokens =
  | Token of token_env
  | Variant of tokens list
  | Seq of tokens list
  
type sel = V of string list | S of string | G

type pat =
    I of string      (* ideogram z określonym mode *)
  | C of string      (* kategoria semantyczna *)
  | Sym of string    (* symbol *)
  | O of string      (* forma ortograficzna o ustalonej wielkości liter *)
  | T of string      (* forma ortograficzna w dowolnej konfiguracji wielkości liter *)
  | N of string      (* znak interpuncyjny *)
  | Lem of string * string * sel list (* lemat, pos i interp *)
  | SmallLet         (* dowolna mała litera *)
  | CapLet           (* dowolna wielka litera *)
  | Letters          (* dowolna sekwencja znaków *)


let empty_token_env = {
  orth="";corr_orth="";beg=0;len=0;next=0; token=Symbol ""; attrs=[]; weight=0.; lemma_frequency=0.; morf_frequency=0.;
  tagger_output=[];args=[]}

(* let internet_mode = ref false *)

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

let mte_filename = resource_path ^ "/tokenizer/mte_20151215.tab"
let mte_filename2 = resource_path ^ "/tokenizer/mte.tab"

let top_level_domains_filename = resource_path ^ "/tokenizer/top-level-domains.tab"

module OrderedTokenEnv = struct

  type t = token_env

  let compare = compare

end

module TokenEnvSet = Xset.Make(OrderedTokenEnv)

module OrderedAttr = struct

  type t = attr

  let compare = compare

end

module AttrQMap = Xmap.MakeQ(OrderedAttr)

type ont = {number: string; gender: string; no_sgjp: bool; poss_ndm: bool; exact_case: bool; ont_cat: string}
  
module OrderedOnt = struct

  type t = ont

  let compare = compare

end

module OntSet = Xset.Make(OrderedOnt)

let known_lemmata = ref (StringMap.empty : OntSet.t StringMap.t StringMap.t)
let known_orths = ref StringSet.empty
let theories_paths = ref ([] : string list)

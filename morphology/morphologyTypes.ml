(*
 *  ENIAMmorphology, a morphological analyser and a guesser for Polish
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

type star = Productive | Star | Ndm | Dial | Acro | Aux | Aux2

type phon_rule = {pset: string; pfind: string; psuf: string; plang: string}

type rule = {star: star; pref: string; find: string; set: string; tags: (string * string) list;
  interp: string; id: string; freq: int; ntype_freq: StringQMap.t}

type phon_orth = {phon: string; mapping: phon_rule(*(string * string)*) list}
type form = {orth: string; phon_orth: phon_orth list; interp: string; freq: int; genre: string; validated: bool; candidates: (string * rule * phon_orth * phon_orth list) list}
type entry = {lemma: string; (*phon_lemma: string list;*) cat: string; forms: form list;
  proper_type: string; (*ndm: bool;*) stem: string; phon_stem: string list; aspect: string; ntype: string list}

let empty_form = {orth=""; phon_orth=[]; interp=""; freq=1; genre=""; validated=false; candidates=[]}
let empty_entry = {lemma=""; (*phon_lemma=[];*) cat=""; forms=[]; proper_type=""; (*ndm=false;*)
  stem=""; phon_stem=[]; aspect=""; ntype=[]}

let empty_rule = {star=Productive; pref=""; find=""; set=""; tags=[]; interp=""; id=""; freq=0; ntype_freq=StringQMap.empty}

let string_of_tags tags =
  String.concat " " (Xlist.map tags (fun (k,v) -> k ^ "=" ^ v))

let string_of_rule r =
  Printf.sprintf "%s\t%s\t%s\t%s" r.pref r.find r.set
    (string_of_tags r.tags)

type lu_entry = {lemma1: string; lemma2: string; rel_id: int; lu_stem: string;
                 lu_validated: bool; validated1: bool; validated2: bool}

(* type form = {orth: string; interp: string; freq: int; genre: string; validated: bool}
type entry = {lemma: string; cat: string; forms: form list; proper_type: string; ndm: bool; stem: string}

type star = Productive | Star | Ndm

type rule = {star: star; pref: string; find: string; set: string; tags: (string * string) list;
  interp: string; id: string; freq: int} *)

let resource_path =
  try Sys.getenv "ENIAM_RESOURCE_PATH"
  with Not_found ->
    if Sys.file_exists "/usr/share/eniam" then "/usr/share/eniam" else
    if Sys.file_exists "/usr/local/share/eniam" then "/usr/local/share/eniam" else
    if Sys.file_exists "resources" then "resources" else
    failwith "resource directory does not exists"

let alt_filename = resource_path ^ "/morphology/alt.tab"
let stem_filename = resource_path ^ "/morphology/stem.tab"
let rules_filename = resource_path ^ "/morphology/freq_rules.tab"
let wyglos_filename = resource_path ^ "/morphology/wyglos.tab"
let lemmata_filename = resource_path ^ "/morphology/lemmata.tab"

let alt_supplement_filename = resource_path ^ "/morphology/alt_supplement.tab"

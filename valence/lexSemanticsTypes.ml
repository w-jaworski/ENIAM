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

open SubsyntaxTypes
open Xstd

type frame = {
  selectors: LCGlexiconTypes.selector_constraint list;
  senses: ((*WalTypes.sense **) string * (string * int) list * float) list;
  cats: (string * string list) list;
  positions: WalTypes.position list;
  arole: string;
  arole_attr: string;
  arev: bool;
  agf: string;
  sem_args: string list;
  rev_hipero: bool;
  (*snode: string list;*)
  sopinion: WalTypes.opinion;
  fopinion: WalTypes.opinion;
  }

let empty_frame = {selectors=[]; senses=[]; cats=["X",["X"]]; positions=[]; arole=""; arole_attr=""; arev=false; agf=""; sem_args=[]; rev_hipero=false; (*snode=[];*)
  sopinion=WalTypes.Nieokreslony; fopinion=WalTypes.Nieokreslony}

type lex_sem = {
  schemata: (LCGlexiconTypes.selector_constraint list *
             (string * string list) list * (* sensy *)
             (*string list **) (* has_context *)
             (LCGtypes.direction * LCGtypes.grammar_symbol) list * (* local_schema *)
             (LCGtypes.direction * LCGtypes.grammar_symbol) list * (* schema *)
             (LCGtypes.direction * LCGtypes.grammar_symbol) list) list; (* distant_schema *)
  lex_entries: (LCGlexiconTypes.selector_constraint list *
                LCGtypes.grammar_symbol) list;
  frames: frame list;
  (* cats: (string * string list) list; *)
  }

let empty_lex_sem = {
  schemata=[]; lex_entries=[]; frames=[]}

let unknown_sense_weight = -1.

let coercions_filename = WalTypes.data_path ^ "/coercions.tab"

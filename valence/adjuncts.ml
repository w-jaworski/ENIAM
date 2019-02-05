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

open WalTypes
open Xstd

open LCGlexiconTypes

let nie_vebs = StringSet.of_list ["fin";"bedzie";"praet";"winien";"impt";
                                  "imps";"pred";"inf";"pcon";"pant"]

let imp_aux = StringSet.of_list ["niech";"niechaj";"niechże";"niechajże"]

let rec check_selector_lex_constraints lexemes pos = function
    [] -> true
  | (Negation,Eq,["neg"]) :: selectors ->
    if not (StringSet.mem lexemes "nie") && (StringSet.mem nie_vebs pos) then false
    else check_selector_lex_constraints lexemes pos selectors
  | (Mood,Eq,["conditional"]) :: selectors ->
    if not (StringSet.mem lexemes "by") && (pos = "praet" || pos = "winien") then false
    else check_selector_lex_constraints lexemes pos selectors
  | (Mood,Eq,["imperative"]) :: selectors ->
    if StringSet.is_empty (StringSet.intersection lexemes imp_aux) && pos = "fin" then false
    else check_selector_lex_constraints lexemes pos selectors
  | _  :: selectors -> check_selector_lex_constraints lexemes pos selectors


(*
 *  ENIAMgenerator: generator of inflected phrases for Polish
 *  Copyright (C) 2020 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
 *  Copyright (C) 2020 Institute of Computer Science Polish Academy of Sciences
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
open SubsyntaxTypes
open MorphologyTypes

let rec manage_spaces = function
    [] -> []
  | [" ","interp",[]] -> []
  | (" ","interp",[]) :: ("-","interp",[]) :: l -> manage_spaces (("-","interp",[]) :: l)
  | (" ","interp",[]) :: l -> (" ","interp",[]) :: (manage_spaces l)
  | ("-","interp",[]) :: (" ","interp",[]) :: l -> manage_spaces (("-","interp",[]) :: l)
  | ("-","interp",[]) :: l -> ("-","interp",[]) :: (manage_spaces l)
  | (_,"interp",_) :: _ -> failwith "manage_spaces"
  | t :: l -> t :: (manage_spaces l)

let generate_np_number_case number case phrase =
  let phrase = Xlist.map phrase (fun (lemma,pos,tags) ->
    let tags = Xlist.map tags (function
        V[tag] -> tag
      | S "c" -> case
      | S "n" -> number
      | _ -> failwith "generate_np_number_case") in
    lemma,pos,tags) in
  let phrase = manage_spaces (List.flatten (Xlist.map phrase (fun t -> [t;" ","interp",[]]))) in
  let l = Xlist.map phrase (fun (lemma,pos,tags) -> 
    let interp = String.concat ":" (pos :: tags) in
    Inflexion.disambiguate [] [Acro;Aux;Aux2;Ndm;Dial] (Inflexion.synthetize lemma interp)) in
  Xlist.multiply_list l

let generate_gerp_number_case number case phrase =
  let phrase = Xlist.map phrase (fun (lemma,pos,tags) ->
    let pos,tags = 
      if pos = "inf" then
        let aspect = match tags with [aspect] -> aspect | _ -> failwith "generate_gerp_number_case" in
        "ger", [S "n";S "c";V["n"];aspect;V["aff"]] 
      else pos,tags in
    let tags = Xlist.map tags (function
        V[tag] -> tag
      | S "c" -> case
      | S "n" -> number
      | _ -> failwith "generate_gerp_number_case") in
    lemma,pos,tags) in
  let phrase = manage_spaces (List.flatten (Xlist.map phrase (fun t -> [t;" ","interp",[]]))) in
  let l = Xlist.map phrase (fun (lemma,pos,tags) -> 
    let interp = String.concat ":" (pos :: tags) in
    Inflexion.disambiguate [] [Acro;Aux;Aux2;Ndm;Dial] (Inflexion.synthetize lemma interp)) in
  Xlist.multiply_list l

let generate_adjp_number_case_gender number case gender phrase =
  let phrase = Xlist.map phrase (fun (lemma,pos,tags) ->
    let tags = Xlist.map tags (function
        V[tag] -> tag
      | S "c" -> case
      | S "n" -> number
      | S "g" -> gender
      | _ -> failwith "generate_adjp_number_case_gender") in
    lemma,pos,tags) in
  let phrase = manage_spaces (List.flatten (Xlist.map phrase (fun t -> [t;" ","interp",[]]))) in
  let l = Xlist.map phrase (fun (lemma,pos,tags) -> 
    let interp = String.concat ":" (pos :: tags) in
    Inflexion.disambiguate [] [Acro;Aux;Aux2;Ndm;Dial] (Inflexion.synthetize lemma interp)) in
  Xlist.multiply_list l

let generate_ip new_pos number person phrase =
  let phrase = Xlist.map phrase (fun (lemma,pos,tags) ->
    let tags = Xlist.map tags (function
        V[tag] -> tag
      | _ -> failwith "generate_ip") in
    if pos = "inf" then
      let aspect = match tags with [a] -> a | _ -> failwith "generate_ip" in
      lemma,new_pos,[number;person;aspect]
    else lemma,pos,tags) in
  let phrase = manage_spaces (List.flatten (Xlist.map phrase (fun t -> [t;" ","interp",[]]))) in
  let l = Xlist.map phrase (fun (lemma,pos,tags) -> 
    let interp = String.concat ":" (pos :: tags) in
    Inflexion.disambiguate [] [Acro;Aux;Aux2;Ndm;Dial] (Inflexion.synthetize lemma interp)) in
  Xlist.multiply_list l

let cases = ["nom";"gen";"dat";"acc";"inst";"loc"]
let numbers = ["sg";"pl"]
let genders = ["m1";"m2";"m3";"n";"f"]
  
let generate_case_grouped_np phrases =
  let map = Xlist.fold phrases StringMap.empty (fun map phrase ->
    Xlist.fold cases map (fun map case ->
      Xlist.fold numbers map (fun map number ->
        Xlist.fold (generate_np_number_case number case phrase) map (fun map l ->
          let form = String.concat "" (Xlist.map l (fun i ->  i.Inflexion.lemma)) in
          StringMap.add_inc map form (StringSet.singleton case) (fun set -> StringSet.add set case))))) in
  let map = StringMap.fold map StringMap.empty (fun map form set ->
    let s = String.concat "." (Tagset.sort_tags (StringSet.to_list set)) in
    StringMap.add_inc map s [form] (fun l -> form :: l)) in
  List.rev (StringMap.fold map [] (fun l s forms -> (s,forms) :: l))
  
let generate_case_grouped_gerp phrases =
  let map = Xlist.fold phrases StringMap.empty (fun map phrase ->
    Xlist.fold cases map (fun map case ->
      Xlist.fold numbers map (fun map number ->
        Xlist.fold (generate_gerp_number_case number case phrase) map (fun map l ->
          let form = String.concat "" (Xlist.map l (fun i ->  i.Inflexion.lemma)) in
          StringMap.add_inc map form (StringSet.singleton case) (fun set -> StringSet.add set case))))) in
  let map = StringMap.fold map StringMap.empty (fun map form set ->
    let s = String.concat "." (Tagset.sort_tags (StringSet.to_list set)) in
    StringMap.add_inc map s [form] (fun l -> form :: l)) in
  List.rev (StringMap.fold map [] (fun l s forms -> (s,forms) :: l))
  
let generate_case_grouped_adjp phrases =
  let map = Xlist.fold phrases StringMap.empty (fun map phrase ->
    Xlist.fold cases map (fun map case ->
      Xlist.fold numbers map (fun map number ->
        Xlist.fold genders map (fun map gender ->
          Xlist.fold (generate_adjp_number_case_gender number case gender phrase) map (fun map l ->
            let form = String.concat "" (Xlist.map l (fun i ->  i.Inflexion.lemma)) in
            StringMap.add_inc map form (StringSet.singleton case) (fun set -> StringSet.add set case)))))) in
  let map = StringMap.fold map StringMap.empty (fun map form set ->
    let s = String.concat "." (Tagset.sort_tags (StringSet.to_list set)) in
    StringMap.add_inc map s [form] (fun l -> form :: l)) in
  List.rev (StringMap.fold map [] (fun l s forms -> (s,forms) :: l))
  
  

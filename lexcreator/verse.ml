(*
 *  ENIAMlexcreator: tool for phrase selection from corpus
 *  Copyright (C) 2007, 2019 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
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

let string_of_verse verse =
  String.concat " " (Xlist.map verse (fun (x,_) -> x)) 

let join map =
  Printf.printf "join |verses|=%d\n" (StringMap.size map);
  StringMap.mapi map (fun id text -> 
    [],text,[],[id,0,0])

let select selection joined_lines =
  StringSet.fold selection StringMap.empty (fun map -> fun k ->
    try
      StringMap.add map k (StringMap.find joined_lines k)
    with Not_found -> map)

let get_verses joined_verses =
  StringMap.fold joined_verses StringMap.empty (fun map -> fun _ -> fun (_,_,_,verses) ->
    Xlist.fold verses map (fun map -> fun (id,i,j) ->
      StringMap.add_inc map id [i,j] (fun l -> (i,j) :: l)))

(*********************************************************************************)

let rec match_vis_beginning_rec = function
    [],l2 -> [],l2
  | t :: l1,(x,s) :: l2 -> if t = x then let f1,f2 = match_vis_beginning_rec (l1,l2) in (x,s) :: f1, f2 else raise Not_found
  | _ -> raise Not_found

let match_vis_beginning list vis =
  try
    match_vis_beginning_rec (list,vis) 
  with Not_found -> [],vis

let move_term_to_pre_vis terms joined_lines =
  let term_list = Str.split (Str.regexp " ") terms in
  StringMap.fold joined_lines StringMap.empty (fun map -> fun k -> fun (pre_vis,vis,post_vis,lines) ->
    let va,vb = match_vis_beginning term_list vis in
    let v = pre_vis @ va, vb,post_vis,lines in
    StringMap.add map k v)

let rec match_vis_rec list vis =
  try 
    let va, vb = match_vis_beginning_rec (list,vis) in
    [],va @ vb
  with Not_found -> 
    if vis = [] then raise Not_found else 
    let va, vb = match_vis_rec list (List.tl vis) in
    (List.hd vis) :: va, vb

let match_vis list vis =
  try
    match_vis_rec list vis
  with Not_found -> vis,[]

let move_term_to_post_vis terms joined_lines =
  let term_list = Str.split (Str.regexp " ") terms in
  StringMap.fold joined_lines StringMap.empty (fun map -> fun k -> fun (pre_vis,vis,post_vis,lines) ->
    let va,vb = match_vis term_list vis in
    let v = pre_vis, va, vb @ post_vis,lines in
    StringMap.add map k v)

let move_term_from_post_visx i list map =
  StringSet.fold list map (fun map -> fun key ->
    let pre_vis,vis,post_vis,lines = StringMap.find map key in
    match post_vis with
      [] -> map
    | x :: l -> StringMap.add map key (pre_vis, vis @ [x], l, lines))

let move_term_from_post_vis_allx list map =
  StringSet.fold list map (fun map -> fun key ->
    let pre_vis,vis,post_vis,lines = StringMap.find map key in
    StringMap.add map key (pre_vis, vis @ post_vis, [], lines))

let move_term_to_post_visx i list map =
  StringSet.fold list map (fun map -> fun key ->
    let pre_vis,vis,post_vis,lines = StringMap.find map key in
    match List.rev vis with
      [] -> map
    | x :: l -> StringMap.add map key (pre_vis, List.rev l, x :: post_vis, lines))

let move_term_to_post_vis_allx list map =
  StringSet.fold list map (fun map -> fun key ->
    let pre_vis,vis,post_vis,lines = StringMap.find map key in
    StringMap.add map key (pre_vis, [], vis @ post_vis, lines))

let move_term_from_pre_visx i list map =
  StringSet.fold list map (fun map -> fun key ->
    let pre_vis,vis,post_vis,lines = StringMap.find map key in
    match List.rev pre_vis with
      [] -> map
    | x :: l -> StringMap.add map key (List.rev l, x :: vis, post_vis, lines))

let move_term_from_pre_vis_allx list map =
  StringSet.fold list map (fun map -> fun key ->
    let pre_vis,vis,post_vis,lines = StringMap.find map key in
    StringMap.add map key ([], pre_vis @ vis, post_vis, lines))

let move_term_to_pre_visx i list map =
  StringSet.fold list map (fun map -> fun key ->
    let pre_vis,vis,post_vis,lines = StringMap.find map key in
    match vis with
      [] -> map
    | x :: l -> StringMap.add map key (pre_vis @ [x], l, post_vis, lines))

let move_term_to_pre_vis_allx list map =
  StringSet.fold list map (fun map -> fun key ->
    let pre_vis,vis,post_vis,lines = StringMap.find map key in
    StringMap.add map key (pre_vis @ vis, [], post_vis, lines))

let remove selection joined_lines =
  StringSet.fold selection joined_lines (fun map -> fun key ->
    StringMap.remove map key)


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

let empty = StringMap.empty

let add_inc pos_map pos lemma entry =
  let lemma_map = try StringMap.find pos_map pos with Not_found -> StringMap.empty in
  let lemma_map = StringMap.add_inc lemma_map lemma [entry] (fun l -> entry :: l) in
  StringMap.add pos_map pos lemma_map

let add_inc_list pos_map pos lemma entries =
  let lemma_map = try StringMap.find pos_map pos with Not_found -> StringMap.empty in
  let lemma_map = StringMap.add_inc lemma_map lemma entries (fun l -> entries @ l) in
  StringMap.add pos_map pos lemma_map

let map pos_map f =
  StringMap.mapi pos_map (fun pos lemma_map ->
      StringMap.mapi lemma_map (fun lemma entries ->
          Xlist.rev_map entries (fun entry ->
              f pos lemma entry)))

let flatten_map pos_map f =
  StringMap.mapi pos_map (fun pos lemma_map ->
      StringMap.mapi lemma_map (fun lemma entries ->
          List.flatten (Xlist.rev_map entries (fun entry ->
              f pos lemma entry))))

let map2 pos_map f =
  StringMap.mapi pos_map (fun pos lemma_map ->
      StringMap.mapi lemma_map (fun lemma entries ->
          f pos lemma entries))

let iter pos_map f =
  StringMap.iter pos_map (fun pos lemma_map ->
      StringMap.iter lemma_map (fun lemma entries ->
          Xlist.iter entries (fun entry ->
              f pos lemma entry)))

let fold pos_map s f =
  StringMap.fold pos_map s (fun s pos lemma_map ->
      StringMap.fold lemma_map s (fun s lemma entries ->
          Xlist.fold entries s (fun s entry ->
              f s pos lemma entry)))

let find pos_map pos lemma =
  try
    StringMap.find (StringMap.find pos_map pos) lemma
  with Not_found -> []

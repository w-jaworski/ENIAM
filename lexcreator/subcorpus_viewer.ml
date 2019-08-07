(*
 *  ENIAMlexcreator: tool for phrase selection from corpus
 *  Copyright (C) 2009, 2019 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
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

open Project
open Xstd
open Types

let set_model (frame,showed_button,(label : GMisc.label)) model = 
  (match model.status with
  | Loaded -> showed_button#set_active false
  | Showed -> showed_button#set_active true);
  label#set_text ("size = " ^ string_of_int (StringSet.size model.ids) ^ " ")

let create name model change =
  let frame = GBin.frame ~label:name () in
  let hbox = GPack.hbox ~packing:frame#add () in
  let label = GMisc.label ~packing:hbox#pack () in
  let showed_button = GButton.check_button ~packing:hbox#pack ~label:"Showed" () in 
  ignore(showed_button#connect#toggled ~callback:(fun () ->
    if showed_button#active then change Showed else change Loaded));
  let viewer = frame,showed_button,label in
  set_model viewer model;
  viewer

let coerce (frame,showed_button,label) = 
  frame#coerce

let destroy (frame,showed_button,label) =
  frame#destroy ()

let set_quantity (frame,showed_button,label) n =
  label#set_text (string_of_int n)

let get_model (frame,showed_button,label) =
  match showed_button#active with
    false -> Loaded
  | true -> Showed

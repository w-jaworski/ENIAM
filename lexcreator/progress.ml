(*
 *  ENIAMlexcreator: tool for phrase selection from corpus
 *  Copyright (C) 2007, 2008, 2019 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
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

let time_string2 time =
  if time > 12000. then Printf.sprintf "%.02f hours" (time /. 3600.) else
  if time > 200. then Printf.sprintf "%.02f minutes" (time /. 60.)
  else Printf.sprintf "%.02f seconds" time 

let time_string rest_time full_time =
  " estimated time " ^ (time_string2 rest_time) ^ " (full time " ^ (time_string2 full_time) ^ ")"

let create name size =
  let window = GWindow.window ~title:name ~width:500 ~border_width:0 ~modal:true () in 
  let progress_bar = GRange.progress_bar ~packing:window#add () in
  window#show ();
  let n = ref 0 in
  let start_time = Unix.gettimeofday () in
  let timeout = if size = 0 then GMain.Timeout.add ~ms:50000 ~callback:(fun () -> true) else
    GMain.Timeout.add ~ms:500 ~callback:(fun () -> 
    progress_bar#set_fraction ((float (!n)) /. (float size));
    let act_time = Unix.gettimeofday () -. start_time in
    let full_time = act_time /. (float (!n)) *. (float size) in
    let rest_time = full_time -. act_time in
    let time_string = time_string rest_time full_time in
    progress_bar#set_text ((string_of_int (!n)) ^ " of " ^ (string_of_int size) ^ time_string);
    true) in 
  window,progress_bar,size,n,timeout

let destroy (window,progress_bar,size,n,timeout) =
  GMain.Timeout.remove timeout;
  window#destroy ()

let next (window,progress_bar,size,n,timeout) =
  incr n

let set (window,progress_bar,size,n,timeout) v =
  n := v

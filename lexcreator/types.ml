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
open Xstd

type subcorpus_status = Loaded | Showed

let string_of_subcstatus = function
  | Loaded -> "Loaded"
  | Showed -> "Showed"

let subcstatus_of_string = function
  | "Loaded" -> Loaded
  | "Showed" -> Showed
  | _ -> failwith "string_of_subcstatus"

type subcorpus = {
    status: subcorpus_status;
    ids: StringSet.t}


type message_from_overseer = 
(*    Init*)
  | Work_with of string * string
  | Kill_yourself
  
type message_to_overseer =
    Ready_to_work
  | Work_done of string * (string * string) list
      
let no_workers = ref 8

let verse_worker_filename =
  if Sys.file_exists "/usr/local/bin/verse_worker" then "verse_worker" else
  if Sys.file_exists "./verse_worker" then "./verse_worker" else (
  print_endline "Could not find verse_worker";
  exit 1)

let low_res_flag = ref false
 

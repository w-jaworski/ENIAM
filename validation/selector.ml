(*
 *  ENIAMselector: tool for selecting not parsed utterances from corpus
 *  Copyright (C) 2022 SELIDOR - T. Puza, Ł. Wasilewski Sp.J.
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
 
open Xjson
open Xstd

let parsed_path = ref "."
let parsed_names = ref []
let test_path = ref "."
let test_filename = ref ""
let print_flag = ref false
  
let check_filename s =
  if Xstring.check_sufix ".tab" s then s else s ^ ".tab"
  
let check_path s =
  if Xstring.check_sufix "/" s then s else s ^ "/"
        
let spec_list = [
  "-s", Arg.String (fun s -> parsed_path := check_path s), "<path> parsed path";
  "-t", Arg.String (fun s -> parsed_names := s :: !parsed_names), "<name> parsed name";
  "-p", Arg.String (fun s -> test_path := check_path s), "<path> test path";
  "-f", Arg.String (fun s -> test_filename := check_filename s), "<filename> test filename";
]

let usage_msg =
  "Usage: select <options>\nOptions are:"
  
let anon_fun s = raise (Arg.Bad ("invalid argument: " ^ s))
 
let load_json_list filename =
  match json_of_string (File.load_file filename) with
    JArray l -> l
  | _ -> failwith "load_json_list"
  
let rec get_text_list = function
    ("text", JString s) :: _ -> s
  | _ :: l -> get_text_list l
  | [] -> raise Not_found
  
let rec get_text = function
    JObject["and",JArray l] -> 
      let s = Xlist.fold l "" (fun s t -> 
        try get_text t with Not_found -> s) in
      if s = "" then raise Not_found else s
  | JObject l -> get_text_list l
  | json -> print_endline ("get_text: " ^ json_to_string_fmt2 "" json); raise Not_found
  
let prepare_parsed set l =
  Xlist.fold l set (fun set t -> 
    try
      StringSet.add set (get_text t)
    with Not_found -> failwith ("prepare_parsed: " ^ json_to_string t))
  
let load_parsed () =
  Xlist.fold !parsed_names StringSet.empty (fun set name ->
    let parsed = load_json_list (!parsed_path ^ name ^ "_parsed.json") in 
    prepare_parsed set parsed)
  
let _ = 
  Arg.parse spec_list anon_fun usage_msg;
  if !parsed_names = [] then (
    print_endline "Parsed names not provided";
    exit 1);
  let parsed = load_parsed () in
  let texts = File.load_tab (!test_path ^ !test_filename) List.hd in
  Xlist.iter texts (fun s ->
    if StringSet.mem parsed s then () else
    print_endline s);
  ()


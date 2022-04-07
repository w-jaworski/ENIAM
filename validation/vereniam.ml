(*
 *  verENIAM: parser for corpora with schema verification
 *  Copyright (C) 2017-2021 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
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
open Xjson

let schema_filename = ref ""
let test_path = ref "."
let test_filename = ref ""
let result_type = ref ""


let spec_list = [
  "-p", Arg.String (fun s -> test_path := Exec2.check_path s), "<path> test path"; 
  "-f", Arg.String (fun s -> test_filename := Exec2.check_filename2 s), "<filename> test filename";
  "-k", Arg.String (fun s -> schema_filename := Exec2.check_filename s), "<filename> schema filename";
  "-r", Arg.String (fun s -> result_type := s), "<json type> result type";
  ] @ Exec2.spec_list

let usage_msg =
  "Usage: vereniam <options>\nParse a given file and check whether parsing results satisfy a given schema.\nOptions are:"

let message = "vereniam, semantic parser for Logical Categorial Grammar formalism and meaning representation structure verifier\n\
Copyright (C) 2021 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>"
  
let load_json_list filename =
  match json_of_string (File.load_file filename) with
    JArray l -> l
  | _ -> failwith "load_json_list"
  
let rec is_error = function
    JObject["and",JArray l] | JObject["and-tuple",JArray l] -> Xlist.fold l false (fun b t -> b || is_error t)
  | JObject l -> 
      Xlist.fold l false (fun b (e,t) -> 
        if e = "error" then true else b)
  | _ -> false
 
let get_sock_addr host_name port =
  let he = Unix.gethostbyname host_name in
  let addr = he.Unix.h_addr_list in
  Unix.ADDR_INET(addr.(0),port)

let _ =
  prerr_endline message;
  Arg.parse spec_list Exec2.anon_fun usage_msg;
  if !schema_filename = "" then (
    print_endline "Schema filename not provided";
    exit 1);
  let l = 
     try XjsonSchema.load !schema_filename
     with Failure e -> print_endline e; exit 1 in
  let schema = XjsonSchema.prepare_schema l in
  if !test_filename = "" then (
    print_endline "Test filename not provided";
    exit 1);
  ignore (Sys.command ("mkdir -p " ^ !Exec2.output_dir));
  Exec2.initialize ();
  let sub_in,sub_out =
    if !Exec2.subsyntax_built_in then stdin,stdout
    else Unix.open_connection (get_sock_addr !Exec2.subsyntax_host !Exec2.subsyntax_port) in
  prerr_endline "Ready!";
  let corpus = File.load_lines (!test_path ^ !test_filename ^ ".tab") in
  let parsed,not_parsed,not_verified,_ = 
    Xlist.fold corpus ([],[],[],1) (fun (parsed,not_parsed,not_verified,i) phrase ->
      Printf.printf "%d of %d %s\n%!" i (Xlist.size corpus) phrase;
      if phrase = "" then (print_endline "empty phrase"; (parsed,not_parsed,not_verified,i+1)) else (      
      let phrase2 = if !Exec2.correct_spelling_flag then FuzzyDetector.correct phrase else phrase in
      let text,tokens,lex_sems = Exec2.process sub_in sub_out phrase2 in
      let t = 
        try Json.normalize (Exec.Json2.convert false text)
        with e -> JObject["error",JString "JSON convert";"msg", JString (Printexc.to_string e);"text", JString phrase] in
      let s = json_to_string_fmt2 "" t in
      if is_error t then parsed, (phrase, s) :: not_parsed, not_verified, i+1 else
      try
        let res = XjsonSchema.string_of_type_expr (XjsonSchema.assign_type schema t) in
        if res <> !result_type && !result_type <> "" then 
          let comm = Printf.sprintf "Invalid result type '%s' in\n %s\n" res s in
          parsed, not_parsed, (phrase, comm) :: not_verified, i+1
        else (phrase, s) :: parsed, not_parsed, not_verified, i+1
      with XjsonSchema.InvalidSchema(s,json) -> 
        let comm = Printf.sprintf "%s in\n%s\n%s" s phrase (json_to_string_fmt2 "" json) in
        parsed, not_parsed, (phrase, comm) :: not_verified, i+1
      | e -> 
        let comm = Printf.sprintf "Error while processing\n%s\n%s" phrase (Printexc.to_string e) in
        parsed, not_parsed, (phrase, comm) :: not_verified, i+1)) in        
  File.file_out (!Exec2.output_dir ^ "parsed_" ^ !test_filename ^ ".tab") (fun file ->
    Xlist.iter (List.rev parsed) (fun (s,_) -> Printf.fprintf file "%s\n" s));
  File.file_out (!Exec2.output_dir ^ !test_filename ^ ".json") (fun file ->
    Printf.fprintf file "[\n%s\n]" (String.concat ",\n" (Xlist.rev_map parsed snd)));
  ignore (Sys.command ("rm -f " ^ !Exec2.output_dir ^ "not_parsed_" ^ !test_filename ^ ".tab"));
  if Xlist.size not_parsed > 0 then File.file_out (!Exec2.output_dir ^ "not_parsed_" ^ !test_filename ^ ".tab") (fun file ->
    Xlist.iter (List.rev not_parsed) (fun (s,_) -> Printf.fprintf file "%s\n" s));
  ignore (Sys.command ("rm -f " ^ !Exec2.output_dir ^ "not_parsed_" ^ !test_filename ^ ".txt"));
  if Xlist.size not_parsed > 0 then File.file_out (!Exec2.output_dir ^ "not_parsed_" ^ !test_filename ^ ".txt") (fun file ->
    Xlist.iter (List.rev not_parsed) (fun (_,s) -> Printf.fprintf file "%s\n\n" s));
  ignore (Sys.command ("rm -f " ^ !Exec2.output_dir ^ "not_verified_" ^ !test_filename ^ ".tab"));
  if Xlist.size not_verified > 0 then File.file_out (!Exec2.output_dir ^ "not_verified_" ^ !test_filename ^ ".tab") (fun file ->
    Xlist.iter (List.rev not_verified) (fun (s,_) -> Printf.fprintf file "%s\n" s));
  ignore (Sys.command ("rm -f " ^ !Exec2.output_dir ^ "not_verified_" ^ !test_filename ^ ".txt"));
  if Xlist.size not_verified > 0 then File.file_out (!Exec2.output_dir ^ "not_verified_" ^ !test_filename ^ ".txt") (fun file ->
    Xlist.iter (List.rev not_verified) (fun (_,s) -> Printf.fprintf file "%s\n\n" s));
  ()


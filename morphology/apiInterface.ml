(*
 *  ENIAMmorphology, a morphological analyser and a guesser for Polish
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

type output = Text | Xml | Html

let output = ref Text
let parser_host = ref "localhost"
let parser_port = ref 5736
(* let output_dir = ref "results/" *)
let spec_list = [
  (* "-r", Arg.String (fun p -> output_dir:=p), "<path> Set output dir (by default results)"; *)
  "--port", Arg.Int (fun p -> parser_port:=p), "<port> Connect to parser on a given port (by default 5439)";
  "--host", Arg.String (fun s -> parser_host:=s), "<hostname> Connect to parser on a given host (by default localhost)";
  "-t", Arg.Unit (fun () -> output:=Text), "Output as plain text (default)";
  "-x", Arg.Unit (fun () -> output:=Xml), "Output as XML";
  "-h", Arg.Unit (fun () -> output:=Html), "Output as HTML";
  ]

let usage_msg =
  "Usage: morphology.api <options>\nInput is a sequence of lines. Empty line ends the sequence and invoke parsing.\nOptions are:"

let message = "ENIAMmorphologyApi, interface for ENIAMmorphology\n\
Copyright (C) 2017 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>\n\
Copyright (C) 2017 Institute of Computer Science Polish Academy of Sciences"

let anon_fun s = raise (Arg.Bad ("invalid argument: " ^ s))

let input_text channel =
  let s = ref (try input_line channel with End_of_file -> "") in
  let lines = ref [] in
  while !s <> "" do
    lines := !s :: !lines;
    s := try input_line channel with End_of_file -> ""
  done;
  String.concat "\n" (List.rev !lines)

(* let r = ref 0 *)

let rec main_loop parser_in parser_out =
  (* incr r; *)
  let query = (try input_line stdin with End_of_file -> "") in
  if query = "" then () else (
    Printf.fprintf parser_out "%s\n%!" query;
    let result,msg = (Marshal.from_channel parser_in : Inflexion.t list * string) in
    (match !output with
       Text ->
            if msg = "" then print_endline (Inflexion.string_of_interpretations result ^ "\n\n")
            else print_endline (query ^ "\n" ^ msg ^ "\n\n")
     | Xml -> print_endline (Xml.to_string (Inflexion.xml_of_interpretations result msg) ^ "\n\n")
     | Html -> print_endline (Inflexion.html_of_interpretations result msg ^ "\n\n"));
    prerr_endline "Done!");
  Printf.fprintf parser_out "\n%!";
  let _ = Unix.shutdown_connection parser_in in
  ()

let get_sock_addr host_name port =
  let he = Unix.gethostbyname host_name in
  let addr = he.Unix.h_addr_list in
  Unix.ADDR_INET(addr.(0),port)

let _ =
  prerr_endline message;
  Arg.parse spec_list anon_fun usage_msg;
  (* ignore (Sys.command ("mkdir -p " ^ !output_dir)); *)
  let parser_in,parser_out = Unix.open_connection (get_sock_addr !parser_host !parser_port) in
  prerr_endline "Ready!";
  main_loop parser_in parser_out

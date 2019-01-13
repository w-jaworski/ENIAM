(*
 *  ENIAMmorphology, a morphological analyser and a guesser for Polish
 *  Copyright (C) 2016-2018 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
 *  Copyright (C) 2016-2018 Institute of Computer Science Polish Academy of Sciences
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

type output = Text | Xml | Html | Marsh

type task = Analyze | Generate

let output = ref Text
let task = ref Analyze
let comm_stdio = ref true
let port = ref 5436
let sort_fun = ref Inflexion.sort_results

let spec_list = [
  "-i", Arg.Unit (fun () -> comm_stdio:=true), "Communication using stdio (default)";
  "-p", Arg.Int (fun p -> comm_stdio:=false; port:=p), "<port> Communication using sockets on given port number";
  "-t", Arg.Unit (fun () -> output:=Text), "Output as plain text (default)";
  "-x", Arg.Unit (fun () -> output:=Xml), "Output as XML";
  "-m", Arg.Unit (fun () -> output:=Marsh), "Output as marshalled Ocaml data structure";
  "-h", Arg.Unit (fun () -> output:=Html), "Output as HTML";
  "--analyze", Arg.Unit (fun () -> task:=Analyze), "Lemmatize given form (default)";
  "--generate", Arg.Unit (fun () -> task:=Generate), "Generate form for a given lemma";
  (* "-r", Arg.String (fun p ->
        SubsyntaxTypes.set_resource_path p;
        MorphologyTypes.set_resource_path p;
        SubsyntaxTypes.set_resource_path p), "<path> Set resource path"; *)
  ]

let usage_msg =
  "Usage: morphology <options>\nInput is a sequence of lines. Empty line shutdown parser.\nOptions are:"

let message = "ENIAMmorphology, a morphological analyser and a guesser for Polish\n\
Copyright (C) 2016-2018 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>\n\
Copyright (C) 2016-2018 Institute of Computer Science Polish Academy of Sciences"

let anon_fun s = raise (Arg.Bad ("invalid argument: " ^ s))

let rec main_loop in_chan out_chan =
  let form = (try input_line in_chan with End_of_file -> "") in
  if form = "" then () else (
    (* print_endline "input text begin";
    print_endline text;
    print_endline "input text end"; *)
    let result,msg = 
      if !task = Analyze then
        Inflexion.catch_get_interpretations form 
	  else Inflexion.catch_synthetize_disambiguate form in
    (match !output with
       Text ->
            if msg = "" then output_string out_chan (Inflexion.string_of_interpretations (!sort_fun result) ^ "\n\n")
            else output_string out_chan (form ^ "\n" ^ msg ^ "\n\n")
     | Xml -> output_string out_chan (Xml.to_string (Inflexion.xml_of_interpretations result msg) ^ "\n\n")
     | Html -> output_string out_chan (Inflexion.html_of_interpretations result msg ^ "\n\n")
     | Marsh -> Marshal.to_channel out_chan (result,msg) []);
    flush out_chan;
    main_loop in_chan out_chan)

let _ =
  prerr_endline message;
  Arg.parse spec_list anon_fun usage_msg;
  sort_fun := if !task = Analyze then Inflexion.sort_results else Inflexion.sort_results2;
  Inflexion.initialize ();
  Gc.compact ();
  prerr_endline "Ready!";
  if !comm_stdio then main_loop stdin stdout
  else
    let sockaddr = Unix.ADDR_INET(Unix.inet_addr_any,!port) in
    Unix.establish_server main_loop sockaddr

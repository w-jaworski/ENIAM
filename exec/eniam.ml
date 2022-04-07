(*
 *  ENIAMexec implements ENIAM processing stream
 *  Copyright (C) 2017-2021 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
 *  Copyright (C) 2017-2018 Institute of Informatics, University of Warsaw
 *  Copyright (C) 2017-2018, 2022 SELIDOR - T. Puza, Ł. Wasilewski Sp.J.
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

let id = string_of_int (Unix.getpid ())

type output = Text | (*Marked |*) Xml | Html | Marsh | (*FStruct |*) JSON (*| Graphviz*) | Worker

let output = ref Html
let comm_stdio = ref true
let name_length = ref 20
let split_pattern = ref ""
let line_mode = ref false
let select_not_parsed_flag = ref false
let json_flag = ref false
let statistics_flag = ref false
let split_corpus = ref false
let parsed_file = ref stdout
let not_parsed_file = ref stdout

let spec_list = [
  "-i", Arg.Unit (fun () -> comm_stdio:=true), "Communication using stdio (default)";
  "-p", Arg.Int (fun p -> comm_stdio:=false; Exec2.port:=p), "<port> Communication using sockets on given port number";
  "-h", Arg.Unit (fun () -> output:=Html), "Output as HTML (default)";
  "-t", Arg.Unit (fun () -> output:=Text), "Output as plain text";
(*   "-r", Arg.Unit (fun () -> output:=Marked), "Output as HTML marked text"; *)
(*   "-f", Arg.Unit (fun () -> output:=FStruct), "Output as feature structure"; *)
  "-x", Arg.Unit (fun () -> output:=Xml), "Output as XML";
  "-j", Arg.Unit (fun () -> output:=JSON; json_flag:=true), "Output as JSON";
  "-m", Arg.Unit (fun () -> output:=Marsh), "Output as marshalled Ocaml data structure";
  "-w", Arg.Unit (fun () -> output:=Worker; comm_stdio:=true; line_mode:=false), "Output as worker in distributed execution";
  "--img", Arg.Int (fun v -> Exec2.img:=v), "<val> Selects which images are included in output html page \n     0 - no images included\n     1 - simple dependency trees included (default)\n     2 - dependency trees included";
  "--sem", Arg.Unit (fun () -> Exec2.semantic_processing_flag:=true), "Perform semantic processing (default)";
  "--no-sem", Arg.Unit (fun () -> Exec2.semantic_processing_flag:=false; Exec2.inference_flag:=false), "Do not perform semantic processing";
  "--infer", Arg.Unit (fun () -> Exec2.inference_flag:=true; Exec2.semantic_processing_flag:=true), "Apply inference rules (default)";
  "--no-infer", Arg.Unit (fun () -> Exec2.inference_flag:=false), "Do not apply inference rules";
  "--sel-not-parsed", Arg.Unit (fun () -> select_not_parsed_flag:=true(*; semantic_processing_flag:=true*)), "Select not parsed";
  "--no-sel-not-parsed", Arg.Unit (fun () -> select_not_parsed_flag:=false), "Do not select not parsed (default)";
  "--json", Arg.Unit (fun () -> json_flag:=true), "Convert to json";
  "--no-json", Arg.Unit (fun () -> json_flag:=false), "Do not convert to json (default)";
  "--max-par-name-length", Arg.Int (fun v -> name_length:=v), "<val> Defines maximum length of paragraph name in visualization (default 20)";
  "--split-pattern", Arg.String (fun s -> split_pattern:=s), "<val> Defines splitting pattern for HTML marked text (default none)";
  "--line-mode", Arg.Unit (fun () -> line_mode:=true), "Line mode";
  "--no-line-mode", Arg.Unit (fun () -> line_mode:=false), "No line mode (default)";
  "--split-corpus", Arg.Unit (fun () -> split_corpus:=true), "Generate corpus spit into parsed and not parsed records";
  "--statistics", Arg.Unit (fun () -> statistics_flag:=true), "Add processing statistics to data";
  ] @ Exec2.spec_list
  
let usage_msg =
  "Usage: eniam <options>\nInput is a sequence of lines. Empty line ends the sequence and invoke parsing. Double empty line shutdown parser.\nOptions are:"

let message = "ENIAM, semantic parser for Logical Categorial Grammar formalism\n\
Copyright (C) 2017 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>\n\
Copyright (C) 2017 Institute of Computer Science Polish Academy of Sciences\n\
Copyright (C) 2022 SELIDOR - T. Puza, Ł. Wasilewski Sp.J."

let input_text channel =
  if !output = Worker then 
    match Marshal.from_channel stdin with
      ExecTypes.Work_with (akt_id,query) ->
        query, Xstring.split "\n" query
    | ExecTypes.Kill_yourself -> "",[]
  else (
    let s = ref (try input_line channel with End_of_file -> "") in
    let lines = ref [] in
    while !s <> "" do
      lines := !s :: !lines;
      s := try input_line channel with End_of_file -> ""
    done;
    let lines = if !Exec2.correct_spelling_flag then Xlist.rev_map !lines FuzzyDetector.correct else List.rev !lines in
    String.concat "\n" lines, lines)

let process sub_in sub_out s =
  let text,tokens,lex_sems = Exec2.process sub_in sub_out s in
  let text = if !select_not_parsed_flag then Exec.select_not_parsed text else text in
  let text = Exec.aggregate_stats text in
  let status = Exec.aggregate_status text in
  let text = if not !json_flag then text else
    try
      let json = if not !Exec2.semantic_processing_flag then Exec.Json2.convert !statistics_flag text else Json.normalize (Exec.Json2.convert !statistics_flag text) in
      ExecTypes.JSONtext (json_to_string_fmt "" json) 
    with e -> ExecTypes.JSONtext (json_to_string_fmt "" (
      JObject["and-tuple",JArray[JObject["error",JString (Printexc.to_string e)];JObject["text",JString s]]])) in
(*   print_endline "process 9"; *)
  if !split_corpus then 
    if status then Printf.fprintf !parsed_file "%s\n%!" s
    else Printf.fprintf !not_parsed_file "%s\n%!" s;
  text,status,tokens,lex_sems

let rec main_loop sub_in sub_out in_chan out_chan =
  let pid = string_of_int (Unix.getpid ()) in
  if !Exec2.debug_flag then prerr_endline (pid ^ " Receiving query");
  let text, lines = input_text in_chan in
  let raw_text = text in
  if !Exec2.debug_flag then prerr_endline (pid ^ " Received query: '" ^ String.escaped raw_text ^ "'");
  if text = "" then (if !Exec2.debug_flag then prerr_endline (pid ^ " Exiting") else ()) else (
    if !line_mode then (match !output with
     | Text -> 
            Xlist.iter lines (fun line ->
              let text,status,tokens,lex_sems = process sub_in sub_out line in  
              Printf.fprintf out_chan "\n#%s\n%!" line;
              Printf.fprintf out_chan "%s\n%!" (String.concat "\n" (Visualization.to_string_text !Exec2.verbosity tokens text))) 
     | Html -> 
          File.file_out (!Exec2.output_dir ^ "parsed_text.html") (fun file ->
            Printf.fprintf file "%s\n" Visualization.html_header;
            Xlist.iter lines (fun line ->
              let text,_,tokens,lex_sems = process sub_in sub_out line in
              if text <> ExecTypes.AltText [] then
              Printf.fprintf file "%s<BR>\n%!" (Visualization.html_of_text_as_paragraph !Exec2.output_dir ExecTypes.Struct !Exec2.img !Exec2.verbosity tokens text));
            Printf.fprintf file "%s\n" Visualization.html_trailer)
     | Xml -> 
          File.file_out (!Exec2.output_dir ^ "parsed_text.xml") (fun file ->
            Printf.fprintf file "%s\n" ExecXMLof.xml_header;
            Xlist.iter lines (fun line ->
              let text,_,tokens,lex_sems = process sub_in sub_out line in
              Printf.fprintf file "%s\n%!" (Xml.to_string_fmt (ExecXMLof.text_as_paragraph "" text)));
            Printf.fprintf file "%s\n" ExecXMLof.xml_trailer)
     | JSON ->
            Xlist.iter lines (fun line ->
              let text,_,tokens,lex_sems = process sub_in sub_out line in
(*             Printf.fprintf out_chan "-------------------------\n";  *)
            (* Printf.fprintf out_chan "%s\n%!" line; *)
              try
                match text with
                  ExecTypes.JSONtext s -> Printf.fprintf out_chan "%s\n%!" s
				| _ -> failwith "main_loop: json"
(*                let json = (*Json.add_text line*) (Json.normalize (Json.convert text)) in
                Printf.fprintf out_chan "%s\n%!" (json_to_string_fmt "" json);*)
(*                 let json2 = JSONconverter.add_text line ((*JSONconverter.convert_jstring_indexes*) (JSONconverter.convert_to_kuba json)) in *)
(*                 Printf.fprintf out_chan "%s\n%!" (JSONconverter.to_string "" json2) *)
              with e -> print_endline line; print_endline (Printexc.to_string e))
     | _ -> failwith "main_loop: ni") 
    else 
      let text,status,tokens,lex_sems = process sub_in sub_out text in
    (match !output with
    | Text -> Printf.fprintf out_chan "%s\n%!" (String.concat "\n" (Visualization.to_string_text !Exec2.verbosity tokens text)) (* FIXME: obcinanie nazw *)
    (* | Marked -> Printf.fprintf out_chan "%s\n%!" (String.concat "\n" (Visualization.marked_string_of_text !Exec2.verbosity tokens text)) *)
    (* | Marked -> Visualization.print_html_marked_text !Exec2.output_dir "marked_text" text !Exec2.img !Exec2.verbosity tokens *)
(*     | Marked -> MarkedHTMLof.print_html_marked_simple_text !Exec2.output_dir "marked_text" !name_length (MarkedHTMLof.marked_string_of_text !Exec2.verbosity tokens text) *)
    | Html -> Visualization.print_html_text !Exec2.output_dir "parsed_text" text !Exec2.img !Exec2.verbosity tokens
    | Xml -> Printf.fprintf out_chan "%s\n%!" (Xml.to_string_fmt (ExecXMLof.text "" text))
(*     | FStruct -> Printf.fprintf out_chan "%s\n%!" (Xml.to_string_fmt (FStruct.text "" text)) *)
    | JSON ->
(*         let json = Json.convert_text "" text in *)
(*         Printf.fprintf out_chan "%s\n%!" (json_to_string_fmt "" json); *)
              (try
                match text with
                  ExecTypes.JSONtext s -> Printf.fprintf out_chan "%s\n\n%!" s
				| _ -> failwith "main_loop: json"
(*                let json = Json.add_text raw_text (Json.normalize (Json.convert text)) in
                Printf.fprintf out_chan "%s\n%!" (json_to_string_fmt "" json);*)
              with e -> print_endline raw_text; print_endline (Printexc.to_string e))
(*        (* Printf.fprintf out_chan "-------------------------\n"; *)
        (* Printf.fprintf out_chan "%s\n%!" raw_text; *)
        let json = JSONconverter.convert text in
(*         Printf.fprintf out_chan "%s\n%!" (JSONconverter.to_string "" json); *)
        let json2 = JSONconverter.add_text raw_text ((*JSONconverter.convert_jstring_indexes*) (JSONconverter.convert_to_kuba json)) in
        Printf.fprintf out_chan "%s\n%!" (JSONconverter.to_string "" json2);
        (* (try
          let time = TestTime.execute_gen json in
          Printf.fprintf out_chan "%s\n%!" (TestTime.string_of "" time)
         with Failure t -> print_endline ("FAILURE: " ^ t)); *)*)
    | Marsh -> Marshal.to_channel out_chan (text,tokens,lex_sems) []; flush out_chan
    | Worker -> Marshal.to_channel out_chan (ExecTypes.Work_done(id, (text,status,tokens,lex_sems))) [Marshal.No_sharing]; flush out_chan);
    if !output <> Worker then prerr_endline "Done!";
    if !Exec2.debug_flag then prerr_endline (pid ^ " Processed query: '" ^ String.escaped raw_text ^ "'");
    main_loop sub_in sub_out in_chan out_chan)

let get_sock_addr host_name port =
  let he = Unix.gethostbyname host_name in
  let addr = he.Unix.h_addr_list in
  Unix.ADDR_INET(addr.(0),port)

let main_preloop in_chan out_chan =
  let sub_in,sub_out =
    if !Exec2.subsyntax_built_in then stdin,stdout
    else (
      let pid = string_of_int (Unix.getpid ()) in
      if !Exec2.debug_flag then prerr_endline (pid ^ " Connecting to subsyntax");
      Unix.open_connection (get_sock_addr !Exec2.subsyntax_host !Exec2.subsyntax_port)) in
  main_loop sub_in sub_out in_chan out_chan
    
let _ =
  prerr_endline message;
  Arg.parse spec_list Exec2.anon_fun usage_msg;
  Exec2.initialize ();
(*   if !output = Marked then MarkedHTMLof.initialize (); *)
(*  let morf_in,morf_out = 
    if !ology_built_in then failwith "domparser: ni" else
    Unix.open_connection (get_sock_addr !morphology_host !morphology_port) in
  ExecTypes.morphology_in := morf_in;
  ExecTypes.morphology_out := morf_out;  *)
  if !split_corpus then (
    parsed_file := open_out "results/parsed.tab";
    not_parsed_file := open_out "results/not_parsed.tab");    
  if !output = Worker then (
(*     Sys.set_signal Sys.sigkill (Sys.Signal_handle(fun n -> prerr_endline "sigkill"; exit 1)); *)
(*     Sys.set_signal Sys.sigterm (Sys.Signal_handle(fun n -> prerr_endline "sigterm"; exit 1)); *)
    Marshal.to_channel stdout (ExecTypes.Ready_to_work id) [Marshal.No_sharing];
    flush stdout);
  if !output <> Worker then prerr_endline "Ready!";
  if !comm_stdio then main_preloop stdin stdout
  else
    let sockaddr = Unix.ADDR_INET(Unix.inet_addr_any,!Exec2.port) in
    Unix.establish_server main_preloop sockaddr

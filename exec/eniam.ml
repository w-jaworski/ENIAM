(*
 *  ENIAMexec implements ENIAM processing stream
 *  Copyright (C) 2017-2018 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
 *  Copyright (C) 2017-2018 Institute of Informatics, University of Warsaw
 *  Copyright (C) 2017-2018 SELIDOR - T. Puza, Ł. Wasilewski Sp.J.
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

  
let id = string_of_int (Unix.getpid ())

type output = Text | (*Marked |*) Xml | Html | Marsh | (*FStruct |*) JSON (*| Graphviz*) | Worker

let output = ref Html
let comm_stdio = ref true
let port = ref 5439
let subsyntax_built_in = ref true
let subsyntax_host = ref "localhost"
let subsyntax_port = ref 5739
(*let morphology_built_in = ref true
let morphology_host = ref "localhost"
let morphology_port = ref 5440*)
let verbosity = ref 1
let img = ref 1
let timeout = ref 30.
let select_sentence_modes_flag = ref false
let select_sentences_flag = ref true
let semantic_processing_flag = ref true
let inference_flag = ref true
let discontinuous_parsing_flag = ref false
let correct_spelling_flag = ref false
let disambiguate_flag = ref true
let select_not_parsed_flag = ref false
let json_flag = ref false
let output_dir = ref "results/"
let name_length = ref 20
let split_pattern = ref ""
let max_cost = ref 2
let internet_mode = ref false
let line_mode = ref false
let split_corpus = ref false
let parsed_file = ref stdout
let not_parsed_file = ref stdout
let spec_list = [
  "-e", Arg.String (fun s -> SubsyntaxTypes.theories:=s :: !SubsyntaxTypes.theories), "<theory> Add theory (may be used multiple times)";
  "-u", Arg.String (fun s -> SubsyntaxTypes.user_theories:=s :: !SubsyntaxTypes.user_theories), "<theory> Add user theory (may be used multiple times)";
  "-i", Arg.Unit (fun () -> comm_stdio:=true), "Communication using stdio (default)";
  "-p", Arg.Int (fun p -> comm_stdio:=false; port:=p), "<port> Communication using sockets on given port number";
  "-h", Arg.Unit (fun () -> output:=Html), "Output as HTML (default)";
  "-t", Arg.Unit (fun () -> output:=Text), "Output as plain text";
(*   "-r", Arg.Unit (fun () -> output:=Marked), "Output as HTML marked text"; *)
(*   "-f", Arg.Unit (fun () -> output:=FStruct), "Output as feature structure"; *)
  "-x", Arg.Unit (fun () -> output:=Xml), "Output as XML";
  "-j", Arg.Unit (fun () -> output:=JSON; json_flag:=true), "Output as JSON";
  "-m", Arg.Unit (fun () -> output:=Marsh), "Output as marshalled Ocaml data structure";
  "-w", Arg.Unit (fun () -> output:=Worker; comm_stdio:=true; line_mode:=false), "Output as worker in distributed execution";
  "-b", Arg.Unit (fun () -> subsyntax_built_in:=true), "Use built in version of ENIAMsubsyntax (default)";
  "--port", Arg.Int (fun p -> subsyntax_built_in:=false; subsyntax_port:=p), "<port> Connect to ENIAMsubsyntax on a given port";
  "--host", Arg.String (fun s -> subsyntax_built_in:=false; subsyntax_host:=s), "<hostname> Connect to ENIAMsubsyntax on a given host (by default localhost)";
(*  "-b2", Arg.Unit (fun () -> morphology_built_in:=true), "Use built in version of ENIAMmorphology (default)";
  "--port2", Arg.Int (fun p -> morphology_built_in:=false; morphology_port:=p), "<port> Connect to ENIAMmorphology on a given port";
  "--host2", Arg.String (fun s -> morphology_built_in:=false; morphology_host:=s), "<hostname> Connect to ENIAMmorphology on a given host (by default localhost)";*)
  "--timeout", Arg.Float (fun x -> timeout:=x), "<seconds> Sets timeout value for parser (default 30 seconds)";
  "-v", Arg.Int (fun v -> verbosity:=v), "<val> Sets verbosity level of parser\n     0 - print only status information\n     1 - print data relevant to the status of a given sentence (default)\n     2 - print all data structures";
  "--img", Arg.Int (fun v -> img:=v), "<val> Selects which images are included in output html page \n     0 - no images included\n     1 - simple dependency trees included (default)\n     2 - dependency trees included";
  "--output", Arg.String (fun s -> output_dir:=s ^ "/"), "<dir> Sets output directory (by default results/)";
  "--sel-modes", Arg.Unit (fun () -> select_sentence_modes_flag:=true), "Select sencence mode";
  "--no-sel-modes", Arg.Unit (fun () -> select_sentence_modes_flag:=false), "Do not select sencence mode (default)";
  "--sel-sent", Arg.Unit (fun () -> select_sentences_flag:=true), "Select parsed sentences (default)";
  "--no-sel-sent", Arg.Unit (fun () -> select_sentences_flag:=false), "Do not select parsed sentences";
  "--sem", Arg.Unit (fun () -> semantic_processing_flag:=true), "Perform semantic processing (default)";
  "--no-sem", Arg.Unit (fun () -> semantic_processing_flag:=false; inference_flag:=false), "Do not perform semantic processing";
  "--disamb", Arg.Unit (fun () -> disambiguate_flag:=true; semantic_processing_flag:=true), "Disambiguate coordination (default)";
  "--no-disamb", Arg.Unit (fun () -> disambiguate_flag:=false), "Do not disambiguate coordination";
  "--infer", Arg.Unit (fun () -> inference_flag:=true; semantic_processing_flag:=true), "Apply inference rules (default)";
  "--no-infer", Arg.Unit (fun () -> inference_flag:=false), "Do not apply inference rules";
  "--sel-not-parsed", Arg.Unit (fun () -> select_not_parsed_flag:=true(*; semantic_processing_flag:=true*)), "Select not parsed";
  "--no-sel-not-parsed", Arg.Unit (fun () -> select_not_parsed_flag:=false), "Do not select not parsed (default)";
  "--json", Arg.Unit (fun () -> json_flag:=true), "Convert to json";
  "--no-json", Arg.Unit (fun () -> json_flag:=false), "Do not convert to json (default)";
  "--discontinuous", Arg.Unit (fun () -> discontinuous_parsing_flag:=true), "Parse discontinuous constituents";
  "--no-discontinuous", Arg.Unit (fun () -> discontinuous_parsing_flag:=false), "Do not parse discontinuous constituents (default)";
  "--partial", Arg.Unit (fun () -> ExecTypes.partial_parsing_flag:=true), "Build derivation trees for partially parsed sentences";
  "--no-partial", Arg.Unit (fun () -> ExecTypes.partial_parsing_flag:=false), "Do not build derivation trees for partially parsed sentences (default)";
(*  "--def-cat", Arg.Unit (fun () -> LCGlexiconTypes.default_category_flag:=true), "Create default semantic category for unknown tokens (default)";
  "--no-def-cat", Arg.Unit (fun () -> LCGlexiconTypes.default_category_flag:=false), "Do not create default semantic category for unknown tokens";*)
  "--internet-mode", Arg.Unit (fun () -> internet_mode:=true), "Relaxed attitude towards interpunction";
  "--no-internet-mode", Arg.Unit (fun () -> internet_mode:=false), "Strict attitude towards interpunction (default)";
  "--max-par-name-length", Arg.Int (fun v -> name_length:=v), "<val> Defines maximum length of paragraph name in visualization (default 20)";
  "--split-pattern", Arg.String (fun s -> split_pattern:=s), "<val> Defines splitting pattern for HTML marked text (default none)";
  "--correct-spelling", Arg.Unit (fun () -> correct_spelling_flag:=true), "Correct spelling errors before parsing";
  "--no-correct-spelling", Arg.Unit (fun () -> correct_spelling_flag:=false), "Do not correct spelling errors before parsing (default)";
  "--max-cost", Arg.Int (fun cost -> max_cost:=cost), "<cost> Maximal parsing cost (default 2)";
  "--line-mode", Arg.Unit (fun () -> line_mode:=true), "Line mode";
  "--no-line-mode", Arg.Unit (fun () -> line_mode:=false), "No line mode (default)";
  "--split-corpus", Arg.Unit (fun () -> split_corpus:=true), "Generate corpus spit into parsed and not parsed records";
  ]

let usage_msg =
  "Usage: domparser <options>\nInput is a sequence of lines. Empty line ends the sequence and invoke parsing. Double empty line shutdown parser.\nOptions are:"

let message = "ENIAM_LCGparser, semantic parser for Logical Categorial Grammar formalism\n\
Copyright (C) 2017 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>\n\
Copyright (C) 2017 Institute of Computer Science Polish Academy of Sciences"

let anon_fun s = raise (Arg.Bad ("invalid argument: " ^ s))

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
    let lines = if !correct_spelling_flag then Xlist.rev_map !lines FuzzyDetector.correct else List.rev !lines in
    String.concat "\n" lines, lines)

let process sub_in sub_out s =
(*   prerr_endline ("process 1: „" ^ s ^ "”"); *)
  let text,tokens =
    if !subsyntax_built_in then Subsyntax.catch_parse_text true false s else (
      try
      (* Printf.fprintf stdout "%s\n\n%!" text; *)
        Printf.fprintf sub_out "%s\n\n%!" s;
        (Marshal.from_channel sub_in : SubsyntaxTypes.text * SubsyntaxTypes.token_env ExtArray.t)
      with e -> AltText[Raw,RawText s;Error,ErrorText ("subsyntax_error: " ^ Printexc.to_string e)], ExtArray.make 0 SubsyntaxTypes.empty_token_env) in
(*   print_endline "process 2"; *)
  let lex_sems,msg = DomainLexSemantics.catch_assign2 tokens text in
    (* print_endline (LexSemanticsStringOf.string_of_lex_sems tokens lex_sems); *)
(*   print_endline "process 3"; *)
  let text = if msg <> "" then AltText[Raw,RawText s;Error,ErrorText ("lexsemantics_error: " ^ msg)] else text in
(*   print_endline "process 4"; *)
  let text = Exec.translate_text text in
(*    print_endline "process 5";  *)
  let text = Exec.parse !timeout !verbosity !max_cost !LCGlexiconTypes.rules !LCGlexiconTypes.dep_rules tokens lex_sems text in
(*   print_endline "process 6"; *)
  let text = if !disambiguate_flag then Exec.disambiguate text else text in (* przy !output = Text || !output = Marked poniższych nie ma *)
  let text = Exec.sort_arguments tokens text in
  let text = Exec.merge_mwe tokens text in
(*   print_endline "process 7"; *)
  let text = if !select_sentence_modes_flag then SelectSent.select_sentence_modes_text text else text in
  let text = if !select_sentences_flag then SelectSent.select_sentences_text ExecTypes.Struct text else text in
(*   print_endline "process 8"; *)
  let text = if !semantic_processing_flag then DomExec.semantic_processing !verbosity tokens lex_sems text else text in
  let text = if !semantic_processing_flag then DomExec.semantic_processing2 !verbosity tokens lex_sems text else text in
  let text = if !inference_flag then DomExec.merge_graph text else text in
  let text = if !inference_flag then Exec.apply_rules_text text else text in
  let text = if !inference_flag (*&& !output = JSON*) then Exec.validate text else text in
  let text = if !select_not_parsed_flag then Exec.select_not_parsed text else text in
  let text = Exec.aggregate_stats text in
  let status = Exec.aggregate_status text in
  let text = if not !json_flag then text else
    let json = if not !semantic_processing_flag then Exec.Json2.convert text else Json.normalize (Exec.Json2.convert text) in
    ExecTypes.JSONtext (Json.to_string "" json) in
(*   print_endline "process 9"; *)
  if !split_corpus then 
    if status then Printf.fprintf !parsed_file "%s\n%!" s
    else Printf.fprintf !not_parsed_file "%s\n%!" s;
  text,status,tokens,lex_sems
  
let rec main_loop sub_in sub_out in_chan out_chan =
  let text, lines = input_text in_chan in
  let raw_text = text in
  if text = "" then () else (
    if !line_mode then (match !output with
      | Html -> 
          File.file_out (!output_dir ^ "parsed_text.html") (fun file ->
            Printf.fprintf file "%s\n" Visualization.html_header;
            Xlist.iter lines (fun line ->
              let text,_,tokens,lex_sems = process sub_in sub_out line in
              if text <> ExecTypes.AltText [] then
              Printf.fprintf file "%s<BR>\n%!" (Visualization.html_of_text_as_paragraph !output_dir ExecTypes.Struct !img !verbosity tokens text));
            Printf.fprintf file "%s\n" Visualization.html_trailer)
      | Xml -> 
          File.file_out (!output_dir ^ "parsed_text.xml") (fun file ->
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
                Printf.fprintf out_chan "%s\n%!" (Json.to_string "" json);*)
(*                 let json2 = JSONconverter.add_text line ((*JSONconverter.convert_jstring_indexes*) (JSONconverter.convert_to_kuba json)) in *)
(*                 Printf.fprintf out_chan "%s\n%!" (JSONconverter.to_string "" json2) *)
              with e -> print_endline line; print_endline (Printexc.to_string e))
     | _ -> failwith "main_loop: ni") 
    else 
      let text,status,tokens,lex_sems = process sub_in sub_out text in
    (match !output with
    | Text -> Printf.fprintf out_chan "%s\n%!" (String.concat "\n" (Visualization.to_string_text !verbosity tokens text)) (* FIXME: obcinanie nazw *)
    (* | Marked -> Printf.fprintf out_chan "%s\n%!" (String.concat "\n" (Visualization.marked_string_of_text !verbosity tokens text)) *)
    (* | Marked -> Visualization.print_html_marked_text !output_dir "marked_text" text !img !verbosity tokens *)
(*     | Marked -> MarkedHTMLof.print_html_marked_simple_text !output_dir "marked_text" !name_length (MarkedHTMLof.marked_string_of_text !verbosity tokens text) *)
    | Html -> Visualization.print_html_text !output_dir "parsed_text" text !img !verbosity tokens
    | Xml -> Printf.fprintf out_chan "%s\n%!" (Xml.to_string_fmt (ExecXMLof.text "" text))
(*     | FStruct -> Printf.fprintf out_chan "%s\n%!" (Xml.to_string_fmt (FStruct.text "" text)) *)
    | JSON ->
(*         let json = Json.convert_text "" text in *)
(*         Printf.fprintf out_chan "%s\n%!" (Json.to_string "" json); *)
              (try
                match text with
                  ExecTypes.JSONtext s -> Printf.fprintf out_chan "%s\n%!" s
				| _ -> failwith "main_loop: json"
(*                let json = Json.add_text raw_text (Json.normalize (Json.convert text)) in
                Printf.fprintf out_chan "%s\n%!" (Json.to_string "" json);*)
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
    main_loop sub_in sub_out in_chan out_chan)

let get_sock_addr host_name port =
  let he = Unix.gethostbyname host_name in
  let addr = he.Unix.h_addr_list in
  Unix.ADDR_INET(addr.(0),port)

let _ =
  prerr_endline message;
  Arg.parse spec_list anon_fun usage_msg;
  SemTypes.user_ontology_flag := true;
  LCGlexicon.initialize ();
  DomainLexSemantics.initialize2 ();
  DomSemantics.initialize ();
  InferenceRulesParser.initialize ();
  Exec.initialize ();
(*   if !output = Marked then MarkedHTMLof.initialize (); *)
  let application_rules = if !internet_mode then LCGrules.application_rules_ignore_brackets else LCGrules.application_rules in
  if !discontinuous_parsing_flag then ExecTypes.lcg_rules := application_rules @ LCGrules.cross_composition_rules
  else ExecTypes.lcg_rules := application_rules;
  if !subsyntax_built_in (*|| !morphology_built_in*) then Subsyntax.initialize ();
  if !correct_spelling_flag then FuzzyDetector.initialize ();
  Gc.compact ();
  let sub_in,sub_out =
    if !subsyntax_built_in then stdin,stdout
    else Unix.open_connection (get_sock_addr !subsyntax_host !subsyntax_port) in
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
  if !comm_stdio then main_loop sub_in sub_out stdin stdout
  else
    let sockaddr = Unix.ADDR_INET(Unix.inet_addr_any,!port) in
    Unix.establish_server (main_loop sub_in sub_out) sockaddr

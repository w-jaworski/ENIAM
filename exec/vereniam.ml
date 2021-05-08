(*
 *  ENIAMexec implements ENIAM processing stream
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

type sentence_split = Full | Partial (*| None*)

let port = ref 5439
let subsyntax_built_in = ref true
let subsyntax_host = ref "localhost"
let subsyntax_port = ref 5739
let sentence_split = ref Full
let par_names = ref false
(*let morphology_built_in = ref true
let morphology_host = ref "localhost"
let morphology_port = ref 5440*)
let verbosity = ref 1
let timeout = ref 30.
let select_sentence_modes_flag = ref false
let select_sentences_flag = ref true
let semantic_processing_flag = ref true
let inference_flag = ref true
let discontinuous_parsing_flag = ref false
let correct_spelling_flag = ref false
let disambiguate_flag = ref true
let output_dir = ref "results/"
let max_cost = ref 2
let internet_mode = ref false
let debug_flag = ref false
let schema_filename = ref ""
let test_path = ref "."
let test_filename = ref ""
let result_type = ref ""

let check_filename s =
  if Xstring.check_sufix ".json" s then s else s ^ ".json"
  
let check_filename2 s =
  if Xstring.check_sufix ".tab" s then Xstring.cut_sufix ".tab" s else s
  
let check_path s =
  if Xstring.check_sufix "/" s then s else s ^ "/"
        

let spec_list = [
  "-e", Arg.String (fun s -> SubsyntaxTypes.theories:=s :: !SubsyntaxTypes.theories), "<theory> Add theory (may be used multiple times)";
  "-u", Arg.String (fun s -> SubsyntaxTypes.user_theories:=s :: !SubsyntaxTypes.user_theories), "<theory> Add user theory (may be used multiple times)";
  "-b", Arg.Unit (fun () -> subsyntax_built_in:=true), "Use built in version of ENIAMsubsyntax (default)";
  "--port", Arg.Int (fun p -> subsyntax_built_in:=false; subsyntax_port:=p), "<port> Connect to ENIAMsubsyntax on a given port";
  "--host", Arg.String (fun s -> subsyntax_built_in:=false; subsyntax_host:=s), "<hostname> Connect to ENIAMsubsyntax on a given host (by default localhost)";
  "-s", Arg.Unit (fun () -> sentence_split:=Full), "Split input into sentences for built-in subsyntax (default)";
  "-a", Arg.Unit (fun () -> sentence_split:=Partial), "Split input into paragraphs, do not split input into sentences, for built-in subsyntax";
  "--def-cat", Arg.Unit (fun () -> SubsyntaxTypes.default_category_flag:=true), "Create default semantic category for unknown tokens, for built-in subsyntax";
  "--no-def-cat", Arg.Unit (fun () -> SubsyntaxTypes.default_category_flag:=false), "Do not create default semantic category for unknown tokens, for built-in subsyntax (default)";
(*  "-b2", Arg.Unit (fun () -> morphology_built_in:=true), "Use built in version of ENIAMmorphology (default)";
  "--port2", Arg.Int (fun p -> morphology_built_in:=false; morphology_port:=p), "<port> Connect to ENIAMmorphology on a given port";
  "--host2", Arg.String (fun s -> morphology_built_in:=false; morphology_host:=s), "<hostname> Connect to ENIAMmorphology on a given host (by default localhost)";*)
  "--timeout", Arg.Float (fun x -> timeout:=x), "<seconds> Sets timeout value for parser (default 30 seconds)";
  "-v", Arg.Int (fun v -> verbosity:=v), "<val> Sets verbosity level of parser\n     0 - print only status information\n     1 - print data relevant to the status of a given sentence (default)\n     2 - print all data structures";
  "--output", Arg.String (fun s -> output_dir:=s ^ "/"), "<dir> Sets output directory (by default results/)";
  "-p", Arg.String (fun s -> test_path := check_path s), "<path> test path"; 
  "-f", Arg.String (fun s -> test_filename := check_filename2 s), "<filename> test filename";
  "-k", Arg.String (fun s -> schema_filename := check_filename s), "<filename> schema filename";
  "-r", Arg.String (fun s -> result_type := s), "<json type> result type";
  "--sel-modes", Arg.Unit (fun () -> select_sentence_modes_flag:=true), "Select sencence mode";
  "--no-sel-modes", Arg.Unit (fun () -> select_sentence_modes_flag:=false), "Do not select sencence mode (default)";
  "--sel-sent", Arg.Unit (fun () -> select_sentences_flag:=true), "Select parsed sentences (default)";
  "--no-sel-sent", Arg.Unit (fun () -> select_sentences_flag:=false), "Do not select parsed sentences";
  "--disamb", Arg.Unit (fun () -> disambiguate_flag:=true; semantic_processing_flag:=true), "Disambiguate coordination (default)";
  "--no-disamb", Arg.Unit (fun () -> disambiguate_flag:=false), "Do not disambiguate coordination";
  "--discontinuous", Arg.Unit (fun () -> discontinuous_parsing_flag:=true), "Parse discontinuous constituents";
  "--no-discontinuous", Arg.Unit (fun () -> discontinuous_parsing_flag:=false), "Do not parse discontinuous constituents (default)";
  "--partial", Arg.Unit (fun () -> ExecTypes.partial_parsing_flag:=true), "Build derivation trees for partially parsed sentences";
  "--no-partial", Arg.Unit (fun () -> ExecTypes.partial_parsing_flag:=false), "Do not build derivation trees for partially parsed sentences (default)";
  "--internet-mode", Arg.Unit (fun () -> internet_mode:=true), "Relaxed attitude towards interpunction";
  "--no-internet-mode", Arg.Unit (fun () -> internet_mode:=false), "Strict attitude towards interpunction (default)";
  "--correct-spelling", Arg.Unit (fun () -> correct_spelling_flag:=true), "Correct spelling errors before parsing";
  "--no-correct-spelling", Arg.Unit (fun () -> correct_spelling_flag:=false), "Do not correct spelling errors before parsing (default)";
  "--max-cost", Arg.Int (fun cost -> max_cost:=cost), "<cost> Maximal parsing cost (default 2)";
  "--debug", Arg.Unit (fun () -> debug_flag:=true), "Print information for debug network communication";
  ]

let usage_msg =
  "Usage: vereniam <options>\nParse a given file and check whether parsing results satisfy a given schema.\nOptions are:"

let message = "vereniam, semantic parser for Logical Categorial Grammar formalism and meaning representation structure verifier\n\
Copyright (C) 2021 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>"

(*let anon_fun s = 
  filename := s*)
let anon_fun s = raise (Arg.Bad ("invalid argument: " ^ s))

let process sub_in sub_out s =
  let pid = string_of_int (Unix.getpid ()) in
(*   prerr_endline ("process 1: „" ^ s ^ "”"); *)
  let text,tokens =
    if !subsyntax_built_in then 
      if !sentence_split = Full then Subsyntax.catch_parse_text true !par_names s
      else Subsyntax.catch_parse_text false !par_names s else (
      try
      (* Printf.fprintf stdout "%s\n\n%!" text; *)
        if !debug_flag then prerr_endline (pid ^ " Sending to subsyntax: " ^ String.escaped s);
        Printf.fprintf sub_out "%s\n\n%!" s;
        if !debug_flag then prerr_endline (pid ^ " Sent");
        (Marshal.from_channel sub_in : SubsyntaxTypes.text * SubsyntaxTypes.token_env ExtArray.t)
      with e -> AltText[Raw,RawText s;Error,ErrorText ("subsyntax_error: " ^ Printexc.to_string e)], ExtArray.make 0 SubsyntaxTypes.empty_token_env) in
  if !debug_flag then prerr_endline (pid ^ " Answer received from subsyntax");
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
  text,tokens,lex_sems
  
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
  Arg.parse spec_list anon_fun usage_msg;
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
  ignore (Sys.command ("mkdir -p " ^ !output_dir));
  SemTypes.user_ontology_flag := true;
  LCGlexicon.initialize ();
  DomainLexSemantics.initialize2 ();
  DomSemantics.initialize ();
  InferenceRulesParser.initialize ();
  let application_rules = if !internet_mode then LCGrules.application_rules_ignore_brackets else LCGrules.application_rules in
  if !discontinuous_parsing_flag then ExecTypes.lcg_rules := application_rules @ LCGrules.cross_composition_rules
  else ExecTypes.lcg_rules := application_rules;
  if !subsyntax_built_in then Subsyntax.initialize ();
  if !correct_spelling_flag then FuzzyDetector.initialize ();
  Gc.compact ();
  let sub_in,sub_out =
    if !subsyntax_built_in then stdin,stdout
    else Unix.open_connection (get_sock_addr !subsyntax_host !subsyntax_port) in
  prerr_endline "Ready!";
  let corpus = File.load_lines (!test_path ^ !test_filename ^ ".tab") in
  let parsed,not_parsed,not_verified,_ = 
    Xlist.fold corpus ([],[],[],1) (fun (parsed,not_parsed,not_verified,i) phrase ->
      Printf.printf "%d of %d %s\n%!" i (Xlist.size corpus) phrase;
      if phrase = "" then (print_endline "empty phrase"; (parsed,not_parsed,not_verified,i+1)) else (      
      let text,tokens,lex_sems = process sub_in sub_out phrase in
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
  File.file_out (!output_dir ^ "parsed_" ^ !test_filename ^ ".tab") (fun file ->
    Xlist.iter (List.rev parsed) (fun (s,_) -> Printf.fprintf file "%s\n" s));
  File.file_out (!output_dir ^ !test_filename ^ ".json") (fun file ->
    Printf.fprintf file "[\n%s\n]" (String.concat ",\n" (Xlist.rev_map parsed snd)));
  ignore (Sys.command ("rm -f " ^ !output_dir ^ "not_parsed_" ^ !test_filename ^ ".tab"));
  if Xlist.size not_parsed > 0 then File.file_out (!output_dir ^ "not_parsed_" ^ !test_filename ^ ".tab") (fun file ->
    Xlist.iter (List.rev not_parsed) (fun (s,_) -> Printf.fprintf file "%s\n" s));
  ignore (Sys.command ("rm -f " ^ !output_dir ^ "not_parsed_" ^ !test_filename ^ ".txt"));
  if Xlist.size not_parsed > 0 then File.file_out (!output_dir ^ "not_parsed_" ^ !test_filename ^ ".txt") (fun file ->
    Xlist.iter (List.rev not_parsed) (fun (_,s) -> Printf.fprintf file "%s\n\n" s));
  ignore (Sys.command ("rm -f " ^ !output_dir ^ "not_verified_" ^ !test_filename ^ ".tab"));
  if Xlist.size not_verified > 0 then File.file_out (!output_dir ^ "not_verified_" ^ !test_filename ^ ".tab") (fun file ->
    Xlist.iter (List.rev not_verified) (fun (s,_) -> Printf.fprintf file "%s\n" s));
  ignore (Sys.command ("rm -f " ^ !output_dir ^ "not_verified_" ^ !test_filename ^ ".txt"));
  if Xlist.size not_verified > 0 then File.file_out (!output_dir ^ "not_verified_" ^ !test_filename ^ ".txt") (fun file ->
    Xlist.iter (List.rev not_verified) (fun (_,s) -> Printf.fprintf file "%s\n\n" s));
  ()


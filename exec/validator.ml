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

let port = ref 5439
let subsyntax_built_in = ref true
let subsyntax_host = ref "localhost"
let subsyntax_port = ref 5739
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
let max_cost = ref 2
let internet_mode = ref false
let filename = ref ""

let spec_list = [
  "-e", Arg.String (fun s -> SubsyntaxTypes.theories:=s :: !SubsyntaxTypes.theories), "<theory> Add theory (may be used multiple times)";
  "-u", Arg.String (fun s -> SubsyntaxTypes.user_theories:=s :: !SubsyntaxTypes.user_theories), "<theory> Add user theory (may be used multiple times)";
  "-b", Arg.Unit (fun () -> subsyntax_built_in:=true), "Use built in version of ENIAMsubsyntax (default)";
  "--port", Arg.Int (fun p -> subsyntax_built_in:=false; subsyntax_port:=p), "<port> Connect to ENIAMsubsyntax on a given port";
  "--host", Arg.String (fun s -> subsyntax_built_in:=false; subsyntax_host:=s), "<hostname> Connect to ENIAMsubsyntax on a given host (by default localhost)";
(*  "-b2", Arg.Unit (fun () -> morphology_built_in:=true), "Use built in version of ENIAMmorphology (default)";
  "--port2", Arg.Int (fun p -> morphology_built_in:=false; morphology_port:=p), "<port> Connect to ENIAMmorphology on a given port";
  "--host2", Arg.String (fun s -> morphology_built_in:=false; morphology_host:=s), "<hostname> Connect to ENIAMmorphology on a given host (by default localhost)";*)
  "--timeout", Arg.Float (fun x -> timeout:=x), "<seconds> Sets timeout value for parser (default 30 seconds)";
  "-v", Arg.Int (fun v -> verbosity:=v), "<val> Sets verbosity level of parser\n     0 - print only status information\n     1 - print data relevant to the status of a given sentence (default)\n     2 - print all data structures";
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
  ]

let usage_msg =
  "Usage: validator <options>\nInput is a sequence of lines. Empty line ends the sequence and invoke parsing. Double empty line shutdown parser.\nOptions are:"

let message = "ENIAM_LCGparser, semantic parser for Logical Categorial Grammar formalism\n\
Copyright (C) 2017 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>\n\
Copyright (C) 2017 Institute of Computer Science Polish Academy of Sciences"

let anon_fun s = 
  filename := s

let process sub_in sub_out s =
(*   print_endline "process 1"; *)
  let text,tokens =
    if !subsyntax_built_in then Subsyntax.catch_parse_text true false s else (
      (* Printf.fprintf stdout "%s\n\n%!" text; *)
    Printf.fprintf sub_out "%s\n\n%!" s;
    (Marshal.from_channel sub_in : SubsyntaxTypes.text * SubsyntaxTypes.token_env ExtArray.t)) in
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

let validate sub_in sub_out = function
    Json.JObject(("text",Json.JString raw_text) :: _) as json1 -> 
      let raw_text2 = if !correct_spelling_flag then FuzzyDetector.correct raw_text else raw_text in
      let text,tokens,lex_sems = process sub_in sub_out raw_text2 in
      (try
         let json2 = Json.normalize (Exec.Json2.convert text) in
         if Json.simple_compare json1 json2 = 0 then () else
         Printf.printf "DIFFERENCE: %s\n%s\n%s\n%!" raw_text (Json.to_string "" json1) (Json.to_string "" json2)
      with e -> print_endline raw_text; print_endline (Printexc.to_string e))
  | _ -> failwith "validate"
  
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
  if !filename = "" then failwith "filename not given" else
  let json = File.load_file !filename in
  let l = match Json.of_string json with
      Json.JArray l -> l
    | _ -> failwith "validator" in
  Xlist.iter l (validate sub_in sub_out)

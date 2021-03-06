(*
 *  ENIAMexec implements ENIAM processing stream
 *  Copyright (C) 2017-2021 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
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
(* let output_dir = ref "results/" *)
let max_cost = ref 2
let internet_mode = ref false
(* let filename = ref "" *)
(* let raw_filename = ref "" *)
(* let omit_labels_flag = ref false *)
let debug_flag = ref false
let test_path = ref "."
let test_filename = ref ""
 
let check_filename s =
  if Xstring.check_sufix ".json" s then s else s ^ ".json"
  
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
(*   "--output", Arg.String (fun s -> output_dir:=s ^ "/"), "<dir> Sets output directory (by default results/)"; *)
  "-p", Arg.String (fun s -> test_path := check_path s), "<path> test path"; 
  "-f", Arg.String (fun s -> test_filename := check_filename s), "<filename> test filename";
(*   "--parsed", Arg.String (fun s -> filename:=s), "<filename> File with parsed data"; *)
(*   "--raw", Arg.String (fun s -> raw_filename:=s), "<filename> File with unprocessed data"; *)
(*   "--omit-labels", Arg.Unit (fun () -> omit_labels_flag:=true), "Omit labels"; *)
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
  "Usage: validator <options>\nReparses a given JSON file with parsed texts and checks whether representations obtained via parsing are identical with original ones.\nOptions are:"

let message = "validator of meaning representation generated by ENIAM\n\
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
  
let rec get_remove_alias_list rev = function
    ("alias", JString s) :: l -> s, List.rev rev @ l
  | t :: l -> get_remove_alias_list (t :: rev) l
  | [] -> raise Not_found
  
let rec get_remove_alias = function
    JObject["and",JArray l] -> 
      let s,l = Xlist.fold l ("",[]) (fun (s,l) t -> 
        let s,t = try get_remove_alias t with Not_found -> s, t in s, t :: l) in
      if s = "" then raise Not_found else s, JObject["and",JArray (List.rev l)]
  | JObject l -> let s,l = get_remove_alias_list [] l in s, JObject l
  | json -> print_endline ("get_remove_alias: " ^ json_to_string_fmt2 "" json); raise Not_found
  
let rec replace_text_list phrase rev = function
    ("text", _) :: l -> List.rev rev @ ["text",JString phrase] @ l
  | t :: l -> replace_text_list phrase (t :: rev) l
  | [] -> List.rev rev
  
let rec replace_text phrase = function
    JObject["and",JArray l] -> 
      let l = Xlist.fold l [] (fun l t -> 
        (replace_text phrase t) :: l) in
      JObject["and",JArray (List.rev l)]
  | JObject l -> let l = replace_text_list phrase [] l in JObject l
  | json -> print_endline ("replace_text: " ^ json_to_string_fmt2 "" json); raise Not_found
  
let validate sub_in sub_out t =
    let phrase = get_text t in
    let phrase,t = 
      try 
        let phrase,t = get_remove_alias t in
        phrase, replace_text phrase t
      with Not_found -> phrase,t in
    if phrase = "" then print_endline ("Text not found in \n" ^ json_to_string_fmt2 "" t) else (
      let text,tokens,lex_sems = process sub_in sub_out phrase in
      try
        let t2 = Json.normalize (Exec.Json2.convert false text) in
        if Json.simple_compare t t2 = 0 then () else (
        print_endline ("\n======================================\n\nOrig text: " ^ json_to_string_fmt2 "" t ^ ",");
        print_endline ("\nParsed text: " ^ json_to_string_fmt2 "" t2 ^ ","))
      with e -> print_endline phrase; print_endline (Printexc.to_string e))

  
(*let validate sub_in sub_out = function
    JObject(("text",JString raw_text) :: _) as json1 -> 
      let raw_text2 = if !correct_spelling_flag then FuzzyDetector.correct raw_text else raw_text in
      let text,tokens,lex_sems = process sub_in sub_out raw_text2 in
      (try
         let json2 = Json.normalize (Exec.Json2.convert false text) in
         if Json.simple_compare json1 json2 = 0 then () else
         Printf.printf "DIFFERENCE: %s\n%s\n%s\n%!" raw_text (json_to_string_fmt "" json1) (json_to_string_fmt "" json2)
      with e -> print_endline raw_text; print_endline (Printexc.to_string e))
  | _ -> failwith "validate"*)
  
(*let select_new raw_filename l =
  let known = Xlist.fold l StringSet.empty (fun known -> function 
      JObject(("text",JString raw_text) :: _) -> StringSet.add known raw_text
    | t -> failwith ("select_new 1: " ^ json_to_string_fmt "" t)) in
  let raw = File.load_tab raw_filename (function 
      [s] -> "",s
    | [i;s] -> i,s
    | _ -> failwith "select_new 2") in
  let raw = Xlist.fold raw [] (fun raw (i,s) -> if StringSet.mem known s then raw else (i,s) :: raw) in
  Xlist.iter (List.rev raw) (fun (i,s) ->
    print_endline (if !omit_labels_flag then s else i ^ "\t" ^ s))*)
    
 
let get_sock_addr host_name port =
  let he = Unix.gethostbyname host_name in
  let addr = he.Unix.h_addr_list in
  Unix.ADDR_INET(addr.(0),port)

let _ =
  prerr_endline message;
  Arg.parse spec_list anon_fun usage_msg;
  if !test_filename = "" then (
    print_endline "Test filename not provided";
    exit 1);
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
  let corpus = load_json_list (!test_path ^ !test_filename) in
  (*if !raw_filename = "" then*) Xlist.iter corpus (validate sub_in sub_out)
(*   else select_new !raw_filename l *)

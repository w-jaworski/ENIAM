open SubsyntaxTypes
open Xstd
open Array

type output = Text | Marked | Xml | Html | Marsh | Graphviz | Conll
type sentence_split = Full | Partial | None

let output = ref Text
let comm_stdio = ref true
let sentence_split = ref Full
let port = ref 789
let par_names = ref false
let output_dir = ref "results/"
let name_length = ref 20
let select_not_parsed = ref false
let sort_sentences = ref false

let spec_list = [
  "-e", Arg.String (fun s -> SubsyntaxTypes.theories:=s :: !SubsyntaxTypes.theories), "<theory> Add theory (may be used multiple times)";
  "-p", Arg.Int (fun p -> comm_stdio:=false; port:=p), "<port> Communication using sockets on given port number";
  "--coord-port", Arg.Int (fun p -> SubsyntaxTypes.coord_enabled:=true; SubsyntaxTypes.coord_port:=p), "<port> Connect to ENIAMcoordination on a given port";
  "--coord-host", Arg.String (fun s -> SubsyntaxTypes.coord_host_name:=s), "<hostname> Connect to ENIAMcoordination on a given host (by default localhost)";
  "--def-cat", Arg.Unit (fun () -> SubsyntaxTypes.default_category_flag:=true), "Create default semantic category for unknown tokens";
  ]

let usage_msg =
  "Usage: subsyntax <options>\nInput is a sequence of lines. Empty line ends the sequence and invoke parsing. Double empty line shutdown parser.\nOptions are:"

let message = "ENIAMsubsyntax: MWE, abbreviation and sentence detecion for Polish\n\
Copyright (C) 2016 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>\n\
Copyright (C) 2016 Institute of Computer Science Polish Academy of Sciences"

let anon_fun s = raise (Arg.Bad ("invalid argument: " ^ s))

let input_text channel =
  let s = ref (try input_line channel with End_of_file -> "") in
  let lines = ref [] in
  while !s <> "" do
    lines := !s :: !lines;
    s := try input_line channel with End_of_file -> ""
  done;
  String.concat "\n" (List.rev !lines)

let rec is_done tokens : bool =
	match tokens with
	[] -> false
	| x :: l -> (match x.token with
		Lemma("DONE", _, _, _) -> true
		| _ -> is_done l
		)

let rec main_loop in_chan out_chan =
  let text = input_text in_chan in
  if text = "" then () else (
    (* print_endline "input text begin";
    print_endline text;
    print_endline "input text end"; *)
    (if !sentence_split = Full || !sentence_split = Partial then
       let text,tokens(*,msg*) =
         if !sentence_split = Full then Subsyntax.catch_parse_text true !par_names text
         else Subsyntax.catch_parse_text false !par_names text in
			 let tokens,_,_,_ = !tokens in
			 if is_done (to_list tokens) then output_string out_chan "DONE token found" else output_string out_chan "No done token found"
    else
      let tokens,msg = Subsyntax.catch_parse text in
			(if msg = "" then output_string out_chan (SubsyntaxStringOf.token_list tokens ^ "\n\n")
			else output_string out_chan (text ^ "\n" ^ msg ^ "\n\n"))
      );
    flush out_chan;
    main_loop in_chan out_chan)

let _ =
  prerr_endline message;
(*   SubsyntaxTypes.theories_paths := theories_paths; *)
  Arg.parse spec_list anon_fun usage_msg;
  Subsyntax.initialize ();
  if !output = Marked then MarkedHTMLof.initialize ();
  Gc.compact ();
  prerr_endline "Ready!";
  if !comm_stdio then main_loop stdin stdout
  else
    let sockaddr = Unix.ADDR_INET(Unix.inet_addr_any,!port) in
    Unix.establish_server main_loop sockaddr

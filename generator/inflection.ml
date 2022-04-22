(*
 *  ENIAMinflection: tool for generating inflected forms of phrases
 *  Copyright (C) 2022 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
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

open Xstd
open Xjson

type mode = TAB | JSON | PATTERN

let mode = ref TAB
let input_path = ref ""
let output_path = ref ""

let check_path s =
  if Xstring.check_sufix "/" s then s else s ^ "/"
        
let spec_list = [
(*  "-c", Arg.String (fun s -> name:=s), "<name> Output category name (obligatory)";
  "-i", Arg.String (fun s -> in_filename:=s), "<filename> Input file (obligatory)";
    
  "-f", Arg.String (fun s -> test_filename := check_filename s), "<filename> test filename";
  "-t", Arg.String (fun s -> parsed_names := s :: !parsed_names), "<name> parsed name";*)
  
  "-p", Arg.String (fun s -> input_path := check_path s), "<path> input path";
  "-s", Arg.String (fun s -> output_path := check_path s), "<path> output path";
  "--tab", Arg.Unit (fun () -> mode := TAB), "generate inflected forms; one file per form";
  "--json", Arg.Unit (fun () -> mode := JSON), "generate a JSON file with inflected forms";
  "--pattern", Arg.Unit (fun () -> mode := PATTERN), "generate inflection patterns";
  ]

let usage_msg =
  "Usage: inflection <options>\nOptions are:"

let message = "inflection: tool for generating inflected forms of phrases"

let anon_fun s = raise (Arg.Bad ("invalid argument: " ^ s))


let print_table name_pref map =
  Xlist.iter map (fun (cat,forms) ->
    let forms = List.sort compare forms in
    File.file_out_append (name_pref ^ cat ^ ".tab") (fun file ->
      Xlist.iter forms (Printf.fprintf file "%s\n")))
  
let clear_line s = 
  if Xstring.check_sufix " " s then failwith ("clear_line space: " ^ s) else
  match Xstring.split "\t" s with
    [s] -> s
  | [s;_] -> s
  | [] -> s
  | _ -> failwith ("clear_line: " ^ s)

let make_unique = function
    JArray l -> 
      let map = Xlist.fold l StringMap.empty (fun map -> function
          JObject["lemma",JString lemma; "forms",JArray forms] -> StringMap.add_inc map lemma forms (fun forms2 -> forms @ forms2)
        | JObject["ndm-lemma",JString lemma] -> StringMap.add_inc map lemma [JString "ndm"] (fun forms2 -> (JString "ndm") :: forms2)
        | _ -> failwith "make_unique 2") in
      let l = StringMap.fold map [] (fun l lemma forms ->
        let map = Xlist.fold forms StringMap.empty (fun map t -> StringMap.add map (json_to_string t) t) in
        let forms = StringMap.fold map [] (fun l _ t -> t :: l) in
        JObject["lemma",JString lemma; "forms",JArray (List.rev forms)] :: l) in
      JArray (List.rev l)
  | _ -> failwith "make_unique 1"

let rec get_np_head = function
    (lemma,"subst",tags) :: _ -> lemma,"subst",tags
  | (lemma,"ndm",[]) :: (_,"adj",[n;c;g;_]) :: _ -> lemma,"subst",[n;c;g]
  | (lemma,pos,tags) :: l -> get_np_head l
  | [] -> failwith "get_np_head"
  
let rec get_adjp_head = function
    (lemma,"adj",tags) :: _ -> lemma,"adj",tags
  | (lemma,pos,tags) :: l -> get_adjp_head l
  | [] -> failwith "get_adjp_head"
  
let generate_dict_np phrases =
(*   print_endline "generate_dict_np 1"; *)
  JArray (List.rev (Xlist.rev_map phrases (fun (lemma,phrase) ->
    match phrase with
      [lemma,"ndm",[]] -> JObject["ndm-lemma",JString lemma]
    | _ -> 
      let _,_,tags = try get_np_head phrase with _ -> failwith ("generate_dict_np: " ^ lemma) in
      let numbers,gender = match tags with
          [SubsyntaxTypes.S "n";_;SubsyntaxTypes.V [gender]] -> Generator.numbers, gender
        | [SubsyntaxTypes.S "n";_;SubsyntaxTypes.V [gender];_] -> Generator.numbers, gender
        | [SubsyntaxTypes.V [number];_;SubsyntaxTypes.V [gender]] -> [number], gender
        | [SubsyntaxTypes.V [number];_;SubsyntaxTypes.V [gender];_] -> [number], gender
        | [SubsyntaxTypes.S n;_;_] -> failwith ("generate_dict_np 1: " ^ n)
        | [SubsyntaxTypes.V l;_;_] -> failwith ("generate_dict_np 2: " ^ String.concat "." l)
        | [_;_;SubsyntaxTypes.S n] -> failwith ("generate_dict_np 3: " ^ n)
        | [_;_;SubsyntaxTypes.V l] -> failwith ("generate_dict_np 4: " ^ String.concat "." l)
        | [_;_;_] -> failwith ("generate_dict_np 5: ")
        | _ -> failwith ("generate_dict_np: " ^ string_of_int (Xlist.size tags)) in
      let forms = Xlist.fold Generator.cases [] (fun forms case ->
        Xlist.fold numbers forms (fun forms number ->
          Xlist.fold (Generator.generate_np_number_case number case phrase) forms (fun forms l ->
            let orth = String.concat "" (Xlist.map l (fun i ->  i.Inflexion.lemma)) in
            (orth,number,case) :: forms))) in
      JObject["lemma",JString lemma; "forms",JArray
        (Xlist.map forms (fun (orth,number,case) -> 
          JObject["orth",JString orth;"number",JString number;"case",JString case;"gender", JString gender]))])))
          
let generate_dict_gerp phrases =
(*   print_endline "generate_dict_gerp 1"; *)
  JArray (List.rev (Xlist.rev_map phrases (fun (lemma,phrase) ->
    let gender = "n" in
    let forms = Xlist.fold Generator.cases [] (fun forms case ->
      Xlist.fold Generator.numbers forms (fun forms number ->
        Xlist.fold (Generator.generate_gerp_number_case number case phrase) forms (fun forms l ->
          let orth = String.concat "" (Xlist.map l (fun i ->  i.Inflexion.lemma)) in
          (orth,number,case) :: forms))) in
    JObject["lemma",JString lemma; "forms",JArray
      (Xlist.map forms (fun (orth,number,case) -> 
        JObject["orth",JString orth;"number",JString number;"case",JString case;"gender", JString gender]))])))
  
let generate_dict_adjp phrases =
(*   print_endline "generate_dict_adjp 1"; *)
  JArray (List.rev (Xlist.rev_map phrases (fun (lemma,phrase) ->
    let forms = Xlist.fold Generator.cases [] (fun forms case ->
      Xlist.fold Generator.numbers forms (fun forms number ->
        Xlist.fold Generator.genders forms (fun forms gender ->
          Xlist.fold (Generator.generate_adjp_number_case_gender number case gender phrase) forms (fun forms l ->
            let orth = String.concat "" (Xlist.map l (fun i ->  i.Inflexion.lemma)) in
            (orth,number,case,gender) :: forms)))) in
    JObject["lemma",JString lemma; "forms",JArray
      (Xlist.map forms (fun (orth,number,case,gender) -> 
        JObject["orth",JString orth;"number",JString number;"case",JString case;"gender", JString gender]))])))
 
let create_np_lexicons name pos number_flag in_filename out_path =
  let phrases = Xlist.map (File.load_lines in_filename) clear_line in
  let l = List.rev (Xlist.fold phrases [] (fun l phrase -> 
    try
      Xlist.fold (CanonicalParser.parse_np_nom number_flag phrase) l (fun l t -> (phrase, t) :: l)
    with 
      CanonicalParser.Strange -> print_endline ("strange " ^ name ^ " " ^ pos ^": " ^ phrase); l
    | CanonicalParser.PatternNotFound e -> print_endline ("not found " ^ name ^ " " ^ pos ^ ": " ^ phrase ^ "   " ^ e); l
    | Failure e -> print_endline ("failure " ^ name ^ " " ^ pos ^": " ^ phrase ^ "   " ^ e); l)) in
  if !mode = TAB then 
    let grouped_phrases = Generator.generate_case_grouped_np (Xlist.rev_map l snd) in
    print_table (out_path ^ name ^ ".np.") grouped_phrases
  else 
    let json = generate_dict_np l in
    File.file_out (out_path ^ name ^ ".np.json") (fun file ->
      Printf.fprintf file "%s\n" (json_to_string_fmt2 "" (make_unique json)))
  
let create_adjp_lexicons name pos in_filename out_path =
  let phrases = Xlist.map (File.load_lines in_filename) clear_line in
  let l = List.rev (Xlist.fold phrases [] (fun l phrase -> 
    try
      Xlist.fold (CanonicalParser.parse_adjp_sg_nom_m phrase) l (fun l t -> (phrase, t) :: l)
    with 
      CanonicalParser.Strange -> print_endline ("strange " ^ name ^ " " ^ pos ^": " ^ phrase); l
    | CanonicalParser.PatternNotFound e -> print_endline ("not found " ^ name ^ " " ^ pos ^ ": " ^ phrase ^ "   " ^ e); l
    | Failure e -> print_endline ("failure " ^ name ^ " " ^ pos ^": " ^ phrase ^ "   " ^ e); l)) in
  if !mode = TAB then 
    let grouped_phrases = Generator.generate_case_grouped_adjp (Xlist.rev_map l snd) in
    print_table (out_path ^ name ^ ".adjp.") grouped_phrases
  else 
    let json = generate_dict_adjp l in
    File.file_out (out_path ^ name ^ ".adjp.json") (fun file ->
      Printf.fprintf file "%s\n" (json_to_string_fmt2 "" (make_unique json)))
  
let copy_lexicon name in_filename out_path =
  let phrases = Xlist.map (File.load_lines in_filename) clear_line in
  let phrases = List.sort compare phrases in
  File.file_out (out_path ^ name ^ ".tab") (fun file ->
    Xlist.iter phrases (Printf.fprintf file "%s\n"))
  
let create_infp_lexicon name pos in_filename out_path =
  let phrases = Xlist.map (File.load_lines in_filename) clear_line in
  let phrases = List.sort compare phrases in
  let json = JArray (List.rev (Xlist.rev_map phrases (fun lemma ->
    JObject["lemma",JString lemma; "forms",JArray[JObject["orth",JString lemma]]]))) in
  File.file_out (out_path ^ name ^ ".infp.json") (fun file ->
    Printf.fprintf file "%s\n" (json_to_string_fmt2 "" (make_unique json)))
  
let create_ip_lexicons name pos in_filename out_path =
  let phrases = Xlist.map (File.load_lines in_filename) clear_line in
  let l = List.rev (Xlist.fold phrases [] (fun l phrase -> 
    try
      Xlist.fold (CanonicalParser.parse_infp phrase) l (fun l t -> (phrase, t) :: l)
    with 
      CanonicalParser.Strange -> print_endline ("strange " ^ name ^ " " ^ pos ^": " ^ phrase); l
    | CanonicalParser.PatternNotFound e -> print_endline ("not found " ^ name ^ " " ^ pos ^ ": " ^ phrase ^ "   " ^ e); l
    | Failure e -> print_endline ("failure " ^ name ^ " " ^ pos ^": " ^ phrase ^ "   " ^ e); l)) in
  if !mode = TAB then 
    let set = Xlist.fold l StringSet.empty (fun set (_,phrase) ->
      let fin_phrases = Generator.generate_ip "fin" "sg" "pri" phrase in
      let impt_phrases = Generator.generate_ip "impt" "sg" "sec" phrase in
      Xlist.fold (fin_phrases @ impt_phrases) set (fun set l ->
        let form = String.concat "" (Xlist.map l (fun i ->  i.Inflexion.lemma)) in
        StringSet.add set form)) in
    let phrases = List.sort compare (StringSet.to_list set) in
    File.file_out (out_path ^ name ^ ".ip.tab") (fun file ->
      Xlist.iter phrases (Printf.fprintf file "%s\n"))  
  else 
    let json = JArray (List.rev (Xlist.rev_map l (fun (lemma,phrase) ->
      let forms_fin = List.rev (Xlist.rev_map (Generator.generate_ip "fin" "sg" "pri" phrase) (fun l ->
        let orth = String.concat "" (Xlist.map l (fun i ->  i.Inflexion.lemma)) in
        JObject["orth",JString orth;"number",JString "sg";"person", JString "pri"])) in
      let forms_impt = List.rev (Xlist.rev_map (Generator.generate_ip "impt" "sg" "sec" phrase) (fun l ->
        let orth = String.concat "" (Xlist.map l (fun i ->  i.Inflexion.lemma)) in
        JObject["orth",JString orth;"number",JString "sg";"person", JString "sec"])) in
      JObject["lemma",JString lemma; "forms",JArray (forms_fin @ forms_impt)]))) in
    File.file_out (out_path ^ name ^ ".ip.json") (fun file ->
      Printf.fprintf file "%s\n" (json_to_string_fmt2 "" (make_unique json)))
 
let create_gerp_lexicons name pos in_filename out_path =
  let phrases = Xlist.map (File.load_lines in_filename) clear_line in
  let l = List.rev (Xlist.fold phrases [] (fun l phrase -> 
    try
      Xlist.fold (CanonicalParser.parse_infp phrase) l (fun l t -> (phrase, t) :: l)
    with 
      CanonicalParser.Strange -> print_endline ("strange " ^ name ^ " " ^ pos ^": " ^ phrase); l
    | CanonicalParser.PatternNotFound e -> print_endline ("not found " ^ name ^ " " ^ pos ^ ": " ^ phrase ^ "   " ^ e); l
    | Failure e -> print_endline ("failure " ^ name ^ " " ^ pos ^": " ^ phrase ^ "   " ^ e); l)) in
  if !mode = TAB then 
    let grouped_phrases = Generator.generate_case_grouped_gerp (Xlist.rev_map l snd) in
    print_table (out_path ^ name ^ ".np.") grouped_phrases
  else 
    let json = generate_dict_gerp l in
    File.file_out (out_path ^ name ^ ".gerp.json") (fun file ->
      Printf.fprintf file "%s\n" (json_to_string_fmt2 "" (make_unique json)))    
    
  
let make_string lemma pos tags =
  match pos with
    "fixed" | "qub" | "interp" | "adv" | "conj" | "ndm" -> lemma
  | "subst" | "adj" | "adja" | "inf" | "ger" ->
      let s = String.concat ":" (Xlist.map tags (function
          SubsyntaxTypes.V [s] -> s
        | SubsyntaxTypes.S v -> "$" ^ v
        | _ -> failwith "canonical_string")) in
      if s = "" then lemma ^ "(" ^ pos ^ ")" else lemma ^ "(" ^ pos ^ ":" ^ s ^ ")"
  | _ -> failwith ("make_string: " ^ lemma ^ ":" ^ pos)
 
let set_str case tags = 
  Xlist.map tags (function
      SubsyntaxTypes.S "str" -> SubsyntaxTypes.V [case]
    | t -> t) 
    
let add_noun_mwes name pos file number_flag filename =
  let phrases = Xlist.map (File.load_lines filename) clear_line in
  let l = List.rev (Xlist.fold phrases [] (fun l phrase -> 
    let tokens = Patterns.parse phrase in
    let tokens = CanonicalParser.disambiguate_tokens tokens in
    if Xlist.size tokens <= 1 then ((*print_endline ("omited " ^ name ^ " " ^ pos ^": " ^ phrase);*) l) else
    try
      Xlist.fold (CanonicalParser.parse_np_nom number_flag phrase) l (fun l tokens -> (phrase,tokens) :: l)
    with 
      CanonicalParser.Strange -> print_endline ("strange " ^ name ^ " " ^ pos ^": " ^ phrase); l
    | CanonicalParser.PatternNotFound e -> print_endline ("not found " ^ name ^ " " ^ pos ^": " ^ phrase ^ "   " ^ e); l)) in
  Xlist.iter l (fun (phrase,tokens) ->
    let _,hpos,htags = get_np_head tokens in
    Printf.fprintf file "%s\t%s\n" 
      (String.concat " " (Xlist.map tokens (fun (lemma,pos,tags) -> 
        make_string lemma pos tags)))
      (make_string phrase hpos htags));
  ()
  
let add_adj_mwes name pos file filename =
  let phrases = Xlist.map (File.load_lines filename) clear_line in
  let l = List.rev (Xlist.fold phrases [] (fun l phrase -> 
    let tokens = Patterns.parse phrase in
    let tokens = CanonicalParser.disambiguate_tokens tokens in
    if Xlist.size tokens <= 1 then ((*print_endline ("omited: " ^ phrase);*) l) else
    try
      Xlist.fold (CanonicalParser.parse_adjp_sg_nom_m phrase) l (fun l tokens -> (phrase,tokens) :: l)
    with 
      CanonicalParser.Strange -> print_endline ("strange " ^ name ^ " " ^ pos ^": " ^ phrase); l
    | CanonicalParser.PatternNotFound e -> print_endline ("not found " ^ name ^ " " ^ pos ^": " ^ phrase ^ "   " ^ e); l)) in
  Xlist.iter l (fun (phrase,tokens) ->
    let _,hpos,htags = get_adjp_head tokens in
    Printf.fprintf file "%s\t%s\n" 
      (String.concat " " (Xlist.map tokens (fun (lemma,pos,tags) -> 
        make_string lemma pos tags)))
      (make_string phrase hpos htags));
  ()
      
(*pozbyć(inf:perf) się	pozbyć się(inf:perf)
pozbyć(ger:$n:$c:n:perf:$a) się	pozbyć się(ger:$n:$c:n:perf:$a)
pozbyć(fin:$n:$p:perf) się	pozbyć się(fin:$n:$p:perf)
pozbyć(impt:$n:$p:perf) się	pozbyć się(impt:$n:$p:perf)
pozbyć(praet:$n:$g:perf) się	pozbyć się(praet:$n:$g:perf)*)
  
let add_verb_mwes name pos file filename =
(*   print_endline ("add_verb_mwes 1: " ^ name ^ " " ^ pos ^ " " ^ filename); *)
  let phrases = Xlist.map (File.load_lines filename) clear_line in
  let l = List.rev (Xlist.fold phrases [] (fun l phrase -> 
    let tokens = Patterns.parse phrase in
    let tokens = CanonicalParser.disambiguate_tokens tokens in
    if Xlist.size tokens <= 1 then ((*print_endline ("omited: " ^ phrase);*) l) else
    try
      Xlist.fold (CanonicalParser.parse_infp phrase) l (fun l tokens -> (phrase,tokens) :: l)
    with 
      CanonicalParser.Strange -> print_endline ("strange " ^ name ^ " " ^ pos ^": " ^ phrase); l
    | CanonicalParser.PatternNotFound e -> print_endline ("not found " ^ name ^ " " ^ pos ^ ": " ^ phrase ^ "   " ^ e); l)) in
(*   print_endline ("add_verb_mwes 2: |l|=" ^ string_of_int (Xlist.size l)); *)
  Xlist.iter l (fun (phrase,tokens) ->
    let hlemma,hpos,htags = List.hd tokens in
    if hpos <> "inf" then print_endline ("invalid phrase " ^ name ^ " " ^ pos ^": " ^ phrase) else
    Printf.fprintf file "%s\t%s\n" 
      (String.concat " " (Xlist.map tokens (fun (lemma,pos,tags) -> 
        make_string lemma pos (set_str "acc" tags))))
      (make_string phrase hpos htags);
    let htags2 = [SubsyntaxTypes.S "n"; SubsyntaxTypes.S "c"; SubsyntaxTypes.V ["n"]] @ htags @ [SubsyntaxTypes.S "a"] in
    let tokens2 = (hlemma,"ger",htags2) :: (List.tl tokens) in
    Printf.fprintf file "%s\t%s\n" 
      (String.concat " " (Xlist.map tokens2 (fun (lemma,pos,tags) -> 
        make_string lemma pos (set_str "gen" tags))))
      (make_string phrase "ger" htags2);
    ());
  ()
  
  
let create_valence_dic out_path =
  print_endline "create_valence_dic";
  let types = Xlist.fold (Array.to_list (Sys.readdir out_path)) [] (fun l s ->
    if Xstring.check_sufix ".tab" s then (Xstring.cut_sufix ".tab" s) :: l else
    if s = "valence.dic" then l else
    failwith ("create_valence_dic: " ^ s)) in
  File.file_out (out_path ^ "valence.dic") (fun file ->
    Printf.fprintf file "@PARAM_NAMES\n\n\n@SELPREF_NAMES\n\n  ";
    let _ = Xlist.fold types 0 (fun n typ ->
      if n = 3 then Printf.fprintf file "\n  ";
      Printf.fprintf file "⟨%s⟩ " typ;
      if n = 3 then 0 else n+1) in
    Printf.fprintf file "\n\n@ROLE_NAMES\n\n\n@LEXICON\n\n";
    Xlist.iter types (fun typ ->
      if typ = "" then () else
      Printf.fprintf file "include-lemmata=%s,pos2=fixed: ⟨%s⟩: ;\n" typ typ));
  ()
 
let load_valence path =
  let valence = DataLoader.load_lexicon (path ^ "valence.dic") in
  Xlist.fold valence StringMap.empty (fun map (i, selectors, cat, sense, schema) ->
    let poss,selectors = DataLoader.extract_selector i "pos2" [] selectors in
    let poss = StringSet.of_list poss in
    try
      let filenames,_ = DataLoader.check_extract_selector i "include-lemmata" [] selectors in
      Xlist.fold filenames map (fun map filename ->
        StringMap.add_inc map filename poss (fun poss2 -> StringSet.union poss poss2))
    with Not_found -> map)
     
let create_inflected in_path out_path =
  print_endline ("create_inflected 1: " ^ in_path);
  let valence = load_valence in_path in
  ignore(Sys.command ("mkdir -p " ^ out_path));
  ignore(Sys.command ("rm -f " ^ out_path ^ "*"));
  StringMap.iter valence (fun name poss ->
    let in_filename = in_path ^ name ^ ".tab" in
    StringSet.iter poss (fun pos -> 
      print_endline ("create_inflected 2: " ^ name ^ " " ^ pos);
      match pos with
        "noun" -> 
          let number_flag = if Xstring.check_prefix "Location" name then false else true in
          create_np_lexicons name pos number_flag in_filename out_path
      | "verb" -> 
          let name = if Xstring.check_sufix ".inf" name then Xstring.cut_sufix ".inf" name else name in
          create_ip_lexicons name pos in_filename out_path;
          if !mode = TAB then copy_lexicon (name ^ ".infp") in_filename out_path else create_infp_lexicon name pos in_filename out_path;
          create_gerp_lexicons name pos in_filename out_path
      | "adj" -> create_adjp_lexicons name pos in_filename out_path
      | _ -> print_endline ("NOT IMPLEMENTED: " ^ name ^ " " ^ pos)));
  if !mode = TAB then create_valence_dic out_path

let create_mwe_dict path =
  let valence = load_valence path in
  File.file_out (path ^ "mwe2.tab") (fun file ->
    StringMap.iter valence (fun name poss ->
      let filename = path ^ name ^ ".tab" in
      StringSet.iter poss (fun pos -> 
(*         print_endline (name ^ " " ^ pos); *)
        match pos with
          "noun" -> add_noun_mwes name pos file true filename
        | "verb" -> add_verb_mwes name pos file filename
        | "adj" -> add_adj_mwes name pos file filename
        | "fixed" -> ()
        | _ -> print_endline ("NOT IMPLEMENTED: " ^ name ^ " " ^ pos))))
 
let fixed = ["anty";"neo";"RF";"pół"]
 
let _ =
  Arg.parse spec_list anon_fun usage_msg;
  if !input_path = "" then (
    print_endline "Input path not provided";
    exit 1);
  if (!mode = TAB || !mode = JSON) && !output_path = "" then (
    print_endline "Output path not provided";
    exit 1);
  CanonicalParser.initialize fixed;
  if !mode = PATTERN then create_mwe_dict !input_path else
  create_inflected !input_path !output_path

 (* 12:46-13:05 *)

(*
 *  ENIAMsubsyntax: tokenization, lemmatization, MWE and sentence detecion for Polish
 *  Copyright (C) 2017-2018 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
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

open Xstd
open SubsyntaxTypes
open Inflexion

exception ParseError of string * string * int

type rel = Eq | Neq

let remove_comments line =
  try
    let n = String.index line '#' in
    String.sub line 0 n
  with Not_found -> line

let rec parse_param_names_rec i0 rev = function
    (i,"@ROLE_NAMES") :: tokens -> i, List.rev rev, (i,"@ROLE_NAMES") :: tokens
  | (i,"@SELPREF_NAMES") :: tokens -> i, List.rev rev, (i,"@SELPREF_NAMES") :: tokens
  | (i,"@LEXICON") :: tokens -> i, List.rev rev, (i,"@LEXICON") :: tokens
  | (i,t) :: tokens -> parse_param_names_rec i0 ((i,t) :: rev) tokens
  | [] -> raise (ParseError("parse_param_names_rec", "unexpexted end of input", i0))

let parse_param_names i0 = function
    (i,"@PARAM_NAMES") :: tokens -> parse_param_names_rec i [] tokens
  | (i,s) :: _ -> raise (ParseError("parse_param_names", "'@PARAM_NAMES' expected while '" ^ s ^ "' found", i))
  | [] -> raise (ParseError("parse_param_names", "unexpexted end of input", i0))

let rec parse_selpref_names_rec i0 rev = function
    (i,"@ROLE_NAMES") :: tokens -> i, List.rev rev, (i,"@ROLE_NAMES") :: tokens
  | (i,"@LEXICON") :: tokens -> i, List.rev rev, (i,"@LEXICON") :: tokens
  | (i,t) :: tokens -> parse_selpref_names_rec i0 ((i,t) :: rev) tokens
  | [] -> raise (ParseError("parse_selpref_names_rec", "unexpexted end of input", i0))

let parse_selpref_names i0 = function
    (i,"@SELPREF_NAMES") :: tokens -> parse_selpref_names_rec i [] tokens
  | (i,s) :: _ -> raise (ParseError("parse_selpref_names", "'@SELPREF_NAMES' expected while '" ^ s ^ "' found", i))
  | [] -> raise (ParseError("parse_selpref_names", "unexpexted end of input", i0))

let rec parse_role_names_rec i0 rev = function
    (i,"@LEXICON") :: tokens -> i, List.rev rev, (i,"@LEXICON") :: tokens
  | (i,t) :: tokens -> parse_role_names_rec i0 ((i,t) :: rev) tokens
  | [] -> raise (ParseError("parse_role_names_rec", "unexpexted end of input", i0))

let parse_role_names i0 = function
    (i,"@ROLE_NAMES") :: tokens -> parse_role_names_rec i [] tokens
  | (i,"@LEXICON") :: tokens -> i, [], (i,"@LEXICON") :: tokens
  | (i,s) :: _ -> raise (ParseError("parse_role_names", "'@ROLE_NAMES' expected while '" ^ s ^ "' found", i))
  | [] -> raise (ParseError("parse_role_names", "unexpexted end of input", i0))

let rec split_semic i0 found rev = function
    (i1,"lemma") :: (i2,"=") :: (i3,";") :: l -> split_semic (if rev = [] then i1 else i0) found ((i1,";") :: (i2,"=") :: (i3,"lemma") :: rev) l
  | (i,";") :: l -> split_semic i ((i0, List.rev rev) :: found) [] l
  | (i,s) :: l -> split_semic (if rev = [] then i else i0) found ((i,s) :: rev) l
  | [] -> if rev = [] then List.rev found else List.rev ((i0, List.rev rev) :: found)

let rec split_comma i0 found rev = function
    (i1,"lemma") :: (i2,"=") :: (i3,",") :: l -> split_comma (if rev = [] then i1 else i0) found ((i1,",") :: (i2,"=") :: (i3,"lemma") :: rev) l
  | (i,",") :: l -> split_comma i ((i0, List.rev rev) :: found) [] l
  | (i,s) :: l -> split_comma (if rev = [] then i else i0) found ((i,s) :: rev) l
  | [] -> if rev = [] then List.rev found else List.rev ((i0, List.rev rev) :: found)

let rec split_star i0 found rev = function
    (i,"*") :: l -> split_star i ((i0, List.rev rev) :: found) [] l
  | (i,s) :: l -> split_star (if rev = [] then i else i0) found ((i,s) :: rev) l
  | [] -> if rev = [] then List.rev found else List.rev ((i0, List.rev rev) :: found)

let rec split_plus i0 found rev = function
    (i,"+") :: l -> split_plus i ((i0, List.rev rev) :: found) [] l
  | (i,s) :: l -> split_plus (if rev = [] then i else i0) found ((i,s) :: rev) l
  | [] -> if rev = [] then List.rev found else List.rev ((i0, List.rev rev) :: found)

(*let catch_selector_of_string i proc s =
  try selector_of_string s
  with _ -> raise (ParseError(proc, "unknown selector: " ^ s, i))*)

let match_selectors = function
    i0,(i,s) :: l -> i,(*catch_selector_of_string i "match_selectors"*) s,l
  | i0,[] -> raise (ParseError("match_selectors", "empty", i0))

let match_relation = function
  (* cat,"=" :: "=" :: l -> cat,StrictEq,l *)
  | i,cat,(_,"!") :: (_,"=") :: l -> i,cat,Neq,l
  | i,cat,(_,"=") :: l -> i,cat,Eq,l
  | _,cat,(i,s) :: l -> raise (ParseError("match_relation", "relation symbol not found: " ^ String.concat " " (s :: Xlist.map l snd), i))
  | i,cat,[] -> raise (ParseError("match_relation", "empty", i))

let rec split_mid i0 rev = function
    (i1,s) :: (i2,"|") :: l -> split_mid i2 ((i1,s) :: rev) l
  | (i1,s1) :: (i2,s2) :: (i3,"|") :: l -> split_mid i3 ((i1,s1^" "^s2) :: rev) l
  | (i1,s1) :: (i2,s2) :: (i3,s3) :: (i4,"|") :: l -> split_mid i4 ((i1,s1^" "^s2^" "^s3) :: rev) l
  | (i1,s1) :: (i2,s2) :: (i3,s3) :: (i4,s4) :: (i5,"|") :: l -> split_mid i5 ((i1,s1^" "^s2^" "^s3^" "^s4) :: rev) l
  | [i,s] -> List.rev ((i,s) :: rev)
  | [i1,s1;i2,s2] -> List.rev ((i1,s1^" "^s2) :: rev)
  | [i1,s1;i2,s2;i3,s3] -> List.rev ((i1,s1^" "^s2^" "^s3) :: rev)
  | [i1,s1;i2,s2;i3,s3;i4,s4] -> List.rev ((i1,s1^" "^s2^" "^s3^" "^s4) :: rev)
  | [] -> raise (ParseError("split_mid", "empty", i0))
  | (i,s) :: l -> raise (ParseError("split_mid", "delimiter not found: " ^ String.concat " " (s :: Xlist.map l snd), i))


let rec check_value i0 selector l =
(*  let vals = try SelectorMap.find selector_values selector
    with Not_found -> raise (ParseError("check_value", "invalid selector: " ^ string_of_selector selector, i0)) in
  if vals = [] then () else
    Xlist.iter l (fun (i,s) ->
        if not (Xlist.mem vals s) then
          raise (ParseError("check_value", "invalid selector: " ^ string_of_selector selector ^ "=" ^ s, i)));*)
  Xlist.map l snd

let match_value = function
    i,cat,rel,[s] -> cat, rel, check_value i cat [s]
  | i,cat,rel,[] -> raise (ParseError("match_value", "empty", i))
  | i,cat,rel,l -> cat, rel, check_value i cat (split_mid i [] l)

let parse_selectors i0 l =
  (* print_endline s; *)
  (* let l = Xlist.map (Str.full_split (Str.regexp "|\\|,\\|=\\|!") s) (function
        Str.Text s -> s
      | Str.Delim s -> s) in *)
  let ll = split_comma i0 [] [] l in
  let l = Xlist.rev_map ll match_selectors in
  let l = Xlist.rev_map l match_relation in
  let l = Xlist.rev_map l match_value in
  l

let manage_lemmata = function
    (i1,"lemma") :: (i2,"=") :: (i3,":") :: (i4,",") :: tokens -> [i1,"lemma";i2,"=";i3,":";i4,","],tokens
  | (i1,"lemma") :: (i2,"=") :: (i3,":") :: (i4,s) :: (i5,",") :: tokens -> [i1,"lemma";i2,"=";i3,":"^s;i5,","],tokens
  | (i1,"lemma") :: (i2,"=") :: (i3,"<") :: (i4,"/") :: (i5,s) :: (i6,",") :: tokens -> [i1,"lemma";i2,"=";i3,"</"^s;i6,","],tokens
  | tokens -> [],tokens

let rec find_colon i0 rev = function
    (i1,"lemma") :: (i2,"=") :: (i3,":") :: l -> find_colon i0 ((i1,":") :: (i2,"=") :: (i3,"lemma") :: rev) l
  | (i,":") :: l -> i,List.rev rev, l
  | (i,s) :: l -> find_colon i0 ((i,s) :: rev) l
  | [] -> raise (ParseError("find_colon", "':' not found", i0))

let parse_cat i0 sefprefs = function
    [i,s] -> if not (StringSet.mem sefprefs s) then raise (ParseError("parse_cat", "unknown sel pref: '"^s^"'", i)) else s, ""
  | [i,s;_,"(";_,a1;_,")"] -> if not (StringSet.mem sefprefs s) then raise (ParseError("parse_cat", "unknown sel pref: '"^s^"'", i)) else s, a1
  | [i,s;_,"(";_,a1;_,a2;_,")"] -> if not (StringSet.mem sefprefs s) then raise (ParseError("parse_cat", "unknown sel pref: '"^s^"'", i)) else s, a1 ^ " " ^ a2
  | [i,s;_,"(";_,a1;_,a2;_,a3;_,")"] -> if not (StringSet.mem sefprefs s) then raise (ParseError("parse_cat", "unknown sel pref: '"^s^"'", i)) else s, a1 ^ " " ^ a2 ^ " " ^ a3
  | [i,s;_,"(";_,a1;_,a2;_,a3;_,a4;_,")"] -> if not (StringSet.mem sefprefs s) then raise (ParseError("parse_cat", "unknown sel pref: '"^s^"'", i)) else s, a1 ^ " " ^ a2 ^ " " ^ a3 ^ " " ^ a4
  | [] -> raise (ParseError("parse_cat", "empty", i0))
  | l -> raise (ParseError("parse_cat", "bad syntax: " ^ String.concat " " (Xlist.map l snd), i0))

let parse_entry i0 params sefprefs roles tokens =
  let prefix,tokens = manage_lemmata tokens in
  let i1,selectors,tokens = find_colon i0 [] tokens in
  let i2,cat,schema = find_colon i1 [] tokens in
  let selectors = parse_selectors i1 (prefix @ selectors) in
  let cat,sense = parse_cat i2 sefprefs cat in
(*  let schema = split_star i2 [] [] schema in
  let schema = Xlist.map schema (fun (i,l) -> parse_position i params sefprefs roles empty_position l) in*)
  i0, selectors, cat, sense, []

let string_of_parse_error filename proc s i line =
  Printf.sprintf "Valence dictionary error\nin file %s\nin line %d: %s\n%s: %s" filename i line proc s

let parse_lexicon filename i0 a params sefprefs roles = function
    (i,"@LEXICON") :: tokens ->
    let entries = split_semic i [] [] tokens in
    Xlist.fold entries ([],true) (fun (entries,is_correct) (i,entry) ->
      try (parse_entry i params sefprefs roles entry) :: entries, is_correct
      with ParseError(proc,s,i) ->
        print_endline (string_of_parse_error filename proc s i a.(i-1));
        entries,false)
  | (i,s) :: _ -> raise (ParseError("parse_lexicon", "'@LEXICON' expected while '" ^ s ^ "' found", i))
  | [] -> raise (ParseError("parse_lexicon", "unexpexted end of input", i0))

let load_lexicon filename =
  let lines = Xstring.split "\n" (File.load_file filename) in
  let a = Array.of_list lines in
  let lines,no_lines = Xlist.fold lines ([],1) (fun (lines,i) line -> (i,line) :: lines, i+1) in
  let lines = Xlist.rev_map lines (fun (i,line) -> i, remove_comments line) in
  let tokens = List.flatten (Xlist.rev_map lines (fun (i,line) ->
      Xlist.rev_map (Str.full_split
                       (Str.regexp "\\]\\| \\|\t\\|\r\\|\\?\\|:\\|;\\|&\\|!\\|=\\|}\\|{\\|,\\|\\*\\|/\\|\\+\\|)\\|(\\||\\|\\[\\|\\") line) (function
            Str.Text s -> i,s
          | Str.Delim s -> i,s))) in
  let tokens = Xlist.fold tokens [] (fun tokens -> function
        _," " -> tokens
      | _,"\t" -> tokens
      | _,"\r" -> tokens
      | i,t -> (i,t) :: tokens) in
  try
    let i,param_names,tokens = parse_param_names 1 tokens in
    let params = Xlist.fold param_names (StringSet.singleton "_") (fun params (_,param) -> StringSet.add params param) in
    let i,selpref_names,tokens = parse_selpref_names i tokens in
    let sefprefs = Xlist.fold selpref_names StringSet.empty (fun sefprefs (_,sefpref) -> StringSet.add sefprefs sefpref) in
    let i,role_names,tokens = parse_role_names i tokens in
    let roles = Xlist.fold role_names StringSet.empty (fun roles (_,role) -> StringSet.add roles role) in
    let lexicon,is_correct = parse_lexicon filename i a params sefprefs roles tokens in
    if is_correct then List.rev lexicon else exit 0
  with ParseError(proc,s,i) ->
    print_endline (string_of_parse_error filename proc s i a.(i-1));
    exit 0

let rec extract_selector i s rev = function
    (t,Eq,x) :: l -> if t = s then x, List.rev rev @ l else extract_selector i s ((t,Eq,x) :: rev) l
  | (t,e,x) :: l -> if t = s then failwith "extract_selector 1" else extract_selector i s ((t,e,x) :: rev) l
  | [] -> failwith ("extract_selector 2: " ^ (*string_of_selector*) s ^ " in line " ^ string_of_int i)

let rec check_extract_selector i s rev = function
    (t,Eq,x) :: l -> if t = s then x, List.rev rev @ l else check_extract_selector i s ((t,Eq,x) :: rev) l
  | (t,e,x) :: l -> if t = s then failwith "check_extract_selector 1" else check_extract_selector i s ((t,e,x) :: rev) l
  | [] -> raise Not_found

let rec check_selector i s = function
    (t,Eq,_) :: l -> if t = s then true else check_selector i s l
  | (t,e,x) :: l -> if t = s then failwith "check_selector" else check_selector i s l
  | [] -> false

let load_include_lemmata filename i data_path = function
    [filename2] ->
      (try
        List.flatten (File.load_tab (data_path ^ "/" ^ filename2 ^ ".tab") (function
          [] -> []
        | [s] -> [s,""](*Xlist.rev_map (Xstring.split "|" s) (fun t -> t,"")*)
        | [s;tags] -> [s,tags]
(*             Printf.printf "load_include_lemmata: %s\n%!" s; *)
(*             Xlist.rev_map (Xstring.split "|" s) (fun t -> t,tags) *)
        | line -> 
            print_endline (string_of_parse_error (data_path ^ "/" ^ filename2 ^ ".tab") 
              "load_include_lemmata" "invalid number of tabulators" i (String.concat "\t" line));exit 0))
      with Unix.Unix_error(Unix.ENOENT, "stat", filename2) ->
        print_endline (string_of_parse_error filename "load_include_lemmata" ("File " ^ filename2 ^ " not found") i "");
        exit 0)
  | l ->
        print_endline (string_of_parse_error filename "load_include_lemmata" ("Invalid filename: " ^ String.concat "|" l) i "");
        exit 0

let extract_valence_lemmata path filename map =
  let valence = load_lexicon (path ^ "/" ^ filename) in
  Xlist.fold valence map (fun map (i, selectors, cat, sense, schema) ->
    let poss,selectors = extract_selector i "pos2" [] selectors in
    let lemmata,selectors =
      try
        let filename2,selectors = check_extract_selector i "include-lemmata" [] selectors in
        load_include_lemmata (path ^ "/" ^ filename) i path filename2,selectors
      with Not_found -> 
        let lemmata,selectors = try check_extract_selector i "lemma" [] selectors with Not_found -> [],selectors in
        ((Xlist.rev_map lemmata (fun s -> s,"")),selectors) in
    let a = Xlist.fold selectors {number=""; gender=""; no_sgjp=false; poss_ndm=false; exact_case=false; ont_cat=cat; html_tags=[]} (fun a -> function
        "number", Eq, [n] -> {a with number=n}
      | "gender", Eq, [g] -> {a with gender=g}
      | "number", Neq, _ -> failwith "extract_valence_lemmata: number!=..."
      | "gender", Neq, _ -> failwith "extract_valence_lemmata: gender!=..."
      | _ -> a) in
    Xlist.fold lemmata map (fun map (lemma,tags) ->
(*       if lemma = "środa" || lemma = "Środa" || lemma = "Września" then print_endline lemma; *)
      let a = Xlist.fold (Xstring.split "|" tags) a (fun a -> function
          "sg" as x -> {a with number=x}
        | "pl" as x -> {a with number=x}
        | "m1" as x -> {a with gender=x}
        | "m2" as x -> {a with gender=x}
        | "m3" as x -> {a with gender=x}
        | "n" as x -> {a with gender=x}
        | "f" as x -> {a with gender=x}
        | "m1:pt" as x -> {a with gender=x}
        | "n:pt" as x -> {a with gender=x}
        | "n:col" as x -> {a with gender=x}
        | "n:ncol" as x -> {a with gender=x}
        | "no-sgjp" -> {a with no_sgjp=true}
        | "poss-ndm" -> {a with poss_ndm=true}
        | "exact-case" -> {a with exact_case=true}
        | "italic" -> {a with html_tags="i" :: a.html_tags}
        | "bold" -> {a with html_tags="b" :: a.html_tags}
        | "underline" -> {a with html_tags="u" :: a.html_tags}
        | s -> failwith ("extract_valence_lemmata: unknown tag '" ^ s ^ "' in lemma '" ^ lemma ^ "'")) in
      let map2 = try StringMap.find map lemma with Not_found -> StringMap.empty in
      let map2 = Xlist.fold poss map2 (fun map2 pos ->
        StringMap.add_inc map2 pos (OntSet.singleton a) (fun set -> OntSet.add set a)) in
      StringMap.add map lemma map2))

let extract_valence_pos path filename map =
  let valence = load_lexicon (path ^ "/" ^ filename) in
  Xlist.fold valence map (fun map (i, selectors, cat, sense, schema) ->
    if check_selector i "include-lemmata" selectors || check_selector i "lemma" selectors then map else
    let poss,selectors = extract_selector i "pos2" [] selectors in
    let modes = try fst (check_extract_selector i "mode" [] selectors) with Not_found -> [""] in
    let a = {number=""; gender=""; no_sgjp=false; poss_ndm=false; exact_case=false; ont_cat=cat; html_tags=[]} in
    Xlist.fold poss map (fun map pos ->
      Xlist.fold modes map (fun map mode ->
        StringMap.add_inc map (pos ^ ":" ^ mode) (OntSet.singleton a) (fun set -> OntSet.add set a))))
      

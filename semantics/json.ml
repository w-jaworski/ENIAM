(*
 *  ENIAMsemantics implements semantic processing for ENIAM
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
open SemTypes

type json =
    JObject of (string * json) list
  | JArray of json list
  | JString of string
  | JNumber of string
  | JNull
  | JTrue
  | JFalse
  | JEmpty
  | JContradiction

let escape s = (* FIXME: escapowanie \u2028 and \u2029 *)
  let t = Buffer.create (Xstring.size s) in
  Int.iter 0 (String.length s - 1) (fun i ->
    match String.get s i with
       '\b' -> Buffer.add_string t "\\b"
     | '\012' -> Buffer.add_string t "\\f"
     | '\n' -> Buffer.add_string t "\\n"
     | '\r' -> Buffer.add_string t "\\r"
     | '\t' -> Buffer.add_string t "\\t"
     | '"' -> Buffer.add_string t "\\\""
     | '\\' -> Buffer.add_string t "\\\\"
     | c -> 
        if Char.code c < 32 then Buffer.add_string t (Printf.sprintf "\\x%02X" (Char.code c))
        else Buffer.add_char t c);
  Buffer.contents t

  
let convert_comma n =
  match Xstring.split_delim "," n with
    [a;b] -> a ^ "." ^ b
  | [a] -> a
  | _ -> failwith ("convert_comma: " ^ n)

let rec to_string spaces = function
    JObject l -> "{" ^ String.concat "," (Xlist.map l (fun (k,v) ->
        Printf.sprintf "\n%s\"%s\": %s" spaces (escape k) (to_string (spaces ^ "  ") v))) ^ "}"
  | JArray l -> "[" ^ String.concat "," (Xlist.map l (fun v ->
        Printf.sprintf "\n%s%s" spaces (to_string (spaces ^ "  ") v))) ^ "]"
  | JString s -> "\"" ^ (escape s) ^ "\""
  | JNumber n -> convert_comma n
  | JNull -> "null"
  | JTrue -> "true"
  | JFalse -> "false"
  | JEmpty -> (*"\"empty\""*)"null"
  | JContradiction -> "contradiction"



type syntax =
      T of string
    | X of string
    | O of string
    | C of string
    | B of string * string * syntax list

let rec string_of_syntax = function
      O s -> "„" ^ s ^ "”"
    | X s -> s
    | T s -> "\"" ^ s ^ "\""
    | C s -> s
    | B(s,t,l) -> s ^ string_of_syntax_list l ^ t

and string_of_syntax_list l =
    String.concat "" (Xlist.map l string_of_syntax)

let rec find_atomic_symbols l =
    List.rev (Xlist.rev_map l (function
      T t -> T t
    | X "[" -> O "["
    | X "]" -> O "]"
    | X "{" -> O "{"
    | X "}" -> O "}"
    | X "," -> O ","
    | X ":" -> O ":"
    | X "null" -> C "null"
    | X "true" -> C "true"
    | X "false" -> C "false"
    | X x -> X x
    | _ -> failwith "find_atomic_symbols"))

let rec find_brackets brackets rev = function
      (O s) :: l ->
         (try
           let t = Xlist.assoc brackets s in
(*            print_endline ("find_brackets 1: " ^ string_of_syntax_list ((O s) :: l)); *)
           let found,l = find_rbracket t brackets [] l in
(*            print_endline ("find_brackets 2: " ^ string_of_syntax_list found); *)
           find_brackets brackets (B(s,t,found) :: rev) l
         with Not_found -> find_brackets brackets ((O s) :: rev) l)
    | B _ :: _ -> failwith "find_brackets"
    | t :: l -> find_brackets brackets (t :: rev) l
    | [] -> List.rev rev

and find_rbracket rb brackets rev = function
      (O s) :: l ->
         if s = rb then List.rev rev, l else
         (try
           let t = Xlist.assoc brackets s in
(*            print_endline ("find_rbracket 1: " ^ string_of_syntax_list ((O s) :: l)); *)
           let found,l = find_rbracket t brackets [] l in
(*            print_endline ("find_rbracket 2: " ^ string_of_syntax_list found); *)
           find_rbracket rb brackets ((B(s,t,found)) :: rev) l
         with Not_found -> find_rbracket rb brackets ((O s) :: rev) l)
    | (B _) :: _ -> failwith "find_rbracket 1"
    | t :: l -> find_rbracket rb brackets (t :: rev) l
    | [] -> failwith "find_rbracket 2"

let rec split_op_comma found rev = function
      (O ",") :: l -> split_op_comma ((List.rev rev) :: found) [] l
    | s :: l -> split_op_comma found (s :: rev) l
    | [] -> if rev = [] then List.rev found else List.rev ((List.rev rev) :: found)

let rec merge_quoted2 rev = function
      [] -> failwith "merge_quoted2"
    | "\"" :: tokens -> String.concat "" (List.rev rev), tokens
    | "\\" :: "\"" :: tokens -> merge_quoted2 ("\"" :: rev) tokens
    | "\\" :: "\\" :: tokens -> merge_quoted2 ("\\" :: rev) tokens
    | "\\" :: s :: tokens -> 
       (match String.get s 0 with
         'b' -> merge_quoted2 ("\b" :: rev) (Xstring.cut_prefix "b" s :: tokens)
	   | 'f' -> merge_quoted2 ("\012" :: rev) (Xstring.cut_prefix "f" s :: tokens)
	   | 'n' -> merge_quoted2 ("\n" :: rev) (Xstring.cut_prefix "n" s :: tokens)
	   | 'r' -> merge_quoted2 ("\r" :: rev) (Xstring.cut_prefix "r" s :: tokens)
	   | 't' -> merge_quoted2 ("\t" :: rev) (Xstring.cut_prefix "t" s :: tokens)
	   | _ -> failwith "merge_quoted2")
    | "\\" :: _ -> failwith "merge_quoted2"
    | s :: tokens -> merge_quoted2 (s :: rev) tokens

let rec merge_quoted rev = function
      [] -> List.rev rev
    | "\"" :: tokens -> let s, tokens = merge_quoted2 [] tokens in merge_quoted ((T s) :: rev) tokens
    | x :: tokens -> merge_quoted ((X x) :: rev) tokens

let is_number s =
  Int.fold 0 (String.length s-1) true (fun b i ->
    if String.get s i = '.' || String.get s i = '-' || (String.get s i >= '0' && String.get s i <= '9') then b else false)

let rec parse_tokens = function
    [B("[","]",l)] -> JArray(List.rev (Xlist.rev_map (split_op_comma [] [] l) parse_tokens))
  | [B("{","}",l)] -> JObject(List.rev (Xlist.rev_map (split_op_comma [] [] l) parse_entry))
  | [T t] -> JString t
  | [C "null"] -> JNull
  | [C "true"] -> JTrue
  | [C "false"] -> JFalse
  | [X x] -> if is_number x then JNumber x else failwith ("parse_tokens: " ^ x)
  | l -> failwith ("parse_tokens: " ^ string_of_syntax_list l)

and parse_entry = function
    T e :: O ":" :: l -> e, parse_tokens l
  | _ -> failwith "parse_entry"

let of_string s =
    let tokens = List.rev (Xlist.rev_map (Str.full_split
                         (Str.regexp "\\]\\| \\|\t\\|\n\\|\r\\|\\:\\|{\\|}\\|,\\|\\[\\|\"\\|\\") s) (function
              Str.Text s -> s
            | Str.Delim s -> s)) in
    let tokens = merge_quoted [] tokens in
    let tokens = List.rev (Xlist.fold tokens [] (fun tokens -> function
          X " " -> tokens
        | X "\t" -> tokens
        | X "\n" -> tokens
        | X "\r" -> tokens
        | t -> t :: tokens)) in
    let l = find_atomic_symbols tokens in
    let l = find_brackets ["{","}";"[","]"] [] l in
    parse_tokens l

let rec validate_linear_term r = function
    Concept{cat="JArray"; sense=sense; relations=Dot; contents=contents} ->
      if sense <> "and" && sense <> "or" && sense <> "and-tuple" && sense <> "or-tuple" && sense <> "with" then r := ("invalid sense for JArray: " ^ sense) :: !r;
(*       Xlist.iter contents (validate_linear_term r) *)
      validate_contents r contents
  | Concept{cat="JObject"; sense=""; relations=relations; contents=Dot} ->
      validate_relations r relations
  | Concept{cat="JNumber"; sense=sense; relations=Dot; contents=Dot} ->
      if sense = "" then r := "empty sense in JNumber" :: !r
  | Concept{cat="JString"; sense=sense; relations=Dot; contents=Dot} ->
      if sense = "" then r := "empty sense in JString" :: !r
  | Concept{cat="JStringE"; sense=sense; relations=Dot; contents=Dot} -> ()
  | Concept{cat="JEmpty"; sense=""; relations=Dot; contents=Dot} -> ()
  | Concept{cat="JContradiction"; sense=""; relations=Dot; contents=Dot} -> ()
  | Variant(e,l) -> Xlist.iter l (fun (_,t) -> validate_linear_term r t)
  | Tuple l -> Xlist.iter l (validate_linear_term r) (* FIXME: proteza *)
  | Dot -> () (* FIXME: proteza *)
  | t -> r := ("validate_linear_term: " ^ SemStringOf.linear_term 0 t) :: !r

and validate_relations r = function
    Dot -> ()
  | Relation(_,"",t) -> validate_linear_term r t
  | Tuple l -> Xlist.iter l (validate_relations r)
(*   | Variant(e,l) -> Xlist.iter l (fun (_,t) -> validate_relations r t) *)
  | t -> r := ("validate_relations: " ^ SemStringOf.linear_term 0 t) :: !r

and validate_contents r = function
  | Tuple l -> Xlist.iter l (validate_contents r)
  | Dot -> ()
  | t -> validate_linear_term r t

let make_with = function
    [t] -> t
  | l -> JObject["with",JArray l]

let make_and = function
    [t] -> t
  | l -> JObject["and",JArray l]

let make_and_tuple = function
    [t] -> t
  | l -> JObject["and-tuple",JArray l]

let rec convert_rels = function
    Tuple l -> List.flatten (Xlist.rev_map l convert_rels)
  | Relation(s,"",t) -> [s,t]
  | Dot -> []
  | t -> print_endline ("convert_rels: " ^ SemStringOf.linear_term 0 t); failwith "convert_rels"

let rec convert_linear_term = function
    Concept{cat="JArray"; sense=sense; relations=Dot; contents=contents} ->
      JObject[sense,JArray(convert_contents contents)]
  | Concept{cat="JObject"; sense=""; relations=relations; contents=Dot} ->
      JObject(convert_relations relations)
  | Concept{cat="JNumber"; sense=sense; relations=Dot; contents=Dot} -> JNumber sense
  | Concept{cat="JString"; sense=sense; relations=Dot; contents=Dot} -> JString sense
  | Concept{cat="JStringE"; sense=sense; relations=Dot; contents=Dot} -> JString sense
  | Concept{cat="JEmpty"; sense=""; relations=Dot; contents=Dot} -> JEmpty
  | Concept{cat="JContradiction"; sense=""; relations=Dot; contents=Dot} -> JContradiction
  | Variant(e,l) -> JObject["with",JArray(Xlist.rev_map l (fun (_,t) -> convert_linear_term t))]
  | Tuple l -> JObject["and",JArray(Xlist.rev_map l convert_linear_term)] (* FIXME: proteza *)
  | Dot -> JObject["with",JArray []] (* FIXME: proteza *)
  | t -> failwith ("convert_linear_term: " ^ SemStringOf.linear_term 0 t)

and convert_relations = function
    Dot -> []
  | Relation(s,"",t) -> [s,convert_linear_term t]
  | Tuple l -> List.flatten (Xlist.rev_map l convert_relations)
  | t -> failwith ("convert_relations: " ^ SemStringOf.linear_term 0 t)

and convert_contents = function
  | Tuple l -> List.flatten (List.rev (Xlist.rev_map l convert_contents))
  | Dot -> []
  | t -> [convert_linear_term t]


let add_text text = function
    JObject l -> JObject(("text",JString text) :: l)
  | t -> print_endline ("add_text: " ^ to_string "" t); failwith "add_text"

exception Contradiction

let rec normalize_contradiction_rec = function
    JObject["with",JArray l] ->
      let l = Xlist.fold l [] (fun l t ->
        try normalize_contradiction_rec t :: l
        with Contradiction -> l) in
	  if l = [] then raise Contradiction else JObject["with",JArray l]
  | JObject l -> JObject(List.rev (Xlist.rev_map l (fun (k,t) -> k,normalize_contradiction_rec t)))
  | JArray l -> JArray(List.rev (Xlist.rev_map l normalize_contradiction_rec))
  | JContradiction -> raise Contradiction
  | t -> t

let normalize_contradiction t =
  try normalize_contradiction_rec t with Contradiction -> JObject["error", JString "contradiction"]

let compare_fst (a,_) (b,_) = compare a b

let rec extract_simple_jobject = function
	JObject[s,JArray l] as t ->
	  (try
	    let map = Xlist.fold l StringMap.empty (fun map -> function
	        JObject["with",_] -> raise Not_found
		  | JObject["and",_] -> raise Not_found
		  | JObject["and-tuple",_] -> raise Not_found
		  | JObject["or",_] -> raise Not_found
		  | JObject["or-tuple",_] -> raise Not_found
		  | JObject[s,t] -> StringMap.add_inc map s [t] (fun l -> t :: l)
		  | t -> raise Not_found) in
		let l = List.sort compare_fst (StringMap.fold map [] (fun l k t -> (k,t) :: l)) in
		match l with
          [] -> failwith "extract_simple_jobject 2"
	    | [t,l] -> JObject[t,extract_simple_jobject (JObject[s,JArray(List.rev l)])]
	    | l -> raise Not_found
	  with Not_found -> t)
  | _ -> failwith "extract_simple_jobject 1"

(*(* SELIDOR *)
let can_combine = StringSet.of_list [
  "text"; "client_declaration"; "client_question";
  "time"; "action"; "organisation"; "doer"; "patient"; "service"; "location"; "price"; "rating ";
  "hour"; "minute"; "monthday"; "time_of_day"; "weekday"; "month"; "week"; "year";
  "name"; "attitude"; "type"; "first_name"; "last_name"; "profession"; "initial";
  "person"; "animal"; "artefact"; "part"; "param"; "value"; "organization"; 
  "town"; "quarter"; "street"; "street_name"; "house_number"; "postal_code"]

let can_combine_or = StringSet.of_list [
   "text"; "client_declaration"; "client_question";
   ]*)
 
(*(* LEKSEEK *)
let can_combine = StringSet.of_list [
  "contained"; "container"; "count"; "drug_form_attr"; "drug_form"; "unit"; "amount"; 
  "chem_compound"; "chem_fun_group"; "substance"; "substance_form"; 
  "id"; "name"; "text"; "data"; "error"; "has"; "substance"; "caused"; "microbe"; 
  "extract"; "herb"; "herb_component"; "herb_adj"; "chem_attr"; "attr"; "substance_name"; 
  "rel"; "commit"; "equiv"; "corresponds_to"; "in_the_form_of"; "as"; "equivalent"; "including"; 
  "dose"; "patient"; "time"; "frequency"; "modality"; "prescribe"; "greater"; "per"; "attr"; 
  "person"; ""; "";  ]

let can_combine_or = StringSet.of_list []*)

let rec combine_jobjects = function
	JObject["and",JArray l] as t ->
	  (try
	    let map = Xlist.fold l StringMap.empty (fun map -> function
	        JObject["with",_] -> raise Not_found
		  | JObject["and",_] -> raise Not_found
		  | JObject["or",_] -> raise Not_found
		  | JObject l ->
		      Xlist.fold l map (fun map (s,t) ->
		        if StringSet.mem !InferenceRulesParser.can_combine s then
		          StringMap.add_inc map s [t] (fun l -> t :: l)
				else raise Not_found)
		  | t -> raise Not_found) in
		let l = List.sort compare_fst (StringMap.fold map [] (fun l k t -> (k,t) :: l)) in
		JObject (List.rev (Xlist.rev_map l (fun (k,l) ->
		  match l with
            [] -> failwith "combine_objects 2"
	      | [t] -> k, t
	      | l -> k, combine_jobjects (JObject["and",JArray(List.rev l)]))))
	  with Not_found -> t)
  | JObject["or",JArray l] as t ->
	  (try
	    let map = Xlist.fold l StringMap.empty (fun map -> function
	        JObject["with",_] -> raise Not_found
		  | JObject["and",_] -> raise Not_found
		  | JObject["or",_] -> raise Not_found
		  | JObject[s,t] -> if StringSet.mem !InferenceRulesParser.can_combine s then StringMap.add_inc map s [t] (fun l -> t :: l) else raise Not_found
(* 		  | JObject l -> raise Not_found *)
(*		      Xlist.fold l map (fun map (s,t) ->
		        (*if StringSet.mem can_combine_or s then
		          StringMap.add_inc map s [t] (fun l -> t :: l)
				else*) raise Not_found)*)
		  | t -> raise Not_found) in
		let l = List.sort compare_fst (StringMap.fold map [] (fun l k t -> (k,t) :: l)) in
		(match l with
		  [k,l2] -> 
		  (match l2 with
            [] -> failwith "combine_objects 2"
	      | [t] -> JObject[k, t]
	      | l -> JObject[k, combine_jobjects (JObject["or",JArray(List.rev l2)])])
        | _ -> raise Not_found)
	  with Not_found -> t)
  | _ -> failwith "combine_objects 1"

let rec normalize_rec = function
	JObject["with",JArray l] ->
(*  	  print_endline ("normalize_rec with 1: " ^ to_string "" (JObject["with",JArray l]));  *)
      let l = Xlist.rev_map l normalize_rec in
(*  	  print_endline ("normalize_rec with 2: " ^ to_string "" (JObject["with",JArray l]));  *)
      let l = List.flatten (Xlist.rev_map l (function JObject["with",JArray l] -> l | t -> [t])) in
(*  	  print_endline ("normalize_rec with 3: " ^ to_string "" (JObject["with",JArray l]));  *)
      let map = Xlist.fold l StringMap.empty (fun map t -> StringMap.add map (to_string "" t) t) in
      let l = List.sort compare_fst (StringMap.fold map [] (fun l k t -> (k,t) :: l)) in
      let l = List.rev (Xlist.rev_map l snd) in
      (match l with
        [] -> JObject["with",JArray []]
	  | [t] -> t
	  | l -> extract_simple_jobject (JObject["with",JArray l]))
  | JObject["and",JArray l] ->
(*       print_endline ("normalize_rec and: " ^ to_string "" (JObject["and",JArray l])); *)
      let l = Xlist.rev_map l normalize_rec in
      let l = List.flatten (Xlist.rev_map l (function JObject["and",JArray l] -> l | JEmpty -> [] | t -> [t])) in
      let map = Xlist.fold l StringMap.empty (fun map t -> StringMap.add map (to_string "" t) t) in
      let l = List.sort compare_fst (StringMap.fold map [] (fun l k t -> (k,t) :: l)) in
      let l = List.rev (Xlist.rev_map l snd) in
      (match l with
        [] -> JEmpty
	  | [t] -> t
	  | l -> combine_jobjects (JObject["and",JArray l]))
  | JObject["or",JArray l] -> (* FIXME: jak należy traktować JEmpty? *)
(*       print_endline ("normalize_rec or: " ^ to_string "" (JObject["or",JArray l])); *)
      let l = Xlist.rev_map l normalize_rec in
      let l = List.flatten (Xlist.rev_map l (function JObject["or",JArray l] -> l | JEmpty -> [] | t -> [t])) in
      let map = Xlist.fold l StringMap.empty (fun map t -> StringMap.add map (to_string "" t) t) in
      let l = List.sort compare_fst (StringMap.fold map [] (fun l k t -> (k,t) :: l)) in
      let l = List.rev (Xlist.rev_map l snd) in
      (match l with
        [] -> JEmpty
	  | [t] -> t
	  | l -> combine_jobjects (JObject["or",JArray l]))
  | JObject["and-tuple",JArray l] ->
(*       print_endline ("normalize_rec and: " ^ to_string "" (JObject["and",JArray l])); *)
      let l = Xlist.rev_map l normalize_rec in
      let l = List.flatten (Xlist.rev_map l (function JObject["and-tuple",JArray l] -> l | JEmpty -> [] | t -> [t])) in
      let map = Xlist.fold l StringMap.empty (fun map t -> StringMap.add map (to_string "" t) t) in
      let l = List.sort compare_fst (StringMap.fold map [] (fun l k t -> (k,t) :: l)) in
      let l = List.rev (Xlist.rev_map l snd) in
      (match l with
        [] -> JEmpty
	  | [t] -> t
	  | l -> JObject["and-tuple",JArray l])
(*      let l = List.rev (Xlist.rev_map l normalize_rec) in
      JObject["and-tuple",JArray l]*)
  | JObject["or-tuple",JArray l] ->
(*       print_endline ("normalize_rec and: " ^ to_string "" (JObject["and",JArray l])); *)
      let l = List.rev (Xlist.rev_map l normalize_rec) in
      JObject["or-tuple",JArray l]
  | JObject l ->
(*       print_endline ("normalize_rec JObject: " ^ to_string "" (JObject l)); *)
      let l = Xlist.rev_map l (fun (k,t) -> k,normalize_rec t) in
      let l = List.sort compare_fst l in
      JObject l
(*   JObject(List.rev (Xlist.rev_map l (fun (k,t) -> k,normalize_rec t))) *)
  | JArray l -> (*JArray(List.rev (Xlist.rev_map l normalize_rec))*) 
      let l = Xlist.rev_map l (fun t -> to_string "" t,normalize_rec t) in
      let l = List.sort compare_fst l in
      let l = List.rev (Xlist.rev_map l snd) in
      JArray l
  | t -> t

let priority = Xlist.fold [
  "name",1;
  "id",2;
  "text",3;
  "data",4;
  "error",4;
  ] StringMap.empty (fun map (k,v) -> StringMap.add map k v)
  
let sort_jobject = function
    JObject l -> 
      let l = Xlist.rev_map l (fun (k,t) -> (try StringMap.find priority k with Not_found -> max_int), (k,t)) in
      let l = List.sort compare_fst l in
      let l = List.rev (Xlist.rev_map l snd) in
      JObject l
  | _ -> failwith "sort_jobject"

let normalize t =
  let t = normalize_contradiction t in
  let t = normalize_rec t in
  let t = normalize_rec t in
  let t = sort_jobject t in
  t

let simple_compare s t =
  compare (to_string "" s) (to_string "" t)
  

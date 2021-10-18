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
open Xjson

let num_of_string_convert_comma n =
  match Xstring.split_delim "," n with
    [a;b] -> Num.div_num (Num.num_of_string (a ^ b)) (Num.power_num (Num.Int 10) (Num.Int (Xstring.size b)))
  | [a] -> Num.num_of_string a
  | _ -> failwith ("num_of_string_convert_comma: " ^ n)

let rec validate_linear_term r = function
    Concept{cat="JArray"; sense=sense; relations=Dot; contents=contents} ->
      if sense <> "list" && sense <> "and" && sense <> "or" && sense <> "and-tuple" && sense <> "or-tuple" && sense <> "with" && sense <> "add" && sense <> "multiply" then r := ("invalid sense for JArray: '" ^ sense ^ "'") :: !r;
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
  | t -> print_endline ("add_text: " ^ json_to_string_fmt "" t); failwith "add_text"

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
	    let map,rest = Xlist.fold l (StringMap.empty,[]) (fun (map,rest) -> function
	        JObject["with",_] as t -> map, t :: rest
		  | JObject["and",_] as t -> map, t :: rest
		  | JObject["or",_] as t -> map, t :: rest
		  | JObject l as t ->
		      (try
		      Xlist.fold l map (fun map (s,t) ->
		        if StringSet.mem !InferenceRulesParser.can_combine s then
		          StringMap.add_inc map s [t] (fun l -> t :: l)
				else raise Not_found), rest
              with Not_found -> map, t :: rest)
		  | t -> map, t :: rest) in
        if StringMap.is_empty map then t else
		let l = List.sort compare_fst (StringMap.fold map [] (fun l k t -> (k,t) :: l)) in
		let comb = JObject (List.rev (Xlist.rev_map l (fun (k,l) ->
		  match l with
            [] -> failwith "combine_objects 2"
	      | [t] -> k, t
	      | l -> k, combine_jobjects (JObject["and",JArray(List.rev l)])))) in
        if rest = [] then comb else JObject["and",JArray (comb :: rest)]
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
(*   	  print_endline ("normalize_rec with 1: " ^ json_to_string_fmt "" (JObject["with",JArray l]));   *)
      let l = Xlist.rev_map l normalize_rec in
(*  	  print_endline ("normalize_rec with 2: " ^ json_to_string_fmt "" (JObject["with",JArray l]));  *)
      let l = List.flatten (Xlist.rev_map l (function JObject["with",JArray l] -> l | t -> [t])) in
(*  	  print_endline ("normalize_rec with 3: " ^ json_to_string_fmt "" (JObject["with",JArray l]));  *)
      let map = Xlist.fold l StringMap.empty (fun map t -> StringMap.add map (json_to_string_fmt "" t) t) in
      let b,map = 
        if StringMap.mem map (json_to_string_fmt "" JEmpty) then 
          true,StringMap.remove map (json_to_string_fmt "" JEmpty) else false,map in
      let l = List.sort compare_fst (StringMap.fold map [] (fun l k t -> (k,t) :: l)) in
      let l = List.rev (Xlist.rev_map l snd) in
      (match l with
        [] -> if b then JEmpty else JObject["with",JArray []]
	  | [t] -> t
	  | [JObject[s1;s2]; JObject[t1;t2]] -> 
	      let a1 = json_to_string_fmt "" (JObject[s1]) in
	      let a2 = json_to_string_fmt "" (JObject[s2]) in
	      let b1 = json_to_string_fmt "" (JObject[t1]) in
	      let b2 = json_to_string_fmt "" (JObject[t2]) in
	      if a1 = b1 then normalize_rec (JObject["and",JArray [JObject[s1];JObject["with",JArray [JObject[s2];JObject[t2]]]]]) else
	      if a1 = b2 then normalize_rec (JObject["and",JArray [JObject[s1];JObject["with",JArray [JObject[s2];JObject[t1]]]]]) else
	      if a2 = b1 then normalize_rec (JObject["and",JArray [JObject[s2];JObject["with",JArray [JObject[s1];JObject[t2]]]]]) else
	      if a2 = b2 then normalize_rec (JObject["and",JArray [JObject[s2];JObject["with",JArray [JObject[s1];JObject[t1]]]]]) else
	      extract_simple_jobject (JObject["with",JArray [JObject[s1;s2]; JObject[t1;t2]]])
	  | l -> extract_simple_jobject (JObject["with",JArray l]))
  | JObject["and",JArray l] ->
(*       print_endline ("normalize_rec and 1: " ^ json_to_string_fmt2 "" (JObject["and",JArray l])); *)
      let l = Xlist.rev_map l normalize_rec in
(*       print_endline ("normalize_rec and 2: " ^ json_to_string_fmt2 "" (JArray l)); *)
      let l = List.flatten (Xlist.rev_map l (function JObject["and",JArray l] -> l | JEmpty -> [] | t -> [t])) in
(*       print_endline ("normalize_rec and 3: " ^ json_to_string_fmt2 "" (JArray l)); *)
      let map = Xlist.fold l StringMap.empty (fun map t -> StringMap.add map (json_to_string_fmt "" t) t) in
      let l = List.sort compare_fst (StringMap.fold map [] (fun l k t -> (k,t) :: l)) in
      let l = List.rev (Xlist.rev_map l snd) in
(*       print_endline ("normalize_rec and 4: " ^ json_to_string_fmt2 "" (JArray l)); *)
      (match l with
        [] -> JEmpty
	  | [t] -> t
	  | l -> combine_jobjects (JObject["and",JArray l]))
  | JObject["or",JArray l] -> (* FIXME: jak należy traktować JEmpty? *)
(*       print_endline ("normalize_rec or: " ^ json_to_string_fmt "" (JObject["or",JArray l])); *)
      let l = Xlist.rev_map l normalize_rec in
      let l = List.flatten (Xlist.rev_map l (function JObject["or",JArray l] -> l | JEmpty -> [] | t -> [t])) in
      let map = Xlist.fold l StringMap.empty (fun map t -> StringMap.add map (json_to_string_fmt "" t) t) in
      let l = List.sort compare_fst (StringMap.fold map [] (fun l k t -> (k,t) :: l)) in
      let l = List.rev (Xlist.rev_map l snd) in
      (match l with
        [] -> JEmpty
	  | [t] -> t
	  | l -> combine_jobjects (JObject["or",JArray l]))
  | JObject["and-tuple",JArray l] ->
(*       print_endline ("normalize_rec and: " ^ json_to_string_fmt "" (JObject["and",JArray l])); *)
      let l = Xlist.rev_map l normalize_rec in
      let l = List.flatten (Xlist.rev_map l (function JObject["and-tuple",JArray l] -> l | JEmpty -> [] | t -> [t])) in
      let map = Xlist.fold l StringMap.empty (fun map t -> StringMap.add map (json_to_string_fmt "" t) t) in
      let l = List.sort compare_fst (StringMap.fold map [] (fun l k t -> (k,t) :: l)) in
      let l = List.rev (Xlist.rev_map l snd) in
      (match l with
        [] -> JEmpty
	  | [t] -> t
	  | l -> JObject["and-tuple",JArray l])
(*      let l = List.rev (Xlist.rev_map l normalize_rec) in
      JObject["and-tuple",JArray l]*)
  | JObject["or-tuple",JArray l] ->
(*       print_endline ("normalize_rec and: " ^ json_to_string_fmt "" (JObject["and",JArray l])); *)
      let l = List.rev (Xlist.rev_map l normalize_rec) in
      JObject["or-tuple",JArray l]
  | JObject["ten-power",JNumber n] -> JNumber (Num.string_of_num (Num.power_num (Num.Int 10) (Num.num_of_string n)))
  | JObject["multiply",JArray l] ->
      (try 
        let x = Xlist.fold l (Num.Int 1) (fun x -> function
            JNumber n -> Num.mult_num x (num_of_string_convert_comma n)
          | JObject["ten-power",JNumber n] -> Num.mult_num x (Num.power_num (Num.Int 10) (Num.num_of_string n))
          | _ -> raise Not_found) in
        JNumber (Num.string_of_num x)
(*         JNumber (Num.approx_num_fix 20 x) *)
      with Not_found -> JObject["multiply",normalize_rec (JArray l)])
  | JObject["add",JArray l] ->
      (try 
        let x = Xlist.fold l (Num.Int 0) (fun x -> function
            JNumber n -> Num.add_num x (Num.num_of_string (json_convert_comma n))
          | _ -> raise Not_found) in
        JNumber (Num.string_of_num x)
(*         JNumber (Num.approx_num_fix 20 x) *)
      with Not_found -> JObject["add",normalize_rec (JArray l)])
  | JObject["list",JArray l] ->
      let l = Xlist.rev_map l (fun t -> normalize_rec t) in
      let l = List.flatten (Xlist.rev_map l (function JObject["list",JArray l] -> l | JEmpty -> [] | t -> [t])) in
      (match l with
        [] -> JEmpty
	  | [t] -> t
	  | l -> JObject["list",JArray l])
  | JObject l ->
(*       print_endline ("normalize_rec JObject: " ^ json_to_string_fmt "" (JObject l)); *)
      let l = Xlist.rev_map l (fun (k,t) -> k,normalize_rec t) in
      let l = List.sort compare_fst l in
      JObject l
(*   JObject(List.rev (Xlist.rev_map l (fun (k,t) -> k,normalize_rec t))) *)
  | JArray l -> (*JArray(List.rev (Xlist.rev_map l normalize_rec))*) 
      let l = Xlist.rev_map l (fun t -> json_to_string_fmt "" t,normalize_rec t) in
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
(*   print_endline ("normalize 1:"  ^ json_to_string_fmt2 "" t); *)
  let t = normalize_contradiction t in
(*   print_endline ("normalize 2:"  ^ json_to_string_fmt2 "" t); *)
  let t = normalize_rec t in
(*   print_endline ("normalize 3:"  ^ json_to_string_fmt2 "" t); *)
  let t = normalize_rec t in
(*   print_endline ("normalize 4:"  ^ json_to_string_fmt2 "" t); *)
  let t = try sort_jobject t with _ -> t in
(*   print_endline ("normalize 5:"  ^ json_to_string_fmt2 "" t); *)
  t

let simple_compare s t =
  compare (json_to_string_fmt "" s) (json_to_string_fmt "" t)
  

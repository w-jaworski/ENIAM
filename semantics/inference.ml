(*
 *  ENIAMsemantics implements semantic processing for ENIAM
 *  Copyright (C) 2018 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
 *  Copyright (C) 2018 LekSeek Sp. z o.o. sp. k.
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

open SemTypes
open Xstd

let rec extract_contents_rec h labels = function
    Dot -> [[]]
  | Tuple l -> 
      let l = List.rev (Xlist.rev_map l (extract_contents_rec h labels)) in
      Xlist.rev_map (Xlist.multiply_list l) List.flatten
  | Variant(e,l) -> 
      labels := (e,h) :: !labels;
      List.flatten (Xlist.rev_map l (fun (_,t) -> extract_contents_rec h labels t))
  | Concept c -> [[Concept c]]
  | _ -> failwith "extract_contents_rec"

let extract_contents h n t =
  let labels = ref [] in
  let ll = extract_contents_rec h labels t in
  !labels, Xlist.fold ll [] (fun ll l -> if Xlist.size l = n then l :: ll else ll)
  
let rec pre_prepare_relations (map,attrs) = function
    Tuple l -> Xlist.fold l (map,attrs) pre_prepare_relations
  | Relation(rel,"",t) ->
      StringMap.add_inc map rel [t] (fun l -> t :: l), attrs
  | SingleRelation t -> map, t :: attrs
  | Dot -> map,attrs
  | Variant(e,l) -> 
      let l = Xlist.fold l [] (fun l (i,t) ->
        (i,pre_prepare_relations (StringMap.empty,[]) t) :: l) in
	  Xlist.fold l (map,attrs) (fun (map,attrs) (i,(map2,attrs2)) ->
		StringMap.fold map2 map (fun map rel l -> 
	      Xlist.fold l map (fun map t ->
	        let t = Variant(e,[i,t]) in
	        StringMap.add_inc map rel [t] (fun l -> t :: l))),
		Xlist.fold attrs2 attrs (fun attrs t ->
	      let t = Variant(e,[i,t]) in
	      t :: attrs))
  | t -> print_endline ("pre_prepare_relations: " ^ SemStringOf.linear_term 0 t); failwith "pre_prepare_relations"

let fold_catch_not_found l s f =
  let s,is_found = Xlist.fold l (s,false) (fun (s,is_found) t ->
    try f s t, true with Not_found -> s, false) in
  if is_found then s else raise Not_found
  
let rec pre_match_attr h role = function
    Val s -> if s = role then [] else raise Not_found
  | Variant(e,l) -> 
      fold_catch_not_found l [] (fun labels (_,t) -> 
        [e,h] @ pre_match_attr h role t @ labels)
  | t -> print_endline ("pre_match_attr: " ^ SemStringOf.linear_term 0 t); failwith "pre_match_attr"
  
(*let rec pre_match_attr h role = function
    Val s -> if s = role then [] else raise Not_found
  | Variant(e,l) -> 
      let labels,is_found = Xlist.fold l ([],false) (fun (labels,is_found)  (_,t) -> 
        try [e,h] @ pre_match_attr h role t @ labels, true 
        with Not_found -> labels, is_found) in
	  if is_found then labels else raise Not_found
  | t -> print_endline ("pre_match_attr: " ^ SemStringOf.linear_term 0 t); failwith "pre_match_attr"*)
      
  
let rec pre_match_args h relations args =
(*   print_endline "pre_match_args 1";  *)
  let map,attrs = pre_prepare_relations (StringMap.empty,[]) relations in
(*   print_endline ("pre_match_args 2: " ^ String.concat " " (Xlist.map attrs (SemStringOf.linear_term 0)));    *)
  let args = match args with
      InferenceRulesParser.Args l -> l
    | _ -> failwith "pre_match_args" in
  Xlist.fold args [] (fun matching2 -> function
      InferenceRulesParser.Role(req,role,t) -> 
        let l = StringMap.find map role in
		let matching = Xlist.fold l [] (fun matching s -> 
		  try 
		    (pre_match_pattern h (s,t)) :: matching
		  with Not_found -> matching) in
		if matching = [] && req = InferenceRulesParser.Req then raise Not_found else
		List.flatten matching @ matching2
	| InferenceRulesParser.Attr(req,role) ->
(*  		print_endline ("pre_match_args B1: " ^ role);  *)
	    let matching = Xlist.fold attrs [] (fun matching s -> 
		  try 
		    (pre_match_attr h role s) :: matching
		  with Not_found -> matching) in
		if matching = [] && req = InferenceRulesParser.Req then raise Not_found else
(* 		print_endline ("pre_match_args B2: " ^ String.concat " " (Xlist.map (List.flatten matching) fst)); *)
		List.flatten matching @ matching2
    | InferenceRulesParser.Dots -> matching2
    | _ -> failwith "pre_match_args")
  
and pre_match_contents h = function
    s :: l1, t :: l2 -> 
(*       print_endline "pre_match_contents 1";  *)
      (pre_match_contents h (l1, l2)) @ (pre_match_pattern h (s,t))
  | [], [] -> 
(*       print_endline "pre_match_contents 2";  *)
      []
  | _ -> 
(* 	  print_endline "pre_match_contents 3";  *)
	  raise Not_found
  
and pre_match_pattern h = function
    Concept({contents=Dot} as c), InferenceRulesParser.Concept(cat,sense,var,InferenceRulesParser.NoContents,args) -> (* FIXME: trzeba zapewnić że pusty contents to zawsze Dot *)
(*        Printf.printf "pre_match_pattern A: %s „%s” <-> %s „%s”\n%!" c.cat c.sense cat sense;  *)
      if (cat = "" || c.cat = cat) && (sense = "" || c.sense = sense) then
        if args = InferenceRulesParser.NoArgs then []
        else pre_match_args (h+1) c.relations args
      else raise Not_found
  | Concept c, InferenceRulesParser.Concept(cat,sense,var,InferenceRulesParser.Contents [InferenceRulesParser.Dots],args) ->
(*       Printf.printf "pre_match_pattern B: %s „%s” <-> %s „%s”\n%!" c.cat c.sense cat sense; *)
      if (cat = "" || c.cat = cat) && (sense = "" || c.sense = sense) then
        if args = InferenceRulesParser.NoArgs then []
        else pre_match_args (h+1) c.relations args
      else raise Not_found
  | Concept c, InferenceRulesParser.Concept("","",var,InferenceRulesParser.NoContents,InferenceRulesParser.NoArgs) ->
(*       Printf.printf "pre_match_pattern C\n%!"; *)
      []
  | Concept{sense=""; cat=""; relations=Dot; contents=c}, InferenceRulesParser.Context contents ->
(*        Printf.printf "pre_match_pattern D\n%!"; *)
      let labels,contents_list = extract_contents (h+1) 1 c in
      if contents_list = [] then raise Not_found else
      let contents_list = Xlist.rev_map contents_list List.hd in
      fold_catch_not_found contents_list labels (fun labels c -> pre_match_pattern (h+1) (c,contents) @ labels)
  | Concept c, InferenceRulesParser.Concept(cat,sense,var,InferenceRulesParser.Contents contents,args) ->
(*        Printf.printf "pre_match_pattern E: %s „%s” <-> %s „%s”\n%!" c.cat c.sense cat sense;  *)
      if (cat = "" || c.cat = cat) && (sense = "" || c.sense = sense) then
        let labels,contents_list = extract_contents (h+1) (Xlist.size contents) c.contents in
        if contents_list = [] then raise Not_found else
        (if args = InferenceRulesParser.NoArgs then []
        else pre_match_args (h+1) c.relations args) @
        (fold_catch_not_found contents_list labels (fun labels cont -> pre_match_contents (h+1) (cont,contents) @ labels))
      else raise Not_found
  | Variant(e,l),r -> 
(*       Printf.printf "pre_match_pattern F\n%!"; *)
      fold_catch_not_found l [e,h] (fun labels (_,t) -> pre_match_pattern h (t,r) @ labels)
  | t,pat -> 
(*     print_endline ("pre_match_pattern G: " ^ SemStringOf.linear_term 0 t); *)
(*     print_endline (InferenceRulesParser.string_of_rule "" pat); *)
    raise Not_found

let rec insert_concept2 variants c = function 
    Variant(e,l) -> 
      if StringSet.mem variants e then (* FIXME: tu jest optymistycze założenie, że warianty do rozwinięcia to te na wierzchu *)
        let l = Xlist.rev_map l (fun (i,t) -> i, insert_concept2 variants c t) in
        Variant(e,List.rev l)
	  else Concept{c with contents=Variant(e,l)}
  | t -> Concept{c with contents=t}

let rec insert_concept variants c = function
    Variant(e,l) -> 
      if StringSet.mem variants e then (* FIXME: tu jest optymistycze założenie, że warianty do rozwinięcia to te na wierzchu *)
        let l = Xlist.rev_map l (fun (i,t) -> i, insert_concept variants c t) in
        Variant(e,List.rev l)
	  else insert_concept2 variants {c with relations=Variant(e,l)} c.contents
  | t -> insert_concept2 variants {c with relations=t} c.contents
  
let rec insert_relation variants (r,a) = function
    Variant(e,l) -> 
      if StringSet.mem variants e then 
        let l = Xlist.rev_map l (fun (i,t) -> i, insert_relation variants (r,a) t) in
        Variant(e,List.rev l)
	  else Relation(r,a,Variant(e,l))
  | t -> Relation(r,a,t)
  
let rec insert_revrelation variants (r,a) = function
    Variant(e,l) -> 
      if StringSet.mem variants e then 
        let l = Xlist.rev_map l (fun (i,t) -> i, insert_revrelation variants (r,a) t) in
        Variant(e,List.rev l)
	  else RevRelation(r,a,Variant(e,l))
  | t -> RevRelation(r,a,t)
  
let rec insert_tuple variants rev = function
    Variant(e,l) :: tail -> 
      if StringSet.mem variants e then 
        let l = Xlist.rev_map l (function 
            (i,Tuple l) -> i, insert_tuple variants rev (l @ tail)
          | (i,t) -> i, insert_tuple variants rev (t :: tail)) in
        Variant(e,List.rev l)
	  else insert_tuple variants (Variant(e,l) :: rev) tail
  | t :: tail -> insert_tuple variants (t :: rev) tail
  | [] -> Tuple rev
  
let rec insert_singlerelation variants = function
    Variant(e,l) -> 
      if StringSet.mem variants e then 
        let l = Xlist.rev_map l (fun (i,t) -> i, insert_singlerelation variants t) in
        Variant(e,List.rev l)
	  else SingleRelation(Variant(e,l))
  | t -> SingleRelation t
  
let rec shift_variants h variants = function
    Concept c -> 
(*        print_endline ("shift_variants A1: " ^ SemStringOf.linear_term 0 (Concept c)); *)
       if h = 0 then Concept c else ((* FIXME: czy ucięcie jest we właściwym miejscu? *)
(*        print_endline "shift_variants A2"; *)
       let c = {c with contents=shift_variants (h-1) variants c.contents} in
(*        print_endline "shift_variants A3"; *)
       let rels = shift_variants (h-1) variants c.relations in
(*        print_endline ("shift_variants A4: " ^ SemStringOf.linear_term 0 rels); *)
       let t = insert_concept variants c rels in
(*        print_endline ("shift_variants A5: " ^ SemStringOf.linear_term 0 t); *)
	   t)
  | Relation(r,a,t) -> 
(*        print_endline "shift_variants B"; *)
       insert_relation variants (r,a) (shift_variants h variants t)
  | RevRelation(r,a,t) -> 
(*        print_endline "shift_variants C"; *)
       insert_revrelation variants (r,a) (shift_variants h variants t)
  | SingleRelation r  -> 
(*        print_endline ("shift_variants D1: " ^ SemStringOf.linear_term 0 (SingleRelation r)); *)
       let t = insert_singlerelation variants r in
(*        print_endline ("shift_variants D2: " ^ SemStringOf.linear_term 0 t); *)
       t
  | Tuple l -> 
(*        print_endline ("shift_variants E1: " ^ SemStringOf.linear_term 0 (Tuple l)); *)
	   let t = insert_tuple variants [] (Xlist.rev_map l (shift_variants h variants)) in
(*        print_endline ("shift_variants E2: " ^ SemStringOf.linear_term 0 t); *)
	   t
  | Variant(e,l) -> Variant(e,Xlist.map l (fun (i,t) -> i, shift_variants h variants t))
  | Dot -> Dot
  | Val s -> Val s
  | t -> failwith ("shift_variants: " ^ SemStringOf.linear_term 0 t)
  
let rec make_args_list = function
    Tuple l -> List.flatten (Xlist.map l make_args_list)
  | Dot -> []
  | t -> [t]
  
let rec prepare_relations (map,attrs,variants) = function
    Tuple l -> Xlist.fold l (map,attrs,variants) prepare_relations
  | Variant _ as t -> map, attrs, t :: variants
  | Relation(rel,"",t) ->
      StringMap.add_inc map rel [t] (fun l -> t :: l), attrs, variants
  | SingleRelation(Val v) -> map, StringSet.add attrs v, variants
  | SingleRelation t -> map, StringSet.add attrs (SemStringOf.linear_term 0 t), variants
  | Dot -> map, attrs, variants
  | t -> print_endline ("prepare_relations: " ^ SemStringOf.linear_term 0 t); failwith "prepare_relations"

let rec match_args relations args =
  let map,attrs,variants = prepare_relations (StringMap.empty,StringSet.empty,[]) relations in
  let args = match args with
      InferenceRulesParser.Args l -> l
    | _ -> failwith "match_args" in
  let matching,map,attrs,dot_flag = Xlist.fold args ([],map,attrs,false) (fun (matching,map,attrs,dot_flag) -> function
      InferenceRulesParser.Role(InferenceRulesParser.Opt,role,t) -> 
        (try 
          let l = StringMap.find map role in
          match l with
            [s] -> 
              let arg_matching = match_pattern (s,t) in
              arg_matching @ matching, StringMap.remove map role, attrs, dot_flag
		  | _ -> raise Not_found
        with Not_found -> matching,map,attrs, dot_flag)
    | InferenceRulesParser.Role(InferenceRulesParser.Multi,role,t) -> 
		(try 
		  let l = StringMap.find map role in
          let arg_matching = List.flatten (Xlist.rev_map l (fun s -> match_pattern (s,t))) in (* FIXME: co tu się dzieje ze zmiennymi? *)
          arg_matching @ matching, StringMap.remove map role, attrs, dot_flag
        with Not_found -> matching,map,attrs, dot_flag)
    | InferenceRulesParser.Role(InferenceRulesParser.Req,role,t) -> 
        let l = StringMap.find map role in
        (match l with
          [s] -> 
            let arg_matching = match_pattern (s,t) in
            arg_matching @ matching, StringMap.remove map role, attrs, dot_flag
        | l -> 
            let matched,not_matched = Xlist.fold l ([],[]) (fun (matched,not_matched) s ->
              if matched <> [] then matched, s :: not_matched else
              try [match_pattern (s,t)], not_matched
              with Not_found -> [], s :: not_matched) in
			if matched = [] then raise Not_found else
            let arg_matching = List.hd matched in
            arg_matching @ matching, StringMap.add map role not_matched, attrs, dot_flag)
    | InferenceRulesParser.Attr(InferenceRulesParser.Req,role) -> 
        if StringSet.mem attrs role then 
          matching, map, StringSet.remove attrs role, dot_flag
		else raise Not_found
    | InferenceRulesParser.Dots -> matching,map,attrs,true
    | _ -> failwith "match_args") in
  if not (StringSet.is_empty attrs) then (
(*     print_endline "match_args: other_attrs";  *)
    raise Not_found) else
  if not dot_flag && not (StringMap.is_empty map) then (
(*     print_endline "match_args: other_args";  *)
    raise Not_found) else
  if not dot_flag && not (variants = []) then (
(*     print_endline "match_args: other_args";  *)
    raise Not_found) else
  let args = variants (*Xlist.rev_map attrs (fun v -> SingleRelation(Val v))*) in
  let other_args = SemGraph.make_tuple (StringMap.fold map args (fun args rel l ->
    Xlist.fold l args (fun args t -> 
      Relation(rel,"",t) :: args))) in
  matching,other_args
  
and match_contents matching = function
    Tuple(Dot :: l1), l2 -> match_contents matching (Tuple l1, l2)
  | Tuple(s :: l1), t :: l2 -> match_contents (match_pattern (s,t) @ matching) (Tuple l1, l2)
  | Dot, [] -> matching
  | Tuple [], [] -> matching
  | _ -> raise Not_found
  
and match_pattern = function
    Concept c, InferenceRulesParser.Concept(cat,sense,var,InferenceRulesParser.NoContents,args) ->
(*       print_endline ("match_pattern A1: " ^ c.cat ^ " „" ^ c.sense ^ "”"); *)
      if (cat = "" || c.cat = cat) && (sense = "" || c.sense = sense) then
(*       if (cat = "" || cat = c.c_cat) && (sense = "" || sense = c.c_sense) then *)
        if args = InferenceRulesParser.NoArgs then if var = -1 then [] else [var,c]
        else (
(*           print_endline "match_pattern A2"; *)
          let matching,other_args = match_args c.relations args in
          let v = if var = -1 then [] else [var,{c with relations=other_args}] in
          v @ matching)
      else raise Not_found
  | Concept c, InferenceRulesParser.Concept(cat,sense,var,InferenceRulesParser.Contents [InferenceRulesParser.Dots],args) ->
(*       print_endline ("match_pattern B1: " ^ c.cat ^ " „" ^ c.sense ^ "”"); *)
      if (cat = "" || c.cat = cat) && (sense = "" || c.sense = sense) then
        if args = InferenceRulesParser.NoArgs then if var = -1 then [] else [var,c]
        else (
(*           print_endline "match_pattern B2"; *)
          let matching,other_args = match_args c.relations args in
          let v = if var = -1 then [] else [var,{c with relations=other_args}] in
          v @ matching)
      else raise Not_found
  | Concept{sense=""; cat=""; relations=Dot; contents=c}, InferenceRulesParser.Context contents ->
(*       Printf.printf "match_pattern D\n%!"; *)
      (match make_args_list c with
        [c] -> match_pattern (c,contents)
	  | _ -> raise Not_found)
  | Concept c, InferenceRulesParser.Concept(cat,sense,var,InferenceRulesParser.Contents contents,args) ->
(*       Printf.printf "match_pattern E1: %s %s <-> %s %s\n%!" c.cat c.sense cat sense; *)
      let cont = make_args_list c.contents in
      if (cat = "" || c.cat = cat) && (sense = "" || c.sense = sense) && Xlist.size cont = Xlist.size contents then
        let matching = Xlist.fold2 cont contents [] (fun matching c pat -> match_pattern (c,pat) @ matching) in
        if args = InferenceRulesParser.NoArgs then if var = -1 then matching else (var,c) :: matching
        else (
(*           print_endline "match_pattern E2"; *)
          let matching2,other_args = match_args c.relations args in
          let v = if var = -1 then [] else [var,{c with relations=other_args; contents=Dot}] in
          v @ matching @ matching2)
      else raise Not_found
  | _ -> raise Not_found
  
(*let _ =
  Subsyntax.initialize ();
  ()*)

(*let ask_morphology query =
(*  Printf.fprintf !ExecTypes.morphology_out "%s\n\n%!" s;
  (Marshal.from_channel !ExecTypes.morphology_in : SubsyntaxTypes.text * SubsyntaxTypes.token_env ExtArray.t)*)
  Printf.fprintf !ExecTypes.morphology_out "%s\n%!" query;
  let result,msg = (Marshal.from_channel !ExecTypes.morphology_in : Inflexion.t list * string) in
  if msg <> "" then failwith ("ask_morphology: " ^ msg) else
  result*)
    
let find_interp attr attrs =
  try 
    let l = DomSemantics.make_list (Xlist.assoc attrs attr) in
    if attr = "GEND" && Xlist.assoc attrs "POS" = Val "subst" then
      Xlist.rev_map l (function "n" -> "n:_" | s -> s)
    else l
  with Not_found -> failwith "find_interp"
    
let rec longest_common_suffix i s t =
  let ns = Xstring.size s in
  let nt = Xstring.size t in
  if i > ns || i > nt then i else 
  if String.get s (ns - i) <> String.get t (nt - i) then i else
  longest_common_suffix (i+1) s t 
    
let select_orth s l =
  let s2 = Xunicode.lowercase_utf8_string s in
  snd (Xlist.select_max_priority (Xlist.rev_map l (fun t -> 
    let t2 = Xunicode.lowercase_utf8_string t in
    longest_common_suffix 1 s2 t2, t)))
    
let rec generate_pattern vars = function
    InferenceRulesParser.Concept(cat,sense,var,contents,args) -> 
      let c = if var = -1 then empty_concept else try IntMap.find vars var with Not_found -> failwith ("generate_pattern: variable " ^ string_of_int var ^ " not_found") in
      let c = if cat = "" then c else {c with cat=cat} in
      let c = if sense = "" then c else {c with sense=sense} in
      let c = match args with 
          InferenceRulesParser.NoArgs -> c
        | InferenceRulesParser.Args args -> {c with relations=SemGraph.make_tuple (c.relations :: Xlist.rev_map args (generate_pattern vars))}
        | _ -> failwith "generate_pattern" in
	  (match contents with
	      InferenceRulesParser.NoContents -> Concept c
		| InferenceRulesParser.Contents l -> Concept {c with contents=Tuple(c.contents :: Xlist.map l (generate_pattern vars))}
		| _ -> failwith "generate_pattern")
  | InferenceRulesParser.Context contents -> Concept{empty_concept with contents=generate_pattern vars contents}
  | InferenceRulesParser.Role(req,role,arg) -> 
      (try
        let c = generate_pattern vars arg in
        Relation(role,"",c)
	  with Not_found -> Dot) (* FIXME: czy przechwytywanie wyjątku jest tu potrzebne? *)
  | InferenceRulesParser.Attr(req,role) -> SingleRelation(Val role)
  | InferenceRulesParser.Render(cat,l) -> failwith "render ni"
(*      let l = Xlist.map l (fun (var,f,interp) ->
        let c = try IntMap.find vars var with Not_found -> failwith ("generate_pattern: variable " ^ string_of_int var ^ " not_found") in
        let interps = Xlist.multiply_list (Xlist.map interp (function 
            "$g" -> find_interp "GEND" c.atrs
          | "$c" -> find_interp "CASE" c.atrs
          | "$n" -> find_interp "NUM" c.atrs
          | s -> [s])) in
(*         let l = Inflexion.disambiguate [] [Acro;Aux;Aux2;Ndm;Dial] (Inflexion.synthetize c.sense "") in *)
(*         let l,msg = Inflexion.catch_synthetize_disambiguate (c.sense ^ ":subst:sg:_:_") in *)
		let l = List.flatten (Xlist.rev_map interps (fun interp -> (*if interp = ["fixed"] then [c.sense] else*) ask_morphology (String.concat ":" (c.sense :: interp)))) in
(*          print_endline (Inflexion.string_of_interpretations l);  *)
(*         if msg <> "" then failwith ("generate_pattern: " ^ msg) else *)
        let l = Xlist.rev_map l (fun t -> t.Inflexion.lemma) in
        match f with
          "lemma" -> select_orth c.sense l
		| "orth" -> select_orth (List.hd (find_interp "ORTH" c.atrs)) l
		| "" -> l
		| _ -> failwith "generate_pattern") in
      let senses = Xlist.rev_map (Xlist.multiply_list l) (String.concat " ") in
	  DomExec.make_variant (Xlist.rev_map senses (fun sense -> Concept {empty_concept with cat=cat; sense=sense}))*)
  | t -> failwith ("generate_pattern: " ^ InferenceRulesParser.string_of_rule "" t)

  
let rec apply_rule_rec prod pat = function
    Variant(e,l) -> (*Variant(e, List.rev (Xlist.rev_map l (fun (i,t) -> i,apply_rule_rec prod pat t)))*)
      let l,is_found = Xlist.fold l ([],false) (fun (l,is_found) (i,t) -> 
        try (i,apply_rule_rec prod pat t) :: l, true 
        with Not_found -> (i,t) :: l, is_found) in
	  if is_found then Variant(e, List.rev l) else raise Not_found
  | t -> 
        let vars = match_pattern (t,pat) in
        let vars = Xlist.fold vars IntMap.empty (fun map (v,t) -> IntMap.add_inc map v t (fun t -> failwith ("apply_rule_rec: multiple assignment for variable " ^ string_of_int v))) in
        generate_pattern vars prod

(* UWAGA: w tej procedurze można wyświetlić informacje do debugowania niedziałających reguł *)
let apply_rule prod pat t =
(*    print_endline ("apply_rule 1: " ^ SemStringOf.linear_term 0 t); *)
(*    print_endline ("apply_rule 2: " ^ InferenceRulesParser.string_of_rule "" pat); *)
  let l = pre_match_pattern 0 (t,pat) in
(*    print_endline ("apply_rule 3: |l|=" ^ string_of_int (Xlist.size l)); *)
  let labels,h = Xlist.fold l (StringSet.empty,-1) (fun (labels,max_h) (label,h) ->
    StringSet.add labels label, max max_h h) in
(*    print_endline ("apply_rule 4: labels=" ^ String.concat " " (StringSet.to_list labels) ^ " h=" ^ string_of_int h); *)
  let t = shift_variants h labels t in
(*    print_endline ("apply_rule 5: " ^ SemStringOf.linear_term 0 t); *)
  apply_rule_rec prod pat t

let rec try_apply_rule t = function
    (pat,prod) :: rules ->
      (try 
        apply_rule prod pat t, true
	  with Not_found -> try_apply_rule t rules)
  | [] -> t,false

let rec apply_rules_rec sense_rules cat_rules = function
(*    Concept({sense="";cat="";contents=Concept c1} as c) -> (* FIXME: to nie zadziała, gdy będzie niejednoznaczność *)
       let rules = (try StringMap.find sense_rules c1.sense with Not_found -> []) @ (try StringMap.find cat_rules c1.cat with Not_found -> []) in
       let t,b = try_apply_rule (Concept c) rules in
       if b then apply_rules_rec sense_rules cat_rules t else
       Concept{c with relations=apply_rules_rec sense_rules cat_rules c.relations; contents=apply_rules_rec sense_rules cat_rules c.contents}*)
  | Concept c ->
	  if c.sense = "" && c.cat = "" then
        let rules = Xlist.fold (snd (extract_contents 0 1 c.contents)) [] (fun rules -> function 
            [Concept c1] -> (try StringMap.find sense_rules c1.sense with Not_found -> []) @ (try StringMap.find cat_rules c1.cat with Not_found -> []) @ rules
		  | _ -> rules) in
        let t,b = try_apply_rule (Concept c) rules in
        if b then apply_rules_rec sense_rules cat_rules t else
        Concept{c with relations=apply_rules_rec sense_rules cat_rules c.relations; contents=apply_rules_rec sense_rules cat_rules c.contents}	  
	  else
(*        print_endline "apply_rules_rec 1"; *)
        let rules = (try StringMap.find sense_rules c.sense with Not_found -> []) @ (try StringMap.find cat_rules c.cat with Not_found -> []) in
(*        print_endline "apply_rules_rec 4"; *)
        let t,b = try_apply_rule (Concept c) rules in
(*        let t = Xlist.fold rules (Concept c) (fun t (prod,pat) -> apply_rule prod pat t) in *)
(*        if b then print_endline "apply_rules_rec 5: true" else print_endline "apply_rules_rec 5: false"; *)
        if b then apply_rules_rec sense_rules cat_rules t else
        Concept{c with relations=apply_rules_rec sense_rules cat_rules c.relations; contents=apply_rules_rec sense_rules cat_rules c.contents}
  | Relation(r,a,t) -> Relation(r,a,apply_rules_rec sense_rules cat_rules t)
  | RevRelation(r,a,t) -> RevRelation(r,a,apply_rules_rec sense_rules cat_rules t)
  | SingleRelation r  -> SingleRelation r
  | Tuple l -> Tuple(Xlist.map l (apply_rules_rec sense_rules cat_rules))
  | Variant(e,l) -> Variant(e,Xlist.map l (fun (i,t) -> i, apply_rules_rec sense_rules cat_rules t))
  | Dot -> Dot
  | Val s -> Val s
  | t -> failwith ("apply_rules_rec: " ^ SemStringOf.linear_term 0 t)
  
let apply_rules t =
  Xlist.fold !InferenceRulesParser.rules t (fun t (prior,sense_rules,cat_rules) ->
(*     print_endline ("apply_rules: " ^ string_of_int prior); *)
    apply_rules_rec sense_rules cat_rules t)
      

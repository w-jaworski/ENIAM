(*
 *  ENIAMgenerator: generator of inflected phrases for Polish
 *  Copyright (C) 2020 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
 *  Copyright (C) 2020 Institute of Computer Science Polish Academy of Sciences
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

exception Strange
 
let add_prior best_prior best_l prior t =
  if prior > best_prior then best_prior,best_l else
  if prior < best_prior then prior,[t] else
  best_prior,t :: best_l
  
let lemmatize_string s =
  snd (Xlist.fold (get_interpretations s) (1000,[]) (fun (best_prior,best_l) i ->
(*       Printf.printf "lemmatize_strings 1: %s\n%!" (string_of_interpretation i);  *)
    let prior = if i.status = LemmaVal || i.status = LemmaAlt then 0 else 20 in (* obecność leksemu w SGJP *)
    if i.star = MorphologyTypes.Productive && i.tags = ["cat","ndm"] then add_prior best_prior best_l prior (i.lemma,"ndm",[]) else
    if i.star = MorphologyTypes.Productive || i.star = MorphologyTypes.Star then
      Xlist.fold (Tagset.parse i.interp) (best_prior,best_l) (fun (best_prior,best_l) (pos,tags) ->
        if pos = "brev" || pos = "fin" || pos = "impt" then best_prior,best_l else
        add_prior best_prior best_l prior (i.lemma,pos,tags))
    else best_prior,best_l))
    

let lemmatize_token = function
    SmallLetter(uc,lc) -> lemmatize_string lc
  | CapLetter(uc,lc) -> lemmatize_string uc
  | AllSmall(uc,fc,lc) -> lemmatize_string lc
  | AllCap(uc,fc,lc) -> lemmatize_string uc
  | FirstCap(uc,fc,lc) -> lemmatize_string fc
  | SomeCap(uc,orth,lc) -> lemmatize_string orth
  | t -> failwith ("lemmatize_token: " ^ SubsyntaxStringOf.string_of_token t)


let disambiguate_variant = function
    Token{orth=""} -> []
  | Token{token=Interp "."} -> []
  | Token{token=Ideogram(_,"dig")} as t -> [t]
  | Token{token=Ideogram(_,"natnum")} -> []
  | Token{token=Ideogram(_,"hour")} -> []
  | Token{token=Ideogram(_,"1dig")} -> []
  | Token{token=Ideogram(_,"2dig")} -> []
  | Token{token=Ideogram(_,"3dig")} -> []
  | Token{token=Ideogram(_,"4dig")} -> []
  | Token{token=Ideogram(_,"posnum")} -> []
  | Token{token=Ideogram(_,"month")} -> []
  | Token{token=Ideogram(_,"day")} -> []
  | Token{token=Ideogram(_,"minute")} -> []
  | Token{token=Ideogram(_,"pref3dig")} -> []
  | Token{token=Ideogram(_,"roman")} -> []
  | Token{token=Ideogram(_,"roman-month")} -> []
  | Token t -> [Token t]
  | Seq[Token _;Token{orth="m"}] -> []
  | Seq[Token _;Token{orth="M"}] -> []
  | Seq[Token _;Token{orth="em"}] -> []
  | Seq[Token _;Token{orth="ż"}] -> []
  | Seq[Token _;Token{orth="Ż"}] -> []
  | Seq[Token _;Token{orth="że"}] -> []
  | Seq[Token _;Token{orth="ń"}] -> []
  | Seq[Token _;Token{orth="by"}] -> []
  | Seq[Token _;Token{orth="ście"}] -> []
  | Seq[Token _;Token{orth="eście"}] -> []
  | Seq[Token _;Token{orth="śmy"}] -> []
  | Seq[Token _;Token{orth="ś"}] -> []
  | Seq[Token _;Token{orth="eś"}] -> []
  | Seq[Token _;Token{orth="by"};Token{orth="m"}] -> []
  | Seq[Token _;Token{orth="by"};Token{orth="ś"}] -> []
  | Seq[Token{orth="x"};Token _] -> []
  | Seq[Token{orth="X"};Token _] -> []
  | Seq[Token{orth=""};Token{orth="."}] -> []
  | Seq[Token{orth=""};Token{orth="..."}] -> []
  | t -> print_endline ("disambiguate_variant: " ^ SubsyntaxStringOf.string_of_tokens 0 t); []
  
let rec disambiguate_tokens = function
    [] -> []
  | Token{token=Interp("<query>")} :: l -> disambiguate_tokens l
  | [Token{token=Interp("</query>")}] -> []
  | Token{token=(AllCap _ as t)} :: l -> t :: disambiguate_tokens l
  | Token{token=(FirstCap _ as t)} :: l -> t :: disambiguate_tokens l
  | Token{token=(SomeCap _ as t)} :: l -> t :: disambiguate_tokens l
  | Token{token=(AllSmall _ as t)} :: l -> t :: disambiguate_tokens l
  | Token{token=(SmallLetter _ as t)} :: l -> t :: disambiguate_tokens l
  | Token{token=(CapLetter _ as t)} :: l -> t :: disambiguate_tokens l
  | Token{token=(Interp "-" as t)} :: l -> t :: disambiguate_tokens l
  | Token{token=(Interp "," as t)} :: l -> t :: disambiguate_tokens l
  | Token{token=(Interp "/" as t)} :: l -> t :: disambiguate_tokens l
  | Token{token=(Interp "’" as t)} :: l -> t :: disambiguate_tokens l
  | Token{token=(Interp "&" as t)} :: l -> t :: disambiguate_tokens l
  | Token{token=(Interp ";" as t)} :: l -> t :: disambiguate_tokens l
  | Token{token=(Interp "(" as t)} :: l -> t :: disambiguate_tokens l
  | Token{token=(Interp ")" as t)} :: l -> t :: disambiguate_tokens l
  | Token{token=(Interp "[" as t)} :: l -> t :: disambiguate_tokens l
  | Token{token=(Interp "]" as t)} :: l -> t :: disambiguate_tokens l
  | Token{token=(Interp "…" as t)} :: l -> t :: disambiguate_tokens l
  | Token{token=(Interp "……" as t)} :: l -> t :: disambiguate_tokens l
  | Token{token=(Interp "_" as t)} :: l -> t :: disambiguate_tokens l
  | Token{token=(Interp "µ" as t)} :: l -> t :: disambiguate_tokens l
  | Token{token=(Interp "≥" as t)} :: l -> t :: disambiguate_tokens l
  | Token{token=(Interp "„" as t)} :: l -> t :: disambiguate_tokens l
  | Token{token=(Interp ":" as t)} :: l -> t :: disambiguate_tokens l
  | Token{token=(Interp "+" as t)} :: l -> t :: disambiguate_tokens l
  | Token{token=(Interp "%" as t)} :: l -> t :: disambiguate_tokens l
  | Token{token=(Interp ">" as t)} :: l -> t :: disambiguate_tokens l
  | Token{token=(Interp "°" as t)} :: l -> t :: disambiguate_tokens l
  | Token{token=(Interp "≤" as t)} :: l -> t :: disambiguate_tokens l
  | Token{token=(Interp "<" as t)} :: l -> t :: disambiguate_tokens l
  | Token{token=(Interp "”" as t)} :: l -> t :: disambiguate_tokens l
  | Token{token=(Interp "?" as t)} :: l -> t :: disambiguate_tokens l
  | Token{token=(Interp "\\" as t)} :: l -> t :: disambiguate_tokens l
  | Token{token=(Symbol "." as t)} :: l -> t :: disambiguate_tokens l
  | Token{token=(Other "α" as t)} :: l -> t :: disambiguate_tokens l
  | Token{token=(Other "β" as t)} :: l -> t :: disambiguate_tokens l
  | Token{token=(Other "γ" as t)} :: l -> t :: disambiguate_tokens l
  | Token{token=(Other "≥" as t)} :: l -> t :: disambiguate_tokens l (* FIXME *)
  | Token{token=(Other "≤" as t)} :: l -> t :: disambiguate_tokens l (* FIXME *)
  | Token{token=(Ideogram(_,"dig") as t)} :: l -> t :: disambiguate_tokens l
  | Variant vl :: l -> 
      let t = match List.flatten (Xlist.rev_map vl disambiguate_variant) with
          [t] -> t
        | _ -> failwith ("disambiguate_tokens: " ^ SubsyntaxStringOf.string_of_tokens 0 (Variant vl)) in
      disambiguate_tokens (t :: l)
  | t :: l -> failwith ("disambiguate_tokens: " ^ SubsyntaxStringOf.string_of_tokens 0 t)
(*   | t :: l -> print_endline ("disambiguate_tokens: " ^ SubsyntaxStringOf.string_of_tokens 0 t); [] *)
  
let rec is_strange = function
    SmallLetter(_,"z") :: l -> is_strange l
  | SmallLetter(_,"o") :: l -> is_strange l
(*   | SmallLetter(_,"i") :: l -> is_strange l *)
  | SmallLetter(_,"w") :: l -> is_strange l
  | SmallLetter(_,"u") :: l -> is_strange l
  | SmallLetter(_,"i") :: l -> is_strange l
  | SmallLetter _ :: _ -> true
  | CapLetter _ :: _ -> true
(*   | Interp " " :: l -> is_strange l *)
  | Interp "-" :: l -> is_strange l
(*  | Interp "," :: l -> is_strange l
  | Interp "’" :: l -> is_strange l*)
  | Interp _ :: _ -> true
  | Symbol _ :: _ -> true
  | Other _ :: _ -> true
  | Ideogram _ :: _ -> true
  | _ :: l -> is_strange l
  | [] -> false
  
let patterns_np = [
  9,"ndm",[LemStar("ndm",[])];
  8,"subst.nom subst.nom._",[LemStar("subst",[S "n";V["nom"];S "g";G]);LemStar("subst",[S "n";V["nom"];G;G])];
  7,"ndm adj.nom",[LemStar("ndm",[]);LemStar("adj",[S "n";V["nom"];S "g";G])];
  6,"ndm adj.nom adj.nom",[LemStar("ndm",[]);LemStar("adj",[S "n";V["nom"];S "g";G]);LemStar("adj",[S "n";V["nom"];S "g";G])];
  5,"subst.nom",[LemStar("subst",[S "n";V["nom"];S "g";G])];
  4,"subst.nom subst.nom",[LemStar("subst",[S "n";V["nom"];S "g";G]);LemStar("subst",[S "n";V["nom"];S "g";G])];
  4,"subst.nom-subst.nom",[LemStar("subst",[S "n";V["nom"];S "g";G]);O "-";LemStar("subst",[S "n";V["nom"];S "g";G])];
  4,"fixed-subst.nom",[LemStar("fixed",[]);O "-";LemStar("subst",[S "n";V["nom"];S "g";G])];
  2,"subst.nom się",[LemStar("subst",[S "n";V["nom"];S "g";G]);Lem("się","qub",[])];
  3,"adj.nom subst.nom",[LemStar("adj",[S "n";V["nom"];S "g";G]);LemStar("subst",[S "n";V["nom"];S "g";G])];
  3,"num.nom.congr subst.nom",[LemStar("num",[S "n";V["nom"];S "g";V["congr"]]);LemStar("subst",[S "n";V["nom"];S "g";G])];
  2,"subst.nom adj.nom",[LemStar("subst",[S "n";V["nom"];S "g";G]);LemStar("adj",[S "n";V["nom"];S "g";G])];
  2,"subst.nom adv adj.nom",[LemStar("subst",[S "n";V["nom"];S "g";G]);LemStar("adv",[G]);LemStar("adj",[S "n";V["nom"];S "g";G])];
  2,"subst.nom adj.nom adv",[LemStar("subst",[S "n";V["nom"];S "g";G]);LemStar("adj",[S "n";V["nom"];S "g";G]);LemStar("adv",[G])];
  1,"subst.nom adja-adj.nom",[
    LemStar("subst",[S "n";V["nom"];S "g";G]);LemStar("adja",[]);O "-";LemStar("adj",[S "n";V["nom"];S "g";G])];
  1,"subst.nom fixed-adj.nom",[
    LemStar("subst",[S "n";V["nom"];S "g";G]);LemStar("fixed",[]);O "-";LemStar("adj",[S "n";V["nom"];S "g";G])];
  1,"subst.nom adj.nom adj.nom",[
    LemStar("subst",[S "n";V["nom"];S "g";G]);LemStar("adj",[S "n";V["nom"];S "g";G]);LemStar("adj",[S "n";V["nom"];S "g";G])];
  1,"subst.nom adj.nom conj adj.nom",[
    LemStar("subst",[S "n";V["nom"];S "g";G]);LemStar("adj",[S "n";V["nom"];S "g";G]);LemStar("conj",[]);LemStar("adj",[S "n";V["nom"];S "g";G])];
  1,"subst.nom adv adj.nom adj.nom",[
    LemStar("subst",[S "n";V["nom"];S "g";G]);LemStar("adv",[G]);LemStar("adj",[S "n";V["nom"];S "g";G]);LemStar("adj",[S "n";V["nom"];S "g";G])];
  1,"adj.nom subst.nom adj.nom",[
    LemStar("adj",[S "n";V["nom"];S "g";G]);LemStar("subst",[S "n";V["nom"];S "g";G]);LemStar("adj",[S "n";V["nom"];S "g";G])];
  ]

let patterns_np2 = [
  "ndm",[LemStar("ndm",[])];
  "subst.gen",[LemStar("subst",[G;V["gen"];G;G])];
  "adj.gen subst.gen",[LemStar("adj",[S "n2";V["gen"];S "g2";G]);LemStar("subst",[S "n2";V["gen"];S "g2";G])];
  "subst.inst",[LemStar("subst",[G;V["inst"];G;G])];
  "adj.inst subst.inst",[LemStar("adj",[S "n2";V["inst"];S "g2";G]);LemStar("subst",[S "n2";V["inst"];S "g2";G])];
  "prep.case subst.case",[LemStar("prep",[S "c2"]);LemStar("subst",[G;S "c2";G;G])];
  "prep.case adj.case subst.case",[LemStar("prep",[S "c2"]);LemStar("adj",[S "n2";S "c2";S "g2";G]);LemStar("subst",[S "n2";S "c2";S "g2";G])];
  ]
  
let patterns_infp = [
  2,"inf",[LemStar("inf",[G])];
  1,"inf subst:gen",[LemStar("inf",[G]);LemStar("subst",[G;V["gen"];G;G])];
  1,"inf subst:dat",[LemStar("inf",[G]);LemStar("subst",[G;V["dat"];G;G])];
  1,"inf subst:str",[LemStar("inf",[G]);LemStar("subst",[G;V["acc"];G;G])];
  1,"inf subst:inst",[LemStar("inf",[G]);LemStar("subst",[G;V["inst"];G;G])];
  1,"inf się",[LemStar("inf",[G]);Lem("się","qub",[])];
  1,"się inf",[Lem("się","qub",[]);LemStar("inf",[G])];
  ]
  
let patterns_infp2 = [
  "prep.case subst.case",[LemStar("prep",[S "c2"]);LemStar("subst",[G;S "c2";G;G])];
  "prep.case adj.case subst.case",[LemStar("prep",[S "c2"]);LemStar("adj",[S "n2";S "c2";S "g2";G]);LemStar("subst",[S "n2";S "c2";S "g2";G])];
  ]
  
let patterns_adjp = [
  4,"adj.nom",[LemStar("adj",[V["sg"];V["nom"];V["m1"];G])];
  3,"adj.nom adj.nom",[LemStar("adj",[V["sg"];V["nom"];V["m1"];G]);LemStar("adj",[V["sg"];V["nom"];V["m1"];G])];
  2,"adv adj.nom",[LemStar("adv",[G]);LemStar("adj",[V["sg"];V["nom"];V["m1"];G])];
  1,"adja-adj.nom",[LemStar("adja",[]);O "-";LemStar("adj",[V["sg"];V["nom"];V["m1"];G])];
  1,"fixed-adj.nom",[LemStar("fixed",[]);O "-";LemStar("adj",[V["sg"];V["nom"];V["m1"];G])];
  ]

let patterns_adjp2 = []
  
let string_of_sels sels =
  let sels = Xlist.map sels (fun (s,l) -> s,List.sort compare l) in
  let sels = List.sort compare sels in
  String.concat "; " (Xlist.map sels (fun (s,l) -> s ^ ": " ^ String.concat "," l ))
  
let group_tokens l =
  let map = Xlist.fold l StringMap.empty (fun map (sels,token) ->
    Xlist.fold sels map (fun map sels ->
      let k = string_of_sels sels in
      StringMap.add_inc map k (sels,[token]) (fun (sels,tokens) -> sels, token :: tokens))) in
  StringMap.fold map [] (fun l _ (sels,tokens) ->
    (sels,tokens) :: l)
  
let rec classify_tokens_rec sels rev = function
    pat :: pats, (orth,(*prior,*)token) :: tokens -> 
      let matched = Xlist.fold token [] (fun matched token ->
        try 
(*           print_endline ("classify_tokens_rec 1: token=" ^ SubsyntaxStringOf.string_of_token token ^ " sels=" ^ string_of_sels sels); *)
          let sels = Patterns.match_token sels (pat,token) in
(*           print_endline ("classify_tokens_rec 2: sels=" ^ String.concat " | " (Xlist.map sels string_of_sels)); *)
          (sels,token) :: matched
        with Not_found -> matched) in
      if matched = [] then [] else
      List.flatten (Xlist.map (group_tokens matched) (fun (matched_sels,matched_tokens) ->
        classify_tokens_rec matched_sels (matched_tokens :: rev) (pats,tokens)))
(*   | [], [] -> [sels, List.rev rev] *)
  | [], tokens -> [sels, List.rev rev, tokens]
  | _, [] -> []
  
exception PatternNotFound of string
  
let classify_tokens patterns patterns2 phrase tokens =
(*   print_endline ("classify_tokens 1: '" ^ phrase ^ "'");  *)
(*   print_endline ("classify_tokens 2: " ^ String.concat " " (Xlist.map tokens (fun (orth,tokens2) -> orth ^ ":" ^ String.concat "|" (Xlist.map tokens2 SubsyntaxStringOf.string_of_token))));  *)
  let l = snd (Xlist.fold patterns (1000,[]) (fun (best_prior,best_l) (prior,pat_name,pattern) ->
(*     print_endline ("classify_tokens 3: '" ^ pat_name ^ "' '" ^ phrase ^ "'"); *)
    let found = classify_tokens_rec [] [] (pattern,tokens) in
    let found = Xlist.fold found [] (fun found (sels,matched,rest) ->
      if rest = [] then (sels,matched,[]) :: found else
      let tail = Xlist.map rest fst in
      let is_rest_matched = Xlist.fold patterns2 false (fun is_rest_matched (pat_name,pattern) ->
        let found2 = classify_tokens_rec [] [] (pattern,rest) in
(*        Xlist.iter found2 (fun (sels2,matched2,rest2) ->
          if rest2 <> [] then 
            Printf.printf "%s TAIL: „%s” „%s”\n" phrase (String.concat " " tail) 
              (String.concat " " (Xlist.map rest2 fst) ^ "”"));*)
        if found2 = [] then is_rest_matched else true) in
      if is_rest_matched then (sels,matched,tail) :: found else found) in
    if found = [] then best_prior,best_l else
    add_prior best_prior best_l prior (pat_name,found))) in
  match l with
    [] -> raise (PatternNotFound(String.concat " " (Xlist.map tokens (fun (orth,tokens2) -> 
            orth ^ ":" ^ String.concat "|" (Xlist.map tokens2 SubsyntaxStringOf.string_of_token)))))
  | [t] -> (*print_endline (phrase ^ " PATTERN: " ^ fst t);*) t
  | _ -> (*print_endline (phrase ^ " MULTIPLE PATTERNS: " ^ String.concat ", " (Xlist.map l fst));*) List.hd l
    
let match_pat_tags pats tags =
  if Xlist.size pats <> Xlist.size tags then failwith ("match_pat_tags: |pats|=" ^ string_of_int (Xlist.size pats) ^ " |tags|" ^ string_of_int (Xlist.size tags)) else
  let l = Xlist.map2 pats tags (fun pat tag ->
    match pat,tag with
      V l,_ -> Xlist.map l (fun s -> V [s])
    | G, l -> Xlist.map l (fun s -> V [s])
    | S v,_ -> [S v]) in
  Xlist.multiply_list l
    
let canonical_string lemma pos tags =
  let s = String.concat ":" (Xlist.map tags (function
      V [s] -> s
    | S v -> "$" ^ v
    | _ -> failwith "canonical_string")) in
  if s = "" then lemma ^ ":" ^ pos else lemma ^ ":" ^ pos ^ ":" ^ s
        
let add_canonical map lemma pos tags =
  StringMap.add map (canonical_string lemma pos tags) (lemma,pos,tags)
    
let extract_sels = function
    ["g",g;"n",n] -> n,g
  | ["n",n;"g",g] -> n,g
  | [] -> [],["?"]
  | sels -> failwith ("extract_sels: " ^ string_of_sels sels)
    
let assign_gender gender matched =
  Xlist.map matched (function (lemma,pos,tags) ->
    lemma,pos,Xlist.map tags (function
        S "g" -> V [gender] 
      | t -> t))
    
let assign_gender2 matched =
  Xlist.map matched (function (lemma,pos,tags) ->
    lemma,pos,Xlist.map tags (function
        V["m1"] -> S "g"
      | t -> t))
    
let assign_number number matched =
  if number = ["pl"] then
    Xlist.map matched (function (lemma,pos,tags) ->
      lemma,pos,Xlist.map tags (function
          S "n" -> V ["pl"] 
        | t -> t))
  else matched
    
let assign_number3 number matched =
  if number = ["pl"] then
    Xlist.map matched (function (lemma,pos,tags) ->
      lemma,pos,Xlist.map tags (function
          S "n" -> V ["pl"] 
        | t -> t)) else
  if number = ["sg"] then
    Xlist.map matched (function (lemma,pos,tags) ->
      lemma,pos,Xlist.map tags (function
          S "n" -> V ["sg"] 
        | t -> t))
  else matched
    
let assign_number2 matched =
  Xlist.map matched (function (lemma,pos,tags) ->
    lemma,pos,Xlist.map tags (function
        V["sg"] -> S "n"
      | t -> t))
    
let assign_case matched =
  Xlist.map matched (function (lemma,pos,tags) ->
    lemma,pos,Xlist.map tags (function
        V ["nom"] -> S "c"
      | t -> t))
    
let set_constraints number_flag patterns pat_name l =
  let pattern = Xlist.fold patterns [] (fun pattern (_,pat_name2,pattern2) ->
    if pat_name = pat_name2 then pattern2 else pattern) in
(*   Printf.printf "set_constraints: pat_name=%s |l|=%d\n" pat_name (Xlist.size l); *)
  Xlist.fold l [] (fun found (sels,matched,tail) ->
    if Xlist.size pattern <> Xlist.size matched then failwith "set_constraints" else
    let matched = Xlist.map2 pattern matched (fun pattern matched -> 
      let map = Xlist.fold matched StringMap.empty (fun map token -> 
        match pattern,token with
          LemStar(_,pats), Lemma(lemma,pos,tags,_) | Lem(_,_,pats), Lemma(lemma,pos,tags,_) -> 
(*             print_endline ("set_constraints: " ^ lemma ^ ":" ^ pos); *)
            Xlist.fold tags map (fun map tags -> 
              Xlist.fold (match_pat_tags pats tags) map (fun map tags -> 
                add_canonical map lemma pos tags))
        | O _, Interp s -> add_canonical map s "interp" []
        | _ -> failwith ("set_constraints:" ^ SubsyntaxStringOf.string_of_token token)) in
      StringMap.fold map [] (fun l _ t -> t :: l)) in
(*     Printf.printf "set_constraints: |matched|=%d\n" (Xlist.size matched); *)
    let numbers,genders = extract_sels sels in
    let tail = Xlist.map tail (fun lemma -> lemma,"fixed",[]) in
    Xlist.fold (Xlist.multiply_list matched) found (fun found matched -> 
      Xlist.fold genders found (fun found gender -> 
        let matched = assign_gender gender matched in
        let matched = if number_flag then assign_number numbers matched else assign_number3 numbers matched in
        let matched = assign_case matched in
        (matched @ tail) :: found)))
    
let set_constraints2 patterns pat_name l =
  let pattern = Xlist.fold patterns [] (fun pattern (_,pat_name2,pattern2) ->
    if pat_name = pat_name2 then pattern2 else pattern) in
(*   Printf.printf "set_constraints: pat_name=%s |l|=%d\n" pat_name (Xlist.size l); *)
  Xlist.fold l [] (fun found (sels,matched,tail) ->
    if Xlist.size pattern <> Xlist.size matched then failwith "set_constraints" else
    let matched = Xlist.map2 pattern matched (fun pattern matched -> 
      let map = Xlist.fold matched StringMap.empty (fun map token -> 
        match pattern,token with
          LemStar(_,pats), Lemma(lemma,pos,tags,_) | Lem(_,_,pats), Lemma(lemma,pos,tags,_) -> 
            Xlist.fold tags map (fun map tags -> 
              Xlist.fold (match_pat_tags pats tags) map (fun map tags -> 
                add_canonical map lemma pos tags))
        | O _, Interp s -> add_canonical map s "interp" []
        | _ -> failwith ("set_constraints:" ^ SubsyntaxStringOf.string_of_token token)) in
      StringMap.fold map [] (fun l _ t -> t :: l)) in
(*     Printf.printf "set_constraints: |matched|=%d\n" (Xlist.size matched); *)
    let tail = Xlist.map tail (fun lemma -> lemma,"fixed",[]) in
    Xlist.fold (Xlist.multiply_list matched) found (fun found matched -> 
        let matched = assign_gender2 matched in
        let matched = assign_number2 matched in
        let matched = assign_case matched in
        (matched @ tail) :: found))
    
let split_gender l =
  Xlist.map l (function
      "m1:pt" -> [["m1"];["pt"]]
    | "n:pt" -> [["n"];["pt"]]
    | "n:col" -> [["n"];["col"]]
    | "n:ncol" -> [["n"];["ncol"]]
    | s -> [[s];["x"]])
    
let subst_of_ger orth tags =
  let lemma =
(*    if Xstring.check_sufix "NIE" orth then (Xstring.cut_sufix "NIE" orth) ^ "NIE" else (* FIXME *)
    if Xstring.check_sufix "NIA" orth then (Xstring.cut_sufix "NIA" orth) ^ "NIE" else (* FIXME *)*)
    if Xstring.check_sufix "nie" orth then (Xstring.cut_sufix "nie" orth) ^ "nie" else
    if Xstring.check_sufix "nia" orth then (Xstring.cut_sufix "nia" orth) ^ "nie" else
    if Xstring.check_sufix "niu" orth then (Xstring.cut_sufix "niu" orth) ^ "nie" else
    if Xstring.check_sufix "niem" orth then (Xstring.cut_sufix "niem" orth) ^ "nie" else
    if Xstring.check_sufix "ń" orth then (Xstring.cut_sufix "ń" orth) ^ "nie" else
    if Xstring.check_sufix "niom" orth then (Xstring.cut_sufix "niom" orth) ^ "nie" else
    if Xstring.check_sufix "niami" orth then (Xstring.cut_sufix "niami" orth) ^ "nie" else
    if Xstring.check_sufix "niach" orth then (Xstring.cut_sufix "niach" orth) ^ "nie" else
    if Xstring.check_sufix "cie" orth then (Xstring.cut_sufix "cie" orth) ^ "cie" else
    if Xstring.check_sufix "cia" orth then (Xstring.cut_sufix "cia" orth) ^ "cie" else
    if Xstring.check_sufix "ciu" orth then (Xstring.cut_sufix "ciu" orth) ^ "cie" else
    if Xstring.check_sufix "ciem" orth then (Xstring.cut_sufix "ciem" orth) ^ "cie" else
    if Xstring.check_sufix "ć" orth then (Xstring.cut_sufix "ć" orth) ^ "cie" else
    if Xstring.check_sufix "ciom" orth then (Xstring.cut_sufix "ciom" orth) ^ "cie" else
    if Xstring.check_sufix "ciami" orth then (Xstring.cut_sufix "ciami" orth) ^ "cie" else
    if Xstring.check_sufix "ciach" orth then (Xstring.cut_sufix "ciach" orth) ^ "cie" else
    raise Strange (*failwith ("subst_of_ger 1: " ^ orth)*) in
  let tags = match tags with 
      [n;c;["n"];a;neg] -> [n;c;["n:ncol"]] 
    | _ -> failwith "subst_of_ger 2" in
  lemma,"subst",tags
  
let adj_of_pact orth tags =
  let lemma =
    if Xstring.check_sufix "ący" orth then (Xstring.cut_sufix "ący" orth) ^ "ący" else
    if Xstring.check_sufix "ącego" orth then (Xstring.cut_sufix "ącego" orth) ^ "ący" else
    if Xstring.check_sufix "ącemu" orth then (Xstring.cut_sufix "ącemu" orth) ^ "ący" else
    if Xstring.check_sufix "ącym" orth then (Xstring.cut_sufix "ącym" orth) ^ "ący" else
    if Xstring.check_sufix "ące" orth then (Xstring.cut_sufix "ące" orth) ^ "ący" else
    if Xstring.check_sufix "ąca" orth then (Xstring.cut_sufix "ąca" orth) ^ "ący" else
    if Xstring.check_sufix "ącej" orth then (Xstring.cut_sufix "ącej" orth) ^ "ący" else
    if Xstring.check_sufix "ącą" orth then (Xstring.cut_sufix "ącą" orth) ^ "ący" else
    if Xstring.check_sufix "ącymi" orth then (Xstring.cut_sufix "ącymi" orth) ^ "ący" else
    if Xstring.check_sufix "ących" orth then (Xstring.cut_sufix "ących" orth) ^ "ący" else
    raise Strange (*failwith ("adj_of_pact 1: " ^ orth)*) in
  let tags = match tags with 
      [n;c;g;a;neg] -> [n;c;g;["pos"]] 
    | _ -> failwith "adj_of_pact 2" in
  lemma,"adj",tags
  
let adj_of_ppas orth tags =
  if Xstring.check_sufix "no" orth then (Xstring.cut_sufix "no" orth) ^ "ny", "adja", [] else
  if Xstring.check_sufix "to" orth then (Xstring.cut_sufix "to" orth) ^ "ty", "adja", [] else
  let lemma =
    if Xstring.check_sufix "ny" orth then (Xstring.cut_sufix "ny" orth) ^ "ny" else
    if Xstring.check_sufix "nego" orth then (Xstring.cut_sufix "nego" orth) ^ "ny" else
    if Xstring.check_sufix "nemu" orth then (Xstring.cut_sufix "nemu" orth) ^ "ny" else
    if Xstring.check_sufix "nym" orth then (Xstring.cut_sufix "nym" orth) ^ "ny" else
    if Xstring.check_sufix "ne" orth then (Xstring.cut_sufix "ne" orth) ^ "ny" else
    if Xstring.check_sufix "na" orth then (Xstring.cut_sufix "na" orth) ^ "ny" else
    if Xstring.check_sufix "nej" orth then (Xstring.cut_sufix "nej" orth) ^ "ny" else
    if Xstring.check_sufix "ną" orth then (Xstring.cut_sufix "ną" orth) ^ "ny" else
    if Xstring.check_sufix "nymi" orth then (Xstring.cut_sufix "nymi" orth) ^ "ny" else
    if Xstring.check_sufix "nych" orth then (Xstring.cut_sufix "nych" orth) ^ "ny" else
    if Xstring.check_sufix "eni" orth then (Xstring.cut_sufix "eni" orth) ^ "ony" else (* FIXME: trzeba sprawdzić alternacje *)
    if Xstring.check_sufix "ani" orth then (Xstring.cut_sufix "ani" orth) ^ "any" else
    if Xstring.check_sufix "ty" orth then (Xstring.cut_sufix "ty" orth) ^ "ty" else
    if Xstring.check_sufix "tego" orth then (Xstring.cut_sufix "tego" orth) ^ "ty" else
    if Xstring.check_sufix "temu" orth then (Xstring.cut_sufix "temu" orth) ^ "ty" else
    if Xstring.check_sufix "tym" orth then (Xstring.cut_sufix "tym" orth) ^ "ty" else
    if Xstring.check_sufix "te" orth then (Xstring.cut_sufix "te" orth) ^ "ty" else
    if Xstring.check_sufix "ta" orth then (Xstring.cut_sufix "ta" orth) ^ "ty" else
    if Xstring.check_sufix "tej" orth then (Xstring.cut_sufix "tej" orth) ^ "ty" else
    if Xstring.check_sufix "tą" orth then (Xstring.cut_sufix "tą" orth) ^ "ty" else
    if Xstring.check_sufix "tymi" orth then (Xstring.cut_sufix "tymi" orth) ^ "ty" else
    if Xstring.check_sufix "tych" orth then (Xstring.cut_sufix "tych" orth) ^ "ty" else
    if Xstring.check_sufix "ci" orth then (Xstring.cut_sufix "ci" orth) ^ "ty" else (* FIXME: trzeba sprawdzić alternacje *)
    raise Strange (*failwith ("adj_of_ppas 1: " ^ orth)*) in
  let tags = match tags with 
      [n;c;g;a;neg] -> [n;c;g;["pos"]] 
    | _ -> failwith "adj_of_ppas 2" in
  lemma,"adj",tags
 
let set_str_case found = function
    "inf subst:str" -> Xlist.map found (function
        t :: (lemma,pos,[n;_;g;p]) :: l -> t :: (lemma,pos,[n;S "str";g;p]) :: l
      | _ -> failwith "set_str_case")
  | _ -> found
    
let select_orths orths found =
  snd (Xlist.fold found (1000,[]) (fun (best_prior,best_l) matched ->
    let prior = Xlist.fold2 orths matched 0 (fun n orth (lemma,pos,tags) ->
      if orth = lemma then n else n+1) in
    add_prior best_prior best_l prior matched))
 
let remove_x found =
  Xlist.map found (fun matched ->
    Xlist.map matched (function 
        lemma,"subst",[n;c;g;V["x"]] -> lemma,"subst",[n;c;g]
      | t -> t))
 
let fixed_set = ref StringSet.empty
 
let parse_np_nom number_flag phrase = 
    if phrase = "" then [] else
    let tokens = Patterns.parse phrase in
    let tokens = disambiguate_tokens tokens in
    if is_strange tokens then (*print_endline ("process_subst: " ^ phrase);*) raise Strange else
    let orths = Xlist.map tokens Tokenizer.get_orth in
    let tokens = Xlist.map tokens (function
        Interp s -> (*print_endline ("process_subst 2");*) s,(*0,*)[Interp s]
      | t -> 
          let orth = Tokenizer.get_orth t in
          let l = if StringSet.mem !fixed_set orth then [orth,"fixed",[]] else lemmatize_token t in
          orth, List.flatten (Xlist.map l (fun (lemma,pos,tags) -> 
(*             Printf.printf "parse_np_nom: %s:%s:%s\n" lemma pos (Tagset.render [tags]); *)
            let lemma,pos,tags = 
              if pos = "ger" then subst_of_ger orth tags else
              if pos = "pact" then adj_of_pact orth tags else
              if pos = "ppas" then adj_of_ppas orth tags else
              lemma,pos,tags in              
            if pos = "subst" then 
              match tags with 
                [n;c;g] -> Xlist.map (split_gender g) (fun gc ->
                  Lemma(lemma,pos,[[n;c] @ gc],"X"))
              | _ -> failwith "process_subst 3" 
            else [Lemma(lemma,pos,[tags],"X")]))) in
    let pat_name, tokens = classify_tokens patterns_np patterns_np2 phrase tokens in
    if pat_name = "subst.nom subst.nom._" then print_endline ("parse_np_nom aposition: " ^ phrase);
    let found = set_constraints number_flag patterns_np pat_name tokens in
    let found = select_orths orths found in
    let found = remove_x found in
(*     Printf.printf "%s: %s\n" phrase pat_name; *)
(*    Xlist.iter tokens (fun (sels,matched,tail) -> Printf.printf "%s | %s | %s\n" (string_of_sels sels) 
      (String.concat "; " (Xlist.map matched (fun l -> String.concat ", " (Xlist.map l SubsyntaxStringOf.string_of_token))))
      (String.concat " " tail));*)
(*    Xlist.iter found (fun matched -> Printf.printf "%s\n" 
      (String.concat " " (Xlist.map matched (fun (lemma,pos,tags) -> canonical_string lemma pos tags))));*)
    found

let parse_infp phrase = 
(*     print_endline ("parse_infp 1: " ^ phrase);  *)
    if phrase = "" then [] else (
    let tokens = Patterns.parse phrase in
    let tokens = disambiguate_tokens tokens in
    if is_strange tokens then (*print_endline ("process_subst: " ^ phrase);*) raise Strange else (
    let orths = Xlist.map tokens Tokenizer.get_orth in
    let tokens = Xlist.map tokens (function
        Interp s -> (*print_endline ("process_subst 2");*) s,(*0,*)[Interp s]
      | t -> 
          let orth = Tokenizer.get_orth t in
          let l = if StringSet.mem !fixed_set orth then [orth,"fixed",[]] else lemmatize_token t in
          orth, List.flatten (Xlist.map l (fun (lemma,pos,tags) -> 
(*             Printf.printf "parse_infp: %s:%s:%s\n" lemma pos (Tagset.render [tags]); *)
            if pos = "subst" then 
              match tags with 
                [n;c;g] -> Xlist.map (split_gender g) (fun gc ->
                  Lemma(lemma,pos,[[n;c] @ gc],"X"))
              | _ -> failwith "parse_infp 3" 
            else [Lemma(lemma,pos,[tags],"X")]))) in
    let pat_name, tokens = classify_tokens patterns_infp patterns_infp2 phrase tokens in
    let found = set_constraints true patterns_infp pat_name tokens in
(*     Printf.printf "parse_infp: |found|=%d\n" (Xlist.size found); *)
    let found = set_str_case found pat_name in
    let found = select_orths orths found in
    let found = remove_x found in
(*     print_endline "parse_infp 2"; *)
    found))

let parse_adjp_sg_nom_m phrase = 
    if phrase = "" then [] else
    let tokens = Patterns.parse phrase in
    let tokens = disambiguate_tokens tokens in
    if is_strange tokens then (*print_endline ("process_subst: " ^ phrase);*) raise Strange else
    let orths = Xlist.map tokens Tokenizer.get_orth in
    let tokens = Xlist.map tokens (function
        Interp s -> (*print_endline ("process_subst 2");*) s,(*0,*)[Interp s]
      | t -> 
          let orth = Tokenizer.get_orth t in
          let l = if StringSet.mem !fixed_set orth then [orth,"fixed",[]] else lemmatize_token t in
          orth, List.flatten (Xlist.map l (fun (lemma,pos,tags) -> 
(*             Printf.printf "parse_np_nom: %s:%s:%s\n" lemma pos (Tagset.render [tags]); *)
            let lemma,pos,tags = 
              if pos = "ger" then subst_of_ger orth tags else
              if pos = "pact" then adj_of_pact orth tags else
              if pos = "ppas" then adj_of_ppas orth tags else
              lemma,pos,tags in              
            if pos = "subst" then 
              match tags with 
                [n;c;g] -> Xlist.map (split_gender g) (fun gc ->
                  Lemma(lemma,pos,[[n;c] @ gc],"X"))
              | _ -> failwith "process_subst 3" 
            else [Lemma(lemma,pos,[tags],"X")]))) in
    let pat_name, tokens = classify_tokens patterns_adjp patterns_adjp2 phrase tokens in
    let found = set_constraints2 patterns_adjp pat_name tokens in
    let found = select_orths orths found in
    let found = remove_x found in
(*     Printf.printf "%s: %s\n" phrase pat_name; *)
(*    Xlist.iter tokens (fun (sels,matched,tail) -> Printf.printf "%s | %s | %s\n" (string_of_sels sels) 
      (String.concat "; " (Xlist.map matched (fun l -> String.concat ", " (Xlist.map l SubsyntaxStringOf.string_of_token))))
      (String.concat " " tail));*)
(*    Xlist.iter found (fun matched -> Printf.printf "%s\n" 
      (String.concat " " (Xlist.map matched (fun (lemma,pos,tags) -> canonical_string lemma pos tags))));*)
    found


let initialize fixed =
  fixed_set := StringSet.of_list fixed;
  Inflexion.initialize ();
  ()
  

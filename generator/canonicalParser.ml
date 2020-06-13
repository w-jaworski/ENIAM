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

(*let lemmatize_strings2 lemma pos tags best_prior best_l is_validated_by_sgjp star has_equal_cases =
(*   Printf.printf "lemmatize_strings2 1: %d %s %s %s \"%s\"\n%!" best_prior lemma pos (Tagset.render [tags]) (MorphologyRules.string_of_star star); *)
    let prior = 
      if is_validated_by_sgjp then 2 else
      if (not has_equal_cases) || star = MorphologyTypes.Aux2 then max_int else 3 in
(*     Printf.printf "lemmatize_strings2 2: %d %s\n%!" prior cat; *)
    if prior > best_prior then best_prior,best_l else
    if prior < best_prior then prior,[(*obs,lem,*)lemma,pos,tags] else
    best_prior,((*obs,lem,*)lemma,pos,tags) :: best_l

let lemmatize_strings l =
  Xlist.fold l (1000,[]) (fun (best_prior,best_l) (s,obs,lem) ->
    let interpretations = get_interpretations s in
    Xlist.fold interpretations (best_prior,best_l) (fun (best_prior,best_l) i ->
(*       Printf.printf "lemmatize_strings 1: %s\n%!" (string_of_interpretation i); *)
      let i = (* FIXME: obejście braku tagu ndm we freq_rules *)
        if i.star = MorphologyTypes.Productive && i.tags = ["cat","ndm"] then 
          {i with star=MorphologyTypes.Ndm} else i in
(*       Printf.printf "lemmatize_strings 2: %s\n%!" (string_of_interpretation i); *)
      let is_validated_by_sgjp = i.status = LemmaVal || i.status = LemmaAlt in
      Xlist.fold (Tagset.parse i.interp) (best_prior,best_l) (fun (best_prior,best_l) (pos,tags) ->  (* zakładam, że tagi generowane przez analizator morfologiczny są poprawne i nie mają _ *)
        if pos = "brev" then best_prior,best_l else
        lemmatize_strings2 i.lemma pos tags best_prior best_l is_validated_by_sgjp i.star (obs = lem))))

let lemmatize_token = function
    SmallLetter(uc,lc) -> lemmatize_strings [uc,(SL : letter_size),(CL : letter_size);lc,SL,SL]
  | CapLetter(uc,lc) -> lemmatize_strings [uc,(CL : letter_size),(CL : letter_size);lc,CL,SL]
  | AllSmall(uc,fc,lc) -> lemmatize_strings [uc,(AS : letter_size),AC;fc,AS,FC;lc,AS,AS]
  | AllCap(uc,fc,lc) -> lemmatize_strings [uc,(AC : letter_size),AC;fc,AC,FC;lc,AC,AS]
  | FirstCap(uc,fc,lc) -> lemmatize_strings [uc,(FC : letter_size),AC;fc,FC,FC;lc,FC,AS]
  | SomeCap(uc,orth,lc) -> lemmatize_strings [uc,(SC : letter_size),AC;orth,SC,SC;lc,SC,AS]
  | t -> failwith ("lemmatize_token: " ^ SubsyntaxStringOf.string_of_token t)*)

let lemmatize_string s =
  Xlist.fold (get_interpretations s) [] (fun l i ->
(*       Printf.printf "lemmatize_strings 1: %s\n%!" (string_of_interpretation i); *)    
(*     let is_validated_by_sgjp = i.status = LemmaVal || i.status = LemmaAlt in *)
    if i.star = MorphologyTypes.Productive && i.tags = ["cat","ndm"] then (i.lemma,"ndm",[]) :: l else
    if i.star = MorphologyTypes.Productive || i.star = MorphologyTypes.Star then
      Xlist.fold (Tagset.parse i.interp) l (fun l (pos,tags) ->
        if pos = "brev" then l else
        (i.lemma,pos,tags) :: l)
    else l)

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
  | Seq[Token{orth="x"};Token _] -> []
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
  | Token{token=(Symbol "." as t)} :: l -> t :: disambiguate_tokens l
  | Token{token=(Other "α" as t)} :: l -> t :: disambiguate_tokens l
  | Token{token=(Other "β" as t)} :: l -> t :: disambiguate_tokens l
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
  
let patterns = [
  1,"subst.nom",[LemStar("subst",[S "n";V["nom"];S "g"])];
  1,"subst.nom się",[LemStar("subst",[S "n";V["nom"];S "g"]);Lem("się","qub",[])];
  1,"adj.nom subst.nom",[LemStar("adj",[S "n";V["nom"];S "g";G]);LemStar("subst",[S "n";V["nom"];S "g"])];
  1,"subst.nom adj.nom",[LemStar("subst",[S "n";V["nom"];S "g"]);LemStar("adj",[S "n";V["nom"];S "g";G])];
  1,"subst.nom adja-adj.nom",[
    LemStar("subst",[S "n";V["nom"];S "g"]);LemStar("adja",[]);O "-";LemStar("adj",[S "n";V["nom"];S "g";G])];
  1,"subst.nom adj.nom adj.nom",[
    LemStar("subst",[S "n";V["nom"];S "g"]);LemStar("adj",[S "n";V["nom"];S "g";G]);LemStar("adj",[S "n";V["nom"];S "g";G])];
  1,"adj.nom subst.nom adj.nom",[
    LemStar("adj",[S "n";V["nom"];S "g";G]);LemStar("subst",[S "n";V["nom"];S "g"]);LemStar("adj",[S "n";V["nom"];S "g";G])];
(*  3,"subst.nom subst.gen",[LemStar("subst",[S "n";V["nom"];S "g"]);LemStar("subst",[G;V["gen"];G])];
  1,"subst.nom adj.gen subst.gen",[
    LemStar("subst",[S "n";V["nom"];S "g"]);LemStar("adj",[S "n2";V["gen"];S "g2";G]);LemStar("subst",[S "n2";V["gen"];S "g2"])];
  1,"subst.nom subst.gen adj.gen",[
    LemStar("subst",[S "n";V["nom"];S "g"]);LemStar("subst",[S "n2";V["gen"];S "g2"]);LemStar("adj",[S "n2";V["gen"];S "g2";G])];
  1,"subst.nom prep.case subst.case",[LemStar("subst",[S "n";V["nom"];S "g"]);LemStar("prep",[S "c2"]);LemStar("subst",[G;S "c2";G])];
  1,"subst.nom prep.case subst.case adj.case",[LemStar("subst",[S "n";V["nom"];S "g"]);LemStar("prep",[S "c2"]);LemStar("subst",[S "n2";S "c2";S "g2"]);LemStar("adj",[S "n2";S "c2";S "g2";G])];*)
  ]

let patterns2 = [
  3,"subst.gen",[LemStar("subst",[G;V["gen"];G])];
  1,"adj.gen subst.gen",[LemStar("adj",[S "n2";V["gen"];S "g2";G]);LemStar("subst",[S "n2";V["gen"];S "g2"])];
  1,"subst.gen adj.gen",[LemStar("subst",[S "n2";V["gen"];S "g2"]);LemStar("adj",[S "n2";V["gen"];S "g2";G])];
  1,"prep.case subst.case",[LemStar("prep",[S "c2"]);LemStar("subst",[G;S "c2";G])];
  1,"prep.case subst.case adj.case",[LemStar("prep",[S "c2"]);LemStar("subst",[S "n2";S "c2";S "g2"]);LemStar("adj",[S "n2";S "c2";S "g2";G])];
  1,"prep.case adj.case subst.case",[LemStar("prep",[S "c2"]);LemStar("adj",[S "n2";S "c2";S "g2";G]);LemStar("subst",[S "n2";S "c2";S "g2"])];
  ]
  
  
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
(*            print_endline ("classify_tokens_rec 1: token=" ^ SubsyntaxStringOf.string_of_token token ^ " sels=" ^ string_of_sels sels);  *)
          let sels = Patterns.match_token sels (pat,token) in
(*            print_endline ("classify_tokens_rec 2: sels=" ^ String.concat " | " (Xlist.map sels string_of_sels));  *)
          (sels,token) :: matched
        with Not_found -> matched) in
      if matched = [] then [] else
      List.flatten (Xlist.map (group_tokens matched) (fun (matched_sels,matched_tokens) ->
        classify_tokens_rec matched_sels (matched_tokens :: rev) (pats,tokens)))
(*   | [], [] -> [sels, List.rev rev] *)
  | [], tokens -> [sels, List.rev rev, tokens]
  | _, [] -> []
  
let oths_of_tokens tokens =
  String.concat " " (Xlist.map tokens (fun (orth,(*_,*)_) -> orth))
  
let classify_tokens phrase tokens =
  let found = Xlist.fold patterns [] (fun found (prior,pat_name,pattern) ->
(*      print_endline ("classify_tokens: '" ^ pat_name ^ "' '" ^ phrase ^ "'");  *)
    let found2 = classify_tokens_rec [] [] (pattern,tokens) in
    let found2 = Xlist.fold found2 [] (fun found2 (sels,matched,tokens) ->
      if tokens = [] then (sels,matched,tokens) :: found2 else
      let x = oths_of_tokens tokens in
      let tail = Xlist.fold patterns2 [] (fun tail (prior,pat_name,pattern) ->
        let tail2 = classify_tokens_rec [] [] (pattern,tokens) in
        Xlist.fold tail2 tail (fun tail (sels,matched,tokens) ->
          let y = oths_of_tokens tokens in
          if tokens = [] then (sels,matched,tokens) :: tail else (
            print_endline ("classify_tokens tail: '" ^ phrase ^ "' '" ^ x ^ "' '" ^ y); tail))) in
      if tail = [] then found2 else (sels,matched,tokens) :: found2) in
    if found2 = [] then found else
    (pat_name,found2) :: found) in
  match found with
    [] -> print_endline ("classify_tokens: '" ^ phrase ^ "' pattern not found"); []
(*   | [t] -> [t] *)
  | _ -> print_endline ("classify_tokens: '" ^ phrase ^ "' patterns: " ^ String.concat ", " (Xlist.map found fst)); found
    
let simplify_gender l =
  Xlist.map l (function
      "m1:pt" -> "m1"
    | "n:pt" -> "n"
    | "n:col" -> "n"
    | "n:ncol" -> "n"
    | s -> s)
    
let subst_of_ger orth tags =
  let lemma =
    if Xstring.check_sufix "NIE" orth then (Xstring.cut_sufix "NIE" orth) ^ "NIE" else (* FIXME *)
    if Xstring.check_sufix "NIA" orth then (Xstring.cut_sufix "NIA" orth) ^ "NIE" else (* FIXME *)
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
    failwith ("subst_of_ger 1: " ^ orth) in
  let tags = match tags with 
      [n;c;g;a;neg] -> [n;c;g] 
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
    failwith ("adj_of_pact 1: " ^ orth) in
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
    failwith ("adj_of_ppas 1: " ^ orth) in
  let tags = match tags with 
      [n;c;g;a;neg] -> [n;c;g;["pos"]] 
    | _ -> failwith "adj_of_ppas 2" in
  lemma,"adj",tags
 
exception Strange
 
let parse_np_nom phrase = 
    let tokens = Patterns.parse phrase in
    let tokens = disambiguate_tokens tokens in
    if is_strange tokens then (*print_endline ("process_subst: " ^ phrase);*) raise Strange else
    let tokens = Xlist.map tokens (function
        Interp s -> (*print_endline ("process_subst 2");*) s,(*0,*)[Interp s]
      | t -> 
          let orth = Tokenizer.get_orth t in
          let (*prior,*) l = lemmatize_token t in
          orth, (*prior,*) Xlist.map l (fun (lemma,pos,tags) -> 
            let lemma,pos,tags = 
              if pos = "ger" then subst_of_ger orth tags else
              if pos = "pact" then adj_of_pact orth tags else
              if pos = "ppas" then adj_of_ppas orth tags else
              lemma,pos,tags in              
            let tags = 
              if pos = "subst" then 
                match tags with 
                  [n;c;g] -> [n;c;simplify_gender g] 
                | _ -> failwith "process_subst 3" 
              else tags in
            Lemma(lemma,pos,[tags],"X"))) in
    let tokens = classify_tokens phrase tokens in
    tokens

let initialize () =
  Inflexion.initialize ();
  ()
  

(*
 *  ENIAMsubsyntax: tokenization, lemmatization, MWE and sentence detecion for Polish
 *  Copyright (C) 2016-2018 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
 *  Copyright (C) 2016-2018 Institute of Computer Science Polish Academy of Sciences
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
open Inflexion
open Xstd

let match_tags a pos tags =
(*   if a.pos <> pos then raise Not_found else *)
  let tags = if a.number <> "" then Tagset.select_tag "numbers" a.number pos tags else tags in
  let tags = if a.gender <> "" then Tagset.select_tag "genders" a.gender pos tags else tags in
  (*if a.col <> "" then Tagset.select_tag "cols" a.col pos tags else*) tags
  
let get_ontological_category lemma pos tags attrs =
(*   Printf.printf "get_ontological_category 1: %s %s %s\n%!" lemma pos (Tagset.simplify_pos pos); *)
  let set = 
    try StringMap.find (StringMap.find !known_lemmata lemma) (Tagset.simplify_pos pos)
    with Not_found -> (*print_endline ("get_ontological_category: " ^ lemma ^ " not found");*) 
      (try StringMap.find !known_pos pos with Not_found -> OntSet.empty) in
(*   Printf.printf "get_ontological_category 2: |set|=%d\n%!" (OntSet.size set); *)
  let l = OntSet.fold set [] (fun l a -> 
    try
      Xlist.iter a.html_tags (fun s -> if not (Xlist.mem attrs (HtmlTag s)) then raise Not_found);
(*      Printf.printf "get_ontological_category 3: %s %s %s\n%!" (Tagset.render [tags]) a.number a.gender; *)
      let tags = match_tags a pos tags in
(*      Printf.printf "get_ontological_category 4: %s %s\n%!" (Tagset.render [tags]) a.ont_cat; *)
      (true,a.no_sgjp,a.poss_ndm,a.exact_case,a.ont_cat,tags) :: l
    with Not_found -> l) in
(*   Printf.printf "get_ontological_category 5: |l|=%d\n%!" (Xlist.size l); *)
  if l = [] then [false,false,false,false,"X",tags] else l
(*   [StringSet.mem !known_lemmata lemma,false,false,"X",tags] (* FIXME: todo *) *)

(* let lemmatize_strings l =
  let l1,l2 = Xlist.fold l ([],[]) (fun (l1,l2) (s,obs,lem) ->
    let l = Inflexion.get_interpretations s in
    Xlist.fold l (l1,l2) (fun (l1,l2) i ->
      if i.Inflexion.status = Inflexion.LemmaVal ||
         i.Inflexion.status = Inflexion.LemmaAlt then (i,obs,lem) :: l1, l2 else
      if StringSet.mem !known_lemmata i.Inflexion.lemma then (i,obs,lem) :: l1, l2 else l1, (i,obs,lem) :: l2)) in
  if l1 = [] then l2,false else l1,true *)

let calculate_priority2 star has_agl_suf equal_case =
  if has_agl_suf || (not equal_case) || star = MorphologyTypes.Aux2 then max_int else 3

let calculate_priority cat is_in_lexicon lemma_in_sgjp is_validated_by_sgjp has_no_sgjp_tag star has_poss_ndm_tag has_exact_case_tag has_agl_suf equal_case =
  let is_in_lexicon = if is_in_lexicon && has_exact_case_tag && not equal_case then false else is_in_lexicon in
  match is_in_lexicon,lemma_in_sgjp,is_validated_by_sgjp,has_no_sgjp_tag,(star = MorphologyTypes.Ndm),has_poss_ndm_tag with
    true,true,true,_,_,_ -> 1,cat
  | true,true,false,true,_,_ -> 1,cat
  | true,true,false,false,_,_ -> calculate_priority2 star has_agl_suf equal_case, "X"
  | true,false,_,_,true,true -> 1,cat
  | true,false,_,_,true,false -> calculate_priority2 star has_agl_suf equal_case, "X"
  | true,false,_,_,false,_ -> 1,cat
  | false,_,true,_,_,_ -> 2,"X"
  | false,_,false,_,_,_ -> calculate_priority2 star has_agl_suf equal_case, "X"

let lemmatize_strings2 lemma pos tags attrs best_prior best_l is_validated_by_sgjp star has_agl_suf has_equal_cases =
(*   Printf.printf "lemmatize_strings2 1: %d %s %s %s \"%s\"\n%!" best_prior lemma pos (Tagset.render [tags]) (MorphologyRules.string_of_star star); *)
  Xlist.fold (get_ontological_category lemma pos tags attrs) (best_prior,best_l) (fun (best_prior,best_l) (is_in_lexicon,has_no_sgjp_tag,has_poss_ndm_tag,has_exact_case_tag,cat,tags) ->
    let prior,cat = calculate_priority cat is_in_lexicon
      (StringSet.mem !Inflexion.lemmata lemma) is_validated_by_sgjp has_no_sgjp_tag
      star has_poss_ndm_tag has_exact_case_tag has_agl_suf has_equal_cases in
(*     Printf.printf "lemmatize_strings2 2: %d %s\n%!" prior cat; *)
    if prior > best_prior then best_prior,best_l else
    if prior < best_prior then prior,[(*obs,lem,*)lemma,pos,tags,cat] else
    best_prior,((*obs,lem,*)lemma,pos,tags,cat) :: best_l)

let lemmatize_strings attrs has_agl_suf agl_suf l =
  Xlist.fold l (1000,[]) (fun (best_prior,best_l) (s,obs,lem) ->
    let interpretations = if agl_suf then get_suf_interpretations s else get_interpretations s in
    Xlist.fold interpretations (best_prior,best_l) (fun (best_prior,best_l) i ->
(*       Printf.printf "lemmatize_strings 1: %s\n%!" (string_of_interpretation i); *)
      let i = (* FIXME: obejście braku tagu ndm we freq_rules *)
        if i.star = MorphologyTypes.Productive && i.tags = ["cat","ndm"] then 
          {i with star=MorphologyTypes.Ndm} else i in
(*       Printf.printf "lemmatize_strings 2: %s\n%!" (string_of_interpretation i); *)
      let is_validated_by_sgjp = i.status = LemmaVal || i.status = LemmaAlt in
      Xlist.fold (Tagset.parse i.interp) (best_prior,best_l) (fun (best_prior,best_l) (pos,tags) ->  (* zakładam, że tagi generowane przez analizator morfologiczny są poprawne i nie mają _ *)
        if pos = "brev" then best_prior,best_l else
        lemmatize_strings2 i.lemma pos tags attrs best_prior best_l is_validated_by_sgjp i.star has_agl_suf (obs = lem))))

let lemmatize_token attrs has_agl_suf agl_suf = function
    SmallLetter(uc,lc) -> lemmatize_strings attrs has_agl_suf agl_suf [uc,(SL : letter_size),(CL : letter_size);lc,SL,SL]
  | CapLetter(uc,lc) -> lemmatize_strings attrs has_agl_suf agl_suf [uc,(CL : letter_size),(CL : letter_size);lc,CL,SL]
  | AllSmall(uc,fc,lc) -> lemmatize_strings attrs has_agl_suf agl_suf [uc,(AS : letter_size),AC;fc,AS,FC;lc,AS,AS]
  | AllCap(uc,fc,lc) -> lemmatize_strings attrs has_agl_suf agl_suf [uc,(AC : letter_size),AC;fc,AC,FC;lc,AC,AS]
  | FirstCap(uc,fc,lc) -> lemmatize_strings attrs has_agl_suf agl_suf [uc,(FC : letter_size),AC;fc,FC,FC;lc,FC,AS]
  | SomeCap(uc,orth,lc) -> lemmatize_strings attrs has_agl_suf agl_suf [uc,(SC : letter_size),AC;orth,SC,SC;lc,SC,AS]
  | Lemma(lemma,pos,interp,_) -> (* ten przypadek nie powinien być używany *)
      Xlist.fold interp (1000,[]) (fun (best_prior,best_l) tags ->
        lemmatize_strings2 lemma pos tags attrs best_prior best_l true MorphologyTypes.Star has_agl_suf true)
  | t -> 1000,[]

let is_known_orth = function
    SmallLetter(uc,lc) -> true
  | CapLetter(uc,lc) -> true
  | AllSmall(uc,fc,lc) -> StringSet.mem !known_orths uc || StringSet.mem !known_orths fc || StringSet.mem !known_orths lc
  | AllCap(uc,fc,lc) -> StringSet.mem !known_orths uc || StringSet.mem !known_orths fc || StringSet.mem !known_orths lc
  | FirstCap(uc,fc,lc) -> StringSet.mem !known_orths uc || StringSet.mem !known_orths fc || StringSet.mem !known_orths lc
  | SomeCap(uc,orth,lc) -> StringSet.mem !known_orths uc || StringSet.mem !known_orths orth || StringSet.mem !known_orths lc
  | Lemma _ -> false
  | t -> true

let rec lemmatize_rec = function
    Token t ->
(*       print_endline ("lemmatize_rec 1: " ^ t.orth ^ " " ^ SubsyntaxStringOf.string_of_token t.token);  *)
      (*let l,b = lemmatize_token (Xlist.mem t.attrs HasAglSuffix) t.token in
      if Xlist.mem t.attrs HasAglSuffix && not b then Variant[],false else
      let t = {t with attrs=Xlist.remove_all t.attrs HasAglSuffix} in
      let attrs = if b then t.attrs else LemmNotVal :: t.attrs in
      let l = List.flatten (Xlist.rev_map l (fun (i,obs,lem) ->
        if (not b) && obs <> lem then [] else
        Xlist.rev_map (Tagset.parse i.Inflexion.interp) (fun (pos,tags) ->  (* zakładam, że tagi generowane przez analizator morfologiczny są poprawne i nie mają _ *)
          Token{t with token=Lemma(i.Inflexion.lemma,pos,[tags]); attrs=attrs}))) in (* FIXME obs,lem,i.Inflexion.star,i.Inflexion.freq,i.Inflexion.tags *)
      if is_known_orth t.token then Variant(Token t :: l), true else Variant l, b*)
      let agl_suf = Xlist.mem t.attrs AglSuffix in
      let has_agl_suf = Xlist.mem t.attrs HasAglSuffix in
      let prior,l = lemmatize_token t.attrs has_agl_suf agl_suf t.token in
      let t = {t with attrs=Xlist.remove_all t.attrs AglSuffix} in
      let t = {t with attrs=Xlist.remove_all t.attrs HasAglSuffix} in
      let attrs = if prior <= 2 then t.attrs else LemmNotVal :: t.attrs in
      let l = Xlist.rev_map l (fun ((*obs,lem,*)lemma,pos,tags,cat) ->
        Token{t with token=Lemma(lemma,pos,[tags],cat); attrs=attrs}) in
(* 	  if is_known_orth t.token then print_endline "lemmatize_rec: known_orth"; *)
(* 	  if is_known_orth t.token && not has_agl_suf then print_endline "lemmatize_rec: known_orth 2"; *)
      if is_known_orth t.token && not has_agl_suf && not agl_suf then [Token t],Variant l,prior  else [],Variant l,prior
  | Seq[Token{token=Ideogram(_,"roman")} as t1;Token({token=SmallLetter("W","w")} as t2)] -> [Seq[t1;Token t2];Seq[t1;Token {t2 with token=Interp "w"}]], Variant[], 1 (* FIXME: zaślepka, żeby uniknąć przerwania ścieżki gdy „w” nie jest w leksykonie *)
  | Seq[Token({token=SmallLetter("X","x")} as x); t] | Seq[Token({token=CapLetter("X","x")} as x); t] ->
       let l1,t2,prior2 = lemmatize_rec t in
       List.flatten (Xlist.map l1 (fun t1 -> [Seq[Token x; t1];Seq[Token{x with token=Interp "x"}; t1]])), Seq[Token x; t2], prior2
  | Seq[Token({token=SmallLetter("X","x")} as x); t; s] | Seq[Token({token=CapLetter("X","x")} as x); t; s] ->
       let l1,t2,prior2 = lemmatize_rec (Seq[t; s]) in
       List.flatten (Xlist.map l1 (fun t1 -> [Seq[Token x; t1];Seq[Token{x with token=Interp "x"}; t1]])), Seq[Token x; t2], prior2
  | Seq[Token({token=SmallLetter("X","x")} as x); t; s; r] | Seq[Token({token=CapLetter("X","x")} as x); t; s; r] ->
       let l1,t2,prior2 = lemmatize_rec (Seq[t; s; r]) in
       List.flatten (Xlist.map l1 (fun t1 -> [Seq[Token x; t1];Seq[Token{x with token=Interp "x"}; t1]])), Seq[Token x; t2], prior2
  | Seq l ->
(*       print_endline ("lemmatize_rec 2: " ^ SubsyntaxStringOf.string_of_tokens 0 (Seq l)); *)
      (try
        let l,prior = Xlist.fold l ([],0) (fun (l,prior) t ->
          let t1,t2,prior2 = lemmatize_rec t in
(*          print_endline ("lemmatize_rec 2a: " ^ SubsyntaxStringOf.string_of_tokens 0 (Variant t1));
          print_endline ("lemmatize_rec 2b: " ^ SubsyntaxStringOf.string_of_tokens 0 t2);*)
          match t1, t2 with
            [], Variant[] -> raise Not_found
          | [t], Variant[] -> t :: l, prior
          | [], _ -> t2 :: l, max prior prior2
          | _ -> failwith "lemmatize_rec") in
        if prior = 0 then [Seq(List.rev l)], Variant[], 1000
        else [], Seq(List.rev l), prior
      with Not_found -> [],Variant[],1000)
       (* if t1 <>
          if t = Variant[] then raise Not_found else
          t :: l, b && b2) in
        Seq(List.rev l), b
      with Not_found -> Variant[],false)*)
  | Variant l ->
(*       print_endline ("lemmatize_rec 3: " ^ SubsyntaxStringOf.string_of_tokens 0 (Variant l)); *)
      let l1,l2,prior = Xlist.fold l ([],[],1000) (fun (l1,l2,best_prior) t ->
        let t1,t2,prior = lemmatize_rec t in
        if prior > best_prior then t1 @ l1,l2,best_prior else
        if prior < best_prior then t1 @ l1,[t2],prior else
        t1 @ l1,t2 :: l2,best_prior) in
      l1,Variant l2,prior
      (* let l1,l2 = Xlist.fold l ([],[]) (fun (l1,l2) t ->
        let t,b = lemmatize_rec t in
        if t = Variant[] then l1,l2 else
        if b then t :: l1, l2 else l1, t :: l2) in
      if l1 = [] then Variant l2,false else Variant l1,true *)

let lemmatize l =
  List.rev (Xlist.rev_map l (fun t ->
    let t1,t2,_ = lemmatize_rec t in
(* 	Printf.printf "lemmatize 1: %s\n%!" (SubsyntaxStringOf.string_of_tokens 0 (Variant (t2 :: t1))); *)
    Variant (t2 :: t1)))
    (* fst (lemmatize_rec t))) *)


let rec translate_tokens_rec paths =
  List.rev (Xlist.rev_map paths (fun t ->
    let t = {t with args=translate_tokens_rec t.args} in
    let lemma, pos, tags, pos_add =
      match t.token with
      | Interp lemma -> lemma,"interp",[],""
	  | Ideogram(lemma,mode) -> lemma,"symbol",[[mode]],mode
      | Other lemma -> lemma,"other",[],""
      | t -> "","",[],"" in
    if pos = "" then t else
    let cats = get_ontological_category lemma (pos ^ ":" ^ pos_add) tags t.attrs in
    let cat  = match cats with 
        [is_in_lexicon,has_no_sgjp_tag,has_poss_ndm_tag,has_exact_case_tag,cat,tags] -> cat
      | _ -> failwith ("translate_tokens_rec: multiple cats [" ^ String.concat "; " (Xlist.map cats (fun (_,_,_,_,cat,_) -> cat)) ^ "]") in
    {t with token=Lemma(lemma,pos,[tags],cat)}))

let rec translate_tokens paths =
  List.rev (List.flatten (Xlist.rev_map paths (fun t ->
    let t = {t with args=translate_tokens_rec t.args} in
    let lemma, pos, tags, pos_add =
      match t.token with
      | Interp lemma -> lemma,"interp",[],""
	  | Ideogram(lemma,mode) -> lemma,"symbol",[[mode]],mode
      | Other lemma -> lemma,"other",[],""
      | t -> "","",[],"" in
    if pos = "" then [t] else
    let cats = get_ontological_category lemma (pos ^ ":" ^ pos_add) tags t.attrs in
    Xlist.map cats (fun (is_in_lexicon,has_no_sgjp_tag,has_poss_ndm_tag,has_exact_case_tag,cat,tags) -> 
      {t with token=Lemma(lemma,pos,[tags],cat)}))))

 

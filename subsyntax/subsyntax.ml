(*
 *  ENIAMsubsyntax: tokenization, lemmatization, MWE and sentence detecion for Polish
 *  Copyright (C) 2016-2018 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
 *  Copyright (C) 2016-2018 Institute of Computer Science Polish Academy of Sciences
 *  Copyright (C) 2019 LekSeek Sp. z o.o. sp. k.
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

let load_lemma_frequencies filename map =
  let l = Str.split_delim (Str.regexp "\n") (File.load_file filename) in
  Xlist.fold l map (fun map line ->
    if String.length line = 0 then map else
    if String.get line 0 = '#' then map else
    match Str.split_delim (Str.regexp "\t") line with
      [count; lemma; cat] -> StringMap.add map (lemma ^ "\t" ^ cat) (log10 (float_of_string count +. 1.))
    | _ -> failwith ("load_lemma_frequencies: " ^ line))

let lemma_frequencies = ref (StringMap.empty : float StringMap.t)

let modify_weights paths =
  List.rev (Xlist.fold paths [] (fun paths t ->
    let w = Xlist.fold t.attrs t.weight (fun w -> function
        TokNotFound -> w -. 25.
      | LemmNotVal -> w -. 20.
      | NotValProper -> w -. 1.
      | LemmLowercase -> w -. 0.1
      | _ -> w) in
    let freq = match t.token with
        Lemma(lemma,cat,_,_) -> (try StringMap.find !lemma_frequencies (lemma ^ "\t" ^ cat) with Not_found -> w)
(*       | Proper(lemma,cat,_,_) -> (try StringMap.find !lemma_frequencies (lemma ^ "\t" ^ cat) with Not_found -> w) *)
      | _ -> w in
    {t with weight = w +. freq; lemma_frequency=freq} :: paths))

(**********************************************************************************)

module OrderedStringList = struct

  type t = string list

  let compare x y = compare (Xlist.sort x compare) (Xlist.sort y compare)

end

module OrderedStringListList = struct

  type t = string list list

  let compare x y = compare (Xlist.sort x compare) (Xlist.sort y compare)

end

module StringListMap = Xmap.Make(OrderedStringList)
module StringListListMap = Xmap.Make(OrderedStringListList)
module StringListListSet = Xset.Make(OrderedStringListList)

type tree = T of tree StringListMap.t | S of StringSet.t

let single_tags = function
    [_] :: _ -> true
  | _ -> false

let rec make_tree interp =
  if single_tags interp then S (StringSet.of_list (List.flatten (List.flatten interp))) else
  let map = Xlist.fold interp StringListMap.empty (fun map tags ->
    StringListMap.add_inc map (List.hd tags) [List.tl tags] (fun l -> (List.tl tags) :: l)) in
  T(StringListMap.map map make_tree)

let is_s_tree map =
  StringListListMap.fold map false (fun b _ -> function
      S _ -> true
    | T _ -> b)

let rec fold_tree_rec rev s f = function
    S set -> f s (List.rev rev) set
  | T map -> StringListMap.fold map s (fun s tag tree ->
       fold_tree_rec (tag :: rev) s f tree)

let fold_tree tree s f = fold_tree_rec [] s f tree

let rec combine_interps_rec map =
  if is_s_tree map then
    StringListListMap.fold map [] (fun interp tail_tags -> function
        S tag -> ((Xlist.sort (StringSet.to_list tag) compare) :: tail_tags) :: interp
      | _ -> failwith "combine_interps_rec")
  else
    let map = StringListListMap.fold map StringListListMap.empty (fun map tail_tags tree ->
      fold_tree tree map (fun map head_tags tag ->
        StringListListMap.add_inc map ((Xlist.sort (StringSet.to_list tag) compare) :: tail_tags) [head_tags] (fun l -> head_tags :: l))) in
    combine_interps_rec (StringListListMap.map map make_tree)

let combine_interp interp =
  try
    let map = StringListListMap.add StringListListMap.empty [] (make_tree interp) in
    combine_interps_rec map
  with e -> failwith ("combine_interp: " ^ Printexc.to_string e)

let combine_pos = StringSet.of_list ["subst"; "depr"; "ppron12"; "ppron3"; "siebie"; "adj"; "num"; "ger"; "praet"; "fin"; "impt"; "imps"; "pcon"; "ppas"; "pact";
  "inf"; "bedzie"; "aglt"; "winien"; "pant"; "prep"]

let combine_subst_tags = function
    [n;c;g] -> Xlist.map (Xlist.multiply_list [n;c;g]) (fun l -> Xlist.map l (fun x -> [x]))
  | [n;c;[g];[col]] -> Xlist.map (Xlist.multiply_list [n;c;[g ^ ":" ^ col]]) (fun l -> Xlist.map l (fun x -> [x]))
  | _ -> failwith "combine_subst_tags"

let combine_interps paths =
  let map = Xlist.fold paths StringMap.empty (fun map t -> 
    let tok,interp = match t.token with 
        Lemma(lemma,pos,interp,cat) -> Lemma(lemma,pos,[],cat),interp
      | t -> t,[] in
    let t = {t with token=tok} in
    StringMap.add_inc map (SubsyntaxStringOf.string_of_token_env t) (t,interp) (fun (_,interps) -> t, interp @ interps)) in
  let paths = StringMap.fold map [] (fun paths _ (t,interps) ->
    let tok = match t.token with 
        Lemma(lemma,pos,[],cat) -> Lemma(lemma,pos,interps,cat)
      | t -> t in
    {t with token=tok} :: paths) in
  Xlist.rev_map paths (fun t ->
    match t.token with
      Lemma(lemma,pos,interp,cat) ->
        (* Printf.printf "%s %s %s\n" lemma pos (Tagset.render interp); *)
        if StringSet.mem combine_pos pos && interp = [[]] then failwith ("combine_interps: interp=[[]] for " ^ lemma ^ ":" ^ pos) else
        let interp =
          if pos = "subst" then List.flatten (Xlist.map interp combine_subst_tags) else
          if pos = "ppron12" then Xlist.map interp (fun tags -> if Xlist.size tags = 4 then tags @ [["_"]] else tags)
          else interp in
        let interp =
          if StringSet.mem combine_pos pos then combine_interp interp else
          StringListListSet.to_list (StringListListSet.of_list interp) in
        {t with token=Lemma(lemma,pos,interp,cat)}
    | _ -> t)

(**********************************************************************************)

let select_tokens paths =
  List.rev (Xlist.fold paths [] (fun paths t ->
    match t.token with
(*      RomanDig(v,cat) -> {t with token=Lemma(v,cat,[[]])} :: paths
    | Interp orth -> {t with token=Lemma(orth,"interp",[[]])} :: paths
    | Dig(value,cat) -> {t with token=Lemma(value,cat,[[]])} :: paths
    | Other2 orth -> {t with token=Lemma(orth,"unk",[[]])} :: paths
    | Lemma(lemma,cat,interp) -> t :: paths
    | Proper _ -> failwith "select_tokens"
    | Compound _ -> t :: paths*)
(*       RomanDig(v,cat) -> t :: paths *)
    | Interp orth -> t :: paths
	| Ideogram(value,mode) -> t :: paths
    | Other orth -> t :: paths
    | Lemma(lemma,pos,interp,_) -> if pos = "brev" then paths else t :: paths
(*     | Proper(lemma,pos,interp,cat) -> if pos = "brev" then paths else t :: paths *)
(*     | Compound _ -> t :: paths *)
    | _ -> paths))

let add_token paths (q,t,n) =
  let map = try IntMap.find paths t.beg with Not_found -> IntMap.empty in
  let map = IntMap.add_inc map t.next [q,t,n] (fun l -> (q,t,n) :: l) in
  IntMap.add paths t.beg map

let rec select_tokens2_rec last paths nodes map =
  let node = IntSet.min_elt nodes in
  if node = last then try snd (IntMap.find map node) with Not_found -> failwith "select_tokens2_rec: token graph is not connected" else
  let nodes = IntSet.remove nodes node in
  if not (IntMap.mem map node) then select_tokens2_rec last paths nodes map else
  let qselected,selected = IntMap.find map node in
  let map2 = try IntMap.find paths node with Not_found -> IntMap.empty in
  let map = IntMap.fold map2 map (fun map next l ->
    Xlist.fold l map (fun map (q,t,n) ->
      let selected = IntSet.add selected n in
      let qselected = qselected+q in
      IntMap.add_inc map t.next (qselected,selected) (fun (qselected2,selected2) ->
        if qselected2 > qselected then qselected2,selected2 else
        if qselected2 < qselected then qselected,selected else
        qselected,IntSet.union selected selected2))) in
  select_tokens2_rec last paths nodes map

let rec calculate_quality q = function
    FC :: l -> calculate_quality (q-2) l
  | CS :: l -> calculate_quality (q-2) l
  | MaybeCS :: l -> calculate_quality q l
  | HasAglSuffix :: l -> calculate_quality q l
  | AglSuffix :: l -> calculate_quality q l
  | MWE :: l -> calculate_quality (q+6) l
  | Capitalics :: l -> calculate_quality (q+1) l
  | LemmNotVal :: l -> calculate_quality (q-5) l
  | TokNotFound :: l -> calculate_quality (q-10) l
  | NotValProper :: l -> calculate_quality (q-1) l
  | LemmLowercase :: l -> calculate_quality q l
  | Roman :: l -> calculate_quality q l
  | SentBeg :: l -> calculate_quality q l
  | SentBegEnd :: l -> calculate_quality q l
  | SentEnd :: l -> calculate_quality q l
  | BrevLemma _ :: l -> calculate_quality q l
  | Disamb _ :: l -> calculate_quality q l
  | [] -> q

let added_quality t =
  match Tokenizer.get_pos t with
    "prep" -> 11
  | "conj" -> 11
  | "qub" -> 11
  | "interj" -> 11
  | "adv" -> 11
  | "adja" -> 11
  | "comp" -> 11
  | "ppron12" -> 11
  | "ppron3" -> 11
  | _ -> 0

let select_tokens2 paths =
  (* print_endline "select_tokens2" ; *)
  let beg,last = Xlist.fold paths (max_int,-1) (fun (beg,last) t ->
    min beg t.beg, max last t.next) in
  let nodes = Xlist.fold paths IntSet.empty (fun nodes t ->
    IntSet.add (IntSet.add nodes t.beg) t.next) in
  let paths2,_ = Xlist.fold paths ([],1) (fun (paths2,n) t ->
    (* Printf.printf "%3d %3d %s\n%!" (added_quality t.token) (calculate_quality 0 t.attrs) (SubsyntaxStringOf.string_of_token_env t); *)
    (added_quality t.token + calculate_quality 0 t.attrs, t, n) :: paths2, n+1) in
  let paths2 = Xlist.fold paths2 IntMap.empty add_token in
  let selected = select_tokens2_rec last paths2 nodes (IntMap.add IntMap.empty beg (0,IntSet.empty)) in
  (* print_endline (String.concat " " (StringSet.to_list selected)); *)
  IntMap.fold paths2 [] (fun paths _ map ->
    IntMap.fold map paths (fun paths _ l ->
      Xlist.fold l paths (fun paths (q,t,n) ->
        if IntSet.mem selected n then t :: paths else paths)))

let remove l s =
  Xlist.fold l [] (fun l t ->
      if s = t then l else t :: l)

let get_sock_addr host_name port =
  let he = Unix.gethostbyname host_name in
  let addr = he.Unix.h_addr_list in
  Unix.ADDR_INET(addr.(0),port)

let initialize () =
  Inflexion.initialize ();
  Url.top_level_domains := Url.load_top_level_domains ();
  known_lemmata := File.catch_no_file (DataLoader.extract_valence_lemmata data_path "valence.dic") !known_lemmata;
  known_lemmata :=
    Xlist.fold !theories !known_lemmata (fun map theory ->
      File.catch_no_file (DataLoader.extract_valence_lemmata (theories_path ^ theory) "valence.dic") map);
  known_lemmata :=
    Xlist.fold !user_theories !known_lemmata (fun map theory ->
      File.catch_no_file (DataLoader.extract_valence_lemmata (user_theories_path ^ theory) "valence.dic") map);
  known_pos := File.catch_no_file (DataLoader.extract_valence_pos data_path "valence.dic") !known_pos;
  known_pos :=
    Xlist.fold !theories !known_pos (fun map theory ->
      File.catch_no_file (DataLoader.extract_valence_pos (theories_path ^ theory) "valence.dic") map);
  known_pos :=
    Xlist.fold !user_theories !known_pos (fun map theory ->
      File.catch_no_file (DataLoader.extract_valence_pos (user_theories_path ^ theory) "valence.dic") map);
  let mwe_dict,mwe_dict2 = MWE.load_mwe_dicts () in
  MWE.mwe_dict := mwe_dict;
  MWE.mwe_dict2 := mwe_dict2;
  lemma_frequencies := File.catch_no_file (load_lemma_frequencies lemma_frequencies_filename) StringMap.empty;
(*  if !coord_enabled then
  let c_in,c_out = 
    if !coord_enabled then 
      Unix.open_connection (get_sock_addr !coord_host_name !coord_port)
	else stdin,stdout in
  coord_in := c_in;
  coord_out := c_out;  *)
(*   proper_names := load_proper_names (); *)
  if !coord_enabled then Coordination.initialize ();
  ()

let disambiguate_coordination paths =
(*  try
(*  let c_in,c_out = 
    if !coord_enabled then 
      Unix.open_connection (get_sock_addr !coord_host_name !coord_port)
	else stdin,stdout in
  coord_in := c_in;
  coord_out := c_out;  *)
  Marshal.to_channel !coord_out paths []; 
  flush !coord_out;
  let paths,msg = (Marshal.from_channel !coord_in : token_env list * string) in
(*   Unix.shutdown_connection !coord_in; *)
  if msg <> "" then failwith ("disambiguate_coordination: " ^ msg) else paths
  with Not_found -> failwith "disambiguate_coordination: Not_found"
     | End_of_file -> failwith "disambiguate_coordination: End_of_file"*)
  Coordination.disambiguate paths

let parse query =
  let l = Patterns.parse query in
(*  print_endline "XXXXXXXXXXXXXXXXXXXXXXXXX a1"; 
  Xlist.iter l (fun t -> print_endline (SubsyntaxStringOf.string_of_tokens 0 t));
  print_endline "XXXXXXXXXXXXXXXXXXXXXXXXX a2"; *)
  let l = Lemmatization.lemmatize l in
  (* print_endline "a6"; *)
  let paths = Patterns.translate_into_paths l in
(*  print_endline "XXXXXXXXXXXXXXXXXXXXXXXXX a7"; 
  print_endline (SubsyntaxStringOf.token_list false (fst paths)); *)
(*  print_endline "XXXXXXXXXXXXXXXXXXXXXXXXX a8";*)
(*   print_endline (SubsyntaxStringOf.token_list false (fst paths)); *)
  (* print_endline "XXXXXXXXXXXXXXXXXXXXXXXXX a9"; *)
  let paths = MWE.process paths in
(*   print_endline "XXXXXXXXXXXXXXXXXXXXXXXXX a12"; *)
(*   print_endline (SubsyntaxStringOf.token_list false paths);  *)
  let paths,last = if !prescription_rule then MWE.process_prescription_rule paths else paths in
  let paths = if !coord_enabled then disambiguate_coordination paths else paths in
(*   let paths =  if !recognize_proper_names then List.rev (Xlist.rev_map paths find_proper_names) else paths in *)
(*   print_endline "XXXXXXXXXXXXXXXXXXXXXXXXX a13";  *)
  (* print_endline (SubsyntaxStringOf.token_list false paths); *)
  let paths = combine_interps paths in
  let paths = Xlist.sort paths Patterns.compare_token_record in
  (* print_endline "a14"; *)
  let paths = modify_weights paths in
(*   print_endline "XXXXXXXXXXXXXXXXXXXXXXXXX a16"; *)
(*   print_endline (SubsyntaxStringOf.token_list false paths); *)
  let paths = select_tokens paths in
  let paths = Xlist.sort paths Patterns.compare_token_record in
  let paths = Patterns.remove_inaccessible_tokens paths 0 last in
  let paths = Xlist.sort paths Patterns.compare_token_record in
  let paths = if !concraft_enabled then Concraft.process_paths paths else paths in
  (* print_endline "XXXXXXXXXXXXXXXXXXXXXXXXX a16"; *)
  (* print_endline (SubsyntaxStringOf.token_list false paths); *)
  let paths = if !concraft_disambiguate then Concraft.disambiguate paths last else paths in
  (* print_endline "XXXXXXXXXXXXXXXXXXXXXXXXX a17"; *)
  (* print_endline (SubsyntaxStringOf.token_list false paths); *)
(*   let paths = if !strong_disambiguate_flag then select_tokens2 paths else paths in (* Ta procedura wycina potrzebne tokeny *) *)
(*   let paths = Patterns.process_interpunction paths in *)
(*   let paths = Xlist.sort paths Patterns.compare_token_record in *)
  (* print_endline "XXXXXXXXXXXXXXXXXXXXXXXXX a18"; *)
  (* print_endline (SubsyntaxStringOf.token_list false paths); *)
  let paths = Xlist.sort paths Patterns.compare_token_record in
  (* print_endline "XXXXXXXXXXXXXXXXXXXXXXXXX a19"; *)
(*  let min_len,max_len = Patterns.calculate_length (paths,last) in
  Printf.printf "no_tokens1 2 in [%d,%d] dev=%f\n%!" min_len max_len (float(2*(max_len-min_len))/.float(max_len+min_len));
  let min_len,max_len = Patterns.calculate_length2 (paths,last) in
  Printf.printf "no_tokens2 2 in [%d,%d] dev=%f\n%!" min_len max_len (float(2*(max_len-min_len))/.float(max_len+min_len));*)
(*  let len = Patterns.calculate_no_tokens (paths,last) in
  Printf.printf "no_tokens3 2 in %d\n%!" len;*)
  let t_len,t_len_nann = Patterns.count_recognized_tokens (paths,last) in
(*   Printf.printf "percent of not recognized tokens in %d/%d=%f\n%!" len_nann len ((float len_nann)/.float len); *)
  let c_len,c_len_nann = Patterns.count_recognized_characters (paths,last) in
(*   Printf.printf "percent of not recognized characters in %d/%d=%f\n%!" len_nann len ((float len_nann)/.float len); *)
(*  let len,len_nann,nann_orths = Patterns.count_annotated_lexemes 0 last paths in
  Printf.printf "percent of not semantically annotated lexemes in %d/%d=%f %s\n%!" len_nann len ((float len_nann)/.float len) (String.concat " " nann_orths);*)
  paths,(t_len,t_len_nann,c_len,c_len_nann)(*, next_id*)

let parse_text_tokens sentence_split_flag par_names_flag tokens query =
(*   print_endline ("parse_text_tokens 1: " ^ query); *)
  let paragraphs = Xstring.split "\n\\|\r" query in
  let paragraphs = List.rev (Xlist.fold paragraphs [] (fun l -> function "" -> l | s -> s :: l)) in
(*   print_endline ("parse_text_tokens 2: " ^ query); *)
  let paragraphs = List.rev (Xlist.rev_map paragraphs (fun paragraph ->
    if par_names_flag then
      match Xstring.split_delim "\t" paragraph with
        [name; paragraph] -> 
(*           let paragraph = if paragraph = "" || paragraph = " " then "¶" else paragraph in (* FIXME: to koniecznie trzeba poprawić tak by napisy zawierające jedynie białe znaki nie wywracały parsera / chyba poprawione *) *)
          (match Xstring.split " | " name with 
            [name; id] -> name, id, paragraph
          | _ -> name,"", paragraph)
      | _ -> failwith ("parse_text_tokens: " ^ paragraph)
    else "", "", paragraph)) in
(*   print_endline ("parse_text_tokens 3: " ^ query); *)
  let n = if Xlist.size paragraphs = 1 then 0 else 1 in (* FIXME: powyższe do przeniesienia do osobnej procedury *)
  let paragraphs,_ = Xlist.fold paragraphs ([],n) (fun (paragraphs,n) (name,id,paragraph) ->
    try
      (* print_endline paragraph; *)
      let paths,stats = parse paragraph in
      (* print_endline "parse_text 1"; *)
      let pid = 
        if !inner_pid_counter then (
          incr pid_counter;
          string_of_int !pid_counter ^ "_" )
        else if n = 0 then "" else string_of_int n ^ "_" in
      let sentences =
        if sentence_split_flag then Sentences.split_into_sentences pid paragraph tokens paths
        else Sentences.no_split_into_sentences pid paragraph tokens paths in
      (AltParagraph ((if par_names_flag then [Name,RawParagraph name] else []) @ (if id = "" then [] else [Identifier,RawParagraph id]) @
        [Raw,RawParagraph paragraph] @ (if sentences = [] then [] else [Struct,StructParagraph(stats,sentences)]))) :: paragraphs, n+1
    with e ->
      (AltParagraph ((if par_names_flag then [Name,RawParagraph name] else []) @ (if id = "" then [] else [Identifier,RawParagraph id]) @
        [Raw,RawParagraph paragraph; Error,ErrorParagraph (Printexc.to_string e)])) :: paragraphs, n+1) in
  AltText[Raw,RawText query; Struct,StructText(List.rev paragraphs)], tokens

let parse_text sentence_split_flag par_names_flag query =
  (* print_endline ("parse_text: " ^ query); *)
  let tokens = ExtArray.make 100 empty_token_env in
  let _ = ExtArray.add tokens empty_token_env in (* id=0 jest zarezerwowane dla pro; FIXME: czy to jest jeszcze aktualne? *)
  let text,tokens = parse_text_tokens sentence_split_flag par_names_flag tokens query in
(*  let min_len,max_len = Sentences.calculate_length text tokens in
  Printf.printf "no_tokens1 3 in [%d,%d] dev=%f\n%!" min_len max_len (float(2*(max_len-min_len))/.float(max_len+min_len));
  let min_len,max_len = Sentences.calculate_length2 text tokens in
  Printf.printf "no_tokens2 3 in [%d,%d] dev=%f\n%!" min_len max_len (float(2*(max_len-min_len))/.float(max_len+min_len));*)
  text,tokens

let catch_parse text =
  try
    let tokens,stats = parse text in tokens,""
  with 
    BrokenPaths(beg,last,n,paths) -> [], Printf.sprintf "BrokenPaths beg=%d last=%d n=%d\n%s\n" beg last n (SubsyntaxStringOf.token_list false paths)
  | e -> [], Printexc.to_string e

let catch_parse_text sentence_split_flag par_names_flag text =
  try
    parse_text sentence_split_flag par_names_flag text
  with e ->
    AltText[Raw,RawText text; Error,ErrorText (Printexc.to_string e)],
    ExtArray.make 0 empty_token_env

    
    
let is_parsed tokens paths last =
(*   Printf.printf "is_parsed: last=%d\n" last; *)
  let set = Xlist.fold paths (IntSet.singleton 0) (fun set (id,lnode,rnode) ->
    let t = ExtArray.get tokens id in    
(*     Printf.printf "is_parsed: lnode=%d rnode=%d orth=%s token=%s cat=%s\n" lnode rnode t.orth (SubsyntaxStringOf.string_of_token t.token) t.cat; *)
    if IntSet.mem set lnode && Tokenizer.get_cat t.token <> "X" && Tokenizer.get_cat t.token <> "MWEcomponent" then IntSet.add set rnode else set) in
(*   if IntSet.mem set last then Printf.printf "is_parsed: true\n" else Printf.printf "is_parsed: false\n"; *)
  IntSet.mem set last
    
let rec select_not_parsed_sentence mode tokens = function
  | QuotedSentences sentences ->
      let sentences = Xlist.rev_map sentences (fun p ->
        let sentence = select_not_parsed_sentence mode tokens p.sentence in
        {p with sentence=List.hd sentence}) in
      [QuotedSentences(List.rev sentences)]
  | AltSentence l ->
      let l = List.flatten (Xlist.rev_map l (fun (mode,sentence) ->
        Xlist.rev_map (select_not_parsed_sentence mode tokens sentence) (fun s -> mode, s))) in
      [AltSentence(List.rev l)]
  | RawSentence s -> (*Printf.printf "select_not_parsed_sentence: %s\n" s;*) [RawSentence s]
  | StructSentence(paths,last) -> if is_parsed tokens paths last then [] else [StructSentence(paths,last)]
  | DepSentence _ -> failwith "select_not_parsed_sentence"
  | ErrorSentence s -> (*Printf.printf "select_not_parsed_sentence: %s\n" s;*) [ErrorSentence s]

let rec select_not_parsed_paragraph mode tokens = function
    RawParagraph s -> RawParagraph s
  | StructParagraph(stats,sentences) ->
      let sentences = Xlist.rev_map sentences (fun p ->
        let sentence = select_not_parsed_sentence mode tokens p.sentence in
        {p with sentence=List.hd sentence}) in
      StructParagraph(stats,List.rev sentences)
  | AltParagraph l ->
      let l = Xlist.rev_map l (fun (mode,paragraph) ->
        mode, select_not_parsed_paragraph mode tokens paragraph) in
      AltParagraph(List.rev l)
  | ErrorParagraph s -> ErrorParagraph s

let rec select_not_parsed_text mode tokens = function
    RawText s -> RawText s
  | StructText paragraphs ->
      let paragraphs = Xlist.rev_map paragraphs (fun paragraph ->
        select_not_parsed_paragraph mode tokens paragraph) in
      StructText(List.rev paragraphs)
  | AltText l -> AltText(Xlist.map l (fun (mode,text) ->
       mode, select_not_parsed_text mode tokens text))
  | ErrorText s -> ErrorText s

let select_not_parsed tokens text =
  select_not_parsed_text Struct tokens text
  
let get_line_key_text line =
  match Xstring.split "\t" line with
    [s] -> "",s
  | [key;s] -> key,s
  | _ -> failwith ("get_line_text: " ^ line)
  
let rec is_not_validated_lemma recogn_flag = function
    Token{token=Lemma(_,_,_,cat);attrs=a} -> Xlist.mem a LemmNotVal || (recogn_flag && cat = "X")
  | Token _ -> false
  | Seq l -> Xlist.fold l false (fun b t -> b || is_not_validated_lemma recogn_flag t)
  | Variant l -> Xlist.fold l true (fun b t -> b && is_not_validated_lemma recogn_flag t)
  
let rec get_lemma_cats = function
    Token{token=Lemma(_,_,_,cat);attrs=a} -> if Xlist.mem a LemmNotVal || cat = "X" then [] else [cat]
  | Token _ -> []
  | Seq l -> List.flatten (Xlist.map l get_lemma_cats)
  | Variant l -> List.flatten (Xlist.map l get_lemma_cats)
  
let rec get_orth = function
    Token t -> t.orth ^ if t.beg + t.len = t.next then "" else " "
  | Seq l -> String.concat "" (Xlist.map l get_orth)
  | Variant(t :: _) -> get_orth t
  | Variant [] -> failwith "get_orth: ni"
  
let left_prefix_size = ref 24
let right_prefix_size = ref 22
let name_length = ref 20
    
let rec find_not_validated_lemmata_context recogn_flag key found rev = function
    [] -> found
  | token :: tokens ->
      let found = 
        if is_not_validated_lemma recogn_flag token then 
          (key,
           Xlist.rev_map (Xlist.prefix !left_prefix_size rev) get_orth, 
           get_orth token, 
           Xlist.map (Xlist.prefix !right_prefix_size tokens) get_orth) :: found
        else found in
      find_not_validated_lemmata_context recogn_flag key found (token :: rev) tokens
    
let rec find_categorized_lemmata_context key found rev = function
    [] -> found
  | token :: tokens ->
      let found = 
        let cats = get_lemma_cats token in
        if cats <> [] then 
          (cats, key,
           Xlist.rev_map (Xlist.prefix !left_prefix_size rev) get_orth, 
           get_orth token, 
           Xlist.map (Xlist.prefix !right_prefix_size tokens) get_orth) :: found
        else found in
      find_categorized_lemmata_context key found (token :: rev) tokens
    
let compare_lines (_,l1,s1,_) (_,l2,s2,_) =
  match compare (Xunicode.lowercase_utf8_string s1) (Xunicode.lowercase_utf8_string s2) with
    0 -> compare (Xlist.rev_map l1 Xunicode.lowercase_utf8_string) (Xlist.rev_map l2 Xunicode.lowercase_utf8_string)
  | x -> x
  
let print_html_lines path name name_length lines =
  File.file_out (path ^ "/" ^ name ^ ".html") (fun file ->
    Printf.fprintf file "%s\n" SubsyntaxHTMLof.html_header;
    Printf.fprintf file "<TABLE border=1><col width=\"180\">\n";
    Xlist.iter lines (fun (name,left,s,right) ->
      Printf.fprintf file "<TR><TD>%s</TD><TD align=\"right\">%s</TD><TD>%s</TD><TD>%s</TD><TR>\n" 
        (MarkedHTMLof.check_name_length name_length name) 
        (SubsyntaxHTMLof.escape_html (String.concat "" left)) 
        (SubsyntaxHTMLof.escape_html s) 
        (SubsyntaxHTMLof.escape_html (String.concat "" right)));
    Printf.fprintf file "</TABLE>\n";
    Printf.fprintf file "%s\n" SubsyntaxHTMLof.html_trailer)
  
let print_not_validated_lemmata recogn_flag result_path result_name text =
  let lines = Xstring.split "\n" text in
  let lines = Xlist.fold lines [] (fun lines line ->
    let key,text = get_line_key_text line in
    try
      let tokens = Patterns.parse text in
      let tokens = Lemmatization.lemmatize tokens in
      let tokens = Patterns.normalize_tokens [] tokens in
(*       Xlist.iter tokens (fun t -> print_endline (SubsyntaxStringOf.string_of_tokens_simple t)); *)
      find_not_validated_lemmata_context recogn_flag key lines [] tokens
    with e -> (key,Printexc.[to_string e],"",[]) :: lines) in
  let lines = Xlist.sort lines compare_lines in
  print_html_lines result_path (if recogn_flag then "not-recognized-" ^ result_name else "not-validated-" ^ result_name) !name_length lines;
  ()

let print_categorized_lemmata result_path result_name text =
  let lines = Xstring.split "\n" text in
  print_endline "print_categorized_lemmata 1";
  let lines = Xlist.fold lines [] (fun lines line ->
    let key,text = get_line_key_text line in
    try
      print_endline key;
      let tokens = Patterns.parse text in
      let tokens = Lemmatization.lemmatize tokens in
      let tokens = Patterns.normalize_tokens [] tokens in
(*       Xlist.iter tokens (fun t -> print_endline (SubsyntaxStringOf.string_of_tokens_simple t)); *)
      find_categorized_lemmata_context key lines [] tokens
    with e -> lines) in
  print_endline "print_categorized_lemmata 2";
  let map = Xlist.fold lines StringMap.empty (fun map (cats,key,left,token,right) ->
    Xlist.fold cats map (fun map cat ->
      StringMap.add_inc map cat [key,left,token,right] (fun l -> (key,left,token,right) :: l))) in
  print_endline "print_categorized_lemmata 3";
  StringMap.iter map (fun cat lines -> 
    print_endline cat;
    let lines = Xlist.sort lines compare_lines in
    print_html_lines result_path ("categorized-" ^ cat ^ "-" ^ result_name) !name_length lines);
  ()

let print_sentences result_path result_name par_names_flag text =
  let text,_ = parse_text true par_names_flag text in
  let qmap = fold_text Struct StringQMap.empty (fun mode qmap t ->
    match mode,t with
      Raw,RawSentence s -> StringQMap.add qmap s
    | _ -> qmap) text in
  let l = List.sort compare (StringQMap.fold qmap [] (fun l k v -> (k,v) :: l)) in
  File.file_out (result_path ^ "/" ^ result_name ^ ".tab") (fun file ->
    Xlist.iter l (fun (k,v) -> Printf.fprintf file "%d\t%s\n" v k))
  


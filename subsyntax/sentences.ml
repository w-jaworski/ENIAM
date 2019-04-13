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

open Printf
open SubsyntaxTypes
open Xstd

let find_bracket_begs tokens chart last beg_selector =
  Int.fold 0 last [] (fun found lnode ->
    Xlist.fold chart.(lnode) found (fun found (id,rnode) ->
      if beg_selector tokens id then (id,lnode,rnode) :: found else found))

let rec find_bracket_ends tokens chart mid_selector end_selector found nodes =
  if IntSet.is_empty nodes then found else
  let lnode = IntSet.min_elt nodes in
  let nodes = IntSet.remove nodes lnode in
  let found,nodes = Xlist.fold chart.(lnode) (found,nodes) (fun (found,nodes) (id,rnode) ->
    if end_selector tokens id then (id,lnode,rnode) :: found, nodes else
    if mid_selector tokens id then found, IntSet.add nodes rnode else
    found, nodes) in
  find_bracket_ends tokens chart mid_selector end_selector found nodes

let rec find_bracket_paths tokens chart mid_selector rnode map nodes =
  if IntSet.is_empty nodes then failwith "find_bracket_paths" else
  let lnode = IntSet.min_elt nodes in
  let ids = try IntMap.find map lnode with Not_found -> IntSet.empty in
  if lnode = rnode then IntSet.to_list ids else
  let nodes = IntSet.remove nodes lnode in
  let map,nodes = Xlist.fold chart.(lnode) (map,nodes) (fun (map,nodes) (id,rnode) ->
    if not (mid_selector tokens id) then map, nodes else
    let ids = IntSet.add ids id in
    let map = IntMap.add_inc map rnode ids (fun set -> IntSet.union set ids) in
    let nodes = IntSet.add nodes rnode in
    map,nodes) in
  find_bracket_paths tokens chart mid_selector rnode map nodes

let get_raw_sentence a beg len =
  let next = beg + len in
  let beg = beg / factor + (if beg mod factor < 50 then 0 else 1) in
  let next = next / factor + (if next mod factor < 50 then 0 else 1) in
  (* if beg mod factor <> 0 then failwith ("get_raw_sentence: beg " ^ string_of_int beg) else
  if len mod factor <> 0 then failwith ("get_raw_sentence: len " ^ string_of_int len) else *)
  let buf = Buffer.create 512 in
  (* Int.iter (beg / factor - 1) (beg / factor + len / factor - 2) (fun i -> *)
  Int.iter beg (next - 1) (fun i ->
    (* printf "%d" i; printf " %s\n%!" a.(i); *)
    Buffer.add_string buf  a.(i)(*try a.(i) with _ -> "<" ^ string_of_int i ^ ">"*));
  Buffer.contents buf

let parse_bracket_rule paragraph tokens chart last beg_selector mid_selector end_selector command =
  let begs = find_bracket_begs tokens chart last beg_selector in
  let found = Xlist.fold begs [] (fun found (beg_id,beg_l,beg_r) ->
    let ends = find_bracket_ends tokens chart mid_selector end_selector [] (IntSet.singleton beg_r) in
    (* if ends = [] then failwith "parse_bracket_rule: end not found" else *)
    Xlist.fold ends found (fun found (end_id,end_l,end_r) ->
      let ids = find_bracket_paths tokens chart mid_selector end_l IntMap.empty (IntSet.singleton beg_r) in
      (beg_id,ids,end_id,beg_l,end_r) :: found)) in
  Xlist.iter found (fun (beg_id,ids,end_id,lnode,rnode) ->
    let beg_t = ExtArray.get tokens beg_id in
    let end_t = ExtArray.get tokens end_id in
    let beg = beg_t.beg in
    let len = end_t.beg+end_t.len-beg_t.beg in
    let id = ExtArray.add tokens {empty_token_env with orth=get_raw_sentence paragraph beg len;
                                                   beg=beg; len=len; next=end_t.next;
                                                   token=command (*tokens*) (beg_id::end_id::ids)} in
    chart.(lnode) <- (id,rnode) :: chart.(lnode))

let find_or_sentence paragraph tokens chart last =
  parse_bracket_rule paragraph tokens chart last
    (fun tokens id -> (ExtArray.get tokens id).token = Interp "<or>")
    (fun tokens id -> false)
    (fun tokens id -> (ExtArray.get tokens id).token = Interp "<sentence>")
    (fun ids -> Interp "<or-sentence>")

let find_slash_or_sentence paragraph tokens chart last =
  parse_bracket_rule paragraph tokens chart last
    (fun tokens id -> (ExtArray.get tokens id).token = Interp "</sentence>")
    (fun tokens id -> false)
    (fun tokens id -> (ExtArray.get tokens id).token = Interp "</or>")
    (fun ids -> Interp "</or-sentence>")

let find_sentence paragraph tokens chart last =
  parse_bracket_rule paragraph tokens chart last
    (fun tokens id ->
      let t = (ExtArray.get tokens id).token in
      t = Interp "<sentence>" || t = Interp "<or-sentence>")
    (fun tokens id ->
      let t = (ExtArray.get tokens id).token in
      t <> Interp "<sentence>" && t <> Interp "<or-sentence>" && t <> Interp "</sentence>")
    (fun tokens id -> (ExtArray.get tokens id).token = Interp "</sentence>")
    (fun ids -> Tokens("sentence",ids))

let find_quoted_sentences paragraph tokens chart last =
  parse_bracket_rule paragraph tokens chart last
    (fun tokens id -> (ExtArray.get tokens id).token = Interp "„s")
    (fun tokens id ->
      match (ExtArray.get tokens id).token with
        Tokens("sentence",_) -> true
      | _ -> false)
    (fun tokens id -> (ExtArray.get tokens id).token = Interp "”s")
    (fun ids -> Tokens("quoted_sentences",ids))

let find_paren_sentences paragraph tokens chart last =
  parse_bracket_rule paragraph tokens chart last
    (fun tokens id -> (ExtArray.get tokens id).token = Interp "(s")
    (fun tokens id ->
      match (ExtArray.get tokens id).token with
        Tokens("sentence",_) -> true
      | _ -> false)
    (fun tokens id -> (ExtArray.get tokens id).token = Interp ")s")
    (fun ids -> Tokens("paren_sentences",ids))

let find_query paragraph tokens chart last =
  parse_bracket_rule paragraph tokens chart last
    (fun tokens id -> (ExtArray.get tokens id).token = Interp "<query>")
    (fun tokens id ->
      match (ExtArray.get tokens id).token with
        Tokens("sentence",_) -> true
      | Tokens("quoted_sentences",_) -> true
      | Tokens("paren_sentences",_) -> true
      | _ -> false)
    (fun tokens id -> (ExtArray.get tokens id).token = Interp "</query>")
    (fun ids -> Tokens("query",ids))

let find_query2 paragraph tokens chart last =
  parse_bracket_rule paragraph tokens chart last
    (fun tokens id -> (ExtArray.get tokens id).token = Interp "<query>")
    (fun tokens id -> true)
    (fun tokens id -> (ExtArray.get tokens id).token = Interp "</query>")
    (fun ids -> Tokens("query",ids))

let find_tokens_in_chart tokens chart lnode rnode cat =
  let found = Xlist.fold chart.(lnode) [] (fun found (id,rnode2) ->
      if rnode = rnode2 then
        let t = ExtArray.get tokens id in
        match t.token with
          Tokens(cat2,ids) -> if cat = cat2 then ids :: found else found
        | _ -> found
      else found) in
  match found with
    [x] -> x
  | [] -> failwith "Unable to extract sentences. Check punctuation."
  | _ -> failwith "find_tokens_in_chart"

(*let find_tokens_in_chart_id tokens chart lnode rnode cat =
  let found = Int.fold 0 last [] (fun ids lnode ->
          Xlist.fold chart.(lnode) ids (fun ids (id,rnode) ->
            id
            (id,lnode,rnode) :: paths)) in*)

let rec add_struct_sentence_ids_rec pid n sentences =
  Xlist.fold sentences ([],n) (fun (l,n) -> function
      {sentence=AltSentence[Raw,s;Struct,QuotedSentences sentences]} as p ->
         let sentences, n = add_struct_sentence_ids_rec pid n sentences in
         {p with sentence=AltSentence[Raw,s;Struct,QuotedSentences (List.rev sentences)]} :: l, n
    | p -> {p with file_prefix=pid ^ string_of_int n} :: l, n+1)

let add_struct_sentence_ids pid sentences =
  match sentences with
    [{sentence=AltSentence[Raw,_;Struct,QuotedSentences _]}] -> List.rev (fst (add_struct_sentence_ids_rec pid 1 sentences))
  | [p] -> [{p with file_prefix=pid}]
  | _ -> List.rev (fst (add_struct_sentence_ids_rec pid 1 sentences))

let prepare_indexes paths =
  let set = Xlist.fold paths IntSet.empty (fun set (_,beg,next) ->
    IntSet.add (IntSet.add set beg) next) in
  let map,last = Xlist.fold (Xlist.sort (IntSet.to_list set) compare) (IntMap.empty,0) (fun (map,n) x ->
    IntMap.add map x n, n+1) in
  List.rev (Xlist.rev_map paths (fun (id,beg,next) ->
    (id,IntMap.find map beg,IntMap.find map next))), last - 1

(**let make_paths tokens ids =
  let paths = Xlist.map ids (fun id ->
    let t = ExtArray.get tokens id in
    id,t.beg,t.next) in
  prepare_indexes paths

let par_compare (s:sentence_env) (t:sentence_env) = compare (s.sbeg,s.snext) (t.sbeg,t.snext)

let rec extract_sentences_rec tokens id =
  let t = ExtArray.get tokens id in
  match t.token with
    Tokens("sentence",ids) ->
      let paths,last = make_paths tokens ids in
      [{sid=string_of_int id; sbeg=t.beg; slen=t.len; snext=t.next; file_prefix="";
        sentence=AltSentence([Raw,RawSentence t.orth; ENIAM,StructSentence(paths,last)])}]
  | Tokens("quoted_sentences",ids) ->
      [{sid=string_of_int id; sbeg=t.beg; slen=t.len; snext=t.next; file_prefix="";
        sentence=AltSentence[Raw,RawSentence t.orth;
          Struct,QuotedSentences(List.sort par_compare (List.flatten (Xlist.rev_map ids (extract_sentences_rec tokens))))]}]
  | Tokens("paren_sentences",ids) ->
      [{sid=string_of_int id; sbeg=t.beg; slen=t.len; snext=t.next; file_prefix="";
        sentence=AltSentence[Raw,RawSentence t.orth;
          Struct,QuotedSentences(List.sort par_compare (List.flatten (Xlist.rev_map ids (extract_sentences_rec tokens))))]}]
  | _ -> []

let extract_sentences pid tokens chart last =
  let ids = find_tokens_in_chart tokens chart 0 last "query" in
  let sentences = List.sort par_compare (List.flatten (Xlist.rev_map ids (fun id ->
    extract_sentences_rec tokens id))) in
  add_struct_sentence_ids pid sentences
(*  let paths = Int.fold 0 last [] (fun paths lnode ->
    Xlist.fold chart.(lnode) paths (fun paths (id,rnode) ->
      (id,lnode,rnode) :: paths)) in
  [{pid=string_of_int "xx"; pbeg=0; plen=0;
    psentence=AltSentence[Raw,RawSentence paragraph;
                          ENIAM,StructSentence("",paths,last)]}]*)

let extract_sentences2 pid tokens chart last =
  let ids = find_tokens_in_chart tokens chart 0 last "query" in
  let paths,last = make_paths tokens ids in
  let sentences = [{sid="0"; sbeg=0; slen=last; snext=last; file_prefix="";
    sentence=AltSentence([ENIAM,StructSentence(paths,last)])}] in
  add_struct_sentence_ids pid sentences**)

(*
let is_sentence = function
    Sentence _ -> true
  | _ -> false

let get_sentence t =
  match t.token with
    Sentence(paths,last) -> paths,last
  | _ -> failwith "get_sentence"

let rec find_query2 found map = function
    [] -> found
  | t :: l ->
      if not (IntMap.mem map t.beg) then find_query2 found map l else
      if t.token = Interp "</query>" then find_query2 ((IntMap.find map t.beg) :: found) map l else
      if not (is_sentence t.token) then find_query2 found map l else
      let tokens = IntSet.add (IntMap.find map t.beg) t.id in
      find_query2 found (IntMap.add_inc map t.next tokens (fun tokens2 -> IntSet.union tokens tokens2)) l

let rec find_query found = function
    [] -> failwith "find_query"
  | t :: l ->
      if t.beg = 0 && t.token = Interp "<query>" then
        find_query2 found (IntMap.add IntMap.empty t.next IntSet.empty) l
      else
       if t.beg > 0 then found else find_query found l

let extract_sentences par (paths,last) =
  let par = Array.of_list (Xunicode.utf8_chars_of_utf8_string par) in
  let paths,last = PrePaths.sort (paths,last) in
  let found = find_query [] paths in
  let pars = Xlist.fold found [] (fun pars set ->
    Xlist.fold paths [] (fun sentences t -> if IntSet.mem set t.id then
      let paths,last = get_sentence t in
      {pid=string_of_int t.id; pbeg=t.beg; plen=t.len;
       psentence=AltSentence[Raw,RawSentence (get_raw_sentence par t.beg t.len);
                             ENIAM,StructSentence("",paths,(*last*)10)]} :: sentences else sentences) :: pars) in (* FIXME: (*last*)10 !!!! *)
  match pars with
    [sentences] -> add_struct_sentence_ids sentences
  | _ -> failwith "extract_sentences"
*)

let make_ids tokens paths =
  List.rev (Xlist.rev_map paths (fun t ->
    let n = ExtArray.add tokens t in
    n,t.beg,t.next))

let make_chart paths last =
  let chart = Array.make (last+1) [] in
  Xlist.iter paths (fun (id,beg,next) ->
    chart.(beg) <- (id,next) :: chart.(beg));
  chart

(*let split_into_sentences pid paragraph tokens paths =
  (* print_endline "split_into_sentences"; *)
  let paths = make_ids tokens paths in
  let paths,last = prepare_indexes paths in
  let chart = make_chart paths last in
  let par = Array.of_list ([""] @ Xunicode.utf8_chars_of_utf8_string paragraph @ [""]) in
  find_or_sentence par tokens chart last;
  find_slash_or_sentence par tokens chart last;
  find_sentence par tokens chart last;
  find_quoted_sentences par tokens chart last;
  find_paren_sentences par tokens chart last;
  find_query par tokens chart last;
  extract_sentences pid tokens chart last*)

(*let no_split_into_sentences pid paragraph tokens paths =
  (* print_endline "no_split_into_sentences"; *)
  let paths = make_ids tokens paths in
  let paths,last = prepare_indexes paths in
  let chart = make_chart paths last in
  let par = Array.of_list ([""] @ Xunicode.utf8_chars_of_utf8_string paragraph @ [""]) in
  find_query2 par tokens chart last;
  extract_sentences2 pid tokens chart last*)

let string_of_bipaths bipaths =
  let bipaths = Xlist.map bipaths (fun paths ->
    let paths = Xlist.map paths (fun t ->
      Printf.sprintf "%s-%d-%d" (if t.orth = "" then Tokenizer.get_lemma t.token else t.orth) t.beg t.next) in
    String.concat " " paths) in
  String.concat "\n" bipaths
  
let merge_empty_tail = function
    [{token=Interp "</query>"} as t5] :: [{token=Interp "</sentence>"} as t4] :: [{token=Interp "</clause>"} as t3] :: [{token=Interp "<clause>"} as t2] :: [{token=Interp "</sentence>"} as t1] :: bipaths ->
      [{t5 with beg=t1.beg; len=t1.len+t2.len+t3.len+t4.len+t5.len; orth=t1.orth^t2.orth^t3.orth^t4.orth^t5.orth}] :: bipaths
  | bipaths -> bipaths
  
(*let rec merge_sentences2 rev = function 
    [{token=Interp "<sentence>"} as t] :: bipaths -> t :: rev, bipaths
  | [] -> print_endline (string_of_bipaths [rev]); failwith "merge_sentences2"
  | bicomp :: bipaths -> merge_sentences2 (bicomp @ rev) bipaths
  
let rec merge_sentences found = function 
    [] -> found
  | [{token=Interp "</query>"}] :: bipaths -> merge_sentences found bipaths
  | [{token=Interp "<query>"}] :: bipaths -> merge_sentences found bipaths
  | [{token=Interp "</sentence>"} as t] :: bipaths -> 
      let sentence,bipaths = merge_sentences2 [t] bipaths in
      merge_sentences (sentence :: found) bipaths
  | bipaths -> print_endline (string_of_bipaths bipaths); failwith "merge_sentences"*)
  
let rec merge_sentences found rev = function 
    [] -> failwith "merge_sentences 1"
  | [{token=Interp "</query>"}] :: bipaths -> 
      if bipaths <> [] then failwith "merge_sentences 2" else
      if rev = [] then found else (List.rev rev) :: found
  | [{token=Interp "."} as t] :: bipaths ->
(*   | [{token=Interp ";"} as t] :: bipaths ->  *)
      merge_sentences ((List.rev (t :: rev)) :: found) [] bipaths
  | [{token=Interp "<query>"}] :: bipaths -> merge_sentences found rev bipaths
  | bicomp :: bipaths -> merge_sentences found (bicomp @ rev) bipaths
      
let get_beg_len_next paths =
  let beg,last,next = Xlist.fold paths (max_int,-1,-1) (fun (beg,last,next) t ->
    min beg t.beg,
    max last (t.beg+t.len),
    max next t.next) in
  beg, last - beg, next
      
let process_paths tokens beg last paths = (* FIXME: trzeba osobno obsługiwać przypadek z <query> i </query> bo teraz trawia wewnątrz <sentence> *)
  let len,len_nann,nann_orths = Patterns.count_annotated_lexemes beg last paths in
  let paths = Patterns.process_interpunction beg last paths in
  let paths = Xlist.sort paths Patterns.compare_token_record in
  let no_tokens = Patterns.calculate_no_tokens beg last paths in
  let paths = Lemmatization.translate_tokens paths in
  let paths = if !default_category_flag then paths else Patterns.remove_category "X" paths in
  let paths = if !default_category_flag then paths else Patterns.remove_category "MWEcomponent" paths in
  let paths = Xlist.sort paths Patterns.compare_token_record in
  let paths,f = 
    try Patterns.remove_inaccessible_tokens paths beg last,true 
    with BrokenPaths _ -> [],false in
  if f then 
    let paths = Xlist.sort paths Patterns.compare_token_record in
    let paths,_ = Patterns.uniq (Xlist.sort paths Patterns.compare_token_record,0) in
    let paths = make_ids tokens paths in
    let paths,last = prepare_indexes paths in
    (ENIAM,StructSentence(paths,last)), no_tokens
  else (Error,ErrorSentence ("sentence not lemmatized: " ^ String.concat " " nann_orths)), no_tokens
      
let split_into_sentences pid paragraph tokens paths =
(*   print_endline ("split_into_sentences 1:\n" ^ SubsyntaxStringOf.token_list paths); *)
  let paragraph = Array.of_list ([""] @ Xunicode.utf8_chars_of_utf8_string paragraph @ [""]) in
  let bipaths = Patterns.biconnected_compontents paths in
(*   Xlist.iter bipaths (fun paths -> print_endline ("split_into_sentences 2:\n" ^ SubsyntaxStringOf.token_list paths)); *)
(*   let bipaths = merge_empty_tail bipaths in *)
  let sentences = merge_sentences [] [] bipaths in
  let sentences,_ = Xlist.fold (List.rev sentences) ([],1) (fun (sentences,n) paths ->
    let beg,len,next = get_beg_len_next paths in
    let orth = get_raw_sentence paragraph beg len in
    let sent,no_tokens = process_paths tokens beg next paths in
    let sentence = {sid=string_of_int n; sbeg=beg; slen=len; snext=next; file_prefix=""; no_tokens=no_tokens;
      sentence=AltSentence [Raw,RawSentence orth; sent]} in
    sentence :: sentences, n+1) in
(*  let sentences = if sentences <> [] then sentences else
    let sent = process_paths tokens 0 0 [] in
    [{id="1"; beg=0; len=0; next=0; file_prefix=""; sentence=AltSentence [Raw,RawSentence "";ENIAM,StructSentence(paths,last)]}] in*)
  add_struct_sentence_ids pid (List.rev sentences)
  
let no_split_into_sentences pid paragraph tokens paths =
  (* print_endline "no_split_into_sentences"; *)
(*  let query = List.hd paths in
  let query2 = List.tl paths in
  let paths = List.List.rev (List.tl paths)*)
  let beg,len,next = get_beg_len_next paths in
  let sent,no_tokens = process_paths tokens beg next paths in
  let sentence = {sid="0"; sbeg=beg; slen=len; snext=next; file_prefix=""; no_tokens=no_tokens;
    sentence=AltSentence [sent]} in
  add_struct_sentence_ids pid [sentence]
  
(*let calculate_length_sentence min_len max_len paths last tokens =
  let map = IntMap.add IntMap.empty 0 (min_len,max_len) in
  let map = Xlist.fold paths map (fun map (id,beg,next) ->
    let beg_min,beg_max = try IntMap.find map beg with Not_found -> failwith "calculate_length" in
    let next_min,next_max = try IntMap.find map next with Not_found -> max_int,0 in
    let i = if (ExtArray.get tokens id).orth = "" then 0 else 1 in
    IntMap.add map next (min (beg_min+i) next_min, max (beg_max+i) next_max)) in
  try IntMap.find map last with Not_found -> failwith "calculate_length"

let calculate_length text tokens =
  fold_text Struct (0,0) (fun _ (min_len,max_len) -> function
    StructSentence(paths,last) ->
      calculate_length_sentence min_len max_len paths last tokens
  | _ -> min_len,max_len
  ) text
  
let calculate_length_sentence2 min_len max_len paths last tokens =
  let map = IntMap.add IntMap.empty 0 (min_len,max_len) in
  let map = Xlist.fold paths map (fun map (id,beg,next) ->
    let beg_min,beg_max = try IntMap.find map beg with Not_found -> failwith "calculate_length" in
    let next_min,next_max = try IntMap.find map next with Not_found -> max_int,0 in
    let i = match (ExtArray.get tokens id).token with 
      | Symbol _  -> 0
      | Ideogram _ -> 0
      | Interp _  -> 0
      | _ -> if (ExtArray.get tokens id).orth = "" then 0 else 1 in
    IntMap.add map next (min (beg_min+i) next_min, max (beg_max+i) next_max)) in
  try IntMap.find map last with Not_found -> failwith "calculate_length"

let calculate_length2 text tokens =
  fold_text Struct (0,0) (fun _ (min_len,max_len) -> function
    StructSentence(paths,last) ->
      calculate_length_sentence2 min_len max_len paths last tokens
  | _ -> min_len,max_len
  ) text*)
  

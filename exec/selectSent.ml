(*
 *  ENIAMexec implements ENIAM processing stream
 *  Copyright (C) 2016-2017 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
 *  Copyright (C) 2016-2017 Institute of Computer Science Polish Academy of Sciences
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

open ExecTypes
open Xstd

let select_mode = function
    (Raw,_),_ -> failwith "select_mode"
  | _,(Raw,_) -> failwith "select_mode"
  | (Struct,_),_ -> failwith "select_mode"
  | _,(Struct,_) -> failwith "select_mode"
  | (CONLL,s),_ -> CONLL,s
  | _,(CONLL,s) -> CONLL,s
  | (ENIAM,s),_ -> ENIAM,s
  | _,(ENIAM,s) -> ENIAM,s
  | (Swigra,s),_ -> Swigra,s
  | _,(Swigra,s) -> Swigra,s
  | (Mate,s),_ -> Mate,s
  | _,(Mate,s) -> Mate,s
  | _ -> failwith "select_mode: ni"

let rec select_sentence_modes_sentence = function
    RawSentence s -> failwith "select_sentence_modes_sentence"
  | StructSentence(paths,last) -> failwith "select_sentence_modes_sentence"
  | DepSentence paths  -> failwith "select_sentence_modes_sentence"
  | QuotedSentences sentences ->
      let sentences = Xlist.rev_map sentences (fun p ->
        let sentence,_ = select_sentence_modes_sentence p.sentence in
        {p with sentence=sentence}) in
      QuotedSentences(List.rev sentences), Parsed
  | AltSentence l ->
      let raw,selected = Xlist.fold l ([],[]) (fun (raw,selected) (mode,sentence) ->
        if mode = Raw then (mode,sentence) :: raw, selected else
        let sentence,status = select_sentence_modes_sentence sentence in
        if status <> Parsed (*&& status <> NotTranslated*) then raw,selected else
        match selected with
          [] -> raw,[mode,sentence]
        | [mode2,sentence2] -> raw,[select_mode ((mode,sentence),(mode2,sentence2))]
        | _ -> failwith "select_sentence_modes_sentence") in
      AltSentence(raw @ selected), Parsed
  | ENIAMSentence result -> ENIAMSentence result, result.status
  (* | CONLLSentence result -> CONLLSentence result, result.status
  | SemSentence result -> SemSentence result, result.status *)
  | ErrorSentence s -> failwith "select_sentence_modes_sentence"

let rec select_sentence_modes_paragraph = function
    RawParagraph s -> RawParagraph s
  | StructParagraph(stats,sentences) ->
      let sentences = Xlist.rev_map sentences (fun p ->
        let sentence,_ = select_sentence_modes_sentence p.sentence in
        {p with sentence=sentence}) in
      StructParagraph(stats,List.rev sentences)
  | AltParagraph l ->
      let l = Xlist.rev_map l (fun (mode,paragraph) ->
        mode, select_sentence_modes_paragraph paragraph) in
      AltParagraph(List.rev l)
  | ErrorParagraph s -> ErrorParagraph s

let rec select_sentence_modes_text = function
    RawText s -> RawText s
  | StructText paragraphs ->
      let paragraphs = Xlist.rev_map paragraphs (fun paragraph ->
        select_sentence_modes_paragraph paragraph) in
      StructText(List.rev paragraphs)
  | JSONtext s -> JSONtext s
  | AltText l -> AltText(Xlist.map l (fun (mode,text) ->
       mode, select_sentence_modes_text text))
  | ErrorText s -> ErrorText s

let add_token paths (q,p) =
  let map = try IntMap.find paths p.beg with Not_found -> IntMap.empty in
  let map = IntMap.add_inc map p.next [q,p] (fun l -> (q,p) :: l) in
  IntMap.add paths p.beg map

let rec select_sentences_rec last paths nodes map =
  let node = IntSet.min_elt nodes in
  if node = last then try snd (IntMap.find map node) with Not_found -> failwith "select_sentences_rec" else
  let nodes = IntSet.remove nodes node in
  if not (IntMap.mem map node) then select_sentences_rec last paths nodes map else
  let qselected,selected = IntMap.find map node in
  let map2 = try IntMap.find paths node with Not_found -> IntMap.empty in
  let map = IntMap.fold map2 map (fun map next l ->
    Xlist.fold l map (fun map (q,p) ->
      let selected = StringSet.add selected p.id in
      let qselected = qselected+q in
      IntMap.add_inc map p.next (qselected,selected) (fun (qselected2,selected2) ->
        if qselected2 > qselected then qselected2,selected2 else
        if qselected2 < qselected then qselected,selected else
        qselected,StringSet.union selected selected2))) in
  select_sentences_rec last paths nodes map

let rec calculate_quality mode = function
    RawSentence s -> -2
  | StructSentence(paths,last) -> -2
  | DepSentence paths  -> -2
  | QuotedSentences sentences ->
      Xlist.fold sentences 0 (fun q p -> q + calculate_quality mode p.sentence)
  | AltSentence l ->
      Xlist.fold l (-1000000) (fun q (mode,sentence) ->
        max q (calculate_quality mode sentence))
  | ENIAMSentence result ->
      if mode = ENIAM && result.status = Parsed then 0 else -1
  (* | CONLLSentence result -> CONLLSentence result, result.status
  | SemSentence result -> SemSentence result, result.status *)
  | ErrorSentence s -> -2

let select_sentences mode paths =
  if paths = [] then paths else
  let beg,last = Xlist.fold paths (max_int,-1) (fun (beg,last) p ->
    min beg p.beg, max last p.next) in
  let nodes = Xlist.fold paths IntSet.empty (fun nodes p ->
    IntSet.add (IntSet.add nodes p.beg) p.next) in
  let paths2 = Xlist.rev_map paths (fun p -> calculate_quality mode p.sentence, p) in
  let paths2 = Xlist.fold paths2 IntMap.empty add_token in
  let selected = select_sentences_rec last paths2 nodes (IntMap.add IntMap.empty beg (0,StringSet.empty)) in
  (* print_endline (String.concat " " (StringSet.to_list selected)); *)
  Xlist.fold paths [] (fun paths p ->
    if StringSet.mem selected p.id then p :: paths else paths)

let rec select_sentences_sentence mode = function
    QuotedSentences sentences ->
      let sentences = Xlist.rev_map sentences (fun p ->
        {p with sentence=select_sentences_sentence mode p.sentence}) in
      QuotedSentences(select_sentences mode sentences)
  | AltSentence l ->
      let l = Xlist.rev_map l (fun (mode,sentence) ->
        mode, select_sentences_sentence mode sentence) in
      AltSentence(List.rev l)
  | t -> t

let rec select_sentences_paragraph mode = function
    RawParagraph s -> RawParagraph s
  | StructParagraph(stats,sentences) ->
      let sentences = Xlist.rev_map sentences (fun p ->
        {p with sentence=select_sentences_sentence mode p.sentence}) in
      StructParagraph(stats,select_sentences mode sentences)
  | AltParagraph l ->
      let l = Xlist.rev_map l (fun (mode,paragraph) ->
        mode, select_sentences_paragraph mode paragraph) in
      AltParagraph(List.rev l)
  | ErrorParagraph s -> ErrorParagraph s

let rec select_sentences_text mode = function
    RawText s -> RawText s
  | StructText paragraphs ->
      let paragraphs = Xlist.rev_map paragraphs (fun paragraph ->
        select_sentences_paragraph mode paragraph) in
      StructText(List.rev paragraphs)
  | JSONtext s -> JSONtext s
  | AltText l -> AltText(Xlist.map l (fun (mode,text) ->
       mode, select_sentences_text mode text))
  | ErrorText s -> ErrorText s

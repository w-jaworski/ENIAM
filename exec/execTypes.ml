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

open LCGtypes
open Xstd

type status =
    Idle | PreprocessingError | LexiconError
  | ParseError | ParseTimeout | Parsed | PartialParsed | TooManyNodes | NotParsed
  | NotReduced | ReductionError | ReductionError2 | ReductionError3 (*| NotTranslated*)
  | SemValenceError | SemGraphError | SemGraphError2 | SemNotValidated | SemParsed | PartialSemParsed
  | Inferenced | InferenceError

type eniam_parse_result = {
(*  file_prefix: string;*)
  status: status;
  msg: string;
  lex_time: float;
  parse_time: float;
  reduction_time: float;
  sem_time: float;
  paths_size: int;
  chart_size: int;
  dependency_tree_size: int;
  chart1: chart;
  dep_chart1: dep_tree;
  chart2: chart;
  dep_chart2: dep_tree;
  references2: linear_term ExtArray.t;
  chart3: chart;
  parsed_dep_chart3: (SymbolMap.key * linear_term) list;
  not_parsed_dep_chart3: int *
   (grammar_symbol * linear_term) list list *
   (grammar_symbol * linear_term) list *
   (grammar_symbol * linear_term) list list;
  references3: linear_term ExtArray.t;
  term4: linear_term;
  dependency_tree4: linear_term array;
  dependency_tree5: linear_term array;
  dependency_tree6a: linear_term array;
  dependency_tree6b: linear_term array;
  dependency_tree7: linear_term array;
  dependency_tree8: linear_term ExtArray.t;
  dependency_tree9: linear_term array;
  semantic_graph10: SemTypes.linear_term array;
  semantic_graph11: SemTypes.linear_term;
  semantic_graph12: SemTypes.linear_term;
  semantic_graph13: SemTypes.linear_term;
(**  text_fragments: string IntMap.t array;**)
  par_string: string;
  node_mapping: int Xstd.IntMap.t;
  merge_edges: int;
  }

type mode =
    Raw | Struct | CONLL | ENIAM | Mate | Swigra | POLFIE | Error | Name | Identifier

type stats = {c_len: int; c_len_nann: int; t_len: int; t_len_nann: int; c_len2: int; c_len2_nann: int; t_len2: int; t_len2_nann: int} (* c_ -> character; t_ -> token; len -> length; _nann -> not annotated; 2 -> parsowanie zdań *)
    
type sentence =
    RawSentence of string
  | StructSentence of (int * int * int) list * int (* (id * lnode * rnode) list * last *)
  | DepSentence of (int * (int * string) list * string) array list (* (id * (super * label) list * sem) conll_id *)
  | QuotedSentences of paragraph_record list
  | AltSentence of (mode * sentence) list  (* string = etykieta np raw, nkjp, krzaki *)
  | ENIAMSentence of eniam_parse_result
  | ErrorSentence of string

and paragraph_record = {id: string; beg: int; len: int; next: int; sentence: sentence; file_prefix: string; no_tokens: int} (* beg i len liczone po znakach unicode ( * 100 ???) *)

and paragraph =
    RawParagraph of string
  | StructParagraph of stats * paragraph_record list (* stats * zdania *)
  | AltParagraph of (mode * paragraph) list
  | ErrorParagraph of string

type text =
    RawText of string
  | StructText of paragraph list
  | JSONtext of string
  | AltText of (mode * text) list
  | ErrorText of string

(*

type sum_result = {
  no_queries: int;
  no_pre_error: int;
  no_lex_error: int;
  no_parse_error: int;
  no_timeout: int;
  no_reduction_error: int;
  no_sem_error: int;
  no_not_parsed: int;
  no_not_reduced: int;
  no_too_many_nodes: int;
  no_not_translated: int;
  no_parsed: int;
  sum_pre_time1: float;
  sum_pre_time2: float;
  sum_lex_time: float;
  sum_parse_time: float;
  sum_reduction_time: float;
  sum_sem_time: float;
  }*)

type message_to_worker =
    Work_with of string * string
  | Kill_yourself

type message_from_worker =
    Ready_to_work of string
  | Work_done of
      string  * (text * bool * SubsyntaxTypes.token_env ExtArray.t * LexSemanticsTypes.lex_sem ExtArray.t)

let time_fun = Unix.gettimeofday
(* let time_fun = Sys.time () *)

let empty_eniam_parse_result = {
  (* file_prefix=""; *)
  status=Idle;
  msg="";
  lex_time=0.;
  parse_time=0.;
  reduction_time=0.;
  sem_time=0.;
  paths_size=0;
  chart_size=0;
  dependency_tree_size=0;
  chart1=[| |];
  dep_chart1=DepNode(-100,[],[],[]);
  chart2=[| |];
  dep_chart2=DepNode(-100,[],[],[]);
  references2=ExtArray.make 0 Dot;
  chart3=[| |];
  parsed_dep_chart3=[];
  not_parsed_dep_chart3=(-100,[],[],[]);
  references3=ExtArray.make 0 Dot;
  term4=Dot;
  dependency_tree4=[| |];
  dependency_tree5=[| |];
  dependency_tree6a=[| |];
  dependency_tree6b=[| |];
  dependency_tree7=[| |];
  dependency_tree8=ExtArray.make 0 Dot;
  dependency_tree9=[| |];
  semantic_graph10=[| |];
  semantic_graph11=SemTypes.Dot;
  semantic_graph12=SemTypes.Dot;
  semantic_graph13=SemTypes.Dot;
(**  text_fragments=[| |];**)
  par_string="";
  node_mapping=IntMap.empty;
  merge_edges=0;
  }


(*

let empty_sum_result = {
  no_queries=0;
  no_pre_error=0;
  no_lex_error=0;
  no_parse_error=0;
  no_timeout=0;
  no_reduction_error=0;
  no_sem_error=0;
  no_not_parsed=0;
  no_not_reduced=0;
  no_too_many_nodes=0;
  no_not_translated=0;
  no_parsed=0;
  sum_pre_time1=0.;
  sum_pre_time2=0.;
  sum_lex_time=0.;
  sum_parse_time=0.;
  sum_reduction_time=0.;
  sum_sem_time=0.;
  }

*)

let zero_stats = {c_len=0; c_len_nann=0; t_len=0; t_len_nann=0; c_len2=0; c_len2_nann=0; t_len2=0; t_len2_nann=0}
let max_int_stats = {c_len=max_int; c_len_nann=max_int; t_len=max_int; t_len_nann=max_int; c_len2=max_int; c_len2_nann=max_int; t_len2=max_int; t_len2_nann=max_int}

let rec map_sentence mode f = function
  | QuotedSentences sentences ->
      let sentences = Xlist.rev_map sentences (fun p ->
        let sentence = map_sentence mode f p.sentence in
        {p with sentence=sentence}) in
      QuotedSentences(List.rev sentences)
  | AltSentence l ->
      let l = Xlist.rev_map l (fun (mode,sentence) ->
        mode, map_sentence mode f sentence) in
      AltSentence(List.rev l)
  | s -> f mode s

let rec map_paragraph mode f = function
    RawParagraph s -> RawParagraph s
  | StructParagraph(stats,sentences) ->
      let sentences = Xlist.rev_map sentences (fun p ->
        let sentence = map_sentence mode f p.sentence in
        {p with sentence=sentence}) in
      StructParagraph(stats,List.rev sentences)
  | AltParagraph l ->
      let l = Xlist.rev_map l (fun (mode,paragraph) ->
        mode, map_paragraph mode f paragraph) in
      AltParagraph(List.rev l)
  | ErrorParagraph s -> ErrorParagraph s

let rec map_text mode f = function
    RawText s -> RawText s
  | StructText paragraphs ->
      let paragraphs = Xlist.rev_map paragraphs (fun paragraph ->
        map_paragraph mode f paragraph) in
      StructText(List.rev paragraphs)
  | JSONtext s -> JSONtext s
  | AltText l -> AltText(Xlist.map l (fun (mode,text) ->
       mode, map_text mode f text))
  | ErrorText s -> ErrorText s

let rec map_sentence_par_string mode par_string f = function
  | QuotedSentences sentences ->
      let sentences = Xlist.rev_map sentences (fun p ->
        let sentence = map_sentence_par_string mode par_string f p.sentence in
        {p with sentence=sentence}) in
      QuotedSentences(List.rev sentences)
  | AltSentence l ->
      let l = Xlist.rev_map l (fun (mode,sentence) ->
        mode, map_sentence_par_string mode par_string f sentence) in
      AltSentence(List.rev l)
  | s -> f mode par_string s

let rec map_paragraph_par_string mode par_string f = function
    RawParagraph s -> RawParagraph s
  | StructParagraph(stats,sentences) ->
      let sentences = Xlist.rev_map sentences (fun p ->
        let sentence = map_sentence_par_string mode par_string f p.sentence in
        {p with sentence=sentence}) in
      StructParagraph(stats,List.rev sentences)
  | AltParagraph l ->
      let l = Xlist.rev_map l (fun (mode,paragraph) ->
        mode, map_paragraph_par_string mode par_string f paragraph) in
      AltParagraph(List.rev l)
  | ErrorParagraph s -> ErrorParagraph s

let rec find_paragraph_string mode = function
    RawParagraph s -> if mode = Raw then [s] else []
  | StructParagraph(_,sentences) -> []
  | AltParagraph l -> List.flatten (Xlist.map l (fun (mode,paragraph) -> find_paragraph_string mode paragraph))
  | ErrorParagraph s -> []
  
let rec map_text_par_string mode f = function
    RawText s -> RawText s
  | StructText paragraphs ->
      let paragraphs = Xlist.rev_map paragraphs (fun paragraph ->
        let par_string = 
          match find_paragraph_string Struct paragraph with
            [par_string] -> par_string 
		  | _ -> failwith "map_text_par_string" in
        map_paragraph_par_string mode par_string f paragraph) in
      StructText(List.rev paragraphs)
  | JSONtext s -> JSONtext s
  | AltText l -> AltText(Xlist.map l (fun (mode,text) ->
       mode, map_text_par_string mode f text))
  | ErrorText s -> ErrorText s



let rec fold_sentence mode s f = function
    QuotedSentences sentences ->
    Xlist.fold sentences s (fun s p ->
        fold_sentence mode s f p.sentence)
  | AltSentence l ->
    Xlist.fold l s (fun s (mode,sentence) ->
        fold_sentence mode s f sentence)
  | t -> f mode s t

let rec fold_paragraph mode s f = function
    RawParagraph _ -> s
  | StructParagraph(stats,sentences) ->
    Xlist.fold sentences s (fun s p ->
        fold_sentence mode s f p.sentence)
  | AltParagraph l ->
    Xlist.fold l s (fun s (mode,paragraph) ->
        fold_paragraph mode s f paragraph)
  | ErrorParagraph _ -> s

let rec fold_text mode s f = function
    RawText _ -> s
  | StructText paragraphs ->
    Xlist.fold paragraphs s (fun s paragraph ->
        fold_paragraph mode s f paragraph)
  | JSONtext _ -> s
  | AltText l ->
    Xlist.fold l s (fun s (mode,text) ->
        fold_text mode s f text)
  | ErrorText _ -> s

let rules_filename = LCGlexiconTypes.resource_path ^ "/LCGlexicon/lexicon-pl.dic"
let colours_filename = WalTypes.data_path ^ "/colours.tab"

let lcg_rules = ref ([] : (int * (LCGtypes.linear_term ExtArray.t ->
          (LCGtypes.SymbolMap.key * LCGtypes.linear_term) list ->
          (LCGtypes.SymbolMap.key * LCGtypes.linear_term) list ->
          (LCGtypes.SymbolMap.key * LCGtypes.linear_term) list))
         list)

let pro_rules = ref ([] : (int * (LCGtypes.linear_term ExtArray.t ->
          (LCGtypes.SymbolMap.key * LCGtypes.linear_term) list ->
          (LCGtypes.SymbolMap.key * LCGtypes.linear_term) list))
         list)
(* ref ([] :: ((LCGtypes.grammar_symbol * LCGtypes.linear_term) * int) list)  *)
         
let partial_parsing_flag = ref false

(*let morphology_in = ref stdin
let morphology_out = ref stdout*)

(*
 *  ENIAMexec implements ENIAM processing stream
 *  Copyright (C) 2016-2018 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
 *  Copyright (C) 2016-2018 Institute of Computer Science Polish Academy of Sciences
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

open LCGtypes
open Xstd
open Printf
open SubsyntaxTypes
open ExecTypes

let string_of_status = function
      Idle -> "Idle"
    | PreprocessingError -> "PreprocessingError"
(*     | NotLemmatized -> "NotLemmatized" *)
    | LexiconError -> "LexiconError"
    | ParseError -> "ParseError"
    | ParseTimeout -> "ParseTimeout"
    | Parsed -> "Parsed"
    | PartialParsed -> "PartialParsed"
    | TooManyNodes -> "TooManyNodes"
    | NotParsed -> "NotParsed"
    | NotReduced -> "NotReduced"
    | ReductionError -> "ReductionError"
    | ReductionError2 -> "ReductionError2"
    | ReductionError3 -> "ReductionError3"
    | SemValenceError -> "SemValenceError"
    | SemGraphError -> "SemGraphError"
    | SemGraphError2 -> "SemGraphError2"
    | SemNotValidated -> "SemNotValidated"
    | SemParsed -> "SemParsed"
    | PartialSemParsed -> "PartialSemParsed"
    | Inferenced -> "Inferenced"
    | InferenceError -> "InferenceError"

(*
let string_of_status = function
    ExecTypes.Idle -> "idle"
  | ExecTypes.PreprocessingError -> "error_pre"
  | ExecTypes.LexiconError -> "error_lex"
  | ExecTypes.ParseError -> "error_parse"
  | ExecTypes.ParseTimeout -> "timeout"
  | ExecTypes.NotParsed -> "not_parsed"
  | ExecTypes.ReductionError -> "error_reduction"
  | ExecTypes.TooManyNodes -> "too_many_nodes"
  | ExecTypes.NotReduced -> "not_reduced"
  | ExecTypes.SemError -> "error_sem"
  | ExecTypes.NotTranslated -> "not_translated"
  | ExecTypes.Parsed -> "parsed"
*)

let string_of_stats s =
  Printf.sprintf "percentage of not recognized %d/%d=%f tokens %d/%d=%f characters\n" s.t_len_nann s.t_len ((float s.t_len_nann)/.float s.t_len) s.c_len_nann s.c_len ((float s.c_len_nann)/.float s.c_len) ^
  Printf.sprintf "percentage of not parsed %d/%d=%f tokens %d/%d=%f characters" s.t_len2_nann s.t_len2 ((float s.t_len2_nann)/.float s.t_len2) s.c_len2_nann s.c_len2 ((float s.c_len2_nann)/.float s.c_len2)
 

let page_header path =
"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">
<html>
  <head>
        <META HTTP-EQUIV=\"CONTENT-TYPE\" CONTENT=\"text/html; charset=utf8\">
        <TITLE>ENIAM: Kategorialny Parser Składniowo-Semantyczny</TITLE>
        <META HTTP-EQUIV=\"Content-Language\" CONTENT=\"pl\">
  </head>

  <body>
 <center>"

let page_trailer =
"</center>
  </body>
</html>"

let print_other_result file cg_bin_path query msg =
  fprintf file "%s\n" (page_header cg_bin_path);
  fprintf file "\n<H3>%s</H3>\n" query;
  fprintf file "\n<P>%s\n" msg(*generate_status_message result result.status*);
  fprintf file "%s\n" page_trailer

let string_of_mode = function
    Raw -> "Raw"
  | Struct -> "Struct"
  | CONLL -> "CONLL"
  | ENIAM -> "ENIAM"
  | Mate -> "Mate"
  | Swigra -> "Swigra"
  | POLFIE -> "POLFIE"
  | Error -> "Error"
  | Name -> "Name"
  | Identifier -> "Id"

let html_header =
"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">
<html>
  <head>
	<META HTTP-EQUIV=\"CONTENT-TYPE\" CONTENT=\"text/html; charset=utf8\">
	<TITLE>ENIAM: Kategorialny Parser Składniowo-Semantyczny</TITLE>
	<META HTTP-EQUIV=\"Content-Language\" CONTENT=\"pl\">
  </head>

  <body>
 <center>"

let html_header_title title =
"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">
<html>
  <head>
	<META HTTP-EQUIV=\"CONTENT-TYPE\" CONTENT=\"text/html; charset=utf8\">
	<TITLE>" ^ title ^ "</TITLE>
	<META HTTP-EQUIV=\"Content-Language\" CONTENT=\"pl\">
  </head>

  <body>
 <center>"

let html_trailer =
"</center>
  </body>
</html>"

let escape_html s =
(*   try *)
  let t = Buffer.create (Xstring.size s) in
  Int.iter 0 (String.length s - 1) (fun i ->
    match String.get s i with
       '<' -> Buffer.add_string t "&lt;"
     | '>' -> Buffer.add_string t "&gt;"
     | '&' -> Buffer.add_string t "&amp;"
     | '\n' -> Buffer.add_string t "<BR>"
     | c -> Buffer.add_char t c);
  Buffer.contents t
(*   with e -> failwith ("escape_html: '" ^ s ^ "' " ^ Printexc.to_string e) *)

let get_prefix s =
  if Xstring.size s > 1000 then 
    String.sub s 0 1000 ^ "........"
  else s

let rec extract_pos_cat_internal vars = function
  | Atom x -> x
  | AVar x -> (try extract_pos_cat_internal vars (Xlist.assoc vars x) with Not_found -> failwith "extract_pos_cat_internal")
  | With l -> String.concat "&" (Xlist.map l (extract_pos_cat_internal vars))
  | Zero -> "0"
  | Top -> "T"

let rec extract_pos_cat vars = function
  | Tensor [] -> failwith "extract_pos_cat: ni"
  | Tensor [pos] -> extract_pos_cat_internal vars pos
  | Tensor [pos;_] -> extract_pos_cat_internal vars pos
  | Tensor [pos;_;_] -> extract_pos_cat_internal vars pos
  | Tensor (Atom "num" :: _) -> "Number"
  | Tensor (Atom "aglt" :: _) -> "Aglt"
  | Tensor (Atom "prepnp" :: _) -> "Prep"
  | Tensor (Atom "comparp" :: _) -> "Compar"
  | Tensor (Atom "cp" :: _) -> "Comp"
  | Tensor [_;cat;_;_] -> extract_pos_cat_internal vars cat
  | Tensor [_;_;cat;_;_] -> extract_pos_cat_internal vars cat
  | Tensor [_;_;_;cat;_;_] -> extract_pos_cat_internal vars cat
  | Tensor [_;_;_;_;cat;_;_] -> extract_pos_cat_internal vars cat
  | Tensor [_;_;_;_;_;cat;_;_] -> extract_pos_cat_internal vars cat
  | Tensor [_;_;_;_;_;_;cat;_;_] -> extract_pos_cat_internal vars cat
  | Tensor [_;_;_;_;_;_;_;cat;_;_] -> extract_pos_cat_internal vars cat
  (* | Tensor (pos :: cat :: _) -> (*extract_pos_cat_internal vars pos ^ "*" ^*) extract_pos_cat_internal vars cat *)
  | Tensor _ as t -> print_endline ("Unknown symbol " ^ LCGstringOf.grammar_symbol 0 t); "Unknown"
  | Plus l -> failwith "extract_pos_cat: ni"
  | StarWith l -> failwith "extract_pos_cat: ni"
  | Imp(s,d,t2) -> extract_pos_cat vars s
  | One -> failwith "extract_pos_cat: ni"
  | ImpSet(s,l) -> extract_pos_cat vars s
  | WithVar(v,g,e,s) -> extract_pos_cat ((v,g) :: vars) s
  | Star(_,s) -> extract_pos_cat vars s
  | Conj _ -> "Conj"
  | Preconj -> "Preconj"
  | Bracket(lf,rf,s) -> extract_pos_cat vars s
  | BracketSet d -> "BracketSet"
  | Maybe s -> failwith "extract_pos_cat: ni"

let omited = StringSet.of_list ["<subst>";"<depr>";"<ppron12>";"<ppron3>";"<siebie>";"<prep>";
  "<num>";"<intnum>";"<realnum>";"<intnum-interval>";"<realnum-interval>";"<symbol>";"<ordnum>";
  "<date>";"<date-interval>";"<hour-minute>";"<hour>";"<hour-minute-interval>";"<hour-interval>";
  "<year>";"<year-interval>";"<day>";"<day-interval>";"<day-month>";"<day-month-interval>";
  "<month-interval>";"<roman>";"<roman-interval>";"<roman-ordnum>";"<match-result>";"<url>";
  "<email>";"<obj-id>";"<adj>";"<apron>";"<adjc>";"<adjp>";"<adja>";"<adv>";"<ger>";"<pact>";
  "<ppas>";"<fin>";"<bedzie>";"<praet>";"<winien>";"<impt>";"<imps>";"<pred>";"<aglt>";"<inf>";
  "<pcon>";"<pant>";"<qub>";"<comp>";"<compar>";"<conj>";"<interj>";"<sinterj>";"<burk>";
  "<interp>";"<part>";"<unk>";"<building-number>";"<html-tag>";"<list-item>";"<numcomp>";
  "<phone-number>";"<postal-code>";"<sentence>";"<paragraph>"]

let cat_tokens_sequence par_string node_mapping g =
  let _,_,l = LCGchart.fold g (0,0,[]) (fun (m,n,l) (symbol,node1,node2,cost,sem,layer) ->
    node1,node2,
    (if m < node1 then
      if n < node1 then [n, node1, MarkedHTMLof.get_text_fragment par_string node_mapping n node1, "null"]
      else if n = node1 then []
      else [node1, n, MarkedHTMLof.get_text_fragment par_string node_mapping node1 n, "overlap"]
    else if m = node1 then
      if n < node2 then [m, n, MarkedHTMLof.get_text_fragment par_string node_mapping m n, "overlap"]
      else if n = node2 then []
      else [node1, node2, MarkedHTMLof.get_text_fragment par_string node_mapping node1 node2, "overlap"]
    else failwith "cat_tokens_sequence") @
    [node1, node2, MarkedHTMLof.get_text_fragment par_string node_mapping node1 node2, extract_pos_cat [] symbol] @ l) in
  let map = Xlist.fold l IntMap.empty (fun map (m,n,text,symbol) ->
    IntMap.add_inc map (1000000*m+n) [text,symbol] (fun l -> (text,symbol) :: l)) in
  let map = IntMap.map map (fun l ->
    let t,ov,set = Xlist.fold l ("",false,StringSet.empty) (fun (t,ov,set) (text,symbol) ->
      if symbol = "null" then text,ov,set
      else if symbol = "overlap" then t,true,set
      else if StringSet.mem omited symbol then text,ov,set
      else t,ov,StringSet.add set symbol) in
    let l = if StringSet.is_empty set then [t] else StringSet.to_list set in
    if ov then "OVERLAP{" ^ String.concat " " l ^ "}" else
    match l with
      [t] -> t
    | _ -> "{" ^ String.concat " " l ^ "}") in
  let l = List.sort compare (IntMap.fold map [] (fun l k texts -> (k,texts) :: l)) in
(*  let l = Xlist.sort l (fun (m1,n1,text1,symbol1) (m2,n2,text2,symbol2) ->
    if m1 <> m2 then compare m1 m2 else
    if n1 <> n2 then compare n1 n2 else
    compare symbol1 symbol2) in
  let l = if l = [] then l else
    Xlist.fold (List.tl l) [List.hd l] (fun l a ->
      match l with
        [] -> failwith "cat_tokens_sequence"
      | b :: l -> if a = b then b :: l else a :: b :: l) in*)
  String.concat " " (Xlist.map l (fun (n,texts) -> texts))


(* verbosity:
  0 -> jedynie informacja o statusie zdania
  1 -> zawartość struktur danych istotnych dla uzyskanego statusu
  2 -> zawartość wszystkich struktur danych
*)

let html_of_eniam_sentence path file_prefix img verbosity tokens (result : eniam_parse_result) =
  match result.status with
    Idle -> "<font color=\"red\">idle</font>\n"
(*   | NotLemmatized -> sprintf "<font color=\"red\">not_lemmatized</font>\n"  *)
  | LexiconError -> sprintf "<font color=\"red\">error_lex</font>: %s paths_size=%d\n" (escape_html result.msg) result.paths_size
  | ParseError ->
      if verbosity = 0 then () else (
        LCGlatexOf.print_chart path (file_prefix ^ "_1_chart") "a1paper" result.par_string result.node_mapping result.chart1 (ExtArray.make 1 Dot) tokens;
        LCGlatexOf.print_chart path (file_prefix ^ "_2_chart") "a4paper" result.par_string result.node_mapping result.chart2 result.references2 tokens;
        LCGlatexOf.print_references path (file_prefix ^ "_2_references") "a0paper" result.references2);
      sprintf "<font color=\"red\">error_parse</font>: %s paths_size=%d\n" (escape_html result.msg) result.paths_size ^
      (if verbosity = 0 then "" else
        sprintf "<BR><A HREF=\"%s_1_chart.pdf\">Chart 1</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_2_chart.pdf\">Chart 2</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_2_references.pdf\">References 2</A>\n" file_prefix) ^
      ""
  | ParseTimeout ->
      if verbosity < 2 then () else (
        LCGlatexOf.print_chart path (file_prefix ^ "_1_chart") "a1paper" result.par_string result.node_mapping result.chart1 (ExtArray.make 1 Dot) tokens;
        LCGlatexOf.print_references path (file_prefix ^ "_2_references") "a0paper" result.references2);
      if verbosity = 0 then () else (
        LCGlatexOf.print_chart path (file_prefix ^ "_2_chart") "a4paper" result.par_string result.node_mapping result.chart2 result.references2 tokens);
      sprintf "<font color=\"red\">timeout</font>: %s paths_size=%d\n" (escape_html result.msg) result.paths_size ^
      (if verbosity < 2 then "" else
        sprintf "<BR><A HREF=\"%s_1_chart.pdf\">Chart 1</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_2_references.pdf\">References 2</A>\n" file_prefix) ^
      (if verbosity = 0 then "" else
        sprintf "<BR><A HREF=\"%s_2_chart.pdf\">Chart 2</A>\n" file_prefix) ^
      ""
  | NotParsed ->
      if verbosity < 2 then () else (
        LCGlatexOf.print_chart path (file_prefix ^ "_1_chart") "a1paper" result.par_string result.node_mapping result.chart1 (ExtArray.make 1 Dot) tokens);
      if verbosity < 1 then () else (
        LCGlatexOf.print_chart path (file_prefix ^ "_2_chart") "paperheight=10.75in,paperwidth=80cm" result.par_string result.node_mapping result.chart2 result.references2 tokens);
      if verbosity < 2 then () else (
        LCGlatexOf.print_references path (file_prefix ^ "_2_references") "a0paper" result.references2;
        LCGlatexOf.print_references path (file_prefix ^ "_3_references") "a0paper" result.references3;
        LCGlatexOf.print_chart path (file_prefix ^ "_3_chart") "a4paper" result.par_string result.node_mapping result.chart3 result.references3 tokens);
      if verbosity = 0 then () else (
        LCGlatexOf.print_chart2 path (file_prefix ^ "_3_chart_selection") "a4paper" result.par_string result.node_mapping (LCGchart.select_maximal result.chart3) result.references3 tokens);
      sprintf "<font color=\"red\">not_parsed</font>: paths_size=%d chart_size=%d\n" result.paths_size result.chart_size ^
      (if verbosity < 2 then "" else
        sprintf "<BR><A HREF=\"%s_1_chart.pdf\">Chart 1</A>\n" file_prefix) ^
      (if verbosity < 1 then "" else
        sprintf "<BR><A HREF=\"%s_2_chart.pdf\">Chart 2</A>\n" file_prefix) ^
      (if verbosity < 2 then "" else
        sprintf "<BR><A HREF=\"%s_2_references.pdf\">References 2</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_3_references.pdf\">References 3</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_3_chart.pdf\">Chart 3</A>\n" file_prefix) ^
      (if verbosity = 0 then "" else
        sprintf "<BR>%s\n" (escape_html (cat_tokens_sequence result.par_string result.node_mapping (LCGchart.select_maximal result.chart1))) ^
        sprintf "<BR><A HREF=\"%s_3_chart_selection.pdf\">Chart 3 Selection</A>\n" file_prefix) ^
      ""
  | ReductionError ->
      if verbosity < 2 then () else (
        LCGlatexOf.print_chart path (file_prefix ^ "_2_chart") "a4paper" result.par_string result.node_mapping result.chart2 result.references2 tokens;
        LCGlatexOf.print_references path (file_prefix ^ "_2_references") "a0paper" result.references2;
        LCGlatexOf.print_chart path (file_prefix ^ "_3_chart") "a4paper" result.par_string result.node_mapping result.chart3 result.references3 tokens);
      if verbosity = 0 then () else (
        LCGlatexOf.print_chart path (file_prefix ^ "_1_chart") "a1paper" result.par_string result.node_mapping result.chart1 (ExtArray.make 1 Dot) tokens;
        LCGlatexOf.print_references path (file_prefix ^ "_3_references") "a0paper" result.references3);
      sprintf "<font color=\"red\">error_reduction</font>: %s paths_size=%d chart_size=%d\n" (escape_html result.msg) result.paths_size result.chart_size ^
      (if verbosity = 0 then "" else
        sprintf "<BR><A HREF=\"%s_1_chart.pdf\">Chart 1</A>\n" file_prefix) ^
      (if verbosity < 2 then "" else
        sprintf "<BR><A HREF=\"%s_2_chart.pdf\">Chart 2</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_2_references.pdf\">References 2</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_3_chart.pdf\">Chart 3</A>\n" file_prefix) ^
      (if verbosity = 0 then "" else
        sprintf "<BR><A HREF=\"%s_3_references.pdf\">References 3</A>\n" file_prefix) ^
      ""
  | TooManyNodes ->
      if verbosity < 2 then () else (
        LCGlatexOf.print_chart path (file_prefix ^ "_1_chart") "a1paper" result.par_string result.node_mapping result.chart1 (ExtArray.make 1 Dot) tokens;
        LCGlatexOf.print_chart path (file_prefix ^ "_2_chart") "a4paper" result.par_string result.node_mapping result.chart2 result.references2 tokens;
        LCGlatexOf.print_references path (file_prefix ^ "_2_references") "a0paper" result.references2;
        LCGlatexOf.print_chart path (file_prefix ^ "_3_chart") "a4paper" result.par_string result.node_mapping result.chart3 result.references3 tokens;
        LCGlatexOf.print_references path (file_prefix ^ "_3_references") "a0paper" result.references3);
      sprintf "<font color=\"red\">to_many_nodes</font>: paths_size=%d chart_size=%d\n" result.paths_size result.chart_size ^
      (if verbosity < 2 then "" else
        sprintf "<BR><A HREF=\"%s_1_chart.pdf\">Chart 1</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_2_chart.pdf\">Chart 2</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_2_references.pdf\">References 2</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_3_chart.pdf\">Chart 3</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_3_references.pdf\">References 3</A>\n" file_prefix) ^
      ""
  | NotReduced ->
      if verbosity < 2 then () else (
        LCGlatexOf.print_chart path (file_prefix ^ "_1_chart") "a1paper" result.par_string result.node_mapping result.chart1 (ExtArray.make 1 Dot) tokens;
        LCGlatexOf.print_chart path (file_prefix ^ "_2_chart") "a4paper" result.par_string result.node_mapping result.chart2 result.references2 tokens;
        LCGlatexOf.print_references path (file_prefix ^ "_2_references") "a0paper" result.references2;
        LCGlatexOf.print_chart path (file_prefix ^ "_3_chart") "a4paper" result.par_string result.node_mapping result.chart3 result.references3 tokens);
      if verbosity = 0 then () else (
        LCGlatexOf.print_references path (file_prefix ^ "_3_references") "a0paper" result.references3;
        Xlatex.latex_file_out path (file_prefix ^ "_4_term") "a3paper" false (fun file ->
          Printf.fprintf file "\\[%s\\]\n" (LCGlatexOf.linear_term 0 result.term4));
        Xlatex.latex_compile_and_clean path (file_prefix ^ "_4_term");
        LCGlatexOf.print_dependency_tree path (file_prefix ^ "_4_dependency_tree") "paperheight=10.75in,paperwidth=540cm" result.dependency_tree4);
      sprintf "<font color=\"red\">not_reduced</font>: paths_size=%d chart_size=%d dependency_tree_size=%d\n" result.paths_size result.chart_size result.dependency_tree_size ^
      (if verbosity < 2 then "" else
        sprintf "<BR><A HREF=\"%s_1_chart.pdf\">Chart 1</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_2_chart.pdf\">Chart 2</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_2_references.pdf\">References 2</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_3_chart.pdf\">Chart 3</A>\n" file_prefix) ^
      (if verbosity = 0 then "" else
        sprintf "<BR><A HREF=\"%s_3_references.pdf\">References 3</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_4_term.pdf\">Term 4</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_4_dependency_tree.pdf\">Dependency Tree References 4</A>\n" file_prefix)  ^
      ""
  | ReductionError2 ->
      if verbosity < 2 then () else (
        LCGlatexOf.print_chart path (file_prefix ^ "_1_chart") "a1paper" result.par_string result.node_mapping result.chart1 (ExtArray.make 1 Dot) tokens;
        LCGlatexOf.print_chart path (file_prefix ^ "_2_chart") "a4paper" result.par_string result.node_mapping result.chart2 result.references2 tokens;
        LCGlatexOf.print_references path (file_prefix ^ "_2_references") "a0paper" result.references2;
        LCGlatexOf.print_chart path (file_prefix ^ "_3_chart") "a4paper" result.par_string result.node_mapping result.chart3 result.references3 tokens);
      if verbosity = 0 then () else (
        LCGlatexOf.print_references path (file_prefix ^ "_3_references") "a0paper" result.references3;
        Xlatex.latex_file_out path (file_prefix ^ "_4_term") "a3paper" false (fun file ->
          Printf.fprintf file "\\[%s\\]\n" (LCGlatexOf.linear_term 0 result.term4));
        Xlatex.latex_compile_and_clean path (file_prefix ^ "_4_term");
        LCGlatexOf.print_dependency_tree path (file_prefix ^ "_4_dependency_tree") "a0paper" result.dependency_tree4);
      sprintf "<font color=\"red\">error_reduction2</font>: %s paths_size=%d chart_size=%d dependency_tree_size=%d\n" (escape_html result.msg) result.paths_size result.chart_size result.dependency_tree_size ^
      (if verbosity < 2 then "" else
        sprintf "<BR><A HREF=\"%s_1_chart.pdf\">Chart 1</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_2_chart.pdf\">Chart 2</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_2_references.pdf\">References 2</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_3_chart.pdf\">Chart 3</A>\n" file_prefix) ^
      (if verbosity = 0 then "" else
        sprintf "<BR><A HREF=\"%s_3_references.pdf\">References 3</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_4_term.pdf\">Term 4</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_4_dependency_tree.pdf\">Dependency Tree References 4</A>\n" file_prefix)  ^
      ""
  | ReductionError3 ->
      if verbosity < 2 then () else (
        LCGlatexOf.print_chart path (file_prefix ^ "_1_chart") "a0paper" result.par_string result.node_mapping result.chart1 (ExtArray.make 1 Dot) tokens;
        LCGlatexOf.print_chart path (file_prefix ^ "_2_chart") "a0paper" result.par_string result.node_mapping result.chart2 result.references2 tokens;
        LCGlatexOf.print_references path (file_prefix ^ "_2_references") "a0paper" result.references2;
        LCGlatexOf.print_chart path (file_prefix ^ "_3_chart") "a0paper" result.par_string result.node_mapping result.chart3 result.references3 tokens;
        LCGlatexOf.print_references path (file_prefix ^ "_3_references") "a0paper" result.references3;
        Xlatex.latex_file_out path (file_prefix ^ "_4_term") "a3paper" false (fun file ->
          Printf.fprintf file "\\[%s\\]\n" (LCGlatexOf.linear_term 0 result.term4));
          Xlatex.latex_compile_and_clean path (file_prefix ^ "_4_term");
        LCGlatexOf.print_dependency_tree path (file_prefix ^ "_4_dependency_tree") "a0paper" result.dependency_tree4;
        LCGlatexOf.print_dependency_tree path (file_prefix ^ "_5_dependency_tree") "a4paper" result.dependency_tree5;
        LCGlatexOf.print_dependency_tree path (file_prefix ^ "_6a_dependency_tree") "a4paper" result.dependency_tree6a;
        LCGlatexOf.print_dependency_tree path (file_prefix ^ "_6b_dependency_tree") "a4paper" result.dependency_tree6b);
      if verbosity = 0 then () else (
        LCGgraphOf.print_dependency_tree path (file_prefix ^ "_6a_dependency_tree") result.dependency_tree6a;
        LCGgraphOf.print_dependency_tree path (file_prefix ^ "_6b_dependency_tree") result.dependency_tree6b;
        LCGgraphOf.print_simplified_dependency_tree path (file_prefix ^ "_6a_simple_dependency_tree") result.dependency_tree6a;
        LCGgraphOf.print_simplified_dependency_tree path (file_prefix ^ "_6b_simple_dependency_tree") result.dependency_tree6b);
      sprintf "<font color=\"red\">error_reduction3</font>: %s paths_size=%d chart_size=%d dependency_tree_size=%d\n" (escape_html result.msg) result.paths_size result.chart_size result.dependency_tree_size ^
      (if verbosity < 2 then "" else
        sprintf "<BR><A HREF=\"%s_1_chart.pdf\">Chart 1</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_2_chart.pdf\">Chart 2</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_2_references.pdf\">References 2</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_3_chart.pdf\">Chart 3</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_3_references.pdf\">References 3</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_4_term.pdf\">Term 4</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_4_dependency_tree.pdf\">Dependency Tree References 4</A>\n" file_prefix  ^
        sprintf "<BR><A HREF=\"%s_5_dependency_tree.pdf\">Dependency Tree References 5</A>\n" file_prefix  ^
        sprintf "<BR><A HREF=\"%s_6a_dependency_tree.pdf\">Dependency Tree References 6a</A>\n" file_prefix  ^
        sprintf "<BR><A HREF=\"%s_6b_dependency_tree.pdf\">Dependency Tree References 6b</A>\n" file_prefix)  ^
      (if verbosity = 0 then "" else
        (if img <> 2 then sprintf "<BR><A HREF=\"%s_6a_dependency_tree.png\">Dependency Tree 6a</A>\n" file_prefix
         else sprintf "<BR><IMG SRC=\"%s_6a_dependency_tree.png\">\n" file_prefix) ^
        (if img <> 2 then sprintf "<BR><A HREF=\"%s_6b_dependency_tree.png\">Dependency Tree 6b</A>\n" file_prefix
         else sprintf "<BR><IMG SRC=\"%s_6b_dependency_tree.png\">\n" file_prefix) ^
        (if img <> 1 then sprintf "<BR><A HREF=\"%s_6a_simple_dependency_tree.png\">Simplified Dependency Tree 6a</A>\n" file_prefix
         else sprintf "<BR><IMG SRC=\"%s_6a_simple_dependency_tree.png\">\n" file_prefix) ^
        (if img <> 1 then sprintf "<BR><A HREF=\"%s_6b_simple_dependency_tree.png\">Simplified Dependency Tree 6b</A>\n" file_prefix
         else sprintf "<BR><IMG SRC=\"%s_6b_simple_dependency_tree.png\">\n" file_prefix)) ^
      ""
  | Parsed | PartialParsed ->
      if verbosity < 2 then () else (
        LCGlatexOf.print_chart path (file_prefix ^ "_1_chart") "a1paper" result.par_string result.node_mapping result.chart1 (ExtArray.make 1 Dot) tokens;
        LCGlatexOf.print_chart path (file_prefix ^ "_2_chart") "a4paper" result.par_string result.node_mapping result.chart2 result.references2 tokens;
        LCGlatexOf.print_references path (file_prefix ^ "_2_references") "a0paper" result.references2;
        LCGlatexOf.print_chart path (file_prefix ^ "_3_chart") "a4paper" result.par_string result.node_mapping result.chart3 result.references3 tokens;
        LCGlatexOf.print_references path (file_prefix ^ "_3_references") "a0paper" result.references3;
        Xlatex.latex_file_out path (file_prefix ^ "_4_term") "a3paper" false (fun file ->
          Printf.fprintf file "\\[%s\\]\n" (LCGlatexOf.linear_term 0 result.term4));
          Xlatex.latex_compile_and_clean path (file_prefix ^ "_4_term");
        LCGlatexOf.print_dependency_tree path (file_prefix ^ "_4_dependency_tree") "a0paper" result.dependency_tree4;
        LCGlatexOf.print_dependency_tree path (file_prefix ^ "_5_dependency_tree") "a4paper" result.dependency_tree5;
        LCGlatexOf.print_dependency_tree path (file_prefix ^ "_6a_dependency_tree") "a4paper" result.dependency_tree6a;
        LCGlatexOf.print_dependency_tree path (file_prefix ^ "_6b_dependency_tree") "a4paper" result.dependency_tree6b);
      if verbosity = 0 then () else (
        LCGgraphOf.print_dependency_tree path (file_prefix ^ "_6a_dependency_tree") result.dependency_tree6a;
        LCGgraphOf.print_simplified_dependency_tree path (file_prefix ^ "_6a_simple_dependency_tree") result.dependency_tree6a);
      if verbosity < 2 then () else (
        LCGgraphOf.print_dependency_tree path (file_prefix ^ "_6b_dependency_tree") result.dependency_tree6b;
        LCGgraphOf.print_simplified_dependency_tree path (file_prefix ^ "_6b_simple_dependency_tree") result.dependency_tree6b);
      sprintf "%s: paths_size=%d chart_size=%d dependency_tree_size=%d\n" 
        (if result.status = Parsed then "parsed" else "partial_parsed") result.paths_size result.chart_size result.dependency_tree_size ^
      (if verbosity < 2 then "" else
        sprintf "<BR><A HREF=\"%s_1_chart.pdf\">Chart 1</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_2_chart.pdf\">Chart 2</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_2_references.pdf\">References 2</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_3_chart.pdf\">Chart 3</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_3_references.pdf\">References 3</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_4_term.pdf\">Term 4</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_4_dependency_tree.pdf\">Dependency Tree References 4</A>\n" file_prefix  ^
        sprintf "<BR><A HREF=\"%s_5_dependency_tree.pdf\">Dependency Tree References 5</A>\n" file_prefix  ^
        sprintf "<BR><A HREF=\"%s_6a_dependency_tree.pdf\">Dependency Tree References 6a</A>\n" file_prefix  ^
        sprintf "<BR><A HREF=\"%s_6b_dependency_tree.pdf\">Dependency Tree References 6b</A>\n" file_prefix)  ^
      (if verbosity = 0 then "" else
        (if img <> 2 then sprintf "<BR><A HREF=\"%s_6a_dependency_tree.png\">Dependency Tree 6a</A>\n" file_prefix
         else sprintf "<BR><IMG SRC=\"%s_6a_dependency_tree.png\">\n" file_prefix) ^
        (if img <> 1 then sprintf "<BR><A HREF=\"%s_6a_simple_dependency_tree.png\">Simplified Dependency Tree 6a</A>\n" file_prefix
         else sprintf "<BR><IMG SRC=\"%s_6a_simple_dependency_tree.png\">\n" file_prefix)) ^
      (if verbosity < 2 then "" else
        (if img <> 2 then sprintf "<BR><A HREF=\"%s_6b_dependency_tree.png\">Dependency Tree 6b</A>\n" file_prefix
         else sprintf "<BR><IMG SRC=\"%s_6b_dependency_tree.png\">\n" file_prefix) ^
        (if img <> 1 then sprintf "<BR><A HREF=\"%s_6b_simple_dependency_tree.png\">Simplified Dependency Tree 6b</A>\n" file_prefix
         else sprintf "<BR><IMG SRC=\"%s_6b_simple_dependency_tree.png\">\n" file_prefix)) ^
      ""
  | SemValenceError ->
      if verbosity = 0 then () else (
        LCGlatexOf.print_dependency_tree path (file_prefix ^ "_6b_dependency_tree") "a3paper" result.dependency_tree6b;
        LCGgraphOf.print_dependency_tree path (file_prefix ^ "_6b_dependency_tree") result.dependency_tree6b;
        if result.dependency_tree7 <> [| |] then LCGlatexOf.print_dependency_tree path (file_prefix ^ "_7_dependency_tree") "a2paper" result.dependency_tree7;
        if ExtArray.size result.dependency_tree8 <> 0 then LCGlatexOf.print_references path (file_prefix ^ "_8_dependency_tree") "a3paper" result.dependency_tree8;
        if result.dependency_tree9 <> [| |] then LCGlatexOf.print_dependency_tree path (file_prefix ^ "_9_dependency_tree") "a3paper" result.dependency_tree9;
        if result.dependency_tree9 <> [| |] then LCGgraphOf.print_dependency_tree path (file_prefix ^ "_9_dependency_tree") result.dependency_tree9);
      sprintf "<font color=\"red\">error_sem_valence</font>: %s paths_size=%d chart_size=%d dependency_tree_size=%d\n" (escape_html result.msg) result.paths_size result.chart_size result.dependency_tree_size ^
      (if verbosity = 0 then "" else
        sprintf "<BR><A HREF=\"%s_6b_dependency_tree.pdf\">Dependency Tree References 6b</A>\n" file_prefix ^
        (if result.dependency_tree7 <> [| |] then sprintf "<BR><A HREF=\"%s_7_dependency_tree.pdf\">Dependency Tree References 7</A>\n" file_prefix else "")  ^
        (if ExtArray.size result.dependency_tree8 <> 0 then sprintf "<BR><A HREF=\"%s_8_dependency_tree.pdf\">Dependency Tree References 8</A>\n" file_prefix else "")  ^
        (if result.dependency_tree9 <> [| |] then sprintf "<BR><A HREF=\"%s_9_dependency_tree.pdf\">Dependency Tree References 9</A>\n" file_prefix else "")  ^
        (if result.dependency_tree9 <> [| |] then sprintf "<BR><IMG SRC=\"%s_9_dependency_tree.png\">\n" file_prefix else "")  ^
        sprintf "<BR><IMG SRC=\"%s_6b_dependency_tree.png\">\n" file_prefix)  ^
      ""
  | SemGraphError ->
      if verbosity = 2 then (
        LCGlatexOf.print_dependency_tree path (file_prefix ^ "_6b_dependency_tree") "a3paper" result.dependency_tree6b;
        LCGgraphOf.print_dependency_tree path (file_prefix ^ "_6b_dependency_tree") result.dependency_tree6b;
        if result.dependency_tree7 <> [| |] then LCGlatexOf.print_dependency_tree path (file_prefix ^ "_7_dependency_tree") "a2paper" result.dependency_tree7;
        if ExtArray.size result.dependency_tree8 <> 0 then LCGlatexOf.print_references path (file_prefix ^ "_8_dependency_tree") "a3paper" result.dependency_tree8;
        if result.dependency_tree9 <> [| |] then LCGlatexOf.print_dependency_tree path (file_prefix ^ "_9_dependency_tree") "a3paper" result.dependency_tree9;
        if result.dependency_tree9 <> [| |] then LCGgraphOf.print_dependency_tree path (file_prefix ^ "_9_dependency_tree") result.dependency_tree9;
        if result.semantic_graph10 <> [| |] then SemLatexOf.print_semantic_graph path (file_prefix ^ "_10_semantic_graph") "a3paper" result.semantic_graph10;
        if result.semantic_graph11 <> SemTypes.Dot then SemGraphOf.print_semantic_graph2 path (file_prefix ^ "_11_semantic_graph") "" result.semantic_graph11);
      if verbosity = 1 then (
        if result.semantic_graph11 <> SemTypes.Dot then SemGraphOf.print_semantic_graph2 path (file_prefix ^ "_11_semantic_graph") "" result.semantic_graph11 else
        if result.semantic_graph10 <> [| |] then SemLatexOf.print_semantic_graph path (file_prefix ^ "_10_semantic_graph") "a3paper" result.semantic_graph10 else (
        LCGlatexOf.print_dependency_tree path (file_prefix ^ "_6b_dependency_tree") "a3paper" result.dependency_tree6b;
        LCGgraphOf.print_dependency_tree path (file_prefix ^ "_6b_dependency_tree") result.dependency_tree6b;
        if result.dependency_tree7 <> [| |] then LCGlatexOf.print_dependency_tree path (file_prefix ^ "_7_dependency_tree") "a2paper" result.dependency_tree7;
        if ExtArray.size result.dependency_tree8 <> 0 then LCGlatexOf.print_references path (file_prefix ^ "_8_dependency_tree") "a3paper" result.dependency_tree8;
        if result.dependency_tree9 <> [| |] then LCGlatexOf.print_dependency_tree path (file_prefix ^ "_9_dependency_tree") "a3paper" result.dependency_tree9;
        if result.dependency_tree9 <> [| |] then LCGgraphOf.print_dependency_tree path (file_prefix ^ "_9_dependency_tree") result.dependency_tree9));
      sprintf "<font color=\"red\">error_sem_graph</font>: %s paths_size=%d chart_size=%d dependency_tree_size=%d\n" (escape_html result.msg) result.paths_size result.chart_size result.dependency_tree_size ^
      (if verbosity = 2 then
        sprintf "<BR><A HREF=\"%s_6b_dependency_tree.pdf\">Dependency Tree References 6b</A>\n" file_prefix ^
        (if result.semantic_graph10 <> [| |] then sprintf "<BR><A HREF=\"%s_10_semantic_graph.pdf\">Semantic Graph References 10</A>\n" file_prefix else "") ^
        (if result.semantic_graph11 <> SemTypes.Dot then sprintf "<BR><IMG SRC=\"%s_11_semantic_graph.png\">\n" file_prefix else "")  ^
        (if result.dependency_tree7 <> [| |] then sprintf "<BR><A HREF=\"%s_7_dependency_tree.pdf\">Dependency Tree References 7</A>\n" file_prefix else "")  ^
        (if ExtArray.size result.dependency_tree8 <> 0 then sprintf "<BR><A HREF=\"%s_8_dependency_tree.pdf\">Dependency Tree References 8</A>\n" file_prefix else "")  ^
        (if result.dependency_tree9 <> [| |] then sprintf "<BR><A HREF=\"%s_9_dependency_tree.pdf\">Dependency Tree References 9</A>\n" file_prefix else "")  ^
        (if result.dependency_tree9 <> [| |] then sprintf "<BR><IMG SRC=\"%s_9_dependency_tree.png\">\n" file_prefix else "")  ^
        sprintf "<BR><IMG SRC=\"%s_6b_dependency_tree.png\">\n" file_prefix else
      if verbosity = 1 then (
        if result.semantic_graph11 <> SemTypes.Dot then sprintf "<BR><IMG SRC=\"%s_11_semantic_graph.png\">\n" file_prefix else
        if result.semantic_graph10 <> [| |] then sprintf "<BR><A HREF=\"%s_10_semantic_graph.pdf\">Semantic Graph References 10</A>\n" file_prefix else
        sprintf "<BR><A HREF=\"%s_6b_dependency_tree.pdf\">Dependency Tree References 6b</A>\n" file_prefix ^
        (if result.dependency_tree7 <> [| |] then sprintf "<BR><A HREF=\"%s_7_dependency_tree.pdf\">Dependency Tree References 7</A>\n" file_prefix else "")  ^
        (if ExtArray.size result.dependency_tree8 <> 0 then sprintf "<BR><A HREF=\"%s_8_dependency_tree.pdf\">Dependency Tree References 8</A>\n" file_prefix else "")  ^
        (if result.dependency_tree9 <> [| |] then sprintf "<BR><A HREF=\"%s_9_dependency_tree.pdf\">Dependency Tree References 9</A>\n" file_prefix else "")  ^
        (if result.dependency_tree9 <> [| |] then sprintf "<BR><IMG SRC=\"%s_9_dependency_tree.png\">\n" file_prefix else "")  ^
        sprintf "<BR><IMG SRC=\"%s_6b_dependency_tree.png\">\n" file_prefix) else "")  ^
      ""
  | SemGraphError2 ->
      if verbosity = 0 then () else (
        SemGraphOf.print_semantic_graph2 path (file_prefix ^ "_11_semantic_graph") "" result.semantic_graph11);
      sprintf "<font color=\"red\">error_sem_graph2</font>: %s paths_size=%d chart_size=%d dependency_tree_size=%d\n" (escape_html result.msg) result.paths_size result.chart_size result.dependency_tree_size ^
      (if verbosity = 0 then "" else
        sprintf "<BR><IMG SRC=\"%s_11_semantic_graph.png\">\n" file_prefix)  ^
      ""
  | SemNotValidated ->
(*       print_endline "html_of_eniam_sentence: SemNotValidated 1"; *)
      if verbosity < 2 then () else (
        LCGgraphOf.print_dependency_tree path (file_prefix ^ "_6b_dependency_tree") result.dependency_tree6b;
        LCGgraphOf.print_dependency_tree path (file_prefix ^ "_9_dependency_tree") result.dependency_tree9;
        LCGlatexOf.print_dependency_tree path (file_prefix ^ "_6b_dependency_tree") "a3paper" result.dependency_tree6b;
        LCGlatexOf.print_dependency_tree path (file_prefix ^ "_7_dependency_tree") "a2paper" result.dependency_tree7;
        LCGlatexOf.print_references path (file_prefix ^ "_8_dependency_tree") "a3paper" result.dependency_tree8;
        LCGlatexOf.print_dependency_tree path (file_prefix ^ "_9_dependency_tree") "a3paper" result.dependency_tree9;
        SemLatexOf.print_semantic_graph path (file_prefix ^ "_10_semantic_graph") "a3paper" result.semantic_graph10;
        SemGraphOf.print_semantic_graph2 path (file_prefix ^ "_11_semantic_graph") "" result.semantic_graph11);
      if verbosity = 0 then () else (
        SemGraphOf.print_semantic_graph2 path (file_prefix ^ "_12_semantic_graph") "" result.semantic_graph12;
        SemGraphOf.print_semantic_graph2 path (file_prefix ^ "_13_semantic_graph") "" result.semantic_graph13);
(*      print_endline ("html_of_eniam_sentence: SemNotValidated 2: ^ |result.msg|=" ^ string_of_int (Xstring.size result.msg));
      let s = escape_html result.msg in
      print_endline ("html_of_eniam_sentence: SemNotValidated 3: ^ |s|=" ^ string_of_int (Xstring.size s));*)
      let s = sprintf "<font color=\"red\">sem_not_validated</font>: %s paths_size=%d chart_size=%d dependency_tree_size=%d\n" (escape_html (get_prefix result.msg)) result.paths_size result.chart_size result.dependency_tree_size in
(*       print_endline "html_of_eniam_sentence: SemNotValidated 4"; *)
      s ^
      (if verbosity < 2 then "" else
        sprintf "<BR><A HREF=\"%s_6b_dependency_tree.pdf\">Dependency Tree References 6b</A>\n" file_prefix  ^
        sprintf "<BR><A HREF=\"%s_7_dependency_tree.pdf\">Dependency Tree References 7</A>\n" file_prefix  ^
        sprintf "<BR><A HREF=\"%s_8_dependency_tree.pdf\">Dependency Tree References 8</A>\n" file_prefix  ^
        sprintf "<BR><A HREF=\"%s_9_dependency_tree.pdf\">Dependency Tree References 9</A>\n" file_prefix  ^
        sprintf "<BR><IMG SRC=\"%s_6b_dependency_tree.png\">\n" file_prefix ^
        sprintf "<BR><IMG SRC=\"%s_9_dependency_tree.png\">\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_10_semantic_graph.pdf\">Semantic Graph References 10</A>\n" file_prefix  ^
        sprintf "<BR><IMG SRC=\"%s_11_semantic_graph.png\">\n" file_prefix)  ^
      (if verbosity = 0 then "" else
        sprintf "<BR><IMG SRC=\"%s_12_semantic_graph.png\">\n" file_prefix  ^
        sprintf "<BR><IMG SRC=\"%s_13_semantic_graph.png\">\n" file_prefix)  ^
      ""
  | SemParsed | PartialSemParsed ->
      if verbosity < 2 then () else (
        LCGgraphOf.print_dependency_tree path (file_prefix ^ "_6b_dependency_tree") result.dependency_tree6b;
        LCGgraphOf.print_dependency_tree path (file_prefix ^ "_9_dependency_tree") result.dependency_tree9;
        LCGlatexOf.print_dependency_tree path (file_prefix ^ "_6b_dependency_tree") "a3paper" result.dependency_tree6b;
        LCGlatexOf.print_dependency_tree path (file_prefix ^ "_7_dependency_tree") "a2paper" result.dependency_tree7;
        LCGlatexOf.print_references path (file_prefix ^ "_8_dependency_tree") "a3paper" result.dependency_tree8;
        LCGlatexOf.print_dependency_tree path (file_prefix ^ "_9_dependency_tree") "a3paper" result.dependency_tree9;
        SemLatexOf.print_semantic_graph path (file_prefix ^ "_10_semantic_graph") "a3paper" result.semantic_graph10;
        SemGraphOf.print_semantic_graph2 path (file_prefix ^ "_11_semantic_graph") "" result.semantic_graph11);
      if verbosity = 0 then () else (
        SemGraphOf.print_semantic_graph2 path (file_prefix ^ "_12_semantic_graph") "" result.semantic_graph12);
      sprintf "%s: paths_size=%d chart_size=%d dependency_tree_size=%d\n" 
        (if result.status = SemParsed then "sem_parsed" else "partial_sem_parsed") result.paths_size result.chart_size result.dependency_tree_size ^
      (if verbosity < 2 then "" else
        sprintf "<BR><A HREF=\"%s_6b_dependency_tree.pdf\">Dependency Tree References 6b</A>\n" file_prefix  ^
        sprintf "<BR><A HREF=\"%s_7_dependency_tree.pdf\">Dependency Tree References 7</A>\n" file_prefix  ^
        sprintf "<BR><A HREF=\"%s_8_dependency_tree.pdf\">Dependency Tree References 8</A>\n" file_prefix  ^
        sprintf "<BR><A HREF=\"%s_9_dependency_tree.pdf\">Dependency Tree References 9</A>\n" file_prefix  ^
        sprintf "<BR><IMG SRC=\"%s_6b_dependency_tree.png\">\n" file_prefix ^
        sprintf "<BR><IMG SRC=\"%s_9_dependency_tree.png\">\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_10_semantic_graph.pdf\">Semantic Graph References 10</A>\n" file_prefix  ^
        sprintf "<BR><IMG SRC=\"%s_11_semantic_graph.png\">\n" file_prefix)  ^
      (if verbosity = 0 then "" else
        sprintf "<BR><IMG SRC=\"%s_12_semantic_graph.png\">\n" file_prefix)  ^
      ""
  | InferenceError ->
      if verbosity < 2 then () else (
        LCGgraphOf.print_dependency_tree path (file_prefix ^ "_6b_dependency_tree") result.dependency_tree6b;
        LCGgraphOf.print_dependency_tree path (file_prefix ^ "_9_dependency_tree") result.dependency_tree9;
        LCGlatexOf.print_dependency_tree path (file_prefix ^ "_6b_dependency_tree") "a3paper" result.dependency_tree6b;
        LCGlatexOf.print_dependency_tree path (file_prefix ^ "_7_dependency_tree") "a2paper" result.dependency_tree7;
        LCGlatexOf.print_references path (file_prefix ^ "_8_dependency_tree") "a3paper" result.dependency_tree8;
        LCGlatexOf.print_dependency_tree path (file_prefix ^ "_9_dependency_tree") "a3paper" result.dependency_tree9;
        SemLatexOf.print_semantic_graph path (file_prefix ^ "_10_semantic_graph") "a3paper" result.semantic_graph10;
        SemGraphOf.print_semantic_graph2 path (file_prefix ^ "_11_semantic_graph") "" result.semantic_graph11);
      if verbosity = 0 then () else (
        SemGraphOf.print_semantic_graph2 path (file_prefix ^ "_12_semantic_graph") "" result.semantic_graph12);
      sprintf "<font color=\"red\">inference_error</font>: %s paths_size=%d chart_size=%d dependency_tree_size=%d\n" (escape_html result.msg) result.paths_size result.chart_size result.dependency_tree_size ^
      (if verbosity < 2 then "" else
        sprintf "<BR><A HREF=\"%s_6b_dependency_tree.pdf\">Dependency Tree References 6b</A>\n" file_prefix  ^
        sprintf "<BR><A HREF=\"%s_7_dependency_tree.pdf\">Dependency Tree References 7</A>\n" file_prefix  ^
        sprintf "<BR><A HREF=\"%s_8_dependency_tree.pdf\">Dependency Tree References 8</A>\n" file_prefix  ^
        sprintf "<BR><A HREF=\"%s_9_dependency_tree.pdf\">Dependency Tree References 9</A>\n" file_prefix  ^
        sprintf "<BR><IMG SRC=\"%s_6b_dependency_tree.png\">\n" file_prefix ^
        sprintf "<BR><IMG SRC=\"%s_9_dependency_tree.png\">\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_10_semantic_graph.pdf\">Semantic Graph References 10</A>\n" file_prefix  ^
        sprintf "<BR><IMG SRC=\"%s_11_semantic_graph.png\">\n" file_prefix)  ^
      (if verbosity = 0 then "" else
        sprintf "<BR><IMG SRC=\"%s_12_semantic_graph.png\">\n" file_prefix)  ^
      ""
  | Inferenced ->
      if verbosity < 2 then () else (
        LCGgraphOf.print_dependency_tree path (file_prefix ^ "_6b_dependency_tree") result.dependency_tree6b;
        LCGgraphOf.print_dependency_tree path (file_prefix ^ "_9_dependency_tree") result.dependency_tree9;
        LCGlatexOf.print_dependency_tree path (file_prefix ^ "_6b_dependency_tree") "a3paper" result.dependency_tree6b;
        LCGlatexOf.print_dependency_tree path (file_prefix ^ "_7_dependency_tree") "a2paper" result.dependency_tree7;
        LCGlatexOf.print_references path (file_prefix ^ "_8_dependency_tree") "a3paper" result.dependency_tree8;
        LCGlatexOf.print_dependency_tree path (file_prefix ^ "_9_dependency_tree") "a3paper" result.dependency_tree9;
        SemLatexOf.print_semantic_graph path (file_prefix ^ "_10_semantic_graph") "a3paper" result.semantic_graph10;
        SemGraphOf.print_semantic_graph2 path (file_prefix ^ "_11_semantic_graph") "" result.semantic_graph11);
      if verbosity = 0 then () else (
        SemGraphOf.print_semantic_graph2 path (file_prefix ^ "_12_semantic_graph") "" result.semantic_graph12;
        SemGraphOf.print_semantic_graph2 path (file_prefix ^ "_13_semantic_graph") "" result.semantic_graph13);
      sprintf "inferenced: paths_size=%d chart_size=%d dependency_tree_size=%d\n" result.paths_size result.chart_size result.dependency_tree_size ^
      (if verbosity < 2 then "" else
        sprintf "<BR><A HREF=\"%s_6b_dependency_tree.pdf\">Dependency Tree References 6b</A>\n" file_prefix  ^
        sprintf "<BR><A HREF=\"%s_7_dependency_tree.pdf\">Dependency Tree References 7</A>\n" file_prefix  ^
        sprintf "<BR><A HREF=\"%s_8_dependency_tree.pdf\">Dependency Tree References 8</A>\n" file_prefix  ^
        sprintf "<BR><A HREF=\"%s_9_dependency_tree.pdf\">Dependency Tree References 9</A>\n" file_prefix  ^
        sprintf "<BR><IMG SRC=\"%s_6b_dependency_tree.png\">\n" file_prefix ^
        sprintf "<BR><IMG SRC=\"%s_9_dependency_tree.png\">\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_10_semantic_graph.pdf\">Semantic Graph References 10</A>\n" file_prefix  ^
        sprintf "<BR><IMG SRC=\"%s_11_semantic_graph.png\">\n" file_prefix)  ^
      (if verbosity = 0 then "" else
        sprintf "<BR><IMG SRC=\"%s_12_semantic_graph.png\">\n" file_prefix  ^
        sprintf "<BR><IMG SRC=\"%s_13_semantic_graph.png\">\n" file_prefix)  ^
      ""
  | _ -> failwith "html_of_eniam_sentence"


let file_prefix_of_mode = function
    Raw -> "R"
  | Struct -> "St"
  | CONLL -> "C"
  | ENIAM -> "E"
  | Mate -> "M"
  | Swigra -> "S"
  | POLFIE -> "P"
  | Error -> "Er"
  | Name -> "N"
  | Identifier -> "I"

let rec html_of_sentence path file_prefix mode img verbosity tokens = function
    RawSentence s -> escape_html s
  | StructSentence(paths,last) -> SubsyntaxHTMLof.html_of_struct_sentence tokens paths last
  | DepSentence paths -> String.concat "<BR>\n" (Xlist.map paths (SubsyntaxHTMLof.html_of_dep_sentence tokens))
  | ENIAMSentence result ->
(*     Int.iter 0 (Array.length result.dependency_tree6a - 1) (fun i -> print_endline ("\n6a " ^ (string_of_int i) ^ ":\n" ^ (LCGstringOf.linear_term 0 result.dependency_tree6a.(i))));
     Int.iter 0 (Array.length result.dependency_tree6b - 1) (fun i -> print_endline ("\n6b " ^ (string_of_int i) ^ ":\n" ^ (LCGstringOf.linear_term 0 result.dependency_tree6b.(i))));*)
     let file_prefix = file_prefix_of_mode mode ^ file_prefix in
(*      print_endline "html_of_sentence 1"; *)
     let s = html_of_eniam_sentence path file_prefix img verbosity tokens result in
(*      print_endline "html_of_sentence 2"; *)
     s
  (* | CONLLSentence result -> html_of_conll_sentence path img verbosity tokens result
  | SemSentence result -> html_of_sem_sentence path img verbosity tokens result *)
  | QuotedSentences sentences ->
      String.concat "<BR>\n" (Xlist.map sentences (fun p ->
        sprintf "id=%s beg=%d len=%d next=%d<BR>%s" p.id p.beg p.len p.next (html_of_sentence path p.file_prefix mode img verbosity tokens p.sentence)))
  | AltSentence l -> (*print_endline "AltSentence";*)
     "<table border=1>" ^
     String.concat "\n" (Xlist.map l (fun (mode,sentence) ->
       sprintf "<tr><td>%s</td><td>%s</td></tr>" (string_of_mode mode) (html_of_sentence path file_prefix mode img verbosity tokens sentence))) ^
     "</table>"
  (* | _ -> failwith "html_of_sentence: ni" *)
  | ErrorSentence s -> sprintf "<font color=\"red\">sentence_error</font>: %s\n" (escape_html s)

let rec html_of_paragraph path mode img verbosity tokens = function
    RawParagraph s -> (*print_endline "RawParagraph";*) escape_html s
  | StructParagraph(s,sentences) -> (*print_endline "StructParagraph";*)
      Printf.sprintf "percentage of not recognized %d/%d=%f tokens %d/%d=%f characters<BR>\n" s.t_len_nann s.t_len ((float s.t_len_nann)/.float s.t_len) s.c_len_nann s.c_len ((float s.c_len_nann)/.float s.c_len) ^
      Printf.sprintf "percentage of not parsed %d/%d=%f tokens %d/%d=%f characters<BR>\n" s.t_len2_nann s.t_len2 ((float s.t_len2_nann)/.float s.t_len2) s.c_len2_nann s.c_len2 ((float s.c_len2_nann)/.float s.c_len2) ^
      String.concat "<BR>\n" (Xlist.map sentences (fun p ->
        sprintf "id=%s beg=%d len=%d next=%d<BR>%s" p.id p.beg p.len p.next (html_of_sentence path p.file_prefix mode img verbosity tokens p.sentence)))
  | AltParagraph l -> (*print_endline "AltParagraph";*)
     "<table border=2>" ^
     String.concat "\n" (Xlist.map l (fun (mode,paragraph) ->
       sprintf "<tr><td>%s</td><td>%s</td></tr>" (string_of_mode mode) (html_of_paragraph path mode img verbosity tokens paragraph))) ^
     "</table>"
  | ErrorParagraph s -> sprintf "<font color=\"red\">paragraph_error</font>: %s\n" (escape_html s)

let rec html_of_text path mode img verbosity tokens = function
    RawText s -> escape_html s
  | StructText paragraphs ->
      String.concat "<BR>\n" (Xlist.map paragraphs (html_of_paragraph path mode img verbosity tokens))
  | JSONtext s -> "<pre>" ^ escape_html s ^ "</pre><BR>"
  | AltText l ->
     "<table border=3>" ^
     String.concat "\n" (Xlist.map l (fun (mode,text) ->
       sprintf "<tr><td>%s</td><td>%s</td></tr>" (string_of_mode mode) (html_of_text path mode img verbosity tokens text))) ^
     "</table>"
  | ErrorText s -> sprintf "<font color=\"red\">text_error</font>: %s\n" (escape_html s)

let html_of_text_as_paragraph path mode img verbosity tokens = function
    AltText[Raw,_;Struct,StructText[paragraph]] -> html_of_paragraph path mode img verbosity tokens paragraph
  | text -> html_of_text path mode img verbosity tokens text

let print_html_text path name text img verbosity tokens (*lex_sems*) =
  File.file_out (path ^ name ^ ".html") (fun file ->
    fprintf file "%s\n" html_header;
    fprintf file "%s<BR>\n" (html_of_text path Struct img verbosity tokens text);
    (* fprintf file "%s<BR>\n" (html_of_tokens tokens); *)
(*    fprintf file "%s<BR>\n" (html_of_tokens_simple_valence tokens);
    fprintf file "%s<BR>\n" (html_of_tokens_valence tokens);*)
    fprintf file "%s\n" html_trailer)

let rec find_prev_next_sentence pid file_prefix rev = function
    AltSentence[Raw,_;Struct,QuotedSentences sentences] ->
      Xlist.fold sentences rev (fun rev p -> find_prev_next_sentence p.id p.file_prefix rev p.sentence)
  | AltSentence[Raw,RawSentence s; mode,ENIAMSentence result] -> file_prefix :: rev
  | AltSentence((Raw,RawSentence s) :: _) -> file_prefix :: rev
  (* | AltSentence[Raw,RawSentence s] -> ("p" ^ pid) :: rev *)
  | _ -> failwith "find_prev_next_sentence: ni"

let rec find_prev_next_paragraph rev = function
    RawParagraph s -> rev
  | StructParagraph(_,sentences) ->
      Xlist.fold sentences rev (fun rev p -> find_prev_next_sentence p.id p.file_prefix rev p.sentence)
  | AltParagraph l -> Xlist.fold l rev (fun rev (mode,paragraph) -> find_prev_next_paragraph rev paragraph)
  | ErrorParagraph s -> rev

let rec make_prev_next_map map prev = function
    [x] -> StringMap.add map x (prev,"")
  | x :: next :: l -> make_prev_next_map (StringMap.add map x (prev,next)) x (next :: l)
  | [] -> failwith "make_prev_next_map"

let print_main_result results_web_path mode path id file_prefix (result : eniam_parse_result) file =
  if mode = ENIAM then () else
  begin
    fprintf file "<h4>Parsed by %s\n</h4>" (string_of_mode mode);
    if result.status = Parsed (* else = SemParsed *)
    then LCGgraphOf.print_simplified_dependency_tree path ("tree" ^ id ^ "_" ^ (string_of_mode mode) ^ "_" ^ file_prefix) result.dependency_tree6a
    else SemGraphOf.print_semantic_graph2 path ("tree" ^ id ^ "_" ^ (string_of_mode mode) ^ "_" ^ file_prefix) "" result.semantic_graph11;
    fprintf file "<P><IMG SRC=\"%stree%s_%s_%s.png\">\n" results_web_path id (string_of_mode mode) file_prefix
  end

let print_not_parsed_main_result mode file =
(*  fprintf file "<h4>Parsing by %s</h4>\n" (string_of_mode mode);
  fprintf file "<p>Not parsed\n";
*)  ()

let print_sentence_to_file path cg_bin_path results_web_path title id file_prefix prev_next_map query sentences file =
  let print_local file = (function
      mode,ENIAMSentence result ->
        if result.status = Parsed || result.status = SemParsed || result.status = PartialParsed || result.status = PartialSemParsed
        then print_main_result results_web_path mode path id file_prefix result file
        else print_not_parsed_main_result mode file
    | _ -> failwith "print_sentence_to_file: not_eniam_sentence") in
  let prev,next = (try StringMap.find prev_next_map file_prefix with Not_found -> failwith "print_sentence_to_file: prev_next") in
  fprintf file "%s\n" (page_header cg_bin_path);
  if prev <> "" then fprintf file "<A HREF=\"%spage%s_%s.html\">Poprzednie zdanie</A> " results_web_path id prev;
  if next <> "" then fprintf file " <A HREF=\"%spage%s_%s.html\">Następne zdanie</A>" results_web_path id next;
  if title then fprintf file "\n<h2>%s</h2>\n" query;

  Xlist.iter sentences (print_local file);

  fprintf file "<p>";
  if prev <> "" then fprintf file "<A HREF=\"%spage%s_%s.html\">Poprzednie zdanie</A><br>" results_web_path id prev;
  if next <> "" then fprintf file " <A HREF=\"%spage%s_%s.html\">Następne zdanie</A><br>" results_web_path id next;
(*  fprintf file "<a target=\"_blank\" href=\"%sparsed_text_%s.html\">Struktura całości</a><br>" results_web_path id;
*)  fprintf file "<a target=\"_blank\" href=\"%spage%s_%s.html\">Statyczny link</a><br>" results_web_path id file_prefix;
  fprintf file "%s\n" page_trailer

let rec print_main_result_sentence path cg_bin_path results_web_path id file_prefix tokens pid prev_next_map = function
    AltSentence[Raw,_;Struct,QuotedSentences sentences] ->
      Xlist.iter sentences (fun p -> print_main_result_sentence path cg_bin_path results_web_path id p.file_prefix tokens p.id prev_next_map p.sentence)
  | AltSentence((Raw,RawSentence query) :: sentences) ->
      File.file_out (path ^ "page" ^ id ^ "_" ^ file_prefix ^ ".html") (fun file ->
        print_sentence_to_file path cg_bin_path results_web_path true id file_prefix prev_next_map query sentences file)
  | _ -> failwith "print_main_result_sentence: ni"

let rec print_main_result_paragraph path cg_bin_path results_web_path id tokens prev_next_map = function
    RawParagraph s -> ()
  | StructParagraph(_,sentences) ->
      Xlist.iter sentences (fun p -> print_main_result_sentence path cg_bin_path results_web_path id p.file_prefix tokens p.id prev_next_map p.sentence)
  | AltParagraph l -> Xlist.iter l (fun (mode,paragraph) -> print_main_result_paragraph path cg_bin_path results_web_path id tokens prev_next_map paragraph)
  | ErrorParagraph s -> File.file_out (path ^ "page" ^ id ^ "_Er.html") (fun file ->
      print_sentence_to_file path cg_bin_path results_web_path false id "Er" prev_next_map ("ErrorParagraph: " ^ s) [] file)

let rec print_main_result_text path cg_bin_path results_web_path id tokens = function
    RawText s -> ()
  | StructText paragraphs ->
      let prev_next_map = make_prev_next_map StringMap.empty ""
        (List.rev (Xlist.fold paragraphs [] find_prev_next_paragraph)) in
      Xlist.iter paragraphs (print_main_result_paragraph path cg_bin_path results_web_path id tokens prev_next_map)
  | JSONtext s -> () (*"<pre>" ^ escape_html s ^ "</pre><BR>"*) (* FIXME *)
  | AltText l -> Xlist.iter l (fun (mode,text) -> print_main_result_text path cg_bin_path results_web_path id tokens text)
  | ErrorText s -> failwith "print_main_result_text: ni"

let rec print_main_result_first_page_sentence path cg_bin_path results_web_path id file_prefix tokens pid prev_next_map = function
    AltSentence[Raw,_;Struct,QuotedSentences sentences] ->
      let p = List.hd sentences in
      print_main_result_first_page_sentence path cg_bin_path results_web_path id p.file_prefix tokens p.id prev_next_map p.sentence
  | AltSentence((Raw,RawSentence query) :: sentences) ->
      print_sentence_to_file path cg_bin_path results_web_path false id file_prefix prev_next_map query sentences stdout
  | _ -> failwith "print_main_result_first_page_sentence: ni"

let rec print_main_result_first_page_paragraph path cg_bin_path results_web_path id tokens prev_next_map = function
    RawParagraph s -> ()
  | StructParagraph(_,sentences) ->
      let p = List.hd sentences in
      print_main_result_first_page_sentence path cg_bin_path results_web_path id p.file_prefix tokens p.id prev_next_map p.sentence
  | AltParagraph l -> Xlist.iter l (fun (mode,paragraph) -> print_main_result_first_page_paragraph path cg_bin_path results_web_path id tokens prev_next_map paragraph)
  | ErrorParagraph s -> print_sentence_to_file path cg_bin_path results_web_path false id "Er" prev_next_map ("ErrorParagraph: " ^ s) [] stdout

let rec print_main_result_first_page_text path cg_bin_path results_web_path id tokens = function
    RawText s -> ()
  | StructText paragraphs ->
      let prev_next_map = make_prev_next_map StringMap.empty ""
        (List.rev (Xlist.fold paragraphs [] find_prev_next_paragraph)) in
      print_main_result_first_page_paragraph path cg_bin_path results_web_path id tokens prev_next_map (List.hd paragraphs)
  | JSONtext s -> () (*"<pre>" ^ escape_html s ^ "</pre><BR>"*) (* FIXME *)
  | AltText l -> Xlist.iter l (fun (mode,text) -> print_main_result_first_page_text path cg_bin_path results_web_path id tokens text)
  | ErrorText s -> failwith "print_main_result_first_page_text: ni"

let to_string_eniam_sentence verbosity tokens (result : eniam_parse_result) =
  let status_string = string_of_status result.status in
  match result.status with
  | NotParsed -> [status_string ^ ": " ^ cat_tokens_sequence result.par_string result.node_mapping (LCGchart.select_maximal result.chart1)]
  | SemNotValidated -> [status_string; get_prefix result.msg; "";
        SemStringOf.linear_term_formatted "" result.semantic_graph12;"";
        SemStringOf.linear_term_formatted "" result.semantic_graph13]
  | SemParsed | PartialSemParsed -> [status_string;"";
        SemStringOf.linear_term_formatted "" result.semantic_graph12]
  | Inferenced -> [status_string;"";
        SemStringOf.linear_term_formatted "" result.semantic_graph12;"";
        SemStringOf.linear_term_formatted "" result.semantic_graph13]
  | _ -> [status_string]

let rec to_string_sentence verbosity tokens = function
    RawSentence s -> []
  | StructSentence(paths,last) -> []
  | DepSentence paths -> []
  | ENIAMSentence result -> to_string_eniam_sentence verbosity tokens result
  | QuotedSentences sentences -> List.flatten (Xlist.map sentences (fun p -> to_string_sentence verbosity tokens p.sentence))
  | AltSentence l -> List.flatten (Xlist.map l (fun (mode,sentence) -> to_string_sentence verbosity tokens sentence))
  | ErrorSentence s -> []

let rec to_string_paragraph verbosity tokens = function
    RawParagraph s -> []
  | StructParagraph(_,sentences) -> List.flatten (Xlist.map sentences (fun p -> to_string_sentence verbosity tokens p.sentence))
  | AltParagraph l -> List.flatten (Xlist.map l (fun (mode,paragraph) -> to_string_paragraph verbosity tokens paragraph))
  | ErrorParagraph s -> ["SubsyntaxError"]

let rec to_string_text verbosity tokens = function
    RawText s -> []
  | StructText paragraphs -> List.flatten (Xlist.map paragraphs (to_string_paragraph verbosity tokens))
  | JSONtext s -> [s]
  | AltText l -> List.flatten (Xlist.map l (fun (mode,text) -> to_string_text verbosity tokens text))
  | ErrorText s -> ["ErrorText"]

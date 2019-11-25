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
open ExecTypes
open Xstd

let string_of_exn = function
    Failure s -> Scanf.unescaped s
  | e -> Printexc.to_string e

let translate_mode = function
    SubsyntaxTypes.Raw -> Raw
  | SubsyntaxTypes.Struct -> Struct
  | SubsyntaxTypes.CONLL -> CONLL
  | SubsyntaxTypes.ENIAM -> ENIAM
  | SubsyntaxTypes.Mate -> Mate
  | SubsyntaxTypes.Swigra -> Swigra
  | SubsyntaxTypes.POLFIE -> POLFIE
  | SubsyntaxTypes.Error -> Error
  | SubsyntaxTypes.Name -> Name
  | SubsyntaxTypes.Identifier -> Identifier

let rec translate_sentence = function
    SubsyntaxTypes.RawSentence s -> RawSentence s
  | SubsyntaxTypes.StructSentence(paths,last) -> StructSentence(paths,last)
  | SubsyntaxTypes.DepSentence(paths) -> DepSentence(paths)
  | SubsyntaxTypes.QuotedSentences sentences ->
      QuotedSentences(Xlist.map sentences (fun p ->
        {id=p.SubsyntaxTypes.sid; beg=p.SubsyntaxTypes.sbeg; len=p.SubsyntaxTypes.slen; next=p.SubsyntaxTypes.snext; file_prefix=p.SubsyntaxTypes.file_prefix;
         no_tokens=p.SubsyntaxTypes.no_tokens; sentence=translate_sentence p.SubsyntaxTypes.sentence}))
  | SubsyntaxTypes.AltSentence l -> AltSentence(Xlist.map l (fun (mode,sentence) ->
      translate_mode mode, translate_sentence sentence))
  | SubsyntaxTypes.ErrorSentence s -> ErrorSentence s

let rec translate_paragraph = function
    SubsyntaxTypes.RawParagraph s -> RawParagraph s
  | SubsyntaxTypes.StructParagraph((t_len,t_len_nann,c_len,c_len_nann),sentences) ->
      let stats = {c_len=c_len; c_len_nann=c_len_nann; t_len=t_len; t_len_nann=t_len_nann; c_len2=0; c_len2_nann=0; t_len2=0; t_len2_nann=0} in
      StructParagraph(stats,Xlist.map sentences (fun p ->
        {id=p.SubsyntaxTypes.sid; beg=p.SubsyntaxTypes.sbeg; len=p.SubsyntaxTypes.slen; next=p.SubsyntaxTypes.snext; file_prefix=p.SubsyntaxTypes.file_prefix;
         no_tokens=p.SubsyntaxTypes.no_tokens; sentence=translate_sentence p.SubsyntaxTypes.sentence}))
  | SubsyntaxTypes.AltParagraph l -> AltParagraph(Xlist.map l (fun (mode,paragraph) ->
      translate_mode mode, translate_paragraph paragraph))
  | SubsyntaxTypes.ErrorParagraph s -> ErrorParagraph s

let rec translate_text = function
    SubsyntaxTypes.RawText s -> RawText s
  | SubsyntaxTypes.StructText paragraphs ->
      StructText(Xlist.map paragraphs translate_paragraph)
  | SubsyntaxTypes.AltText l -> AltText(Xlist.map l (fun (mode,text) ->
      translate_mode mode, translate_text text))
  | SubsyntaxTypes.ErrorText s -> ErrorText s

let clarify_categories cats (*snode*) token =
  match token.SubsyntaxTypes.token with
    SubsyntaxTypes.Lemma(lemma,pos,interp,cat2) ->
      List.flatten (Xlist.map interp (fun interp -> List.flatten (Xlist.map cats (fun (cat,coerced) ->
        (* Printf.printf "lemma=%s pos=%s cat=%s coerced=%s\n%!" lemma pos cat (String.concat "," coerced); *)
        if cat <> cat2 then [] else
        CategoriesPL.clarify_categories false cat coerced (*snode*) (lemma,pos,interp)))))
  | _ -> []

let create_chart rules tokens lex_sems paths last max_cost =
  LCGrenderer.reset_variable_numbers ();
  let chart = LCGchart.make last max_cost in
  let chart = Xlist.fold paths chart (fun chart (id,lnode,rnode) ->
      let t = ExtArray.get tokens id in
      let s = ExtArray.get lex_sems id in
(*       print_endline ("create_chart: orth=" ^ t.SubsyntaxTypes.orth ^ " lemma=" ^ Tokenizer.get_lemma t.SubsyntaxTypes.token ^ " |schemata|=" ^ string_of_int (Xlist.size s.LexSemanticsTypes.schemata)); *)
      LCGrenderer.reset_variable_names ();
      LCGrenderer.add_variable_numbers ();
      (* if s.LexSemanticsTypes.schemata = [] then failwith ("create_chart: no schema for token=" ^ t.SubsyntaxTypes.orth ^ " lemma=" ^ Tokenizer.get_lemma t.SubsyntaxTypes.token) else *)
      Xlist.fold s.LexSemanticsTypes.schemata chart (fun chart (selectors,cats,(*snode,*)local_schema,schema,distant_schema) ->
        let cats = clarify_categories cats (*snode*) t in
      (* let chart = LCGchart.add_inc_list chart lnode rnode s.LexSemanticsTypes.lex_entries 0 in *)
        let l = LCGlexicon.create_entries rules id t t.SubsyntaxTypes.orth cats [selectors,local_schema,schema,distant_schema] s.LexSemanticsTypes.lex_entries in
        Xlist.fold l chart (fun chart (v,cost) ->
          LCGchart.add_inc chart lnode rnode cost v 0))) in
  chart

let rec split_sons left id right = function
    [] -> List.rev (List.sort compare left), List.sort compare right
  | x :: l -> if x < id then split_sons (x :: left) id right l else split_sons left id (x :: right) l

let rec dep_create_rec nodes sons conll_id =
  let node = try IntMap.find nodes conll_id with Not_found -> failwith ("dep_create_rec: unknown node " ^ string_of_int conll_id) in
  let l = try IntMap.find sons conll_id with Not_found -> [] in
  let left,right = split_sons [] conll_id [] l in
  (* Printf.printf "dep_create_rec [%s] %d [%s]\n" (String.concat ";" (Xlist.map left string_of_int)) conll_id (String.concat ";" (Xlist.map right string_of_int)); *)
  DepNode(conll_id, Xlist.map left (dep_create_rec nodes sons), node, Xlist.map right (dep_create_rec nodes sons))

let create_dep_chart dep_rules tokens lex_sems paths =
  print_endline "create_dep_chart 1";
  let sons = Int.fold 1 (Array.length paths - 1) IntMap.empty (fun sons i ->
      let _,sl,_ = paths.(i) in
      let super,_ = List.hd sl in (* FIXME: to trzeba poprawić przy współdzielonych podrzędnikach *)
      IntMap.add_inc sons super [i] (fun l -> i :: l)) in
  print_endline "create_dep_chart 2";
  let nodes = Int.fold 0 (Array.length paths - 1) IntMap.empty (fun nodes i ->
      let id,_,_ = paths.(i) in
      let t = ExtArray.get tokens id in
      let s = ExtArray.get lex_sems id in
      LCGrenderer.reset_variable_names ();
      LCGrenderer.add_variable_numbers ();
      if s.LexSemanticsTypes.schemata = [] then failwith ("create_dep_chart: no schema for token=" ^ t.SubsyntaxTypes.orth ^ " lemma=" ^ Tokenizer.get_lemma t.SubsyntaxTypes.token) else
      Xlist.fold s.LexSemanticsTypes.schemata nodes (fun nodes (selectors,cats,(*snode,*)local_schema,schema,distant_schema) ->
        let cats = clarify_categories ["X",["X"]] (*snode*) t in
      (* let chart = LCGchart.add_inc_list chart lnode rnode s.LexSemanticsTypes.lex_entries 0 in *)
        let l = LCGlexicon.create_entries dep_rules id t t.SubsyntaxTypes.orth cats [selectors,local_schema,schema,distant_schema] s.LexSemanticsTypes.lex_entries in
        let l = Xlist.rev_map l fst in
        IntMap.add_inc nodes i l (fun l2 -> l @ l2))) in
  print_endline "create_dep_chart 3";
  let x = dep_create_rec nodes sons 0 in
  print_endline "create_dep_chart 4";
  x

(**let create_text_fragments tokens paths last =
  try 
  let text_fragments = Array.make last IntMap.empty in
  Xlist.iter paths (fun (id,lnode,rnode) ->
    let t = ExtArray.get tokens id in
    let orth = if t.SubsyntaxTypes.beg + t.SubsyntaxTypes.len = t.SubsyntaxTypes.next
      then t.SubsyntaxTypes.orth else t.SubsyntaxTypes.orth ^ " " in
    text_fragments.(lnode) <- IntMap.add text_fragments.(lnode) rnode orth);
  Int.iter_down 0 (last - 1) (fun i ->
    let map = IntMap.fold text_fragments.(i) text_fragments.(i) (fun map j orth ->
      if j = last then map else
      IntMap.fold text_fragments.(j) map (fun map k orth2 ->
        IntMap.add map k (orth ^ orth2))) in
    text_fragments.(i) <- map);
  text_fragments
  with e -> print_endline (Printexc.to_string e); failwith "create_text_fragments"**)

(*let create_beg_positions tokens paths last =
  let beg_positions = Array.make last (-1) in
  Xlist.iter paths (fun (id,lnode,rnode) ->
    let t = ExtArray.get tokens id in
    beg_positions.(lnode) <- t.SubsyntaxTypes.beg);
  beg_positions

let create_end_positions tokens paths last =
  let end_positions = Array.make last (-1) in
  Xlist.iter paths (fun (id,lnode,rnode) ->
    let t = ExtArray.get tokens id in
    end_positions.(rnode) <- t.SubsyntaxTypes.beg + t.SubsyntaxTypes.len);
  end_positions*)

let eniam_parse_sentence timeout verbosity rules tokens lex_sems paths last par_string max_cost =
  LCGreductions.reset_variant_label ();
  let result = {empty_eniam_parse_result with paths_size = Xlist.size paths} in
  let result = if verbosity = 0 && not !partial_parsing_flag then result else {result with
    par_string=par_string;
    node_mapping=MarkedHTMLof.create_node_mapping par_string tokens paths} in
(**  let result = if verbosity = 0 then result else {result with
    text_fragments=create_text_fragments tokens paths last;
    (*beg_positions=create_beg_positions tokens paths last;
    end_positions=create_end_positions tokens paths last;*)} in**)
(*   if not (Subsyntax.is_parsed tokens paths last) then {result with status=NotLemmatized} else *)
  let time1 = time_fun () in
  try
(*     print_endline "eniam_parse_sentence 1"; *)
    let chart = create_chart rules tokens lex_sems paths last max_cost in
    let result = if verbosity = 0 then result else {result with chart1=chart} in
(*     print_endline "eniam_parse_sentence 2"; *)
    let chart,references = LCGchart.lazify chart in
    let chart = LCGchart.add_pros !pro_rules chart references in
    let result = if verbosity = 0 then result else {result with chart2=LCGchart.copy chart; references2=ExtArray.copy references} in
(*     print_endline "eniam_parse_sentence 3"; *)
    let time2 = time_fun () in
    (* Printf.printf "time2-time1=%f\n%!" (time2 -. time1); *)
    let result = {result with lex_time=time2 -. time1} in
    try
(*       print_endline "eniam_parse_sentence 4"; *)
      let chart = LCGchart.parse !lcg_rules !pro_rules chart references timeout time_fun in (* uwaga: niejawna zmiana imperatywna w references *)
      let time3 = time_fun () in
(*       Printf.printf "time3-time2=%f\n%!" (time3 -. time2); *)
      let result = if verbosity = 0 then result else {result with chart3=chart; references3=ExtArray.copy references} in
      let result = {result with parse_time=time3 -. time2; chart_size=LCGchart.get_no_entries chart} in
(*     print_endline "eniam_parse_sentence 4a"; *)
      let (chart,merge_edges),partial = if !partial_parsing_flag && not (LCGchart.is_parsed chart) then LCGchart.merge result.par_string result.node_mapping chart references,true else (chart,0),false in
(*     print_endline "eniam_parse_sentence 4b"; *)
      if LCGchart.is_parsed chart then
        try
    (* print_endline "eniam_parse_sentence 5"; *)
          let term = LCGchart.get_parsed_term chart in
          let result = if verbosity = 0 then result else {result with term4=term} in
          let dependency_tree = LCGreductions.reduce term references in
          let time4 = time_fun () in
          (* Printf.printf "time4-time3=%f\n%!" (time4 -. time3); *)
          let result = if verbosity = 0 then result else {result with dependency_tree4=Array.copy dependency_tree} in
          let result = {result with reduction_time=time4 -. time3; dependency_tree_size=Array.length dependency_tree} in
              (* print_endline "a 0"; *)
          if LCGreductions.is_reduced_dependency_tree dependency_tree then
            try
    (* print_endline "eniam_parse_sentence 6"; *)
              (* print_endline "a 1"; *)
              LCGreductions.assign_labels dependency_tree; (* uwaga: niejawna zmiana imperatywna w result *)
              let result = if verbosity = 0 then result else {result with dependency_tree5=Array.copy dependency_tree} in
              (* print_endline "a 2"; *)
              LCGreductions.remove_cuts dependency_tree; (* uwaga: niejawna zmiana imperatywna w result *)
              let result = if verbosity = 0 then result else {result with dependency_tree6a=dependency_tree} in
              (* print_endline "a 3"; *)
              (* let dependency_tree = LCGreductions.normalize_variants dependency_tree in *) (* FIXME: trzeba zreimplementować obsługę wielokrotnych etykiet *)
              (* print_endline "a 4"; *)
              LexSemantics.create_tokens_for_artificial_nodes tokens lex_sems dependency_tree;
              let result = (*if verbosity = 0 then result else*) {result with dependency_tree6b=Array.copy dependency_tree} in
              try
                LCGreductions.validate_dependency_tree dependency_tree;
                let time6 = time_fun () in
                (* Printf.printf "time6-time4=%f\n%!" (time6 -. time4); *)
                {result with status=if partial then PartialParsed else Parsed; merge_edges=merge_edges; sem_time=time6 -. time4}
              with e ->
                let time6 = time_fun () in
                {result with status=ReductionError3; msg=string_of_exn e; sem_time=time6 -. time4}
            with e ->
              let time6 = time_fun () in
              {result with status=ReductionError2; msg=string_of_exn e; sem_time=time6 -. time4}
          else
            {result with status=NotReduced}
        with
        | SemTooBig ->
            let time4 = time_fun () in
            {result with status=TooManyNodes; reduction_time=time4 -. time3}
        | e ->
            let time4 = time_fun () in
            {result with status=ReductionError; msg=string_of_exn e; reduction_time=time4 -. time3}
      else {result with status=NotParsed}
    with
      Timeout t ->
        let time3 = time_fun () in
        {result with status=ParseTimeout; msg=Printf.sprintf "%f" t; parse_time=time3 -. time2}
    | e ->
        let time3 = time_fun () in
        {result with status=ParseError; msg=string_of_exn e; parse_time=time3 -. time2}
  with e ->
    let time2 = time_fun () in
    {result with status=LexiconError; msg=string_of_exn e; lex_time=time2 -. time1}

let conll_parse_sentence timeout verbosity dep_rules tokens lex_sems paths =
  LCGreductions.reset_variant_label ();
  let result = {empty_eniam_parse_result with paths_size = Array.length paths} in
  let result = if verbosity = 0 then result else result(*{result with text_fragments=create_dep_text_fragments tokens paths last}*) in (* FIXME *)
  let time1 = time_fun () in
  try
    (* let paths = (*CONLL_adapter.*)convert_dep_tree "" first_try paths tokens in *)
    let chart = create_dep_chart dep_rules tokens lex_sems paths in
    let result = if verbosity = 0 then result else {result with dep_chart1=chart} in
    let chart,references = LCGchart.dep_lazify chart in
    let result = if verbosity = 0 then result else {result with dep_chart2=chart; references2=ExtArray.copy references} in
    let time2 = time_fun () in
    let result = {result with lex_time=time2 -. time1} in
    try
      let chart = LCGchart.dep_parse chart references timeout time_fun in (* uwaga: niejawna zmiana imperatywna w references *)
      let time3 = time_fun () in
      let result = if verbosity = 0 then result else {result with parsed_dep_chart3=chart; references3=references} in
      let result = {result with parse_time=time3 -. time2; (*chart_size=LCGchart.get_no_entries chart*)} in (* FIXME *)
      if LCGchart.is_dep_parsed chart then
        try
          let term = LCGchart.get_dep_parsed_term chart in
          let result = if verbosity = 0 then result else {result with term4=term} in
          let dependency_tree = LCGreductions.reduce term references in
          let time4 = time_fun () in
          let result = if verbosity = 0 then result else {result with dependency_tree4=Array.copy dependency_tree} in
          let result = {result with reduction_time=time4 -. time3; dependency_tree_size=Array.length dependency_tree} in
          if LCGreductions.is_reduced_dependency_tree dependency_tree then
            try
              LCGreductions.assign_labels dependency_tree; (* uwaga: niejawna zmiana imperatywna w result *)
              let result = if verbosity = 0 then result else {result with dependency_tree5=Array.copy dependency_tree} in
              LCGreductions.remove_cuts dependency_tree; (* uwaga: niejawna zmiana imperatywna w result *)
              let result = if verbosity = 0 then result else {result with dependency_tree6a=dependency_tree} in
              let dependency_tree = LCGreductions.normalize_variants dependency_tree in
              let result = (*if verbosity = 0 then result else*) {result with dependency_tree6b=Array.copy dependency_tree} in
              try
                LCGreductions.validate_dependency_tree dependency_tree;
                let time6 = time_fun () in
                {result with status=Parsed; sem_time=time6 -. time4}
              with e ->
                let time6 = time_fun () in
                {result with status=ReductionError3; msg=string_of_exn e; sem_time=time6 -. time4}
            with e ->
              let time6 = time_fun () in
              {result with status=ReductionError2; msg=string_of_exn e; sem_time=time6 -. time4}
          else
            {result with status=NotReduced}
        with
        | SemTooBig ->
            let time4 = time_fun () in
            {result with status=TooManyNodes; reduction_time=time4 -. time3}
        | e ->
            let time4 = time_fun () in
            {result with status=ReductionError; msg=string_of_exn e; reduction_time=time4 -. time3}
      else {result with status=NotParsed}
    with
      Timeout t ->
        let time3 = time_fun () in
        {result with status=ParseTimeout; msg=Printf.sprintf "%f" t; parse_time=time3 -. time2}
    | NotDepParsed(id_ndp,left,l,right) ->
        let time3 = time_fun () in
        {result with status=NotParsed; not_parsed_dep_chart3=(id_ndp,left,l,right); parse_time=time3 -. time2}
    | e ->
        let time3 = time_fun () in
        {result with status=ParseError; msg=string_of_exn e; parse_time=time3 -. time2}
  with e ->
    let time2 = time_fun () in
    {result with status=LexiconError; msg=string_of_exn e; lex_time=time2 -. time1}

(*let swigra_in, swigra_out = (*Unix.open_process "../swigra/parser/run.sh"*)
  if Paths.config.Paths.swigra_enabled then
    Unix.open_process (Paths.config.Paths.swigra_path ^ "run.sh")
  else stdin, stdout
*)

let polfie_results = "results/polfie"

(*let parse_polfie_sentence s = FIXME!!!
  let sentence, choices, fstructure, form = XTNormalizer.load_fstructure (polfie_results ^ "/fstructure-1.pl") in
  XTXmlOf.print (polfie_results ^ "/fstructure-1.xml") sentence choices fstructure form;
 (* ignore @@ Sys.command("rm " ^ polfie_results ^ "/fstructure-1.pl"); *)
 (* let converted = XTToOcaml.lfg_to_conll fstructure in *)
  let linear_term = XTLinearTermOf.from_lfg_term fstructure in
  print_endline (LCGstringOf.linear_term 0 linear_term);
  print_endline "Done POLFIE parsing";
  RawSentence s*)

let parse timeout verbosity max_cost rules dep_rules (*name id*) tokens lex_sems =
  map_text_par_string Struct (fun mode par_string -> function
    RawSentence s ->
      (match mode with
(*        Swigra ->
          if not Paths.config.Paths.swigra_enabled then RawSentence s else (
          Printf.fprintf swigra_out "%s\n%!" s;
          print_endline ("swigra: " ^ input_line swigra_in);
          RawSentence s)*)
      | Name -> print_endline s; RawSentence s
      | Identifier -> RawSentence s
(*       | POLFIE -> parse_polfie_sentence s *)
      | _ -> RawSentence s)
  | StructSentence(paths,last) ->
      (match mode with
        ENIAM ->
          let result = eniam_parse_sentence timeout verbosity rules tokens lex_sems paths last par_string max_cost in
          ENIAMSentence result
      | _ -> failwith "parse 3")
  | DepSentence paths_list ->
      (match mode with
        CONLL | Mate | Swigra ->
          let result = Xlist.fold paths_list empty_eniam_parse_result (fun result paths ->
            if result.status = Parsed then result else
            conll_parse_sentence timeout verbosity dep_rules tokens lex_sems paths) in
          ENIAMSentence result
    | _ -> failwith "parse 2")
  | ErrorSentence s -> ErrorSentence s
  | _ -> failwith "parse 1")


(*

open Printf

let semantic_processing timeout test_only_flag file_prefix tokens lex_sems max_n dependency_tree =
  let time5 = time_fun () in
  let result = {empty_semantic_processing_result with file_prefix=file_prefix} in
  try
    let (*dependency_tree2*)(*sem*)disamb = LCGvalence.assign_frames_and_senses tokens lex_sems dependency_tree in
    let disamb(*sem*) = DisambSelPref.fit_sel_prefs DisambSelPref.fit_node1 (*dependency_tree2*)disamb in
    let (*sem*)disamb = DisambLemma.disambiguate_nodes (*dependency_tree*)(*sem*)disamb in
    let (*sem*)disamb = DisambLemma.remove_unused(*disambiguate_nodes*) (*dependency_tree*)(*sem*)disamb in
    let (*sem*)disamb = DisambLemma.remove_unused_choices(*disambiguate_nodes*) (*dependency_tree*)(*sem*)disamb in
    let (*disamb*)sem = DisambSelPref.fit_sel_prefs DisambSelPref.fit_node2 (*dependency_tree2*)disamb in
    let result = if test_only_flag then result else {result with disamb=disamb} in
    let sem = print_string " 7"; DisambLemma.disambiguate_meanings (*dependency_tree*)sem in
    let sem(*disamb*) = print_string " 8"; DisambLemma.remove_unused_choices(*disambiguate_nodes*) (*dependency_tree*)sem(*disamb*) in
    let result = if test_only_flag then result else {result with sem=sem} in
    let sem2 = SemGraph.translate tokens lex_sems (*disamb*)sem in
    let result = if test_only_flag then result else {result with sem2=sem2} in
    let sem3(*disamb*) = print_string " 10"; SemGraph.make_tree(*disambiguate_nodes*) (*dependency_tree*)sem2(*disamb*) in
    let sem3(*disamb*) = print_string " 11"; SemGraph.simplify_tree(*disambiguate_nodes*) (*dependency_tree*)sem3(*disamb*) in
(*     let sem3(*disamb*) = SemGraph.manage_quantification(*disambiguate_nodes*) (*dependency_tree*)sem3(*disamb*) in  *)
    let sem3(*disamb*) = print_string " 12"; SemGraph.simplify_gender(*disambiguate_nodes*) (*dependency_tree*)sem3(*disamb*) in
(*     if Array.length disamb < 10000 then print_xml_dependency_tree "results/trees/" (id ^ "dis") disamb; *)
    let result = if test_only_flag then result else {result with sem3=sem3} in
    let time6 = time_fun () in
    if SemGraph.validate_semantics sem3 then
      let trees = print_string " 13"; SemGraph.draw_trees max_n sem3 in
      let trees2 = print_string " 14"; Xlist.map trees SemMrl.variable_alpha_convertion in
      let mrls = print_string " 15"; Xlist.map trees2 SemMrl.make_mrl in
      let mrls = print_string " 16"; Xlist.map mrls SemMrl.move_requirements in
      let mrss = print_string " 17"; Xlist.map mrls SemMrl.make_mrs_of_mrl in
      let mrss = print_string " 18"; Xlist.map mrss SemMrl.mrs_handle_alpha_convertion in
      let fols = print_string " 19"; Xlist.map mrss (fun mrs ->
        let l = SemMrl.foll_of_mrs_greedy mrs in
        if l = [] then failwith "empty fol" else
        List.hd l) in
      let result = print_string " 20"; if test_only_flag then result else {result with trees=trees; mrls=fols(*mrls*)} in
      {result with status=Parsed; sem_time=time6 -. time5}
    else {result with status=NotTranslated; sem_time=time6 -. time5}
  with e ->
    let time6 = time_fun () in
    {result with status=SemError; msg=string_of_exn e; sem_time=time6 -. time5}


let rec semantic_processing_sentence timeout test_only_flag tokens lex_sems max_n = function
    RawSentence s -> RawSentence s
  | ENIAMSentence result -> SemSentence (semantic_processing timeout test_only_flag result.file_prefix tokens lex_sems max_n result.dependency_tree)
  | CONLLSentence result -> SemSentence (semantic_processing timeout test_only_flag result.file_prefix tokens lex_sems max_n result.dependency_tree)
  | QuotedSentences sentences ->
      let sentences = Xlist.rev_map sentences (fun p ->
        let sentence = semantic_processing_sentence timeout test_only_flag tokens lex_sems max_n p.psentence in
        {p with psentence=sentence}) in
      QuotedSentences(List.rev sentences)
  | AltSentence l ->
      let l = Xlist.rev_map l (fun (mode,sentence) ->
        mode, semantic_processing_sentence timeout test_only_flag tokens lex_sems max_n sentence) in
      AltSentence(List.rev l)
 | _ -> failwith "semantic_processing_sentence"

let rec semantic_processing_paragraph timeout test_only_flag tokens lex_sems max_n = function
    RawParagraph s -> RawParagraph s
  | StructParagraph sentences ->
      let sentences = Xlist.rev_map sentences (fun p ->
        let sentence = semantic_processing_sentence timeout test_only_flag tokens lex_sems max_n p.psentence in
        {p with psentence=sentence}) in
      StructParagraph(List.rev sentences)
  | AltParagraph l ->
      let l = Xlist.rev_map l (fun (mode,paragraph) ->
        mode, semantic_processing_paragraph timeout test_only_flag tokens lex_sems max_n paragraph) in
      AltParagraph(List.rev l)

let rec semantic_processing_text timeout test_only_flag tokens lex_sems max_n = function
    RawText s -> RawText s
  | StructText paragraphs  ->
      let paragraphs = Xlist.rev_map paragraphs (fun paragraph ->
        semantic_processing_paragraph timeout test_only_flag tokens lex_sems max_n paragraph) in
      StructText(List.rev paragraphs)
  | AltText l -> AltText(Xlist.map l (fun (mode,text) ->
       mode, semantic_processing_text timeout test_only_flag tokens lex_sems max_n text))*)
(**
let eniam_semantic_processing verbosity tokens lex_sems (result : eniam_parse_result) =
  let tree,result =
    try
      let tree = SemValence.assign_frames tokens lex_sems result.dependency_tree6b in
      let result = if verbosity < 2 then result else {result with dependency_tree7=tree} in
      tree,result
    with e -> [| |],{result with status=SemValenceError; msg=string_of_exn e} in
  if result.status = SemValenceError then result else
  let tree,result =
    try
      let tree = SemValence.reduce_tree tokens lex_sems tree in (* FIXME: tokens lex_sems nie są potrzebne *)
      let result = if verbosity < 2 then result else {result with dependency_tree8=tree} in
      tree,result
    with e -> ExtArray.make 0 Dot,{result with status=SemValenceError; msg=string_of_exn e} in
  if result.status = SemValenceError then result else
  let result =
    try
      SemValence.transfer_attributes tree; (* niejawna zmiana imperatywna w tree *)
      result
    with e -> {result with status=SemValenceError; msg=string_of_exn e} in
  if result.status = SemValenceError then result else
  let tree,result =
    try
      Disambiguation.selprefs tree; (* niejawna zmiana imperatywna w tree *)
      let tree = Disambiguation.merge tree in
      (* let tree = Disambiguation.random_tree tokens lex_sems tree in *) (* FIXME: tokens lex_sems nie są potrzebne *)
      let tree = LCGreductions.reshape_dependency_tree(*ExtArray.to_array*) tree in
      LexSemantics.create_tokens_for_artificial_nodes tokens lex_sems tree;
      Coreference.resolve tree;
      let result = if verbosity = 0 then result else {result with dependency_tree9=tree} in
      tree,result
    with e -> [| |],{result with status=SemValenceError; msg=string_of_exn e} in
  if result.status = SemValenceError then result else
  let graph,result =
    try
      let graph = SemGraph.translate tokens lex_sems tree in (* FIXME: pro nie mają id *)
      let result = if verbosity = 0 then result else {result with semantic_graph10=graph} in
      let graph = SemGraph.make_tree graph in
      let result = if verbosity = 0 then result else {result with semantic_graph11=graph} in
      graph,result
    with e -> SemTypes.Dot,{result with status=SemGraphError; msg=string_of_exn e} in
  if result.status = SemGraphError then result else
  let r = ref [] in
  (try SemGraph.validate_translation r graph with e -> r := string_of_exn e :: !r);
  if !r <> [] then {result with status = SemGraphError; msg=String.concat "<BR>" !r} else
  let graph,result =
    try
      let graph = SemGraph.reduce_tree graph in
      let result = (*if verbosity = 0 then result else*) {result with semantic_graph11=graph} in
      graph,result
    with e -> SemTypes.Dot,{result with status=SemGraphError; msg=string_of_exn e} in
  if result.status = SemGraphError then result else
  let r = ref [] in
  (try SemGraph.validate_reduction r graph with e -> r := string_of_exn e :: !r);
  if !r <> [] then {result with status = SemGraphError; msg=String.concat "<BR>" !r} else
  let graph,result =
    try
      let graph = SemGraph.greater_simplify graph in
(*    let graph = SemGraph.manage_quantification graph in  *)
      let graph = SemGraph.simplify_gender graph in
      let graph = SemGraph.manage_variant_labels graph in
      let result = (*if verbosity = 0 then result else*) {result with semantic_graph11=graph; semantic_graph12=graph} in
      graph,result
    with e -> SemTypes.Dot,{result with status=SemGraphError; msg=string_of_exn e} in
  if result.status = SemGraphError then result else
  {result with status = if result.status = PartialParsed then PartialSemParsed else SemParsed}

let semantic_processing verbosity tokens lex_sems text =
  map_text Struct (fun mode -> function
      ENIAMSentence result ->
        if result.status <> Parsed || result.status <> PartialParsed then ENIAMSentence result else
        ENIAMSentence (eniam_semantic_processing verbosity tokens lex_sems result)
    | t -> t) text
**)

(*
let rec extract_query_text = function
    RawText s -> s
  | AltText l -> (try extract_query_text (Xlist.assoc l Raw) with Not_found -> failwith "extract_query_text")
  | _ -> failwith "extract_query_text"

let process_query pre_in pre_out timeout test_only_flag id full_query max_n =
  (* print_endline "process_query 0"; *)
  let result = {empty_result with input_text=translate_text (fst full_query)} in
  let time1 = time_fun () in
  (* print_endline "process_query 1"; *)
  Marshal.to_channel pre_out full_query [];
  flush pre_out;
  (* print_endline "process_query 2"; *)
  let pre_text,tokens,lex_sems,msg,pre_time1 = (Marshal.from_channel pre_in :
          SubsyntaxTypes.text *
          SubsyntaxTypes.token_record ExtArray.t *
          LexSemanticsTypes.lex_sem ExtArray.t * string * float) in
  let time2 = time_fun () in
  let result = if test_only_flag then result else {result with pre_text=translate_text pre_text; tokens=tokens; lex_sems=lex_sems} in
  let result = {result with pre_time1=pre_time1; pre_time2=time2 -. time1} in
  if msg <> "" then {result with status=PreprocessingError; msg=msg} else (
  (* print_endline "process_query 3"; *)
  let parsed_text = parse_text timeout test_only_flag Struct id tokens lex_sems (translate_text pre_text) in
  (* print_endline "process_query 4"; *)
  let time3 = time_fun () in
  let result = if test_only_flag then result else {result with status=Parsed; parsed_text=parsed_text} in
  let result = {result with parse_time=time3 -. time2} in
  (* print_endline "process_query 5"; *)
  let selected_sent_text =
     if not Paths.config.Paths.sentence_selection_enabled then parsed_text
     else select_sentences_text parsed_text in
  (* print_endline "process_query 6"; *)
  let result = if test_only_flag then result else {result with status=Parsed; selected_sent_text=selected_sent_text} in
  let semantic_text = semantic_processing_text timeout test_only_flag tokens lex_sems max_n selected_sent_text in
  (* print_endline "process_query 7"; *)
  let selected_semantic_text =
     if not Paths.config.Paths.sentence_selection_enabled then semantic_text
     else select_sentences_text semantic_text in
  (* print_endline "process_query 8"; *)
  let time4 = time_fun () in
  let result =
    if test_only_flag then result
    else {result with status=Parsed;
      semantic_text=semantic_text;
      selected_semantic_text=selected_semantic_text} in
  let result = {result with semantic_time=time4 -. time3} in
  result)

let print_result file result =
  Printf.fprintf file "query: %s\n" (extract_query_text result.input_text);
  (match result.status with
    Idle -> Printf.fprintf file "idle\n"
  | PreprocessingError -> Printf.fprintf file "error_pre: %s\n" result.msg
  (* | LexiconError -> Printf.fprintf file "error_lex: %s\n" result.msg
  | ParseError -> Printf.fprintf file "error_parse: %s\n" result.msg
  | ParseTimeout -> Printf.fprintf file "timeout: %s\n" result.msg
  | NotParsed -> Printf.fprintf file "not_parsed: paths_size=%d chart_size=%d\n" result.paths_size result.chart_size
  | ReductionError -> Printf.fprintf file "error_reduction: %s\n" result.msg
  | TooManyNodes -> Printf.fprintf file "to_many_nodes: paths_size=%d chart_size=%d\n" result.paths_size result.chart_size
  | NotReduced -> Printf.fprintf file "not_reduced: paths_size=%d chart_size=%d\n" result.paths_size result.chart_size
  | SemError -> Printf.fprintf file "error_sem: %s\n" result.msg
  | NotTranslated -> Printf.fprintf file "not_translated: \n" *)
  (* | Parsed -> Printf.fprintf file "parsed: paths_size=%d chart_size=%d dependency_tree_size=%d\n" result.paths_size result.chart_size result.dependency_tree_size *)
  | Parsed -> Printf.fprintf file "parsed\n"
  | _ -> failwith "print_result");
  (* Printf.fprintf file "times: pre_time1=%f pre_time2=%f lex_time=%f parse_time=%f reduction_time=%f sem_time=%f\n%!"
    result.pre_time1 result.pre_time2 result.lex_time result.parse_time result.reduction_time result.sem_time *)
  Printf.fprintf file "times: pre_time1=%f pre_time2=%f parse_time=%f\n%!"
    result.pre_time1 result.pre_time2 result.parse_time

let add_result sum_result result =
  let sum_result = {sum_result with no_queries=sum_result.no_queries+1} in
  let sum_result = match result.status with
    Idle -> failwith "sum_result"
  | PreprocessingError -> {sum_result with no_pre_error=sum_result.no_pre_error+1}
  (* | LexiconError -> {sum_result with no_lex_error=sum_result.no_lex_error+1}
  | ParseError -> {sum_result with no_parse_error=sum_result.no_parse_error+1}
  | ParseTimeout -> {sum_result with no_timeout=sum_result.no_timeout+1}
  | NotParsed -> {sum_result with no_not_parsed=sum_result.no_not_parsed+1}
  | ReductionError -> {sum_result with no_reduction_error=sum_result.no_reduction_error+1}
  | TooManyNodes -> {sum_result with no_too_many_nodes=sum_result.no_too_many_nodes+1}
  | NotReduced -> {sum_result with no_not_reduced=sum_result.no_not_reduced+1}
  | SemError -> {sum_result with no_sem_error=sum_result.no_sem_error+1}
  | NotTranslated -> {sum_result with no_not_translated=sum_result.no_not_translated+1} *)
  | Parsed -> {sum_result with no_parsed=sum_result.no_parsed+1}
  | _ -> failwith "add_result" in
  {sum_result with
     sum_pre_time1=sum_result.sum_pre_time1 +. result.pre_time1;
     sum_pre_time2=sum_result.sum_pre_time2 +. result.pre_time2;
     (* sum_lex_time=sum_result.sum_lex_time +. result.lex_time; *)
     sum_parse_time=sum_result.sum_parse_time +. result.parse_time;
     (* sum_reduction_time=sum_result.sum_reduction_time +. result.reduction_time;
     sum_sem_time=sum_result.sum_sem_time +. result.sem_time*)}

let print_sum_result file r = failwith "print_sum_result: ni"
  (* Printf.fprintf file "avg_times: pre_time1=%f pre_time2=%f lex_time=%f parse_time=%f reduction_time=%f sem_time=%f\n"
    (r.sum_pre_time1 /. float r.no_queries)
    (r.sum_pre_time2 /. float r.no_queries)
    (r.sum_lex_time /. float r.no_queries)
    (r.sum_parse_time /. float r.no_queries)
    (r.sum_reduction_time /. float r.no_queries)
    (r.sum_sem_time /. float r.no_queries);
  Printf.fprintf file "sum_results: pre_error=%d (%f%%) lex_error=%d (%f%%) parse_error=%d (%f%%) timeout=%d (%f%%) not_parsed=%d (%f%%) reduction_error=%d (%f%%) too_many_nodes=%d (%f%%) not_reduced=%d (%f%%) sem_error=%d (%f%%) not_translated=%d (%f%%) parsed=%d (%f%%)\n%!"
    r.no_pre_error (float r.no_pre_error /. float r.no_queries *. 100.)
    r.no_lex_error (float r.no_lex_error /. float r.no_queries *. 100.)
    r.no_parse_error (float r.no_parse_error /. float r.no_queries *. 100.)
    r.no_timeout (float r.no_timeout /. float r.no_queries *. 100.)
    r.no_not_parsed (float r.no_not_parsed /. float r.no_queries *. 100.)
    r.no_reduction_error (float r.no_reduction_error /. float r.no_queries *. 100.)
    r.no_too_many_nodes (float r.no_too_many_nodes /. float r.no_queries *. 100.)
    r.no_not_reduced (float r.no_not_reduced /. float r.no_queries *. 100.)
    r.no_sem_error (float r.no_sem_error /. float r.no_queries *. 100.)
    r.no_not_translated (float r.no_not_translated /. float r.no_queries *. 100.)
    r.no_parsed (float r.no_parsed /. float r.no_queries *. 100.) *)

let get_sock_addr host_name port =
  let he = Unix.gethostbyname host_name in
  let addr = he.Unix.h_addr_list in
  Unix.ADDR_INET(addr.(0),port)

let generate_queries filename timeout =
  let queries = File.load_lines filename in
  List.rev (fst (Xlist.fold queries ([],1) (fun (l,id) query ->
    let query = try List.hd (Str.split (Str.regexp "\t") query) with _ -> "" in
    (string_of_int id,(query,timeout)) :: l, id+1)))

let generate_queries_id filename timeout =
  let queries = File.load_lines filename in
  List.rev (Xlist.rev_map queries (fun line ->
    match Str.split (Str.regexp "\t") line with
      [id;query] -> id,(query,timeout)
    | _ -> failwith ("generate_queries_id: " ^ line)))

(*let test_process_file filename output_filename timeout =
  let queries = generate_queries filename timeout in
  let ic,oc = Unix.open_connection (get_sock_addr Paths.pre_host Paths.pre_port) in
  File.file_out output_filename (fun file ->
    let _ = Xlist.fold queries empty_sum_result (fun sum_result (id,(query,timeout)) ->
      let result = process_query ic oc timeout true id query 10 in
      print_result file result;
      let sum_result = add_result sum_result result in
      print_sum_result file sum_result;
      sum_result) in
    ());
  Printf.fprintf oc "\n%!";
  let _ = Unix.shutdown_connection ic in
  ()

let process_file_id filename output_filename timeout =
  let queries = generate_queries_id filename timeout in
  let ic,oc = Unix.open_connection (get_sock_addr Paths.pre_host Paths.pre_port) in
  File.file_out output_filename (fun file ->
    let _ = Xlist.fold queries empty_sum_result (fun sum_result (id,(query,timeout)) ->
      let result = process_query ic oc timeout true id query 10 in
      print_result file result;
      let sum_result = add_result sum_result result in
      print_sum_result file sum_result;
      sum_result) in
    ());
  Printf.fprintf oc "\n%!";
  let _ = Unix.shutdown_connection ic in
  ()*)
*)

let rec select_not_parsed_sentence = function
    RawSentence s -> false,RawSentence s
  | StructSentence _ -> failwith "select_not_parsed_sentence"
  | DepSentence _ -> failwith "select_not_parsed_sentence"
  | QuotedSentences sentences ->
      let l = Xlist.fold sentences [] (fun l p ->
        let b,sentence = select_not_parsed_sentence p.sentence in
        if b then {p with sentence=sentence} :: l else l) in
	  if l = [] then false,AltSentence [] else true, QuotedSentences(List.rev l)
  | AltSentence l -> 
      let b,l = Xlist.fold l (false,[]) (fun (b0,l) (mode,sentence) ->
        let b, sentence = select_not_parsed_sentence sentence in
		b0 || b, (mode,sentence) :: l) in
	  if b then true, AltSentence(List.rev l) else false, AltSentence []
  | ENIAMSentence result -> 
      if result.status = Parsed || result.status = PartialParsed || 
        result.status = SemParsed || result.status = PartialSemParsed || result.status = Inferenced 
	  then false,ENIAMSentence result else true,ENIAMSentence result
  | ErrorSentence s -> true,ErrorSentence s

let rec select_not_parsed_paragraph = function
    RawParagraph s -> false,RawParagraph s
  | StructParagraph(stats,sentences) ->
      let l = Xlist.fold sentences [] (fun l p ->
        let b,sentence = select_not_parsed_sentence p.sentence in
        if b then {p with sentence=sentence} :: l else l) in
	  if l = [] then false,AltParagraph [] else true, StructParagraph(stats,List.rev l)
  | AltParagraph l -> 
      let b,l = Xlist.fold l (false,[]) (fun (b0,l) (mode,paragraph) ->
        let b, paragraph = select_not_parsed_paragraph paragraph in
		b0 || b, (mode,paragraph) :: l) in
	  if b then true, AltParagraph(List.rev l) else false, AltParagraph []
  | ErrorParagraph s -> true,ErrorParagraph s

let rec select_not_parsed_text = function
    RawText s -> false,RawText s
  | StructText paragraphs ->
      let l = Xlist.fold paragraphs [] (fun l paragraph ->
        let b,paragraph = select_not_parsed_paragraph paragraph in
        if b then paragraph :: l else l) in
	  if l = [] then false,AltText [] else true,StructText(List.rev l)
  | JSONtext s -> true,JSONtext s
  | AltText l -> 
      let b,l = Xlist.fold l (false,[]) (fun (b0,l) (mode,text) ->
        let b, text = select_not_parsed_text text in
		b0 || b, (mode,text) :: l) in
	  if b then true, AltText(List.rev l) else false, AltText []
  | ErrorText s -> true,ErrorText s

let select_not_parsed text = 
  let b,text = select_not_parsed_text text in
  text

let disambiguate text =
  ExecTypes.map_text Struct (fun mode -> function
      ENIAMSentence result ->
        if result.status <> Parsed then ENIAMSentence result else
        ENIAMSentence {result with dependency_tree6b=Disamb.eniam_disambiguate result.dependency_tree6b}
    | t -> t) text

let sort_arguments tokens text =
  ExecTypes.map_text Struct (fun mode -> function
      ENIAMSentence result ->
        if result.status <> Parsed then ENIAMSentence result else
        ENIAMSentence {result with dependency_tree6b=Disamb.eniam_sort_arguments tokens result.dependency_tree6b}
    | t -> t) text

let merge_mwe tokens text =
  ExecTypes.map_text Struct (fun mode -> function
      ENIAMSentence result ->
        if result.status <> Parsed then ENIAMSentence result else
        ENIAMSentence {result with dependency_tree6b=Disamb.shift_lemma_variants tokens result.dependency_tree6b}
    | t -> t) text
      
let apply_rules_text t =
  map_text Struct (fun mode -> function
  | ENIAMSentence result -> 
      (match result.status with
        SemParsed | PartialSemParsed -> 
		  (try
            ENIAMSentence {result with semantic_graph13=Inference.apply_rules result.semantic_graph12; status=Inferenced}
		  with e -> ENIAMSentence {result with status=InferenceError; msg=string_of_exn e})
      | _ -> ENIAMSentence result)
  | t -> t) t
  
let validate t =
  map_text Struct (fun mode -> function
  | ENIAMSentence result ->
      (match result.status with
        Inferenced ->
			let r = ref [] in
(* 			print_endline "Json.validate"; *)
			(try Json.validate_linear_term r result.semantic_graph13 with e -> r := string_of_exn e :: !r);
(* 			print_endline (String.concat "<BR>" !r); *)
			let result = if !r <> [] then {result with status = SemNotValidated; msg=String.concat "<BR>" !r} else result in
			ENIAMSentence result
      | _ -> ENIAMSentence result)
  | t -> t) t

let rec aggregate_status_sentence status = function
    RawSentence s -> status
  | StructSentence(paths,last) -> false
  | DepSentence paths -> false
  | ENIAMSentence result -> 
      (match result.status with
        PreprocessingError | LexiconError | ParseError | ParseTimeout| TooManyNodes | NotParsed 
      | NotReduced | ReductionError | ReductionError2 | ReductionError3 | SemValenceError
      | SemGraphError | SemGraphError2 | SemNotValidated | InferenceError -> false
      | Parsed | PartialParsed | SemParsed | PartialSemParsed | Inferenced -> status
      | Idle -> failwith "aggregate_status_sentence")
  | ErrorSentence s -> false
  | QuotedSentences sentences ->
    Xlist.fold sentences status (fun status p ->
        aggregate_status_sentence status p.sentence)
  | AltSentence l ->
    Xlist.fold l status (fun status (_,sentence) ->
        aggregate_status_sentence status sentence)

let rec aggregate_status_paragraph status = function
    RawParagraph _ -> status
  | StructParagraph(stats,sentences) ->
    Xlist.fold sentences status (fun status p ->
        aggregate_status_sentence status p.sentence)
  | AltParagraph l ->
    Xlist.fold l status (fun status (_,paragraph) ->
        aggregate_status_paragraph status paragraph)
  | ErrorParagraph _ -> false

let rec aggregate_status_text status = function
    RawText _ -> status
  | StructText paragraphs ->
    Xlist.fold paragraphs status (fun status paragraph ->
        aggregate_status_paragraph status paragraph)
  | JSONtext _ -> status
  | AltText l ->
    Xlist.fold l status (fun status (_,text) ->
        aggregate_status_text status text)
  | ErrorText _ -> false

let aggregate_status text =
  aggregate_status_text true text
  
let min_stats a b = 
  {c_len=min a.c_len b.c_len; c_len_nann=min a.c_len_nann b.c_len_nann; t_len=min a.t_len b.t_len; t_len_nann=min a.t_len_nann b.t_len_nann; 
   c_len2=min a.c_len2 b.c_len2; c_len2_nann=min a.c_len2_nann b.c_len2_nann; t_len2=min a.t_len2 b.t_len2; t_len2_nann=min a.t_len2_nann b.t_len2_nann}

let add_stats a b = 
  {c_len=a.c_len + b.c_len; c_len_nann=a.c_len_nann + b.c_len_nann; t_len=a.t_len + b.t_len; t_len_nann=a.t_len_nann + b.t_len_nann; 
   c_len2=a.c_len2 + b.c_len2; c_len2_nann=a.c_len2_nann + b.c_len2_nann; t_len2=a.t_len2 + b.t_len2; t_len2_nann=a.t_len2_nann + b.t_len2_nann}

let rec aggregate_stats_sentence stats no_tokens no_chars = function (* trzeba połączyć długości ze statusem *)
    RawSentence s -> {stats with t_len2_nann=stats.t_len2_nann+no_tokens; c_len2_nann=stats.c_len2_nann+no_chars}
  | StructSentence(paths,last) -> {stats with t_len2_nann=stats.t_len2_nann+no_tokens; c_len2_nann=stats.c_len2_nann+no_chars}
  | DepSentence paths -> {stats with t_len2_nann=stats.t_len2_nann+no_tokens; c_len2_nann=stats.c_len2_nann+no_chars}
  | ENIAMSentence result -> 
      (match result.status with
        PreprocessingError | LexiconError | ParseError | ParseTimeout| TooManyNodes | NotParsed 
      | NotReduced | ReductionError | ReductionError2 | ReductionError3 | SemValenceError
      | SemGraphError | SemGraphError2 | SemNotValidated | InferenceError -> {stats with t_len2_nann=stats.t_len2_nann+no_tokens; c_len2_nann=stats.c_len2_nann+no_chars}
(*       | PartialParsed | PartialSemParsed -> {stats with t_len2_nann=stats.t_len2_nann+result.merge_edges-1} *) (* FIXME: trzeba poprawić wyliczanie merge_edges*)
      | PartialParsed | PartialSemParsed -> {stats with t_len2_nann=stats.t_len2_nann+(no_tokens*17/100)}
      | Parsed | SemParsed | Inferenced -> stats
      | Idle -> failwith "aggregate_status_sentence")
  | ErrorSentence s -> {stats with t_len2_nann=stats.t_len2_nann+no_tokens; c_len2_nann=stats.c_len2_nann+no_chars}
  | QuotedSentences sentences ->
      Xlist.fold sentences stats (fun stats p ->
        aggregate_stats_sentence stats p.no_tokens (Patterns.count_length p.beg p.next) p.sentence)
  | AltSentence l ->
      Xlist.fold l {stats with t_len2_nann=stats.t_len2_nann+no_tokens; c_len2_nann=stats.c_len2_nann+no_chars} (fun stats2 (_,sentence) ->
        min_stats stats2 (aggregate_stats_sentence stats no_tokens no_chars sentence))
  
let rec aggregate_stats_paragraph = function
    StructParagraph(stats,sentences) -> 
      let stats = Xlist.fold sentences stats (fun stats p -> 
        let no_chars = Patterns.count_length p.beg p.next in
        let stats = {stats with t_len2=stats.t_len2+p.no_tokens; c_len2=stats.c_len2+no_chars} in
        aggregate_stats_sentence stats p.no_tokens no_chars p.sentence) in
      StructParagraph(stats,sentences)
  | AltParagraph l -> AltParagraph(Xlist.map l (fun (mode,text) -> mode, aggregate_stats_paragraph text))
  | t -> t

let rec aggregate_stats_text = function
    StructText paragraphs -> StructText(Xlist.map paragraphs aggregate_stats_paragraph)
  | AltText l -> AltText(Xlist.map l (fun (mode,text) -> mode, aggregate_stats_text text))
  | t -> t

let aggregate_stats text =
  aggregate_stats_text text
  
let rec extract_stats_paragraph = function
    StructParagraph(stats,sentences) -> stats
  | AltParagraph l -> Xlist.fold l max_int_stats (fun stats (mode,text) -> min_stats stats (extract_stats_paragraph text))
  | t -> zero_stats

let rec extract_stats_text = function
    StructText paragraphs -> Xlist.fold paragraphs zero_stats (fun stats p -> add_stats stats (extract_stats_paragraph p))
  | AltText l -> Xlist.fold l max_int_stats (fun stats (mode,text) -> min_stats stats (extract_stats_text text))
  | t -> zero_stats

module Json2 = struct

open Json
open Xjson

let convert_eniam_sentence (result : eniam_parse_result) =
  match result.status with
    Inferenced -> convert_linear_term result.semantic_graph13
  | Parsed | PartialParsed -> LCG_JSONof.linear_term_array result.dependency_tree6a
  | _ -> JObject["error", JString (Visualization.string_of_status result.status); "msg", JString result.msg]

let rec convert_sentence m = function
    RawSentence s -> JObject["with",JArray []]
  | StructSentence(paths,last) -> JObject["error", JString "StructSentence"]
  | DepSentence paths -> JObject["error", JString "DepSentence"]
  | ENIAMSentence result -> convert_eniam_sentence result
  | QuotedSentences sentences -> JObject["error", JString "QuotedSentences"]
  | AltSentence l -> make_with (Xlist.rev_map l (fun (m,t) -> convert_sentence (Visualization.string_of_mode m) t))
  | ErrorSentence s -> JObject["error", JString s]

let rec convert_paragraph m = function (* UWAGA: "and-tuple" make_and_tuple są jednoelementowe w praktyce zwn. merge_graph *)
    RawParagraph s -> 
      if m = "Name" then JObject["name", JString s] else 
      if m = "Id" then JObject["id", JString s] else
      if m = "Raw" then JObject["text", JString s] else
      JObject["with",JArray []]
  | StructParagraph(_,sentences) -> JObject["and-tuple",JArray(Xlist.rev_map sentences (fun p -> convert_sentence "" p.sentence))]
  | AltParagraph l -> make_and_tuple (Xlist.rev_map l (fun (m,t) -> convert_paragraph (Visualization.string_of_mode m) t))
  | ErrorParagraph s -> JObject["error", JString s]

let rec convert_text m = function
    RawText s -> JObject["with",JArray []]
  | StructText paragraphs ->  JObject["and-tuple",JArray(Xlist.rev_map paragraphs (convert_paragraph ""))]
  | JSONtext _ -> failwith "convert_text"
  | AltText l -> make_with (Xlist.rev_map l (fun (m,t) -> convert_text (Visualization.string_of_mode m) t))
  | ErrorText s -> JObject["error", JString s]

let convert statistics_flag t = 
  let json = convert_text "" t in
  if statistics_flag then 
    let c = extract_stats_text t in
    JObject["data",json;
      "c_len",JNumber (string_of_int c.c_len); "c_len_nann",JNumber (string_of_int c.c_len_nann); 
      "t_len",JNumber (string_of_int c.t_len); "t_len_nann",JNumber (string_of_int c.t_len_nann); 
      "c_len2",JNumber (string_of_int c.c_len2); "c_len2_nann",JNumber (string_of_int c.c_len2_nann); 
      "t_len2",JNumber (string_of_int c.t_len2); "t_len2_nann",JNumber (string_of_int c.t_len2_nann)]
  else json

end

let initialize () =
  let valence = DomainLexSemantics.prepare_pro_valence (snd !ValParser.valence) in
  let functs = LCGlexicon.make_pro_rules !LCGlexiconTypes.rules valence in
  let functs = Xlist.rev_map functs fst in
  pro_rules := [(1, fun references args -> LCGrules.forward_application_ignore_brackets references functs args)]

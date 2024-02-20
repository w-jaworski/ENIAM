(*
 *  ENIAMmorphology, a morphological analyser and a guesser for Polish
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

open Xstd
open Printf
open MorphologyTypes

(* let nexus_path = "/home/yacheu/Dokumenty/Badania/Jezyk i Umysl/Przetwarzanie Języka Naturalnego/zasoby/"
let toshiba_ub_path = "/home/wjaworski/Dokumenty/zasoby/"

let get_host_name () =
  let chan = Unix.open_process_in "uname -n" in
  input_line chan

let zasoby_path =
  match get_host_name () with
    "nexus" -> nexus_path
  | "toshiba-UB" -> toshiba_ub_path
(*   | "mozart" -> "." *)
  | s -> failwith ("unknown host: " ^ s) *)

let nlp_resources_path = "../../NLP resources/"
let sgjp_path = nlp_resources_path ^ "SGJP/"
let results_path = "results/"
let lu_path = "../morphology2/plWordnet/"

let lematy_nkjp_filename = "lematy_NKJP1M_freq.tab"

let sgjp_filename2015 = "sgjp-20151020.tab.gz"
let polimorf_filename2015 = "polimorf-20151020.tab.gz"
let sgjp_filename201605 = "sgjp-20160508.tab.gz"
let polimorf_filename201605 = "polimorf-20160508.tab.gz"
let sgjp_filename201607 = "sgjp-20160724.tab.gz"
let polimorf_filename201607 = "polimorf-20160724.tab.gz"
let sgjp_filename201707 = "sgjp-20170730.tab.gz"
let polimorf_filename201707 = "polimorf-20170402.tab.gz"
let sgjp_filename2022 = "sgjp-20220403.tab.gz"
let polimorf_filename2022 = "polimorf-20220403.tab.gz"
let sgjp_filename = "sgjp-20240128.tab.gz"
let polimorf_filename = "polimorf-20240128.tab.gz"

(*let adv_sgjp_filename = "adv_sgjp-20170730.tab"
let adj_sgjp_filename = "adj_sgjp-20170730.tab"
let noun_sgjp_filename = "noun_sgjp-20170730.tab"
let verb_sgjp_filename = "verb_sgjp-20170730.tab"
let adv_polimorf_filename = "adv_polimorf-20170402.tab"
let adj_polimorf_filename = "adj_polimorf-20170402.tab"
let noun_polimorf_filename = "noun_polimorf-20170402.tab"
let verb_polimorf_filename = "verb_polimorf-20170402.tab"*)
(*let adv_sgjp_filename = "adv_sgjp-20220403.tab"
let adj_sgjp_filename = "adj_sgjp-20220403.tab"
let noun_sgjp_filename = "noun_sgjp-20220403.tab"
let verb_sgjp_filename = "verb_sgjp-20220403.tab"
let adv_polimorf_filename = "adv_polimorf-20220403.tab"
let adj_polimorf_filename = "adj_polimorf-20220403.tab"
let noun_polimorf_filename = "noun_polimorf-20220403.tab"
let verb_polimorf_filename = "verb_polimorf-20220403.tab"*)
let adv_sgjp_filename = "adv_sgjp-20240128.tab"
let adj_sgjp_filename = "adj_sgjp-20240128.tab"
let noun_sgjp_filename = "noun_sgjp-20240128.tab"
let verb_sgjp_filename = "verb_sgjp-20240128.tab"
let adv_polimorf_filename = "adv_polimorf-20240128.tab"
let adj_polimorf_filename = "adj_polimorf-20240128.tab"
let noun_polimorf_filename = "noun_polimorf-20240128.tab"
let verb_polimorf_filename = "verb_polimorf-20240128.tab"

(* Test wczytywania słowników i liczenie częstości *)
let _ =
  (* print_endline "sgjp_filename2015";
  let _ = Dict.load_tab (sgjp_path ^ sgjp_filename2015) in
  print_endline "polimorf_filename2015";
  let _ = Dict.load_tab (sgjp_path ^ polimorf_filename2015) in
  print_endline "sgjp_filename201605";
  let _ = Dict.load_tab (sgjp_path ^ sgjp_filename201605) in
  print_endline "polimorf_filename201605";
  let _ = Dict.load_tab (sgjp_path ^ polimorf_filename201605) in
  print_endline "sgjp_filename201607";
  let _ = Dict.load_tab (sgjp_path ^ sgjp_filename201607) in
  print_endline "polimorf_filename201607";
  let _ = Dict.load_tab (sgjp_path ^ polimorf_filename201607) in
  print_endline "sgjp_filename201707";
  let _ = Dict.load_tab (sgjp_path ^ sgjp_filename201707) in
  print_endline "polimorf_filename201707";
  let _ = Dict.load_tab (sgjp_path ^ polimorf_filename201707) in
  print_endline "sgjp_filename";
  let _ = Dict.load_tab (sgjp_path ^ sgjp_filename) in
  print_endline "polimorf_filename";
  let _ = Dict.load_tab (sgjp_path ^ polimorf_filename) in
  print_endline "adv_sgjp_filename";
  let _ = Dict.load_tab (results_path ^ adv_sgjp_filename) in
  print_endline "adj_sgjp_filename";
  let _ = Dict.load_tab (results_path ^ adj_sgjp_filename) in
  print_endline "noun_sgjp_filename";
  let _ = Dict.load_tab (results_path ^ noun_sgjp_filename) in
  print_endline "verb_sgjp_filename";
  let _ = Dict.load_tab (results_path ^ verb_sgjp_filename) in
  print_endline "sgjp_filename2015";
  let dict = Dict.load_tab_full (sgjp_path ^ sgjp_filename2015) in
  Dict.print_quantities "results/proper-type-sgjp-20151020.txt" Dict.proper_type_selector dict;
  Dict.print_quantities "results/genre-sgjp-20151020.txt" Dict.genre_selector dict;
  Dict.print_quantities "results/interp-sgjp-20151020.txt" Dict.interp_selector dict;
  print_endline "polimorf_filename2015";
  let dict = Dict.load_tab_full (sgjp_path ^ polimorf_filename2015) in
  Dict.print_quantities "results/proper-type-polimorf-20151020.txt" Dict.proper_type_selector dict;
  Dict.print_quantities "results/genre-polimorf-20151020.txt" Dict.genre_selector dict;
  Dict.print_quantities "results/interp-polimorf-20151020.txt" Dict.interp_selector dict;
  print_endline "sgjp_filename201605";
  let dict = Dict.load_tab_full (sgjp_path ^ sgjp_filename201605) in
  Dict.print_quantities "results/proper-type-sgjp-20160508.txt" Dict.proper_type_selector dict;
  Dict.print_quantities "results/genre-sgjp-20160508.txt" Dict.genre_selector dict;
  Dict.print_quantities "results/interp-sgjp-20160508.txt" Dict.interp_selector dict;
  print_endline "polimorf_filename201605";
  let dict = Dict.load_tab_full (sgjp_path ^ polimorf_filename201605) in
  Dict.print_quantities "results/proper-type-polimorf-20160508.txt" Dict.proper_type_selector dict;
  Dict.print_quantities "results/genre-polimorf-20160508.txt" Dict.genre_selector dict;
  Dict.print_quantities "results/interp-polimorf-20160508.txt" Dict.interp_selector dict;
  print_endline "sgjp_filename201607";
  let dict = Dict.load_tab_full (sgjp_path ^ sgjp_filename201607) in
  Dict.print_quantities "results/proper-type-sgjp-20160724.txt" Dict.proper_type_selector dict;
  Dict.print_quantities "results/genre-sgjp-20160724.txt" Dict.genre_selector dict;
  Dict.print_quantities "results/interp-sgjp-20160724.txt" Dict.interp_selector dict;
  print_endline "polimorf_filename201607";
  let dict = Dict.load_tab_full (sgjp_path ^ polimorf_filename201607) in
  Dict.print_quantities "results/proper-type-polimorf-20160724.txt" Dict.proper_type_selector dict;
  Dict.print_quantities "results/genre-polimorf-20160724.txt" Dict.genre_selector dict;
  Dict.print_quantities "results/interp-polimorf-20160724.txt" Dict.interp_selector dict;
  print_endline "sgjp_filename201707";
  let dict = Dict.load_tab_full (sgjp_path ^ sgjp_filename201707) in
  Dict.print_quantities "results/proper-type-sgjp-20170730.txt" Dict.proper_type_selector dict;
  Dict.print_quantities "results/genre-sgjp-20170730.txt" Dict.genre_selector dict;
  Dict.print_quantities "results/interp-sgjp-20170730.txt" Dict.interp_selector dict;
  print_endline "polimorf_filename201707";
  let dict = Dict.load_tab_full (sgjp_path ^ polimorf_filename201707) in
  Dict.print_quantities "results/proper-type-polimorf-20170402.txt" Dict.proper_type_selector dict;
  Dict.print_quantities "results/genre-polimorf-20170402.txt" Dict.genre_selector dict;
  Dict.print_quantities "results/interp-polimorf-20170402.txt" Dict.interp_selector dict;
  print_endline "sgjp_filename";
  let dict = Dict.load_tab_full (sgjp_path ^ sgjp_filename) in
(*   Dict.check_lemma_monotonicity "" dict; *) (* NOTE: lematy w słowniku nie są monotoniczne *)
  Dict.print_quantities "results/proper-type-sgjp-20240128.txt" Dict.proper_type_selector dict;
  Dict.print_quantities "results/genre-sgjp-20240128.txt" Dict.genre_selector dict;
  Dict.print_quantities "results/interp-sgjp-20240128.txt" Dict.interp_selector dict;
  print_endline "polimorf_filename";
  let dict = Dict.load_tab_full (sgjp_path ^ polimorf_filename) in
  Dict.print_quantities "results/proper-type-polimorf-20240128.txt" Dict.proper_type_selector dict;
  Dict.print_quantities "results/genre-polimorf-20240128.txt" Dict.genre_selector dict;
  Dict.print_quantities "results/interp-polimorf-20240128.txt" Dict.interp_selector dict;
  print_endline "adv_sgjp_filename";
  let dict = Dict.load_tab_full (results_path ^ adv_sgjp_filename) in
  Dict.check_lemma_monotonicity "" dict;
  print_endline "adj_sgjp_filename";
  let dict = Dict.load_tab_full (results_path ^ adj_sgjp_filename) in
  Dict.check_lemma_monotonicity "" dict;
  print_endline "noun_sgjp_filename";
  let dict = Dict.load_tab_full (results_path ^ noun_sgjp_filename) in
  Dict.check_lemma_monotonicity "" dict;
  print_endline "verb_sgjp_filename";
  let dict = Dict.load_tab_full (results_path ^ verb_sgjp_filename) in
  Dict.check_lemma_monotonicity "" dict;
  print_endline "lematy_nkjp_filename";
  let dict = Dict.load_freq_tab (zasoby_path ^ lematy_nkjp_filename) in
  Dict.print_quantities "results/interp-lematy_nkjp.txt" Dict.interp_selector dict;
  Dict.print_quantities "results/freq-lematy_nkjp.txt" Dict.freq_selector dict;*)
  ()

(* Porównanie wersji słowników *)
let _ =
  (* Dict.compare_dicts_full (sgjp_path ^ sgjp_filename2015) (sgjp_path ^ sgjp_filename201605) "results/comparition_sgjp1_full.out"; *)
  (* Dict.compare_dicts_full (sgjp_path ^ sgjp_filename201605) (sgjp_path ^ sgjp_filename201707) "results/comparition_sgjp2_full.out"; *)
(*   Dict.compare_dicts (sgjp_path ^ sgjp_filename201707) (sgjp_path ^ sgjp_filename) "results/comparition_sgjp3.out"; *)
(*   Dict.compare_dicts (sgjp_path ^ sgjp_filename2022) (sgjp_path ^ sgjp_filename) "results/comparition_sgjp4.out"; *)
  (* Dict.compare_dicts_full (sgjp_path ^ polimorf_filename2015) (sgjp_path ^ polimorf_filename201605) "results/comparition_polimorf1_full.out"; *)
  (* Dict.compare_dicts_full (sgjp_path ^ polimorf_filename201605) (sgjp_path ^ polimorf_filename201707) "results/comparition_polimorf2_full.out"; *)
(*   Dict.compare_dicts (sgjp_path ^ polimorf_filename201707) (sgjp_path ^ polimorf_filename) "results/comparition_polimorf3.out"; *)
  ()

let load_lemmata filename = 
  File.load_tab filename (function
      [lemma; g] -> lemma, g
    | [lemma] -> lemma, ""
    | line -> failwith ("load_lemmata: " ^ String.concat "\t" line))
  
let classify_ndm lemma g =
  let l = Inflexion.synthetize lemma ("subst:_:_:" ^ g) in
(*   Printf.printf "classify_ndm: %s %d\n%!" lemma (Xlist.size l); *)
  let inflected = Xlist.fold l false (fun b t ->
    if t.Inflexion.status = Inflexion.LemmaVal || t.Inflexion.status = Inflexion.LemmaAlt then
      let cat = Inflexion.get_tag t.Inflexion.tags "cat" in
      if cat = "ndm" then b else true      
    else b) in
  let ndm = Xlist.fold l false (fun b t ->
    if t.Inflexion.status = Inflexion.LemmaVal || t.Inflexion.status = Inflexion.LemmaAlt then
      let cat = Inflexion.get_tag t.Inflexion.tags "cat" in
      if cat = "ndm" then true else b      
    else b) in
  inflected, ndm
  
let split_ndm path =
  Inflexion.initialize ();
  let lemmata = load_lemmata (path ^ "lemma_noun_other.tab") in
  let inflected, ndm = Xlist.fold lemmata ([],[]) (fun (inflected,ndm) (lemma,g) ->
    let is_inflected, is_ndm = classify_ndm lemma g in
    (if is_inflected then (lemma,g) :: inflected else inflected), 
    (if is_ndm then (lemma,g) :: ndm else ndm)) in
  File.file_out (path ^ "lemma_noun_other_infl.tab") (fun file ->
    Xlist.iter (Xlist.sort inflected compare) (fun (lemma,g) -> Printf.fprintf file "%s\t%s\n" lemma g));
  File.file_out (path ^ "lemma_noun_other_ndm.tab") (fun file ->
    Xlist.iter (Xlist.sort ndm compare) (fun (lemma,g) -> Printf.fprintf file "%s\t%s\n" lemma g))

(* Podział słownika *)
let _ =
  (* Dict.split_dict sgjp_path sgjp_filename201707 results_path; *)
  (* Dict.split_dict sgjp_path sgjp_filename201607 results_path;  *)
  (* Dict.split_dict sgjp_path sgjp_filename201605 results_path; *)
(*   Dict.split_dict sgjp_path polimorf_filename results_path;   *)
  (* Dict.split_language "data/obce_langs.tab" sgjp_path sgjp_filename results_path; *)
(*  Dict.split_dict sgjp_path sgjp_filename results_path; 
  Dict.split_noun_lemmata sgjp_path sgjp_filename results_path;
  split_ndm results_path; *)
  ()

(* Usunięcie form z prefixami *)
let _ =
  (* Dict.remove_prefix_forms results_path "sgjp_selected.tab" "results/verb_sgjp_no_pref.tab"; *)
  (* Dict.remove_prefix_forms results_path verb_sgjp_filename "results/verb_sgjp_no_pref.tab"; *)
  ()

let test_process_interps path filename =
  let dict = Dict.load_tab_full (path ^ filename) in
  let dict = Dict.merge_entries dict in
  let _ = Dict.process_interps dict in
  ()

(* Test mapowania interpretacji *)
let _ =
  (* test_process_interps results_path adv_sgjp_filename; *)
  (* test_process_interps results_path adj_sgjp_filename; *)
  (* test_process_interps results_path verb_sgjp_filename; *)
  (* test_process_interps results_path noun_sgjp_filename; *)
  (* test_process_interps results_path "lang_all_sgjp-20170730.tab"; *)
  ()


let obce = StringSet.of_list (File.load_lines "../morphology/data/obce.tab")

let find_not_fonetic_parsed ex path filename =
  let dict = Dict.load_tab_full (path ^ filename) in
  let dict = Dict.remove_exceptional_lemmata_gen ex dict in
  Xlist.iter dict (fun e ->
    Xlist.iter e.forms (fun f ->
      if Fonetics.translate false Fonetics.rules f.orth = [] then printf "%s\n%!" e.lemma))

(* Test translacji fonetycznej *)
let _ =
(*   find_not_fonetic_parsed obce results_path adv_sgjp_filename; *)
  (* find_not_fonetic_parsed obce results_path adj_sgjp_filename; *)
  (* find_not_fonetic_parsed obce results_path noun_sgjp_filename; *)
  (* find_not_fonetic_parsed obce results_path verb_sgjp_filename; *)
  (* find_not_fonetic_parsed obce sgjp_path sgjp_filename; *)
  (* find_not_fonetic_parsed obce sgjp_path polimorf_filename; *)
  ()

let test_fonetics path filename =
  let dict = Dict.load_tab_full (path ^ filename) in
  (* let dict = Dict.remove_exceptional_lemmata_gen obce dict in *)
  Xlist.iter dict (fun e ->
    Xlist.iter e.forms (fun f ->
    try
      Fonetics.translate_and_check true Fonetics.core_rules Fonetics.core_rev_rules f.orth
    with
      Fonetics.NotFound(orth,phon) -> printf "NF lemma=%s orth=%s phon=%s\n%!" e.lemma orth phon
    | Fonetics.NotEqual(orth,phon,t) -> printf "NE lemma=%s orth=%s phon=%s: %s\n%!" e.lemma orth phon t
    | Fonetics.MulipleSolutions(orth,phon,l) -> printf "MS lemma=%s orth=%s phon=%s: %s\n%!" e.lemma orth phon (String.concat " " l)))

(* Test translacji fonetycznej *)
let _ =
(*   test_fonetics results_path adv_sgjp_filename; *)
(*   test_fonetics results_path adj_sgjp_filename; *)
  (* test_fonetics results_path noun_sgjp_filename; *)
  (* test_fonetics results_path verb_sgjp_filename; *)
  ()

let _ =
  try
    (* Fonetics.latex_of_core_rules "data/fonetics.dic";
    Fonetics.latex_of_non_core_rules "data/fonetics.dic";
    Fonetics.latex_of_non_core_rev_rules "data/fonetics.dic"; *)
    (* MorphologyRules.latex_of_alternations "../morphology/data/alternations.dic"; *)
    (* MorphologyRules.latex_of_schemata (); *)
    (* MorphologyRules.latex_of_interp_rules (); *)
    (* Dict.latex_of_wyglos "resources/wyglos.tab"; *)
    ()
  with Failure s -> print_endline s

(* Wypisanie nieodmiennych *)
(*let _ =
  let dict = Dict.load_tab_full (sgjp_path ^ sgjp_filename) in
  let dict = Dict.merge_entries dict in
  let dict = Dict.process_interps dict in
  let dict = Dict.mark_ndm dict in
  Dict.print_ndm "results/ndm-sgjp.tab" dict;
  ()*)

let check_stem_generation path filename =
  let dict = Dict.load_tab_full (path ^ filename) in
  let dict = Dict.merge_entries dict in
  let dict = Dict.process_interps dict in
  (* let dict = Dict.mark_ndm dict in *)
  let dict = Dict.remove_exceptional_lemmata dict in
  let dict = Dict.find_kolwiek_suffixes dict in
  let _ = Dict.generate_stem dict in
  ()

(* Sprawdzenie działania stemowania *)
let _ =
  (* check_stem_generation sgjp_path sgjp_filename; *)
  (* check_stem_generation results_path adj_sgjp_filename; *)
  (* check_stem_generation results_path noun_sgjp_filename; *)
  (* check_stem_generation results_path noun_polimorf_filename; *)
  ()

(* let _ =  print_endline "1" *)
let compound_rules = MorphologyRules.make_compound_rules ()
(* let _ =  print_endline "2" *)
let compound_rule_trees = MorphologyRules.make_compound_rule_trees compound_rules
(* let _ =  print_endline "3" *)
let interp_compound_rule_trees = MorphologyRules.make_interp_compound_rule_trees compound_rules
(* let _ =  print_endline "4" *)

(* let _ = MorphologyRules.CharTrees.print_rules "results/rules_tree.txt" compound_rule_trees *)

let find_not_validated_forms rules path filename out_filename =
  ignore (Sys.command ("rm -f " ^ out_filename));
  Dict.iter_tab (path ^ filename) 100000 (fun dict ->
(*   let dict = Dict.load_tab (path ^ filename) in *)
    let dict = Dict.assign_entry_cat dict in
    let dict = Dict.phon_validate rules dict in
    let dict = Dict.remove_validated_forms dict in
    Dict.print_append out_filename dict)

let find_not_validated_entries rules path filename out_filename =
  ignore (Sys.command ("rm -f " ^ out_filename));
  Dict.iter_tab (path ^ filename) 100000 (fun dict ->
(*     let dict = Dict.load_tab (path ^ filename) in *)
    let dict = Dict.merge_entries dict in
    let dict = Dict.process_interps dict in
  (* let dict = Dict.mark_ndm dict in
  let dict = Dict.remove_ndm dict in *)
  (* let dict = Dict.remove_exceptional_lemmata dict in *)
    let dict = Dict.phon_validate rules dict in
    let dict = Dict.remove_validated_entries dict in
    Dict.print_append out_filename dict)

(* Wypisanie niezwalidowanych form *)
let _ =
(*   find_not_validated_forms compound_rule_trees results_path adj_sgjp_filename "results/not_validated_adj2.tab"; *)
  (* find_not_validated_entries compound_rule_trees results_path adj_sgjp_filename "results/not_validated_adj.tab"; *)
(*    find_not_validated_entries compound_rule_trees results_path adj_polimorf_filename "results/not_validated_p_adj.tab";  *)
  (* find_not_validated_entries compound_rule_trees "results/" "not_validated_adj.tab" "results/not_validated_adj2.tab"; *)
(*   find_not_validated_entries compound_rule_trees results_path adv_sgjp_filename "results/not_validated_adv.tab"; *)
(*    find_not_validated_entries compound_rule_trees results_path adv_polimorf_filename "results/not_validated_p_adv.tab";  *)
(*  find_not_validated_entries compound_rule_trees results_path noun_sgjp_filename "results/not_validated_noun.tab";
  find_not_validated_forms compound_rule_trees results_path noun_sgjp_filename "results/not_validated_noun2.tab";*)
(*  find_not_validated_entries compound_rule_trees results_path noun_polimorf_filename "results/not_validated_p_noun.tab";
  find_not_validated_forms compound_rule_trees results_path noun_polimorf_filename "results/not_validated_p_noun2.tab"; *)
(*    find_not_validated_entries compound_rule_trees results_path "not_validated_noun.tab" "results/not_validated_noun3.tab";  
   find_not_validated_forms compound_rule_trees results_path "not_validated_noun.tab" "results/not_validated_noun4.tab"; *)
  (* find_not_validated_entries compound_rule_trees results_path verb_sgjp_filename "results/not_validated_verb.tab"; *)
(*   find_not_validated_forms compound_rule_trees results_path verb_sgjp_filename "results/not_validated_verb2.tab"; *)
  (*find_not_validated_entries compound_rule_trees results_path verb_polimorf_filename "results/not_validated_p_verb.tab";
  find_not_validated_forms compound_rule_trees results_path verb_polimorf_filename "results/not_validated_p_verb2.tab"; *)
  (* find_not_validated_forms compound_rule_trees results_path "sgjp_selected.tab" "results/not_validated_verb.tab"; *)
  (* find_not_validated_forms compound_rule_trees results_path "lang_en_sgjp-20170730.tab" "results/lang_en.tab"; *)
  (* find_not_validated_forms compound_rule_trees results_path "lang_fr_sgjp-20170730.tab" "results/lang_fr.tab"; *)
  (* find_not_validated_forms compound_rule_trees results_path "lang_de_sgjp-20170730.tab" "results/lang_de.tab"; *)
  (* find_not_validated_forms compound_rule_trees results_path "lang_acro_sgjp-20170730.tab" "results/lang_acro.tab"; *)
  (* find_not_validated_forms compound_rule_trees results_path "lang_la_sgjp-20170730.tab" "results/lang_la.tab"; *)
  (* find_not_validated_forms compound_rule_trees results_path "lang_es_sgjp-20170730.tab" "results/lang_es.tab"; *)
  (* find_not_validated_forms compound_rule_trees results_path "lang_all_sgjp-20170730.tab" "results/lang_all.tab"; *)
  (* find_not_validated_entries compound_rule_trees results_path "lang_all_sgjp-20170730.tab" "results/lang_all.tab"; *)
  (* find_not_validated_entries compound_rule_trees results_path noun_polimorf_filename "results/not_validated_p_noun.tab"; *)
  (* find_not_validated_forms compound_rule_trees results_path "not_validated_p_noun.tab" "results/not_validated_p_noun2.tab"; *)
  (* find_not_validated_forms compound_rule_trees "data/" "noun-supplement-polimorf.tab" "results/not_validated_sup_noun.tab"; *)
  ()

let find_not_validated_lemmata rules path filename out_filename =
  let dict = Dict.load_tab (path ^ filename) in
  let dict = Dict.merge_entries dict in
  let dict = Dict.process_interps dict in
  let dict = Dict.remove_exceptional_lemmata dict in
  let dict = Dict.phon_validate rules dict in
  let dict = Dict.remove_validated_entries dict in
  Dict.print_lemmata out_filename dict

(* Wypisanie niezwalidowanych lematów *)
let _ =
(*    find_not_validated_lemmata compound_rule_trees results_path "not_validated_noun.tab" "results/not_validated_noun_lemma.tab";  *)
  (* find_not_validated_lemmata compound_rule_trees results_path "not_validated_verb.tab" "results/not_validated_verb_lemma.tab";  *)
  (* find_not_validated_lemmata compound_rule_trees results_path "sgjp_selected.tab" "results/not_validated_verb_lemma.tab";   *)
  ()

let find_validated_lemmata rules path filename out_filename =
  let dict = Dict.load_tab (path ^ filename) in
  let dict = Dict.merge_entries dict in
  let dict = Dict.process_interps dict in
  let dict = Dict.phon_validate rules dict in
  let dict = Dict.remove_not_validated_entries dict in
  Dict.print_lemmata out_filename dict

(* Wypisanie zwalidowanych lematów *)
let _ =
  (* find_validated_lemmata compound_rule_trees results_path adj_sgjp_filename "results/validated_adj.tab";
  find_validated_lemmata compound_rule_trees results_path noun_sgjp_filename "results/validated_noun.tab";
  find_validated_lemmata compound_rule_trees results_path verb_sgjp_filename "results/validated_verb.tab"; *)
  ()

let test_lemmatize lemma orth =
  printf "test_lemmatize: %s %s\n%!" lemma orth;
  let simple_lemma = Stem.simplify_lemma lemma in
  let phon_orths = Fonetics.translate true Fonetics.core_rules orth in
  printf "phon_orths: \n  %s\n%!" (String.concat "\n  " (Xlist.map phon_orths Fonetics.string_of_phon));
  Xlist.iter phon_orths (fun phon_orth ->
    Xlist.iter (MorphologyRules.CharTrees.find compound_rule_trees phon_orth.phon) (fun (stem,rule) ->
      let candidate_lemmas = Fonetics.translate_simple true Fonetics.core_rev_rules (stem ^ rule.set) in
      let candidate_lemmas = if candidate_lemmas = [] then ["???"] else candidate_lemmas in
      Xlist.iter candidate_lemmas (fun candidate_lemma ->
      if candidate_lemma = simple_lemma then printf "E" else printf " ";
      printf " %s %s %s %s\n%!" phon_orth.phon stem (string_of_rule rule) candidate_lemma)))

(* Sprawdzenie przebiegu lematyzacji *)
let _ =
(*   Xlist.iter compound_rules (fun rule -> print_endline (string_of_rule rule)); *)
(*  test_lemmatize "duszy" "dusi";
  test_lemmatize "duży" "duzi";*)
(*  test_lemmatize "gwiazda" "gwieździe"; 
  test_lemmatize "lisi" "lisi";*)
  (* test_lemmatize "delegacja" "delegacyj"; *)
  (* test_lemmatize "delegacja" "delegacji"; *)
  (* test_lemmatize "stodoła" "stodole"; *)
  (* test_lemmatize "komedia" "komedii"; *)
  (* test_lemmatize "Zenobia" "Zenobij";
  test_lemmatize "aerografia" "aerografij";
  test_lemmatize "Oktawia" "Oktawij";
  test_lemmatize "Olimpia" "Olimpij"; *)
  (* test_lemmatize "Amati" "Amatiego"; *)
  (* test_lemmatize "plemię" "plemieniu"; *)
  (* test_lemmatize "Abisynia" "Abisynii";
  test_lemmatize "Abisynia" "Abisynią";
  test_lemmatize "Abisynia" "Abisynij"; *)
  (* test_lemmatize "kokosz" "kokosze"; *)
  (* test_lemmatize "Kempisti" "Kempisty"; *)
  (* test_lemmatize "witarianin" "witariany"; *)
  (* test_lemmatize "witarianin" "witarianom";
  test_lemmatize "witarianin" "witarianów"; *)
  (* test_lemmatize "konsumpcjonizm" "konsumpcjoniźmie"; *)
(*  test_lemmatize "Dziadoszanin" "Dziadoszanom";
  test_lemmatize "Polanin" "Polanom";*)
(*   test_lemmatize "kuchnia" "kuchen"; *)
(*   test_lemmatize "Miedzno" "Miedźnie"; *)
(*  test_lemmatize "" "";
  test_lemmatize "" "";
  test_lemmatize "" "";*)
  (* test_lemmatize "Czuwaszja" "Czuwaszyj"; *)
  (* test_lemmatize "donieść" "doniesiona"; *)
  (* test_lemmatize "zlodzić" "zlodzona"; *)
  (* test_lemmatize "ciąć" "tniemy";
  test_lemmatize "prać" "pierzemy";
  test_lemmatize "myć" "myjemy";
  test_lemmatize "obuć" "obujemy"; *)
  (* test_lemmatize "bać" "boimy";
  test_lemmatize "stać" "stoimy";
  test_lemmatize "szczać" "szczymy";
  test_lemmatize "amnestiować" "amnestiujemy"; *)
  (* test_lemmatize "powziąć" "poweźmiemy";
  test_lemmatize "dowiedzieć" "dowiemy";
  test_lemmatize "dobyć" "dobędziemy"; *)
  (* test_lemmatize "nadeżreć" "nadżarto";
  test_lemmatize "pleść" "pleciono"; *)
  (* test_lemmatize "głaskać" "głaszczmy";
  test_lemmatize "chlastać" "chlaszczmy";
  test_lemmatize "mamleć" "mamlajmy";
  test_lemmatize "ostać" "ostańmy";
  test_lemmatize "dopowiedzieć" "dopowiedzmy"; *)
  (* test_lemmatize "nazywać" "nazywoł";
  test_lemmatize "Danja" "Danji";
  test_lemmatize "Kolej" "Koleji";
  test_lemmatize "rozumiem" "rozumicie"; *)
  (* test_lemmatize "podesłać" "podścielemy"; *)
  (* test_lemmatize "izolować" "izolował"; *)
  (* test_lemmatize "IKEA" "IKE-ach";
  test_lemmatize "IKEA" "IKE-ę";
  test_lemmatize "IKEA" "IKE-i";
  test_lemmatize "ZUS" "ZUS-u";
  test_lemmatize "ZUS" "ZUS-ie";
  test_lemmatize "WAT" "WA-cie"; *)
  (* test_lemmatize "ICJ" "ICJ-ocie";
  test_lemmatize "ICJ" "ICJ-otach"; *)
  (* test_lemmatize "WAT" "Wacie";
  test_lemmatize "WAT" "WACIE";
  test_lemmatize "WAT" "WAcie"; *)
  (* test_lemmatize "BOŚ" "BOŚ-u"; *)
  (* test_lemmatize "upgrade" "upgrade’om"; *)
  (* test_lemmatize "software" "software’y"; *)
  (* test_lemmatize "spray" "sprayu";  *)
  (* test_lemmatize "unixowość" "unixowościach";   *)
  (* test_lemmatize "rolls-royce" "rolls-roysie"; *)
  (* test_lemmatize "Arrow" "Arrowa";*)
  (* test_lemmatize "Boy" "Boyowie"; *)
  (* test_lemmatize "Chomsky" "Chomskiego"; *)
  (* test_lemmatize "Bradbury" "Bradburych"; *)
  (* test_lemmatize "compact" "compakcie";
  test_lemmatize "Dixa" "Diksie"; *)
  (* test_lemmatize "developerski" "developersko"; *)
  (* test_lemmatize "Jessica" "Jessice"; *)
  (* test_lemmatize "Gurkha" "Gurce";
  test_lemmatize "Gurkha" "Gurkhi"; *)
  (* test_lemmatize "heavymetalowiec" "heavymetalowca"; *)
  (* test_lemmatize "niesoftware’owy" "niesoftware’owi"; *)
  (* test_lemmatize "Barrès" "Barrès’go"; *)
  (* test_lemmatize "Dolphy" "Dolphy’ego"; *)
  (* test_lemmatize "Bernoulli" "Bernoulliego"; *)
  (* test_lemmatize "Handke" "Handkami"; *)
  (* test_lemmatize "Radetzky" "Radetzky’ego"; *)
  (* test_lemmatize "Max" "Maksa"; *)
  (* test_lemmatize "Montesquieu" "Montesquieugo"; *)
  (* test_lemmatize "Depardieu" "Depardieugo"; *)
  (* test_lemmatize "Java" "Javie"; *)
  (* test_lemmatize "anglaise" "anglaise’a"; *)
  (* test_lemmatize "Aristide" "Aristide’a"; *)
  (* test_lemmatize "Barrès" "Barrès’go"; *)
  (* test_lemmatize "Beaumarchais" "Beaumarchais’go"; *)
  (* test_lemmatize "Beauvoir" "Beauvoira"; *)
  (* test_lemmatize "Bernoulli" "Bernoulliego"; *)
  (* test_lemmatize "Astaire" "Astaire’a"; *)
  (* test_lemmatize "Avignon" "Avignonami"; *)
  (* test_lemmatize "Benveniste" "Benveniste’a"; *)
  (* test_lemmatize "Kayah" "Kai"; *)
  (* test_lemmatize "jockey" "jockei"; *)
  (* test_lemmatize "Radetzky" "Radetzky’ego"; *)
  (* test_lemmatize "bonvivant" "bonvivantach"; *)
  (* test_lemmatize "Lefebvre" "Lefebvre’a"; *)
  (* test_lemmatize "Kayah" "Kayom"; *)
  (* test_lemmatize "Dixa" "Diksie"; *)
  (* test_lemmatize "White" "Whicie";
  test_lemmatize "Voit" "Voicie";
  test_lemmatize "Violeta" "Violecie";
  test_lemmatize "veto" "vecie"; *)
  (* test_lemmatize "Andrássy" "Andrássyowie"; *)
  (* test_lemmatize "Cezanne" "Cezanne’ami";
  test_lemmatize "Connery" "Connery’ego"; *)
  (* test_lemmatize "Barrés" "Barrés’go"; *)
  (* test_lemmatize "IKEA" "IKE-i"; *)
  (* test_lemmatize "mix" "miksowi";
  test_lemmatize "Laxa" "Laksie"; *)
  (* test_lemmatize "münsterski" "münstersku"; *)
  (* test_lemmatize "würzburski" "würzburskiemu"; *)
  (* test_lemmatize "polje" "poljom"; *)
  (* test_lemmatize "drivie" "drive"; *)
(*  test_lemmatize "Korčula" "Korčulę"; (* FIXME: problem z tym, że č jest notacją dla cz *)
  test_lemmatize "Korczula" "Korczulę";
  test_lemmatize "Kortchula" "Kortchulę";*)
  (* test_lemmatize "" "";
  test_lemmatize "" "";
  test_lemmatize "" "";  *)
  ()

  
(* Generowanie reguł *)
let _ =
  (* Dict.generate_rules compound_rule_trees results_path adj_sgjp_filename "results/rules-odm-adj.txt"; *)
  (* Dict.generate_rules compound_rule_trees results_path noun_sgjp_filename "results/rules-odm-noun.txt"; *)
  (* Dict.generate_rules compound_rule_trees results_path adv_sgjp_filename "results/rules-adv.txt"; *)
  (* Dict.generate_rules compound_rule_trees results_path verb_sgjp_filename "results/rules-verb.txt"; *)
  (* Dict.generate_rules compound_rule_trees results_path "test.tab" "results/rules-test.txt"; *)
  (* Dict.generate_rules compound_rule_trees results_path "not_validated_noun.tab" "results/rules-nv-noun.txt"; *)
  (* Dict.generate_rules compound_rule_trees results_path "not_validated_p_noun.tab" "results/rules-nv-noun.txt"; *)
  (* Dict.generate_rules compound_rule_trees results_path "not_validated_verb.tab" "results/rules-nv-verb.txt"; *)
  (* Dict.generate_rules compound_rule_trees results_path "sgjp_selected.tab" "results/rules-selected.txt"; *)
  (* Dict.generate_rules compound_rule_trees results_path "verb_sgjp_no_pref.tab" "results/rules-verb.txt"; *)
  (* Dict.generate_rules compound_rule_trees results_path "selected_verb.tab" "results/rules-verb.txt"; *)
  (* Dict.generate_rules_lu compound_rule_trees 142 lu_path "results/rules-142_lu.txt"; *)
  (* Dict.generate_rules_lu compound_rule_trees 148 lu_path "results/rules-148_lu.txt"; *)
  (* Dict.generate_rules_lu compound_rule_trees 42 lu_path "results/rules-42_lu.txt"; *)
  (* ignore (Sys.command "totem ~/Dokumenty/Inne/gong/gong_05m_00s.ogg"); *)
  ()

let find_interp_validated_lemmata interp_rules path filename out_filename =
  let dict = Dict.load_tab (path ^ filename) in
  let dict = Dict.merge_entries dict in
  let dict = Dict.process_interps dict in
  (* let dict = Dict.mark_ndm dict in (* FIXME: remove_ndm? *) *)
  (* let dict = Dict.remove_exceptional_lemmata dict in *)
  (* let dict = find_kolwiek_suffixes dict in *)
  (* let dict = generate_stem dict in *)
  let dict = Dict.phon_validate_interp interp_rules dict in
  let dict = Dict.remove_not_validated_entries dict in
  Dict.print_lemmata out_filename dict

let find_not_interp_validated_lemmata interp_rules path filename out_filename =
  let dict = Dict.load_tab (path ^ filename) in
  let dict = Dict.merge_entries dict in
  let dict = Dict.process_interps dict in
  (* let dict = Dict.mark_ndm dict in (* FIXME: remove_ndm? *) *)
  (* let dict = Dict.remove_exceptional_lemmata dict in *)
  (* let dict = find_kolwiek_suffixes dict in *)
  (* let dict = generate_stem dict in *)
  let dict = Dict.phon_validate_interp interp_rules dict in
  let dict = Dict.remove_validated_entries dict in
  Dict.print_lemmata out_filename dict

let find_not_interp_validated_entries interp_rules path filename out_filename =
  let dict = Dict.load_tab (path ^ filename) in
  let dict = Dict.merge_entries dict in
  let dict = Dict.process_interps dict in
  (* let dict = Dict.mark_ndm dict in
  let dict = Dict.remove_ndm dict in *)
  let dict = Dict.phon_validate_interp interp_rules dict in
  let dict = Dict.remove_validated_entries dict in
  Dict.print out_filename dict

let find_not_interp_validated_forms interp_rules path filename out_filename =
  let dict = Dict.load_tab (path ^ filename) in
  let dict = Dict.merge_entries dict in
  let dict = Dict.process_interps dict in
  let dict = Dict.remove_exceptional_lemmata dict in
  let dict = Dict.phon_validate_interp interp_rules dict in
  let dict = Dict.remove_validated_forms dict in
  Dict.print out_filename dict

(* Wypisanie lematów ze zwalidowaną interpretacją *)
let _ =
  (* find_interp_validated_lemmata interp_compound_rule_trees results_path noun_sgjp_filename "results/interp_validated_noun.tab";
  find_not_interp_validated_lemmata interp_compound_rule_trees results_path noun_sgjp_filename "results/interp_not_validated_noun.tab";
  find_interp_validated_lemmata interp_compound_rule_trees results_path adj_sgjp_filename "results/interp_validated_adj.tab";
  find_not_interp_validated_lemmata interp_compound_rule_trees results_path adj_sgjp_filename "results/interp_not_validated_adj.tab";
  find_interp_validated_lemmata interp_compound_rule_trees results_path adv_sgjp_filename "results/interp_validated_adv.tab";
  find_not_interp_validated_lemmata interp_compound_rule_trees results_path adv_sgjp_filename "results/interp_not_validated_adv.tab";
  find_interp_validated_lemmata interp_compound_rule_trees results_path verb_sgjp_filename "results/interp_validated_verb.tab";
  find_not_interp_validated_lemmata interp_compound_rule_trees results_path verb_sgjp_filename "results/interp_not_validated_verb.tab";
  find_interp_validated_lemmata interp_compound_rule_trees results_path "verb_sgjp_no_pref.tab" "results/interp_validated_no_pref_verb.tab";
  find_not_interp_validated_lemmata interp_compound_rule_trees results_path "verb_sgjp_no_pref.tab" "results/interp_not_validated_no_pref_verb.tab"; *)
  (* find_interp_validated_lemmata interp_compound_rule_trees results_path "sgjp_selected.tab" "results/interp_validated_verb.tab"; *)
  (* find_not_interp_validated_lemmata interp_compound_rule_trees results_path "sgjp_selected.tab" "results/interp_not_validated_verb.tab"; *)
  (* find_not_interp_validated_entries interp_compound_rule_trees results_path verb_sgjp_filename "results/selected_verb.tab"; *)
  (* find_not_interp_validated_entries interp_compound_rule_trees results_path "verb_sgjp_no_pref.tab" "results/selected_verb.tab"; *)
  (* find_not_interp_validated_forms interp_compound_rule_trees results_path "lang_en_sgjp-20170730.tab" "results/lang_en.tab"; *)
  (* find_not_interp_validated_forms interp_compound_rule_trees results_path "lang_fr_sgjp-20170730.tab" "results/lang_fr.tab"; *)
  (* find_not_interp_validated_forms interp_compound_rule_trees results_path "lang_de_sgjp-20170730.tab" "results/lang_de.tab"; *)
  (* find_not_interp_validated_forms interp_compound_rule_trees results_path "lang_acro_sgjp-20170730.tab" "results/lang_acro.tab"; *)
  (* find_not_interp_validated_forms interp_compound_rule_trees results_path "lang_all_sgjp-20170730.tab" "results/lang_all.tab"; *)
  (* find_not_interp_validated_entries interp_compound_rule_trees results_path "lang_all_sgjp-20170730.tab" "results/interp_not_validated_lang_all.tab"; *)
  (* find_not_interp_validated_forms interp_compound_rule_trees "data/" "noun-supplement-polimorf.tab" "results/not_validated_sup_noun2.tab"; *)
  ()

let test_interp_lemmatize lemma orth =
  printf "test_interp_lemmatize: %s %s\n%!" lemma orth;
  let entry = {empty_entry with lemma=lemma; cat="noun"; forms=[{empty_form with orth=orth}]} in
  let entry = Dict.create_candidates false interp_compound_rule_trees entry in
  Xlist.iter entry.forms (fun f ->
    printf "phon_orths: \n  %s\n%!" (String.concat "\n  " (Xlist.map f.phon_orth Fonetics.string_of_phon));
    Xlist.iter f.candidates (fun (stem,rule,s,t) ->
      printf "%s %s %s %s\n%!" s.phon stem (string_of_rule rule) rule.interp))

let _ =
  (* test_interp_lemmatize "Benveniste" "Benveniście"; *)
  (* test_interp_lemmatize "allemande" "allemandzie"; *)
  (* test_interp_lemmatize "Depardieu" "Depardieuch"; *)
  (* test_interp_lemmatize "Braille" "Braille’u"; *)
  (* test_interp_lemmatize "FAMA" "FAMA"; *)
  (* test_interp_lemmatize "lichy" "lichego"; *)
  (* test_interp_lemmatize "niekaraluszy" "niekaraluszych"; *)
  (* test_interp_lemmatize "ninja" "ninjami"; *)
  (* test_interp_lemmatize "ninja" "ninji"; *)
  (* test_interp_lemmatize "mix" "miksowi"; *)
  (* test_interp_lemmatize "münsterski" "münstersku"; *)
  (* test_interp_lemmatize "Trubieckoj" "Trubieckich"; *)
  (* test_interp_lemmatize "Balazs" "Balazsu"; *)
  (* test_interp_lemmatize "Blake" "Blakiem";
  test_interp_lemmatize "Bogorodckij" "Bogorodckiego";
  test_interp_lemmatize "BUW" "BUW-ie";
  test_interp_lemmatize "Bush" "Bushe"; *)
  (* test_interp_lemmatize "ensemble" "ensemblowi";
  test_interp_lemmatize "Anouilh" "Anouilhe";
  test_interp_lemmatize "Bandtkie" "Bandtkimi";*)
  (* test_interp_lemmatize "Jokai" "Jokaiemu"; *)
(*  test_interp_lemmatize "Joszua" "Joszui";
  test_interp_lemmatize "Linde" "Lindymi";
  test_interp_lemmatize "drive" "drivie"; *)
  (* test_interp_lemmatize "FPŻ" "FPŻ-y"; *)
  (* test_interp_lemmatize "Jacques" "Jaki"; *)
  (*test_interp_lemmatize "" "";
  test_interp_lemmatize "" "";
  test_interp_lemmatize "" "";*)
  ()

(* Generowanie reguł dla interpretacji *)
let _ =
  (* Dict.generate_interp_rules compound_rule_trees interp_compound_rule_trees ["pref";"cat";"flex";"lemma"] results_path adv_sgjp_filename "results/interp_rules_adv.tab"; *)
  (* Dict.generate_interp_rules compound_rule_trees interp_compound_rule_trees ["pref";"cat";"flex";"lemma"] results_path adj_sgjp_filename "results/interp_rules_adj.tab"; *)
  (* Dict.generate_interp_rules compound_rule_trees interp_compound_rule_trees ["cat";"flex";"lemma";"palat";"velar"] sgjp_path "sgjp_selected.tab" "results/interp_rules_s.tab"; *)
  (* Dict.generate_interp_rules compound_rule_trees interp_compound_rule_trees ["cat";"flex";"lemma"] results_path "sgjp_selected.tab" "results/interp_rules_s2.tab"; *)
  (* Dict.generate_interp_rules compound_rule_trees interp_compound_rule_trees ["cat";"flex";"lemma";"palat";"velar";"con"] results_path noun_sgjp_filename "results/interp_rules_noun4.tab"; *)
  (* Dict.generate_interp_rules compound_rule_trees interp_compound_rule_trees ["cat";"flex";"lemma";"palat";"velar"] results_path noun_sgjp_filename "results/interp_rules_noun3.tab"; *)
  (* Dict.generate_interp_rules compound_rule_trees interp_compound_rule_trees ["cat";"flex";"lemma";"palat"] results_path noun_sgjp_filename "results/interp_rules_noun2.tab"; *)
  (* Dict.generate_interp_rules compound_rule_trees interp_compound_rule_trees ["cat";"flex";"lemma"] results_path noun_sgjp_filename "results/interp_rules_noun1.tab"; *)
  (* Dict.generate_interp_rules compound_rule_trees interp_compound_rule_trees ["pref";"cat";"flex";"flex2";"group";"lemma"] results_path "sgjp_selected.tab" "results/interp_rules_verb.tab"; *)
  (* Dict.generate_interp_rules compound_rule_trees interp_compound_rule_trees ["pref";"cat";"flex";"flex2";"group";"lemma"] results_path "verb_sgjp_no_pref.tab" "results/interp_rules_verb.tab"; *)
  (* Dict.generate_interp_rules compound_rule_trees interp_compound_rule_trees ["pref";"cat";"flex";"flex2";"group";"lemma"] results_path verb_sgjp_filename "results/interp_rules_verb.tab"; *)
  (* Dict.generate_interp_rules compound_rule_trees interp_compound_rule_trees ["pref";"cat";"flex";"flex2"] results_path verb_sgjp_filename "results/interp_rules_verb2.tab"; *)
  (* ignore (Sys.command "totem ~/Dokumenty/Inne/gong/gong_00m_30s.ogg"); *)
  (* ignore (Sys.command "totem ~/\"Muzyka/Era/Era 1998 Era/Era 02 Ameno (remix).ogg\""); *)
  ()

  (* Printexc.record_backtrace true;
  (try *)
  (* with e -> Printexc.print_backtrace stdout; print_endline (Printexc.to_string e));   *)

let sources = [
  sgjp_path, sgjp_filename;
  "data/", "noun-supplement-acro.tab";
  "data/", "noun-supplement-polimorf.tab";
  "data/", "dial_ach.tab";
  "data/", "dial_ami2.tab";
  "data/", "dial_ami3.tab";
  "data/", "dial_ami4.tab";
  "data/", "dial_ami.tab";
  "data/", "dial_ą2.tab";
  "data/", "dial_ą.tab";
  "data/", "dial_ę.tab";
  "data/", "dial_my.tab";
  "data/", "dial_sz.tab";
  "data/", "dial_ym.tab";
  ]

(* Generowanie złożonych reguł zaopatrzonych we frekwencje *)
let _ =
  (* Dict.generate_rule_frequencies interp_compound_rule_trees results_path "verb_sgjp_no_pref.tab" "results/freq_rules-verb.tab"; *)
  (* Dict.generate_rule_frequencies interp_compound_rule_trees results_path adv_sgjp_filename "results/freq_rules-adv.tab"; *)
  (* Dict.generate_rule_frequencies interp_compound_rule_trees results_path adj_sgjp_filename "results/freq_rules-adj.tab"; *)
  (* Dict.generate_rule_frequencies interp_compound_rule_trees results_path noun_sgjp_filename "results/freq_rules-noun.tab"; *)
  (* Dict.generate_rule_frequencies interp_compound_rule_trees results_path verb_sgjp_filename "results/freq_rules-verb.tab"; *)
  (* Dict.generate_rule_frequencies interp_compound_rule_trees results_path "lang_all_sgjp-20170730.tab" "results/freq_rules-lang.tab"; *)
  (* Dict.generate_rule_frequencies interp_compound_rule_trees results_path "lang_jaki.tab" "results/freq_rules-jaki.tab"; *)
  (* Dict.generate_rule_frequencies interp_compound_rule_trees sgjp_path sgjp_filename "results/freq_rules.tab"; *)
  (* Dict.generate_rule_frequencies interp_compound_rule_trees results_path "noun_zmiekczenie.tab" "results/freq_rules-zmiekczenie.tab"; *)
  (* Dict.generate_rule_frequencies interp_compound_rule_trees "data/" "noun-supplement-polimorf.tab" "results/freq_rules-supplement-polimorf.tab"; *)
  (* Dict.generate_rule_frequencies interp_compound_rule_trees "data/" "noun-supplement-acro.tab" "results/freq_rules-supplement-acro.tab"; *)
  (* Dict.generate_rule_frequencies interp_compound_rule_trees "data/" "dial_ach.tab" "results/freq_rules-dial_ach.tab"; *)
  (* Dict.generate_rule_frequencies_list interp_compound_rule_trees sources "results/freq_rules_all.tab"; *)
  (* ignore (Sys.command "totem ~/Dokumenty/Inne/gong/gong_05m_00s.ogg"); *)
  ()

let test_freq_rules_attributes rules_filename =
  let rules = MorphologyRules.load_freq_rules rules_filename in
  let map = Xlist.fold rules StringMap.empty (fun map r ->
    StringMap.add_inc map (r.pref ^ " " ^ r.find) [r] (fun l -> r :: l)) in
  StringMap.iter map (fun _ rules ->
    let map = Xlist.fold rules StringMap.empty (fun map r ->
      let k = MorphologyRules.string_of_star r.star ^ " " ^ String.concat " " (List.sort compare (Xlist.map r.tags (fun (k,v) -> k ^ "=" ^ v))) ^ " " ^ r.interp in
      StringMap.add_inc map k [r] (fun l -> r :: l)) in
    StringMap.iter map (fun _ rules ->
      if Xlist.size rules > 1 then
        Xlist.iter rules (fun r -> print_endline (MorphologyRules.string_of_freq_rule r))))

let _ =
  (* test_freq_rules_attributes "results/freq_rules_all.tab"; *)
  (* test_freq_rules_attributes "resources/freq_rules.tab"; *)
  ()

let generate_alt rules_filename path filename out_filename =
  let rules = MorphologyRules.load_freq_rules rules_filename in
  let rules = MorphologyRules.CharTrees.create rules in
  let dict = Dict.load_tab (path ^ filename) in
  let dict = Dict.merge_entries dict in
  let dict = Dict.process_interps dict in
  let dict = Dict.remove_cat "cond" dict in
  (* let dict = Dict.mark_ndm dict in *)
  let dict = Dict.validate_interp rules dict in
  let dict = Dict.remove_validated_forms dict in
  Dict.print out_filename dict

let generate_alt_translate rules_filename path filename out_filename =
  let rules = MorphologyRules.load_freq_rules rules_filename in
  let rules = MorphologyRules.CharTrees.create rules in
  let dict = Dict.load_tab (path ^ filename) in
  let dict = Dict.merge_entries dict in
  (* let dict = Dict.process_interps dict in *)
  let dict = Dict.process_polimorf_interps dict in
  let dict = Dict.remove_cat "cond" dict in
  (* let dict = Dict.mark_ndm dict in *)
  let dict = Dict.validate_interp_translate rules dict in
  let dict = Dict.remove_validated_forms dict in
  Dict.print out_filename dict

let find_freq_validated_lemmata rules_filename path filename out_filename =
  let rules = MorphologyRules.load_freq_rules rules_filename in
  let rules = MorphologyRules.CharTrees.create rules in
  let dict = Dict.load_tab (path ^ filename) in
  let dict = Dict.merge_entries dict in
  let dict = Dict.process_interps dict in
  let dict = Dict.remove_cat "cond" dict in
  let dict = Dict.validate_interp rules dict in
  let dict = Dict.remove_not_validated_entries dict in
  Dict.print_lemmata out_filename dict

let find_not_freq_validated_lemmata rules_filename path filename out_filename =
  let rules = MorphologyRules.load_freq_rules rules_filename in
  let rules = MorphologyRules.CharTrees.create rules in
  let dict = Dict.load_tab (path ^ filename) in
  let dict = Dict.merge_entries dict in
  let dict = Dict.process_interps dict in
  let dict = Dict.remove_cat "cond" dict in
  let dict = Dict.validate_interp rules dict in
  let dict = Dict.remove_validated_entries dict in
  Dict.print_lemmata out_filename dict

(* Walidacja reguł zaopatrznych we frekwencje/generowanie listy wyjątków *)
let _ =
  (* find_freq_validated_lemmata "results/freq_rules_all.tab" results_path noun_sgjp_filename "results/freq_validated_noun.tab";
  find_not_freq_validated_lemmata "results/freq_rules_all.tab" results_path noun_sgjp_filename "results/freq_not_validated_noun.tab";
  find_freq_validated_lemmata "results/freq_rules_all.tab" results_path adj_sgjp_filename "results/freq_validated_adj.tab";
  find_not_freq_validated_lemmata "results/freq_rules_all.tab" results_path adj_sgjp_filename "results/freq_not_validated_adj.tab";
  find_freq_validated_lemmata "results/freq_rules_all.tab" results_path adv_sgjp_filename "results/freq_validated_adv.tab";
  find_not_freq_validated_lemmata "results/freq_rules_all.tab" results_path adv_sgjp_filename "results/freq_not_validated_adv.tab";
  find_freq_validated_lemmata "results/freq_rules_all.tab" results_path verb_sgjp_filename "results/freq_validated_verb.tab";
  find_not_freq_validated_lemmata "results/freq_rules_all.tab" results_path verb_sgjp_filename "results/freq_not_validated_verb.tab";
  find_freq_validated_lemmata "results/freq_rules_all.tab" results_path "verb_sgjp_no_pref.tab" "results/freq_validated_no_pref_verb.tab";
  find_not_freq_validated_lemmata "results/freq_rules_all.tab" results_path "verb_sgjp_no_pref.tab" "results/freq_not_validated_no_pref_verb.tab"; *)
  (* find_freq_validated_lemmata "resources/freq_rules.tab" results_path noun_sgjp_filename "results/freq_validated_noun.tab";
  find_not_freq_validated_lemmata "resources/freq_rules.tab" results_path noun_sgjp_filename "results/freq_not_validated_noun.tab";
  find_freq_validated_lemmata "resources/freq_rules.tab" results_path adj_sgjp_filename "results/freq_validated_adj.tab";
  find_not_freq_validated_lemmata "resources/freq_rules.tab" results_path adj_sgjp_filename "results/freq_not_validated_adj.tab";
  find_freq_validated_lemmata "resources/freq_rules.tab" results_path adv_sgjp_filename "results/freq_validated_adv.tab";
  find_not_freq_validated_lemmata "resources/freq_rules.tab" results_path adv_sgjp_filename "results/freq_not_validated_adv.tab";
  find_freq_validated_lemmata "resources/freq_rules.tab" results_path verb_sgjp_filename "results/freq_validated_verb.tab";
  find_not_freq_validated_lemmata "resources/freq_rules.tab" results_path verb_sgjp_filename "results/freq_not_validated_verb.tab";
  find_freq_validated_lemmata "resources/freq_rules.tab" results_path "verb_sgjp_no_pref.tab" "results/freq_validated_no_pref_verb.tab";
  find_not_freq_validated_lemmata "resources/freq_rules.tab" results_path "verb_sgjp_no_pref.tab" "results/freq_not_validated_no_pref_verb.tab"; *)
  (* generate_alt "results/freq_rules-adj.tab" results_path adj_sgjp_filename "results/alt-adj.tab"; *)
  (* generate_alt "results/freq_rules_all.tab" sgjp_path sgjp_filename "results/alt.tab"; *)
  (* generate_alt "results/freq_rules-lang.tab" results_path "lang_all_sgjp-20170730.tab" "results/alt-lang.tab"; *)
  (* generate_alt "results/freq_rules.tab" results_path "lang_all_sgjp-20170730.tab" "results/alt-lang.tab"; *)
  (* generate_alt "results/freq_rules_all.tab" results_path noun_sgjp_filename "results/alt-noun.tab"; *)
  (* generate_alt_translate "results/freq_rules-lang.tab" results_path "lang_all_sgjp-20170730.tab" "results/alt-lang-tr.tab"; *)
  (* generate_alt_translate "results/freq_rules_all.tab" sgjp_path sgjp_filename "results/alt-tr.tab"; *)
  (* generate_alt_translate "results/freq_rules.tab" sgjp_path polimorf_filename "results/alt-polimorf-tr.tab"; *)
  (* generate_alt_translate "results/freq_rules_all.tab" results_path noun_sgjp_filename "results/alt-noun-tr.tab"; *)
  (* ignore (Sys.command "totem ~/Dokumenty/Inne/gong/gong_05m_00s.ogg"); *)
  ()

(* Generowanie stemów z regułami *)
let _ =
  (* Dict.generate_stem_dict "results/freq_rules_adj.tab" results_path adj_sgjp_filename "results/stem-adj.tab"; *)
  (* Dict.generate_stem_dict "results/old/freq_rules_all.tab" sgjp_path sgjp_filename "results/stem.tab"; *)
  (* ignore (Sys.command "totem ~/Dokumenty/Inne/gong/gong_05m_00s.ogg"); *)
  ()

(**********************************************************************************)
(* Testy *)

let print_interpretations l =
  Xlist.iter (Xlist.sort l compare) (fun t ->
    print_endline (Inflexion.string_of_interpretation t))

let _ =
(*   Inflexion.initialize (); *)
  (* let l = Inflexion.get_interpretations "życzliwą" in
  print_interpretations l;
  let l = Inflexion.get_interpretations "żyźniejszego" in
  print_interpretations l;
  let l = Inflexion.get_interpretations "zwiśli" in
  print_interpretations l;
  let l = Inflexion.get_interpretations "najzieleńsza" in
  print_interpretations l;
  let l = Inflexion.get_interpretations "najtandetniejsza" in
  print_interpretations l;
  let l = Inflexion.get_interpretations "nieżelazny" in
  print_interpretations l;
  let l = Inflexion.get_interpretations "któregokolwiek" in
  print_interpretations l;
  let l = Inflexion.get_interpretations "większą" in
  print_interpretations l;
  let l = Inflexion.get_interpretations "bordo" in
  print_interpretations l;
  let l = Inflexion.get_interpretations "sexi" in
  print_interpretations l;
  let l = Inflexion.get_interpretations "sexy" in
  print_interpretations l;
  let l = Inflexion.get_interpretations "sepulkową" in
  print_interpretations l;*)
  (* let l = Inflexion.get_interpretations "profesory" in
  print_interpretations l; *)
  (* let l = Inflexion.get_interpretations "chrobotnąwszy" in
  print_interpretations l;
  let l = Inflexion.get_interpretations "chronografowi" in
  print_interpretations l;
  let l = Inflexion.get_interpretations "chowaniami" in
  print_interpretations l;
  let l = Inflexion.get_interpretations "Czechami" in
  print_interpretations l;
  let l = Inflexion.get_interpretations "Włoszech" in
  print_interpretations l;*)
  (* let l = Inflexion.get_interpretations "Puchatku" in
  print_interpretations l; *)
  (* let l = Inflexion.get_interpretations "telefoń" in
  print_interpretations l; *)
  (* let l = Inflexion.get_interpretations "miałczy" in
  print_interpretations l;
  let l = Inflexion.get_interpretations "miauczy" in
  print_interpretations l; *)
  (* let l = Inflexion.get_interpretations "Puchatek" in
  print_interpretations l;
  let l = Inflexion.get_interpretations "Gózd" in
  print_interpretations l; *)
  (* let l = Inflexion.get_interpretations "Goghów" in
  print_interpretations l; *)
  (* let l = Inflexion.get_interpretations "ABBie" in
  print_interpretations l;
  let l = Inflexion.get_interpretations "ABBBie" in
  print_interpretations l; *)
  (* let l = Inflexion.get_interpretations "tkach" in
  print_interpretations l; *)
  (* let l = Inflexion.get_interpretations "TK-ach" in
  print_interpretations l;
  let l = Inflexion.get_interpretations "TKach" in
  print_interpretations l; *)
  (* let l = Inflexion.get_interpretations "TK" in
  print_interpretations l; *)
(*  let l = Inflexion.get_interpretations "TTTTTTTTK" in
  print_interpretations l;
  let l = Inflexion.get_interpretations "mycie" in
  print_interpretations l;*)
  ()

let test_inflexion path filename =
  let dict = Dict.load_tab (path ^ filename) in
  let dict = Dict.merge_entries dict in
  let dict = Dict.process_interps dict in
  let dict = Dict.remove_cat "cond" dict in
  Xlist.iter dict (fun entry ->
    (* let simple_lemma = Stem.simplify_lemma entry.lemma in *)
    Xlist.iter entry.forms (fun form ->
      let l = Inflexion.get_interpretations form.orth in
      let n = Xlist.fold l 0 (fun n t ->
        let lemma = if t.Inflexion.lemma_suf = "" then t.Inflexion.lemma else t.Inflexion.lemma ^ ":" ^ t.Inflexion.lemma_suf in
        if lemma = entry.lemma && t.Inflexion.interp = form.interp then n+1 else n) in
      if n <> 1 then printf "%d\t%s\t%s\t%s\n%!" n form.orth entry.lemma form.interp))

let _ =
  (* test_inflexion sgjp_path sgjp_filename; *)
  (* test_inflexion results_path noun_sgjp_filename; *)
  (* test_inflexion results_path adj_sgjp_filename; *)
  (* test_inflexion results_path adv_sgjp_filename; *)
  ()


(* Wydobycie form wygłosu *)
let _ =
  (* Dict.generate_wyglos results_path noun_sgjp_filename "results/wyglos.tab"; *)
  ()

let mode_check mode t =
  if mode = "" then true else
  try let _ = StringQMap.find t.Inflexion.ntype_freq mode in true with Not_found -> false
  
let gender_selectors = [
  "subst:sg:nom:m1","m1";
  "subst:sg:nom:m2","m2";
  "subst:sg:nom:m3","m3";
  "subst:sg:nom:n:col","n:col";
  "subst:sg:nom:n:ncol","n:ncol";
  "subst:sg:nom:f","f";
  "subst:pl:nom:m1:pt","m1:pt";
  "subst:pl:nom:n:pt","n:pt";
  ]

let estimate_genders mode lemma gender =
  if gender <> "" then [gender] else 
  List.rev (Xlist.fold gender_selectors [] (fun found (interp,g) ->
    let l = Inflexion.synthetize lemma interp in
    let b = Xlist.fold l false (fun b t ->
      if not (mode_check mode t) then b else 
      t.Inflexion.status = Inflexion.LemmaVal || t.Inflexion.status = Inflexion.LemmaAlt || b) in
    if b then g :: found else found))
    
let string_of_interpretation t =
  Printf.sprintf "%s\t%d\t%s\t%s\t%s\t%s\t%s" t.Inflexion.lemma t.Inflexion.freq (Inflexion.string_of_status t.Inflexion.status)
    (MorphologyRules.string_of_star t.Inflexion.star) t.Inflexion.find
    (String.concat " " (Xlist.map t.Inflexion.tags (fun (k,v) -> k ^ "=" ^ v)))
    (String.concat " " (StringQMap.fold t.Inflexion.ntype_freq [] (fun l k v -> (k ^ "=" ^ string_of_int v) :: l)))
 
let select_freq mode t =
  let n = Xlist.size (Xunicode.utf8_chars_of_utf8_string t.Inflexion.find) in
  let f = 
    if mode = "" then t.Inflexion.freq else
    try StringQMap.find t.Inflexion.ntype_freq mode with Not_found -> 0 in
  (1000000 * n) + f
  
let get_gender t =
  let s = List.hd (Xstring.split "|" t.Inflexion.interp) in
  match Xstring.split ":" s with
    ["subst"; _; _; gender] -> gender
  | ["subst"; _; _; gender; col] -> gender ^ ":" ^ col
  | ["depr"; _; _; "m2"] -> "m1"
  | ["unk"] -> "unk"
  | _ -> failwith ("get_gender: " ^ t.Inflexion.interp)
 
let select_interpretations mode lemma l =
  let lsuf = if Xstring.check_sufix "a" lemma then "a" else "" in
  let l = Xlist.fold l [] (fun l t ->
(*     print_endline ("select_interpretations: " ^ string_of_interpretation t); *)
    let cat = Inflexion.get_tag t.Inflexion.tags "cat" in
    let gender = get_gender t in
    if gender = "unk" then print_endline ("select_interpretations: unk for " ^ lemma);
    if t.Inflexion.star = Dial then l else
    if mode <> "acronym" && t.Inflexion.star = Acro then l else
    if mode = "acronym" && t.Inflexion.star = Productive && cat <> "ndm" then l else
    if mode = "lastname" && lsuf <> "a" && cat = "ndm" && gender = "f" then {t with Inflexion.find="xxxxxxxxxx"} :: l else
    if mode <> "acronym" && (*mode <> "lastname" &&*) cat = "ndm" then l else
(*     if mode = "lastname" && lsuf = "a" && cat = "ndm" then l else *)
    t :: l) in
  let l = 
    if mode <> "acronym" then l else
    let selected = Xlist.fold l [] (fun selected t -> 
      let cat = Inflexion.get_tag t.Inflexion.tags "cat" in
      if cat="ndm" || Xlist.size (Xstring.split "-" t.Inflexion.lemma) > 1 then t :: selected else selected) in
    if selected = [] then l else selected in
  List.rev (Xlist.sort l (fun t1 t2 -> compare (select_freq mode t1) (select_freq mode t2)))
(*  let val_freq = Xlist.fold l 0 (fun val_freq t ->
    if t.Inflexion.status = Inflexion.LemmaVal then max t.Inflexion.freq val_freq else val_freq) in
  let sel = Xlist.fold l [] (fun sel t ->
    if t.Inflexion.freq >= val_freq || 
       t.Inflexion.status = Inflexion.LemmaVal ||  
       t.Inflexion.status = Inflexion.LemmaAlt then t :: sel else sel) in
  if Xlist.size sel = 1 then sel else l*)
  
let print_interpretations2 l =
  Xlist.iter (Xlist.sort l compare) (fun t ->
    print_endline (string_of_interpretation t))
  
let add_interpretations map lemma interp l =
  let lsufs = Xlist.fold l StringSet.empty (fun lsufs t ->
    let lsuf =
      try Xlist.assoc t.Inflexion.tags "lemma" 
      with Not_found -> "none" in
    StringSet.add lsufs lsuf) in
  let e = interp ^ "_" ^ String.concat "_" (StringSet.to_list lsufs) in
  let b = if l = [] then false else
           (List.hd l).Inflexion.status = Inflexion.LemmaVal || 
           (List.hd l).Inflexion.status = Inflexion.LemmaAlt in  
  let l = Xlist.map l (fun t ->
    lemma ^ "\t" ^ string_of_interpretation 
      (if StringSet.size lsufs = 1 then 
        {t with Inflexion.tags=List.remove_assoc "lemma" t.Inflexion.tags}
      else t)) in
  StringMap.add_inc map e [b,l] (fun l2 -> (b,l) :: l2)
(*  Xlist.fold l map (fun map t ->
    let s = lemma ^ "\t" ^ string_of_interpretation 
      (if StringSet.size lsufs = 1 then 
        {t with Inflexion.tags=List.remove_assoc "lemma" t.Inflexion.tags}
      else t) in
    StringMap.add_inc map e [s] (fun l -> s :: l))*)
  
let test_lemma_generation mode (lemma,gender) = 
  let genders = estimate_genders mode lemma gender in
  if genders = [] then print_endline ("\nLemma " ^ lemma ^ " not found in dictionary") else
  Xlist.iter genders (fun g ->
    let numbers = if g = "m1:pt" || g = "n:pt" then ["pl"] else ["sg";"pl"] in
    Xlist.iter numbers (fun n ->
      Xlist.iter ["nom";"gen";"dat";"inst";"loc";"voc"] (fun c ->
        let interp = "subst:" ^ n ^ ":" ^ c ^ ":" ^ g in
        let l = (*Inflexion.disambiguate [] [Acro;Aux;Aux2;Ndm;Dial]*) (Inflexion.synthetize lemma interp) in
        let l = select_interpretations mode lemma l in
        if Xlist.size l >= 1 && 
          ((List.hd l).Inflexion.status = Inflexion.LemmaVal || 
           (List.hd l).Inflexion.status = Inflexion.LemmaAlt) then () else (
          print_endline ("\n" ^ lemma ^ " " ^ interp);
          print_interpretations2 l));
      if g = "m1" && n = "pl" then (
        let interp = "depr:pl:nom:m2" in
        let l = (*Inflexion.disambiguate [] [Acro;Aux;Aux2;Ndm;Dial]*) (Inflexion.synthetize lemma interp) in
        let l = select_interpretations mode lemma l in
        if Xlist.size l >= 1 && 
          ((List.hd l).Inflexion.status = Inflexion.LemmaVal || 
           (List.hd l).Inflexion.status = Inflexion.LemmaAlt) then () else (
          print_endline ("\n" ^ lemma ^ " " ^ interp);
          print_interpretations2 l))))
 
let generate_forms mode map (lemma,gender) =
  let genders = estimate_genders mode lemma gender in
  if genders = [] then (print_endline ("\nLemma " ^ lemma ^ " not found in dictionary"); map) else
  Xlist.fold genders map (fun map g ->
    let numbers = if g = "m1:pt" || g = "n:pt" then ["pl"] else ["sg";"pl"] in
    Xlist.fold numbers map (fun map n ->
      let map = Xlist.fold ["nom";"gen";"dat";"inst";"loc";"voc"] map (fun map c ->
        let interp = "subst:" ^ n ^ ":" ^ c ^ ":" ^ g in
        let l = (*Inflexion.disambiguate [] [Acro;Aux;Aux2;Ndm;Dial]*) (Inflexion.synthetize lemma interp) in
        let l = select_interpretations mode lemma l in
        add_interpretations map lemma interp l) in
      if g = "m1" && n = "pl" then 
        let interp = "depr:pl:nom:m2" in
        let l = (*Inflexion.disambiguate [] [Acro;Aux;Aux2;Ndm;Dial]*) (Inflexion.synthetize lemma interp) in
        let l = select_interpretations mode lemma l in
        add_interpretations map lemma interp l else map))

let test_lemma_generation2 out_path mode lemmata = 
  ignore (Sys.command ("rm -f " ^ out_path ^ "*"));
  ignore (Sys.command ("mkdir -p " ^ out_path));
  let map = Xlist.fold lemmata StringMap.empty (generate_forms mode) in
  File.file_out (out_path ^ "stats.tsv") (fun stat_file ->
    StringMap.iter map (fun e l ->
      let l2 = Xlist.fold l [] (fun l2 (b,x) -> if b then l2 else x :: l2) in
      Printf.fprintf stat_file "%d\t%d\t%f\t%s\n" 
       (Xlist.size l2) (Xlist.size l) (float (Xlist.size l2) /. float (Xlist.size l)) 
       (String.concat "\t" (Xstring.split "_" e));
      let l2 = List.flatten l2 in
      if l2 <> [] then File.file_out (out_path ^ e ^ ".tab") (fun file ->
        Xlist.iter l2 (Printf.fprintf file "%s\n"))))

        
let test_lemmata = [
(*  "krowa","f";*)
  "WAT","m3"; (* dobry przykład na prezentację sg:dat *)
(*  "niebiosa","n:pt";
  "Zebrzydowice","n:pt";
  "wszyscy","m1:pt";
  "kot","";
  "osioł","";
  "alkohol","m3";
  "tłok","m3"; (* sg:gen -> tłoka (narzędzie); tłoku (grupa ludzi) *)
  "Kozioł","m1"; (* sg:gen -> Kozioła *)
  "Babiec","m3";*)
  "ANSI","m3";
  ]
  (* przykład z sg:loc:f dla lematu ~nia *)
 
let vovels = StringSet.of_list ["a"; "ą"; "e"; "ę"; "i"; "o"; "ó"; "u"; "y"; "A"; "E"; "I"; "O"; "Ó"; "U"; "Y"; "ä"; "ö"; "ü"; "ű"; "á"; "å"; "é"; "è"; "ë"; "í"; ""; ""; ""]
let consonants = StringSet.of_list ["b"; "c"; "ć"; "d"; "f"; "g"; "h"; "j"; "k"; "l"; "ł"; "m"; "n"; "ń"; "p"; "q"; "r"; "s"; "ś"; "t"; "v"; "w"; "x"; "z"; "ź"; "ż"; "B"; "C"; "Ć"; "D"; "F"; "G"; "H"; "J"; "K"; "L"; "Ł"; "M"; "N"; "Ń"; "P"; "Q"; "R"; "S"; "Ś"; "T"; "V"; "W"; "X"; "Z"; "Ź"; "Ż"; "ñ"; ""; ""; ""; ""]

let rec get_vovels rev = function
    s :: l when StringSet.mem vovels s -> get_vovels (s :: rev) l
  | s :: l when StringSet.mem consonants s -> rev, s :: l
  | "-" :: l -> rev, "-" :: l
  | s :: l -> print_endline ("get_vovels: " ^ s); rev, s :: l
  | [] -> rev, []
 
let rec get_consonants rev = function
    s :: l when StringSet.mem consonants s -> get_consonants (s :: rev) l
  | s :: l when StringSet.mem vovels s -> rev, s :: l
  | "-" :: l -> rev, "-" :: l
  | s :: l -> print_endline ("get_consonants: " ^ s); rev, s :: l
  | [] -> rev, []
 
let has_vovel_sufix = function
    "a" :: _ -> true
  | "ą" :: _ -> true
  | "e" :: _ -> true
  | "ę" :: _ -> true
  | "i" :: _ -> true
  | "o" :: _ -> true
  | "ó" :: _ -> true
  | "u" :: _ -> true
  | "y" :: _ -> true
  | _ -> false

let is_nontrivial_wyglos l = 
  let _,l = get_consonants [] l in
  match l with
    "e" :: _ -> true
  | "ó" :: _ -> true
  | "ą" :: _ -> true
  | _ -> false

let select_wyglos lemmata = 
  List.rev (Xlist.fold lemmata [] (fun lemmata (lemma,gender) -> 
    let l = List.rev (Xunicode.utf8_chars_of_utf8_string lemma) in
    if l = [] then failwith "select_wyglos" else
    if has_vovel_sufix l then lemmata else
    if is_nontrivial_wyglos l then (lemma,gender) :: lemmata else lemmata))

let dec_cvc = StringSet.of_list [
  "biec"; "biel"; "bień"; "ciek"; "cień"; "dzień"; "giec"; "gieć"; "giel"; "gieł"; "gień"; "gier"; 
  "kieć"; "kiel"; "kier"; "miec"; "miel"; "niec"; "niek"; "piec"; "pieć"; "piel"; "pień"; "siec"; 
  "siek"; "sień"; "wiec"; "wiel"; "wień"; "wies"; "ziec"; "rzec"; "rzeł"; "ziek"; "ciec"; 
  "giew"; "kiec"; "kieł"; "kień"; "kierz"; "kiew"; "wieś"; "zieł"; "bniec"; "cer"; "cieł"; "dziec"; 
  "fiec"; "gierz"; "giez"; "kiep"; "pies"; "zień"; "dziek"]    

let dec_vc = StringSet.of_list [
  "ąb"; "ec"; "ech"; "eć"; "ek"; "el"; "eł"; "em"; "en"; "eń"; "er"; "et"; "ew"; 
  "ób"; "ód"; "óg"; "ój"; "ól"; "ół"; "ór"; "ót"; "ów"; "óz"; "ąg"; "ąż"; "ecz"; 
  "ej"; "ep"; "erz"; "ez"; "óbr"; "ódz"; "ódź"; "órz"; "ózd"; "óż"; "ąd"; "ądź"; 
  "ąt"; "ąz"; "ąź"; "eb"; "esz"; "eż"; "óć"; "órg"; "óź"; "óźdź"; ""; ""]
  
let dec_c = StringSet.of_list [
  "b"; "c"; "ch"; "cz"; "ć"; "d"; "dz"; "dź"; "f"; "g"; "h"; "j"; "k"; "l"; "ł"; 
  "m"; "n"; "ń"; "p"; "r"; "rz"; "s"; "sz"; "ś"; "t"; "w"; "x"; "z"; "ż"; "sch"; 
  "gh"; "dż"; "tsch"; "ź"; "tch"; ""]
  
(* FIXME: trzeba sprawdzić, że nie mieszam reguł 'ie' z 'e' i 'ió' z 'ó' *)
let make_decision_class s =
  if StringSet.mem dec_cvc s then "CVC" else
  if StringSet.mem dec_vc s then "VC" else 
  if StringSet.mem dec_c s then "C" else (
  print_endline ("make_decision_class: " ^ s);
  s)
 
let single_consonant = StringSet.of_list ["b"; "c"; "ć"; "d"; "f"; "g"; "h"; "j"; "k"; "l"; "ł"; "m"; "n"; "ń"; "p"; "q"; "r"; "s"; "ś"; "t"; "v"; "w"; "z"; "ź"; "ż"; "ch"; "cz"; "dz"; "dź"; "dż"; "rz"; "sz"]

let double_consonant = StringSet.of_list ["x"; "bs"; "cht"; "ck"; "dt"; "fs"; "ft"; "hm"; "hr"; "jb"; "jch"; "jf"; "jk"; "jm"; "jn"; "js"; "jsz"; "jt"; "ks"; "lc"; "lcz"; "ld"; "lk"; "ll"; "lm"; "ls"; "lt"; "lz"; "łb"; "łc"; "łch"; "łć"; "łt"; "łz"; "mc"; "mm"; "mp"; "ms"; "mś"; "nc"; "nch"; "ncz"; "nd"; "ng"; "nk"; "nn"; "ns"; "nsz"; "nt"; "nz"; "ńć"; "prz"; "ps"; "rb"; "rc"; "rch"; "rcz"; "rć"; "rd"; "rg"; "rk"; "rl"; "rn"; "rs"; "rsz"; "rś"; "rt"; "rw"; "ss"; "st"; "szcz"; "szk"; "th"; "tt"; "tz"; "wc"; "ws"; "zd"; "źdź"; "żdż"; "łk"; "ph"; "br"; "lp"; "krz"; "łcz"; "łm"; "łp"; "mcz"; "ml"; "ńcz"; "pcz"; "pr"; "rdź"; "rzch"; "sk"; "szt"; "ść"; "śń"; "trz"; "bcz"; "dźc"; "wcz"; "chrz"; "bd"; "bl"; "dr"; "fl"; "gr"; "jc"; "jcz"; "jd"; "jl"; "kl"; "kt"; "lb"; "lf"; "lg"; "łn"; "łw"; "łż"; "mń"; "mr"; "nr"; "pt"; "rf"; "rm"; "rń"; "rp"; "rzb"; "rzg"; "rż"; "tch"; "tl"; "tr"; "tw"; "wr"; "zg"; "zw"; "ższ"; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""]

let triple_consonant = StringSet.of_list ["ffs"; "fft"; "kść"; "ldt"; "lls"; "mpc"; "mpf"; "ndt"; "nsch"; "nsk"; "nts"; "ntz"; "rbst"; "rckx"; "rgh"; "rkt"; "rndt"; "rst"; "rszcz"; "rszk"; "rszt"; "rth"; "rts"; "rtsch"; "rtz"; "rzk"; "tsch"; "bsk"; "jsk"; "jszcz"; "jść"; "lsk"; "łst"; "ńsk"; "rpc"; "rsk"; "rśl"; "str"; "szczk"; "wsk"; "kst"; "lft"; "mbr"; "mpt"; "ptr"; "rpt"; "rść"; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""]

let count_consonants s =
  if StringSet.mem single_consonant s then 1 else 
  if StringSet.mem double_consonant s then 2 else 
  if StringSet.mem triple_consonant s then 3 else (
  print_endline ("count_consonants: " ^ s);
  0)
  
let trivial_vovels = StringSet.of_list ["ae"; "aie"; "aue"; "Aue"; "aye"; "ee"; "eie"; "eue"; "eye"; "iae"; "oe"; "ue"; "uie"; "uye"; "ye"]
  
let classify_vovels s =
  if StringSet.mem trivial_vovels s then "X" else
  match s with
    "e" -> "e"
  | "ie" -> "e"
  | "ó" -> "ó"
  | "ió" -> "ó"
  | "ą" -> "ą"
  | "ią" -> "ą"
  | "aó" -> "ó"
  | _ -> print_endline ("classify_vovels: " ^ s); s
 
type sample = {
  lemma: string; gender: string; mode: string; suf1: string; suf2: string; suf3: string; suf3con: string; 
  no_consonants: int; vovel_type: string; short: bool; dec: string list}
 
let no_consonants_rule ex =
  if ex.no_consonants > 1 then "C" else "X"
 
let classify_vovels_rule ex =
  if ex.vovel_type = "X" then "C" else "X"
 
let a_rule ex =
  if ex.vovel_type = "ą" then 
  if ex.mode = "lastname" || ex.mode = "firstname" then "C" else
  match ex.suf1 with
  | "b" -> "VC"
  | "c" -> "C" 
  | "ch" -> "C" 
  | "cz" -> "C" 
  | "dz" -> "C" 
  | "dź" -> "C"(*"VC"*) (* FIXME: wątpliwe *)
  | "d" -> "C" 
  | "g" -> "C" 
  | "k" -> "C" 
  | "p" -> "C" 
  | "s" -> "C" 
  | "t" -> "C" 
  | "ć" -> "C" 
  | "ś" -> "C" 
  | "ź" -> "C"(*"VC"*) (* FIXME: wątpliwe *)
  | "z" -> "C" 
  | "ż" -> "C" 
  | _ -> (*"C"*)"X"  
  else "X"
 
let is_short_rule ex = 
  if ex.short && ex.vovel_type = "e" then "C" else "X"

let o_rule ex =
  if ex.vovel_type = "ó" then 
  match ex.suf1 with
    "b" -> "VC" (* VC-2 {VC C}-2 *)
  | "d" -> "VC" (* VC-8 {VC C}-6 *)
  | "ć" -> "C" 
  | "ch" -> "C" 
  | "cz" -> "C" 
  | "dz" -> "C" (* C-1 *)
  | "dź" -> "VC" (* C-1 *)
  | "g" -> "VC" (* VC-31 *)
  | "j" -> "VC" (* VC-9 {VC C}-15 *)
  | "k" -> "C" (* C-1 *)
  | "l" -> "VC" (* C-2 VC-2 {VC C}-9 *)
  | "ł" -> "VC" (* VC-4 {VC C}-13 *)
  | "r" -> "VC" (* C-3 VC-8 {VC C}-33 *)
  | "rz" -> if ex.mode = "toponym" then "VC" else "C" (* C-6 *)
  | "s" -> "C" (* C-1 *)
  | "sz" -> "C" 
  | "t" -> "VC" (* VC-2 {VC C}-3 *)
  | "w" -> "VC" (* VC-112 *)
  | "z" -> "VC" (* VC-3 {VC C}-1 *)
  | "ź" -> "C" 
  | "ż" -> if ex.mode = "toponym" then "VC" else "C" (* C-5 *) (* ! *)
  | _ -> (*"VC"*)"X"
  else "X"

(* 
ó-l-b: VC-1
ó-l-B: C-1

ó-l-Chr: {VC C}-1
ó-l-Kr: C-1
ó-l-kr: {VC C}-1
ó-l-r: {VC C}-1

ó-l-M: VC-1
ó-l-Sm: {VC C}-1
ó-l-m: {VC C}-1

ó-l-g: {VC C}-2
ó-l-rg: {VC C}-2

ió-r-P: {VC C}-1
ió-r-p: ! C-1 VC-1 {VC C}-2

ió-r-W: C-1
ió-r-w: ! C-1 {VC C}-1

ió-r-c: {VC C}-1
ió-r-s: {VC C}-1
ió-r-śc: {VC C}-1

ó-r-B: VC-1
ó-r-W: VC-1
ó-r-b: ! VC-1 {VC C}-2
ó-r-c: {VC C}-1
ó-r-ch: {VC C}-6
ó-r-cz: ! VC-1 {VC C}-1
ó-r-d: {VC C}-2
ó-r-g: {VC C}-2
ó-r-j: {VC C}-2
ó-r-kt: VC-1
ó-r-m: {VC C}-2
ó-r-mb: {VC C}-1
ó-r-mp: {VC C}-1
ó-r-p: ! VC-2 {VC C}-2
ó-r-s: {VC C}-1
ó-r-sk: {VC C}-1
ó-r-szcz: {VC C}-1
ó-r-w: {VC C}-1
 *)
 
let ie_rule ex =
  if ex.suf2 = "ie" then 
(*   if ex.suf3con = "g" || ex.suf3con = "k" then "X" else *)
  match ex.suf1 with
  | "b" -> "C" (* C-1 *)
  | "c" -> "CVC" (* ! C-1 CVC-302 *)
  | "ch" -> "C" (* C-10 *)
  | "cz" -> "C" 
  | "d" -> "C" (* C-1 *)
  | "dz" -> "C" (* C-1 *)
  | "dź" -> "C" (* C-2 *)
  | "f" -> "C" 
  | "g" -> "C" 
  | "j" -> "C" (* C-51 *)
  | "k" -> "CVC" (* FIXME: to można uszczegółówić *)
  | "l" -> if (ex.suf3con = "g" || ex.suf3con = "k") && ex.mode <> "lastname" then "CVC" else "C"
  | "m" -> "C" (* C-1 *)
  | "n" -> "C" (* C-1 *)
  | "r" -> "C" (* FIXME: to można uszczegółówić dla g i k *) 
  | "rz" -> "C" (* C-29 *)
  | "s" -> "C" (* ! C-8 CVC-1 *)
  | "sz" -> "C" (* C-4 *)
  | "t" -> "C" (* C-6 *)
  | "w" -> if ex.suf3con = "g" || ex.suf3con = "k" then "CVC" else "C"
  | "ć" -> "C" (* ! C-6 CVC-2 {CVC C}-1 *)
  | "ł" -> if ex.mode <> "lastname" then "CVC" else "C" (* ! C-9 CVC-2 *)
  | "ń" -> "C" (* ! C-38 CVC-6 {CVC C}-8 *)
  | "ś" -> "C" (* C-2 *)
  | "ź" -> "C" 
  | "ż" -> "C" (* C-1 *)
  | _ -> (*"C"*)"X"
  else "X"
  
let e_rule ex =
  if ex.suf2 = "e" then 
  match ex.suf1 with
  | "b" -> "C" (* C-7 *)
  | "c" -> if ex.suf3con = "rz" then "CVC" (* CVC-12 *) else "VC" (*! C-2 VC-90*)
  | "ch" -> "C" (* ! C-18 VC-1 *)
  | "cz" -> "C"
  | "d" -> "C" (* C-1 *)
  | "dź" -> "C" (* C-1 *)
  | "dż" -> "C"
  | "f" -> "C" (* C-1 *)
  | "g" -> "C" (* C-1 *)
  | "h" -> "C"
  | "j" -> "C" (* C-86 *)
  | "k" -> "VC" (* ! C-4 VC-2331 {VC C}-19 *)
  | "l" -> if ex.suf3con = "rz" || ex.suf3con = "r" || ex.suf3con = "n" then "C" else "VC" 
  | "m" -> "C" (* ! C-9 VC-1 *)
  | "n" -> "C" (* ! C-50 VC-3 *)
  | "p" -> "C" (* C-3 *)
  | "r" -> "C" (* ! C-949 VC-4 {VC C}-30 *) (* ! C-30 VC-3 {C VC}-3 *)
  | "rz" -> "C" (* C-19 *)
  | "s" -> "C" (* C-47 *)
  | "sz" -> "C" (* C-16 *)
  | "t" -> "C" (* ! C-37 VC-2 *)
  | "w" -> "C" (* C-20 *)
  | "z" -> "C" (* C-5 *)
  | "ć" -> if ex.mode = "lastname" then "C" else "VC" (* ! C-7 VC-5 {VC C}-4 *)
  | "ł" -> if ex.suf3con = "rz" then "CVC" (* CVC-2 *) else "VC" (* ! VC-6 {VC C}-19 *)
  | "ń" -> "C" (* ! C-40 VC-2 {VC C}-6 *)
  | "ś" -> "C" (* C-9 *)
  | "ź" -> "C"
  | "ż" -> "C"
  | _ -> (*"C"*)"X"
  else "X"

(*
e-ć-rb: C-2
e-ć-mp: C-1
e-ć-p: ! C-2 VC-3
e-ć-rn: C-1

e-ć-ch: ! C-1 VC-1 {VC C}-1
e-ć-l: {VC C}-1
e-ć-r: {VC C}-2
e-ć-ł: VC-1
*)
 
let test_rule name rule data = 
  let rest,strict,correct,incorrect = Xlist.fold data ([],[],[],[]) (fun (rest,strict,correct,incorrect) ex ->
    let cl = rule ex in
    if cl = "X" then ex :: rest, strict, correct, incorrect else
    if Xlist.mem ex.dec cl then 
      if Xlist.size ex.dec = 1 then rest, ex :: strict, correct, incorrect else rest, strict, ex :: correct, incorrect else
    rest, strict, correct, (ex,cl) :: incorrect) in
  Printf.printf "Applying rule %s\n" name;
  Printf.printf "  coverage %d/%d=%f\n" (Xlist.size correct + Xlist.size strict + Xlist.size incorrect) (Xlist.size data) 
    (float (Xlist.size correct + Xlist.size strict + Xlist.size incorrect) /. float (Xlist.size data));
  Printf.printf "  precision %d/%d=%f\n" (Xlist.size correct + Xlist.size strict) (Xlist.size correct + Xlist.size strict + Xlist.size incorrect)
    (float (Xlist.size correct + Xlist.size strict) /. float (Xlist.size correct + Xlist.size strict + Xlist.size incorrect));
  Printf.printf "  strict precision %d/%d=%f\n" (Xlist.size strict) (Xlist.size correct + Xlist.size strict + Xlist.size incorrect)
    (float (Xlist.size strict) /. float (Xlist.size correct + Xlist.size strict + Xlist.size incorrect));
(*  Printf.printf "  classified examples:\n";
  Xlist.iter correct (fun ex ->
    Printf.printf "    %s:%s %s-%s-%s %s %d correct_dec=%s\n" ex.lemma ex.gender ex.suf3 ex.suf2 ex.suf1 ex.vovel_type ex.no_consonants ex.dec);*)
  if Xlist.size incorrect > 0 then Printf.printf "  misclassified examples:\n";
  Xlist.iter incorrect (fun (ex,cl) ->
    let dec = String.concat " " ex.dec in
    let dec = if Xlist.size ex.dec <> 1 then "{" ^ dec ^ "}" else dec in
    Printf.printf "    %s %s:%s %s-%s-%s %s %d correct_dec=%s classified_as=%s\n" ex.mode ex.lemma ex.gender ex.suf3 ex.suf2 ex.suf1 ex.vovel_type ex.no_consonants dec cl);
  rest
 
let print_wyglos_examples data key_fun =
  Printf.printf "\n";
  let map = Xlist.fold data StringMap.empty (fun map ex -> 
    let suf = key_fun ex in
    let map2 = try StringMap.find map suf with Not_found -> StringQMap.empty in
    let dec = String.concat " " ex.dec in
    let dec = if Xlist.size ex.dec <> 1 then "{" ^ dec ^ "}" else dec in
    let map2 = StringQMap.add map2 dec in
    StringMap.add map suf map2) in
  StringMap.iter map (fun suf map2 ->
    let l = StringQMap.fold map2 [] (fun l k v -> (Printf.sprintf "%s-%d" k v) :: l) in
    if Xlist.size l > 1 then Printf.printf "%s: ! %s\n" suf (String.concat " " (Xlist.sort l compare))
    else Printf.printf "%s: %s\n" suf (String.concat " " (Xlist.sort l compare)))

let analyze_wyglos l =
  let data = Xlist.fold l [] (fun data (filename,mode) ->
  let lemmata = load_lemmata filename in
  let lemmata = select_wyglos lemmata in
  Xlist.fold lemmata data (fun data (lemma,gender) ->
    let number = if gender = "n:pt" || gender = "m1:pt" then "pl" else "sg" in
    let interp = "subst:" ^ number ^ ":dat:" ^ gender in
    let l = Inflexion.synthetize lemma interp in
    let b = Xlist.fold l false (fun b t ->
      if t.Inflexion.status = Inflexion.LemmaVal || t.Inflexion.status = Inflexion.LemmaAlt then true else b) in
    if not b then print_endline ("analyze_wyglos: validated form not found for " ^ lemma ^ ":" ^ gender);
    if not b then (
      print_endline (lemma ^ " " ^ interp );
      print_interpretations2 l);
    let l = Xlist.fold l [] (fun l t ->
      let cat = Inflexion.get_tag t.Inflexion.tags "cat" in
      if cat = "ndm" then l else 
      if t.Inflexion.status = Inflexion.LemmaVal then t :: l else l) in
    if l = [] then data else
    let rev = List.rev (Xunicode.utf8_chars_of_utf8_string lemma) in
    let suf1,rev = get_consonants [] rev in
    let suf2,rev = get_vovels [] rev in
    let suf3,rev = get_consonants [] rev in
    let suf3con,_ = if suf3 = [] then "",[] else Dict.get_con2 (List.rev suf3) in
    let suf1 = String.concat "" suf1 in
    let suf2 = String.concat "" suf2 in
    let suf3 = String.concat "" suf3 in
    let set = Xlist.fold l StringSet.empty (fun set t -> 
      StringSet.add set (make_decision_class t.Inflexion.find)) in
    {lemma; gender; mode; suf1; suf2; suf3; suf3con; no_consonants=count_consonants suf1; 
     vovel_type=classify_vovels suf2; short=(rev=[]); dec=Xlist.sort (StringSet.to_list set) compare} :: data)) in
  let data = test_rule "no_consonants_rule" no_consonants_rule data in
  let data = test_rule "classify_vovels_rule" classify_vovels_rule data in
  let data = test_rule "a_rule" a_rule data in
  let data = test_rule "is_short_rule" is_short_rule data in
  let data = test_rule "o_rule" o_rule data in
  let data = test_rule "ie_rule" ie_rule data in
  let data = test_rule "e_rule" e_rule data in
  Printf.printf "%d unclassified examples remains\n" (Xlist.size data);
  print_wyglos_examples data (fun ex -> ex.suf2 ^ "-" ^ ex.suf1);
  print_wyglos_examples data (fun ex -> ex.suf2 ^ "-" ^ ex.suf1 ^ " " ^ ex.mode);
  print_wyglos_examples data (fun ex -> ex.suf2 ^ "-" ^ ex.suf1 ^ "-" ^ Xunicode.lowercase_utf8_string ex.suf3con);
  print_wyglos_examples data (fun ex -> ex.suf2 ^ "-" ^ ex.suf1 ^ "-" ^ Xunicode.lowercase_utf8_string ex.suf3con ^ " " ^ ex.mode);
  print_wyglos_examples data (fun ex -> 
    let suf3rest = Xunicode.lowercase_utf8_string (Xstring.cut_sufix ex.suf3con ex.suf3) in
    let suf3rest = if suf3rest = "" then "" else "-" ^ suf3rest in
    ex.suf2 ^ "-" ^ ex.suf1 ^ "-" ^ Xunicode.lowercase_utf8_string ex.suf3con ^ suf3rest);
  print_wyglos_examples data (fun ex -> 
    let suf3rest = Xunicode.lowercase_utf8_string (Xstring.cut_sufix ex.suf3con ex.suf3) in
    let suf3rest = if suf3rest = "" then "" else "-" ^ suf3rest in
    ex.suf2 ^ "-" ^ ex.suf1 ^ "-" ^ Xunicode.lowercase_utf8_string ex.suf3con ^ suf3rest ^ " " ^ ex.mode);
  ()
    
    
 
(* Generowanie odmienionych form dla leksemów ze słownika *)
(* Założenia: generując formę znamy jej wszystkie cechy fleksyjne *)
let _ = 
  Inflexion.initialize ();
(*    let lemmata = test_lemmata in  *)
(*   let lemmata = File.load_lines "resources/lemmata.tab" in *)
(*   let lemmata = File.load_lines "/home/yacheu/Dokumenty/Selidor/DialogueSystem/NLU/lexemes/data/location/TownName.tab" in *)
(*   let lemmata = File.load_lines "Service.beauty.tab" in *)
(*   let lemmata = File.load_lines (results_path ^ "lemma_noun_akronim.tab") in *)
(*   let lemmata = File.load_lines "Service.beauty.inf.tab" in *)
(*    Xlist.iter lemmata (test_lemma_generation (*"other"*)"acronym");  *)
(*   test_lemma_generation2 "other" lemmata; *)
(*  let map = Xlist.fold lemmata StringMap.empty (generate_forms "other") in
  StringMap.iter map (fun e l ->
    let l2 = Xlist.fold l [] (fun l2 (b,x) -> if b then l2 else x :: l2) in
    Printf.printf "%6d/%6d=%f %s\n" (Xlist.size l2) (Xlist.size l) (float (Xlist.size l2) /. float (Xlist.size l)) e;
    let l2 = List.flatten l2 in
    File.file_out (results_path ^ "forms_noun_other_" ^ e ^ ".txt") (fun file ->
      Xlist.iter l2 (Printf.fprintf file "%s\n")));*)
(*   let lemmata = load_lemmata "results/lemma_noun_other_infl.tab" in *)
(*  test_lemma_generation2 "results/forms_noun_other/" "other" lemmata;
  let lemmata = load_lemmata "results/lemma_noun_acronym.tab" in
  test_lemma_generation2 "results/forms_noun_acronym/" "acronym" lemmata;*)
(*   let lemmata = load_lemmata "results/lemma_noun_firstname.tab" in *)
(*   let lemmata = load_lemmata "results/lemma_noun_toponym.tab" in *)
(*   let lemmata = load_lemmata "results/lemma_noun_lastname.tab" in *)
(*   test_lemma_generation2 "results/forms_noun_lastname/" "lastname" lemmata; *)
(*  analyze_wyglos [
    "results/lemma_noun_firstname.tab","firstname";
    "results/lemma_noun_lastname.tab","lastname";
    "results/lemma_noun_toponym.tab","toponym";
    "results/lemma_noun_other_infl.tab","other"];*)
  ()
  
(*let _ =
  Dict.generate_rule_frequencies_list interp_compound_rule_trees ["./","dziec.tab"] "results/dziec_freq_rules.tab";*)


(* FIXME: kwestia istnienia freq_rule wskazywanej przez klasyfikator wygłosowy *)

(* FIXME: synteza dla Dudziec Dziudziek Godziek Hudziec *)
 
(* FIXME: nie syntezuje się, dla "subst:_:_:_", a jedynie dla "subst:_:_:_:_" *)
  
(* Potrzebne jest narzędzie stwierdzające, czy słowo się odmienia *)
(* Potrzebne jest narzędzie określające rodzaj i pt na podstawie lematu *)
  
(* NOTE: Liczba przykładów na liście location/TownName.tab: 28632, z tego spoza SGJP: 4888; 17,07% *)
(* NOTE: Liczba przykładów na liście beauty/Service.beauty.tab: 90, z tego spoza SGJP: 29; 32,22% *)
(* abdominoplastyka, biorewitalizacja, blefaroplastyka, dekoloryzacja, dermabrazja, elektrosauna, endermolift, endermolifting, endermologia, fotodepilacja, grooming, gruming, hialuronoplastyka, jonoforeza, karboksyterapia, kombo, kriolipoliza, lipolifting, lipoliza, liporadiologia, olaplex, ostrzyk, otoplastyka, oxybrazja, oxymezototerapia, radiofrekwencja, resurfacing, sonosfera, styling *)
(* NOTE: Liczba przykładów na liście beauty/Service.beauty.inf.tab: 273, z tego spoza SGJP: *)
(* NOTE: Wniosek jest taki, że dezambiguacja powinna polegać na posortowaniu od form wyprodukowanych za pomocą najczęściej używanych reguł *)  
  
(* problem z liczbą przy toponimach: w Warszawach, w Katowicu *)
  
(* TODO: trzeba opracować rozróżnianie między końcówkami -a i -u w noun other subst:sg:gen:m3 ε *)
(* TODO: trzeba opracować kiedy znika -e- z wygłosu - rozpoznawanie niepolskich słów? *)
  
(**********************************************************************************)

(*
przykład na problemy z generowaniem przez Morfeusza:

Oława, Mława, Iława, ...

Jaki, Jak

test_fonetics results_path verb_sgjp_filename
NE lemma=nadjeść orth=nadje phon=nadje: nadie
NE lemma=objeść orth=obje phon=obje: obie
NE lemma=odjeść orth=odje phon=odje: odie
NE lemma=podjeść orth=podje phon=podje: podie

test_fonetics results_path noun_sgjp_filename; 
NE lemma=Ilja orth=Ilji phon=I{lj}lji: I{lj}lii
NE lemma=Ilja orth=Ilji phon=I{lj}lji: I{lj}lii
NE lemma=Ilja orth=Iljo phon=I{lj}ljo: I{lj}lio
NE lemma=Ilja orth=Iljom phon=I{lj}ljom: I{lj}liom
NE lemma=Ilja orth=Iljowie phon=I{lj}ljov′e: I{lj}ljowie
NE lemma=Ilja orth=Iljów phon=I{lj}ljóv: I{lj}ljów
NE lemma=Ilja orth=Ilją phon=I{lj}lją: I{lj}lią
NE lemma=Ilja orth=Ilję phon=I{lj}lję: I{lj}lię
NE lemma=Jiři orth=Jiřich phon=Ji{ři}rʲix: Ji{rzi}rich
NE lemma=Jiři orth=Jiřich phon=Ji{ři}rʲix: Ji{rzi}rich
NE lemma=Jiři orth=Jiřie phon=Ji{ři}rʲe: Ji{rzi}rie
NE lemma=Jiři orth=Jiřiego phon=Ji{ři}rʲego: Ji{rzi}riego
NE lemma=Jiři orth=Jiřiemu phon=Ji{ři}rʲemu: Ji{rzi}riemu
NE lemma=Jiři orth=Jiřim phon=Ji{ři}rʲim: Ji{rzi}rim
NE lemma=Jiři orth=Jiřim phon=Ji{ři}rʲim: Ji{rzi}rim
NE lemma=Jiři orth=Jiřim phon=Ji{ři}rʲim: Ji{rzi}rim
NE lemma=Jiři orth=Jiřimi phon=Ji{ři}rʲim′i: Ji{rzi}rimi
NE lemma=Jiři orth=Jiřiowie phon=Ji{ři}rʲov′e: Ji{rzi}riowie
NE lemma=Meljon:Sm1 orth=Meljon phon=Me{lj}ljon: Me{lj}lion
NE lemma=Meljon:Sf orth=Meljon phon=Me{lj}ljon: Me{lj}lion
NE lemma=Perdjon:Sm1 orth=Perdjon phon=Perdjon: Perdion
NE lemma=Perdjon:Sf orth=Perdjon phon=Perdjon: Perdion
NE lemma=Skopje orth=Skopja phon=Sko{pj}pja: Sko{pj}pia
NE lemma=Skopje orth=Skopja phon=Sko{pj}pja: Sko{pj}pia
NE lemma=Skopje orth=Skopjach phon=Sko{pj}pjax: Sko{pj}piach
NE lemma=Skopje orth=Skopjami phon=Sko{pj}pjam′i: Sko{pj}piami
NE lemma=Skopje orth=Skopji phon=Sko{pj}pji: Sko{pj}pii
NE lemma=Skopje orth=Skopjom phon=Sko{pj}pjom: Sko{pj}piom
NE lemma=Skopje orth=Skopju phon=Sko{pj}pju: Sko{pj}pju
NE lemma=Skopje orth=Skopju phon=Sko{pj}pju: Sko{pj}pju
NE lemma=polje orth=polji phon=po{lj}lji: po{lj}lii
NE lemma=polje orth=poljom phon=po{lj}ljom: po{lj}liom
NE lemma=polje orth=polju phon=po{lj}lju: po{lj}lju
NE lemma=polje orth=polju phon=po{lj}lju: po{lj}lju

many stems found for

zmiany od 2017:
dodanie form typu konsumpcjoniźmie
*)

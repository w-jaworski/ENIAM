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

(*let nlp_resources_path = "../../NLP resources/"
let sgjp_path = nlp_resources_path ^ "SGJP/"*)
let sgjp_path = try Sys.argv.(1) with _ -> failwith "SGJP path not provided"
(* let sgjp_filename = "sgjp-20170730.tab" *)
let sgjp_filename = "sgjp-20220403.tab"


let sources = [
  sgjp_path, sgjp_filename;
  "data/", "noun-supplement-acro.tab";
  "data/", "noun-supplement-polimorf.tab";
  "data/", "noun-supplement.tab";
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

let compound_rules = MorphologyRules.make_compound_rules ()
let interp_compound_rule_trees = MorphologyRules.make_interp_compound_rule_trees compound_rules

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

let generate_lemmata path filename out_filename =
  let dict = Dict.load_tab (path ^ filename) in
  let lemmata = Xlist.fold dict StringSet.empty (fun set e ->
    StringSet.add set (Stem.simplify_lemma e.MorphologyTypes.lemma)) in
  File.file_out out_filename (fun file ->
    StringSet.iter lemmata (Printf.fprintf file "%s\n"))


let _ =
  ignore (Sys.command "mkdir -p resources");
  Dict.generate_rule_frequencies_list interp_compound_rule_trees sources "resources/freq_rules.tab";
  generate_alt "resources/freq_rules.tab" sgjp_path sgjp_filename "resources/alt.tab";
  Dict.generate_stem_dict "resources/freq_rules.tab" sgjp_path sgjp_filename "resources/stem.tab";
  Dict.generate_wyglos sgjp_path sgjp_filename "resources/wyglos.tab";
  generate_lemmata sgjp_path sgjp_filename "resources/lemmata.tab";
  ()

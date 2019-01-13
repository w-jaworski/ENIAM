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

let get_form e =
  match e.forms with
    [form] -> form
  | _ -> failwith "get_form"

let load_tab filename =
  File.load_tab filename (function
      orth :: lemma :: interp :: _ ->
        {empty_entry with lemma=lemma; forms=[{empty_form with orth=orth; interp=interp}]}
    | line -> failwith ("load_tab: " ^ (String.concat "\t" line)))

let load_tab_incr filename dict =
  File.fold_tab filename dict (fun dict -> function
      orth :: lemma :: interp :: _ ->
        {empty_entry with lemma=lemma; forms=[{empty_form with orth=orth; interp=interp}]} :: dict
    | line -> failwith ("load_tab_incr: " ^ (String.concat "\t" line)))

let load_tab_full filename =
  File.load_tab filename (function
      [orth; lemma; interp] ->
        {empty_entry with lemma=lemma; forms=[{empty_form with orth=orth; interp=interp}]}
    | [orth; lemma; interp; proper_type] ->
        {empty_entry with lemma=lemma; forms=[{empty_form with orth=orth; interp=interp}]; proper_type=proper_type}
    | [orth; lemma; interp; proper_type; genre] ->
        {empty_entry with lemma=lemma; forms=[{empty_form with orth=orth; interp=interp; genre=genre}]; proper_type=proper_type}
    | line -> failwith ("load_tab_full: " ^ (String.concat "\t" line)))

let load_lu dict id path =
  let filename = path ^ "morf_rel_" ^ string_of_int id ^ "_lu.tab" in
  File.fold_tab filename dict (fun dict -> function
      [lemma1; lemma2] ->
        {lemma1=lemma1; lemma2=lemma2; rel_id=id; lu_stem=""; lu_validated=false;validated1=false;validated2=false} :: dict
    | line -> failwith ("load_lu: " ^ (String.concat "\t" line)))

let pos = StringSet.of_list [
  "subst";"adj";"adv";"interp";"num";"xxx";"prep";"fin";"praet";"qub";"inf";"interj";
  "brev";"numcol";"ppas";"pact";"adja";"conj";"ger";"pcon";"pant";"comp";"depr";
  "adjp";"imps";"impt";"pred";"bedzie";"burk";"aglt";"ppron12";"ppron3";"adjc";
  "winien";"siebie";"numcomp"
  ]

let rec find_pos rev = function
    s :: l -> if StringSet.mem pos s then List.rev rev, s :: l else find_pos (s :: rev) l
  | [] -> failwith "find_pos"

let split_lemma_interp s =
  let l = Xstring.split_delim ":" s in
  let lemma,interp = find_pos [List.hd l] (List.tl l) in
  String.concat ":" lemma, String.concat ":" interp

let rec remove_empties = function
    "" :: l -> remove_empties l
  | l -> l

let split_freq_orth s =
  match remove_empties (Xstring.split " " s) with
    freq :: l -> int_of_string freq, String.concat " " l
  | _ -> failwith "split_freq_orth"

let load_freq_tab filename =
  File.load_tab filename (function
      [freq_orth; lemma_interp] ->
        let freq,orth = split_freq_orth freq_orth in
        let lemma,interp = split_lemma_interp lemma_interp  in
        {empty_entry with lemma=lemma; forms=[{empty_form with orth=orth; interp=interp; freq=freq}]}
    | line -> failwith ("load_freq_tab: " ^ (String.concat "\t" line)))

let proper_type_selector e = e.proper_type
let genre_selector e = (get_form e).genre
let interp_selector e = (get_form e).interp
let freq_selector e = (get_form e).freq

let print_quantities out_filename selector dict =
  let qmap = Xlist.fold dict StringQMap.empty (fun qmap entry ->
    StringQMap.add qmap (selector entry)) in
  File.file_out out_filename (fun file ->
    StringQMap.iter qmap (fun k v ->
      fprintf file "%6d\t%s\n" v k))

(**********************************************************************************)

let load_dict_as_set filename =
  let l = load_tab filename in
  List.sort compare (StringSet.to_list (Xlist.fold l StringSet.empty (fun set entry ->
    let form = get_form entry in
    StringSet.add set (String.concat "\t" [form.orth;entry.lemma;form.interp]))))

let load_dict_as_set_full filename =
  let l = load_tab_full filename in
  List.sort compare (StringSet.to_list (Xlist.fold l StringSet.empty (fun set entry ->
    let form = get_form entry in
    StringSet.add set (String.concat "\t" [form.orth;entry.lemma;form.interp;entry.proper_type;form.genre]))))

let rec compare_dicts_rec file = function
    [],[] -> ()
  | [],b :: lb -> fprintf file "> %s\n" b; compare_dicts_rec file ([],lb)
  | a :: la,[] -> fprintf file "< %s\n" a; compare_dicts_rec file (la,[])
  | a :: la, b :: lb ->
       if a = b then compare_dicts_rec file (la,lb) else
       if a < b then (fprintf file "< %s\n" a; compare_dicts_rec file (la,b :: lb)) else
       (fprintf file "> %s\n" b; compare_dicts_rec file (a :: la,lb))

let compare_dicts filename1 filename2 filename_out =
  let dict1 = load_dict_as_set filename1 in
  let dict2 = load_dict_as_set filename2 in
  File.file_out filename_out (fun file ->
    compare_dicts_rec file (dict1,dict2))

let compare_dicts_full filename1 filename2 filename_out =
  let dict1 = load_dict_as_set_full filename1 in
  let dict2 = load_dict_as_set_full filename2 in
  File.file_out filename_out (fun file ->
    compare_dicts_rec file (dict1,dict2))

(**********************************************************************************)

let find_entry_cat entry =
  let form = get_form entry in
  let cat,tags = match Xstring.split ":" form.interp with
      cat :: tags -> cat,tags
    | _ -> failwith ("find_entry_type: " ^ entry.lemma ^ " " ^ form.orth ^ " "^ form.interp) in
  if cat = "praet" then
    let t = match tags with
      _ :: _ :: t :: _ -> t
    | _ -> failwith ("find_entry_cat: " ^ form.interp) in
    if t = "pri" || t = "sec" || t = "ter" then "cond" else "verb" else
  if cat = "winien" then
    let t = match tags with
      _ :: _ :: t :: _ -> t
    | _ -> failwith ("find_entry_cat: " ^ form.interp) in
    if t = "pri" || t = "sec" || t = "ter" then "cond" else "other" else
  if cat = "subst" || cat = "depr" then "noun" else
  if cat = "adj" || cat = "adja"|| cat = "adjc"|| cat = "adjp" then "adj" else
  if cat = "adv" then "adv" else
  if cat = "inf" || cat = "praet"|| cat = "fin" || cat = "ppas" || cat = "pact" || cat = "pacta" ||
     cat = "impt" || cat = "imps" || cat = "pcon" || cat = "pant" || cat = "ger" || cat = "" then "verb" else
  if cat = "bedzie" || cat = "pred"|| cat = "prep" || cat = "num" || cat = "aglt" ||
     cat = "qub" || cat = "brev" || cat = "comp" || cat = "interj" || cat = "burk" ||
     cat = "conj" || cat = "ppron12" || cat = "ppron3" || cat = "numcomp" || cat = "" then "other" else
  if cat = "cond" then "cond" else
  failwith ("find_entry_cat: " ^ cat)

let assign_entry_cat dict =
  Xlist.rev_map dict (fun entry ->
    {entry with cat = find_entry_cat entry})

let split_dict in_path filename out_path =
  let dict = load_tab (in_path ^ filename) in
  let dict = List.rev (assign_entry_cat dict) in
  let filename = if Xstring.check_sufix ".gz" filename then
    Xstring.cut_sufix ".gz" filename else filename in
  File.file_out (out_path ^ "noun_" ^ filename) (fun noun_file ->
  File.file_out (out_path ^ "adj_" ^ filename) (fun adj_file ->
  File.file_out (out_path ^ "adv_" ^ filename) (fun adv_file ->
  File.file_out (out_path ^ "verb_" ^ filename) (fun verb_file ->
  File.file_out (out_path ^ "other_" ^ filename) (fun other_file ->
  Xlist.iter dict (fun entry ->
    let form = get_form entry in
    try
      let file = match entry.cat with
          "noun" -> noun_file
        | "adj" -> adj_file
        | "adv" -> adv_file
        | "verb" -> verb_file
        | "other" -> other_file
        | "cond" -> raise Not_found
        | _ -> failwith "split_dict" in
      fprintf file "%s\t%s\t%s\n" form.orth entry.lemma form.interp
    with Not_found -> ()))))))

let split_language lang_filename in_path filename out_path =
  let map = File.fold_tab lang_filename StringMap.empty (fun map -> function
      [lemma; lang] -> StringMap.add_inc map lemma lang (fun lang2 -> print_endline ("split_language: " ^ lemma ^ " " ^ lang ^ " " ^ lang2); lang)
    | line -> failwith ("split_language: " ^ (String.concat "\t" line))) in
  let dict = load_tab (in_path ^ filename) in
  let dict = List.rev (assign_entry_cat dict) in
  let filename = if Xstring.check_sufix ".gz" filename then
    Xstring.cut_sufix ".gz" filename else filename in
  let dict_map = Xlist.fold dict StringMap.empty (fun dict_map e ->
    try
      let lang = StringMap.find map e.lemma in
      (* let lang = "all" in *)
      StringMap.add_inc dict_map lang [e] (fun l -> e :: l)
    with Not_found -> dict_map) in
  StringMap.iter dict_map (fun lang dict ->
    File.file_out (out_path ^ "lang_" ^ lang ^ "_" ^ filename) (fun file ->
      Xlist.iter dict (fun entry ->
        let form = get_form entry in
        fprintf file "%s\t%s\t%s\n" form.orth entry.lemma form.interp)))


let merge_entries dict =
  let dict = assign_entry_cat dict in
  let map = Xlist.fold dict StringMap.empty (fun map entry ->
    let form = get_form entry in
    let key =
      if entry.cat = "noun" then
        if form.interp = "" then failwith "merge_entries: empty interp" else
        let gender = match Xstring.split ":" (List.hd (Xstring.split "|" form.interp)) with
            ["depr";_;_;"m2"] -> "m1"
          | "depr" :: _ -> failwith ("merge_entries: " ^ form.interp)
          | [_;_;_;gender] -> gender
          | [_;_;_;gender;col] -> gender ^ ":" ^ col
          | _ -> failwith ("merge_entries: " ^ form.interp) in
        entry.lemma ^ "|" ^ entry.cat ^ "|" ^ gender
      else entry.lemma ^ "|" ^ entry.cat in
    StringMap.add_inc map key entry (fun e ->
      if entry.proper_type <> e.proper_type then
        failwith ("merge_entries: " ^ key ^ " " ^ entry.proper_type ^ " " ^ e.proper_type) else
      {e with forms = form :: e.forms})) in
  StringMap.fold map [] (fun dict _ e -> e :: dict)

let remove_cat cat dict =
  Xlist.fold dict [] (fun dict entry ->
    if entry.cat = cat then dict
    else entry :: dict)

let rec get_aspect lemma = function
    (f : form) :: l ->
      (match Xstring.split ":" f.interp with
        ["inf";a] -> a
      | ["ger";_;_;_;a;_] -> a
      | _ -> get_aspect lemma l)
  | [] -> (*failwith ("get_aspect: " ^ lemma)*) "unk"

let epsilon_lemmata = StringSet.of_list [
  "Berkeley"; "Blake"; "Bourdelle"; "Boyle"; "Boy"; "Braille"; "Braque"; "Brooke"; "Halley";
  "Constable"; "Corneille"; "Delavigne"; "Doyle"; "Drake"; "Dunaway"; "Faraday"; "Gable"; "Gay";
  "George"; "Heaney"; "Google"; "Desargues"; "Disney"; "Doumergue"; "Gaulle"; "Hemingway"; "Hubble";
  "Huxley"; "Jokai"; "Joule"; "Jókai"; "Lisle"; "Locke"; "Losey"; "Macaulay"; "May";
  "McCartney"; "Remarque"; "Searle"; "Shelley"; "Sisley"; "Stanley"; "Thackeray"; "Updike"; "Winfrey";
  "assemblage"; "boutique"; "branle"; "cartridge"; "chippendale"; "collage"; "collége"; "decoupage"; "dodge";
  "entourage"; "freestyle"; "grisaille"; "grunge"; "joule"; "quiche"; "remake"; "rocaille"; "scrabble";
  "siècle"; "playboy"; "oldboy"; "jockey"; "image"; "college"; "cockney"; "Montaigne"; "";
  "allemande"; "anglaise"; "beguine"; "breakdance"; "courante"; "ensemble"; "high-life"; "iPhone"; "iphone";
  "rolls-royce"; "Redgrave"; "cowboy"; "drive"; "Nightingale"; "Presley"; "Ampére"; "Apollinaire"; "Aristide";
  "Arrow"; "Astaire"; "Baudelaire"; "Bellonte"; "Bellow"; "Benveniste"; "Bernadotte"; "Brantôme"; "Bruce";
  "Burne"; "Cezanne"; "Christophe"; "Claude"; "Cruise"; "Daguerre"; "Daisne"; "Danone"; "Faure";
  "Fayette"; "Fontaine"; "Forsythe"; "France"; "Ghelderode"; "Gide"; "Irvine"; "Jarre"; "Laine";
  "Lamartine"; "Larousse"; "Lawrence"; "Leconte"; "Lefebvre"; "Livingstone"; "Lumiere"; "Magritte"; "Matisse";
  "Milne"; "Moliére"; "Montmartre"; "Montparnasse"; "Moore"; "Morse"; "Pierre"; "Racine"; "Robespierre";
  "Sade"; "Sartre"; "Saussure"; "Shakespeare"; "Stone"; "Swinburne"; "Taine"; "Tate"; "Verlaine";
  "Verne"; "Wallace"; "Wayne"; "White"; "Wilde"; "boy"; "brumaire"; "cicerone"; "deadline";
  "empire"; "frimaire"; "genre"; "guide"; "hardcore"; "hardware"; "hate"; "house"; "interface";
  "mainframe"; "offside"; "penthouse"; "performance"; "ragtime"; "regime"; "savoir-vivre"; "software"; "strip-tease";
  "timbre"; "tricorne"; "unicode"; "upgrade"; "Capote"; "Greenpeace"; ""; ""; "";
  ""; ""; ""; ""; ""; ""; ""; ""; "";
  ""; ""; ""; ""; ""; ""; ""; ""; "";
  ]

let e_y_lemmata = StringSet.of_list [
  "Montesquieu"; "Richelieu"; "Rushdie"; "Muskie";
  "Gillespie"; "Depardieu"; "Christie"; "Carnegie"; "Bandtkie"; "Barrie"; ""; ""; "";
  ""; ""; ""; ""; ""; ""; ""; ""; "";
  ]

let e_ndm_lemmata = StringSet.of_list [
  "Barrès"; "Beaumarchais"; "Marchais"; "Rabelais"; "Resnais"; "Mérimée"; "";
  ""; ""; ""; ""; ""; ""; ""; ""; "";
  ""; ""; ""; ""; ""; ""; ""; ""; "";
  ]

let y_lemmata = StringSet.of_list [
  "Bogorodckij"; "Gaudí"; "Szeptyćkyj"; "Toynbee"; "Trubieckoj"; "Wołżskij"; "yuppie"; "Toynbee"; "Zrínyi";
  "Selye"; "Nagy"; ""; ""; ""; ""; ""; ""; "";
  ""; ""; ""; ""; ""; ""; ""; ""; "";
  ""; ""; ""; ""; ""; ""; ""; ""; "";
  ]

let get_lemma_suf lemma =
  let lemma = Stem.simplify_lemma lemma in
  if StringSet.mem epsilon_lemmata lemma then "ε" else
  if StringSet.mem e_ndm_lemmata lemma then "e/ndm" else
  if StringSet.mem e_y_lemmata lemma then "e/y" else
  if StringSet.mem y_lemmata lemma then "y" else
  let lemma_suf =
    if lemma = "" then "" else
    List.hd (List.rev (Xunicode.utf8_chars_of_utf8_string lemma)) in
  match lemma_suf with
    "a" -> "a"
  | "e" -> "e"
  | "o" -> "o"
  | "y" -> "y"
  | "i" -> "y"
  | "ę" -> "ę"
  | _ -> "ε"

let get_orth_suf orth =
  let orth_suf =
    if orth = "" then "" else
    List.hd (List.rev (Xunicode.utf8_chars_of_utf8_string orth)) in
  match orth_suf with
    "j" -> "j"
  | "e" -> "e"
  | _ -> "ε"

let merge_interps lemma forms =
  let lemma_suf = get_lemma_suf lemma in
  let map = Xlist.fold forms StringMap.empty (fun map form ->
    (* printf "merge_interps 1: %s %s\n%!" form.orth form.interp; *)
    StringMap.add_inc map form.orth (StringSet.singleton form.interp) (fun set -> StringSet.add set form.interp)) in
  StringMap.fold map [] (fun forms orth set ->
    (* printf "merge_interps 2: %s %s\n%!" orth (String.concat " " (StringSet.to_list set)); *)
    let orth_suf = get_orth_suf orth in
    match lemma_suf, Xlist.sort (StringSet.to_list set) compare with
      _,["adv"] -> {empty_form with orth=orth; interp="adv:pos"} :: forms
    | _,["adv";"adv:pos"] -> {empty_form with orth=orth; interp="adv:pos"} :: forms
    | _,["adj:pl:acc:m2.m3.f.n:pos";"adj:pl:nom.voc:m2.m3.f.n:pos";"adj:sg:acc:n:pos";"adj:sg:nom.voc:n:pos"] -> {empty_form with orth=orth; interp="adj:sg:nom.acc.voc:n:pos|adj:pl:nom.acc.voc:m2.m3.f.n:pos"} :: forms
    | _,["adj:sg:acc:m1.m2:pos";"adj:sg:gen:m1.m2.m3.n:pos"] -> {empty_form with orth=orth; interp="adj:sg:gen:m1.m2.m3.n:pos|adj:sg:acc:m1.m2:pos"} :: forms
    | _,["adj:sg:dat:f:pos";"adj:sg:gen:f:pos";"adj:sg:loc:f:pos"] -> {empty_form with orth=orth; interp="adj:sg:gen.dat.loc:f:pos"} :: forms
    | _,["adj:sg:acc:m3:pos";"adj:sg:nom.voc:m1.m2.m3:pos"] -> {empty_form with orth=orth; interp="adj:sg:nom.voc:m1.m2.m3:pos|adj:sg:acc:m3:pos"} :: forms
    | _,["adj:pl:acc:m1:pos";"adj:pl:gen:m1.m2.m3.f.n:pos";"adj:pl:loc:m1.m2.m3.f.n:pos"] -> {empty_form with orth=orth; interp="adj:pl:gen.loc:m1.m2.m3.f.n:pos|adj:pl:acc:m1:pos"} :: forms
    | _,["adj:pl:dat:m1.m2.m3.f.n:pos";"adj:sg:inst:m1.m2.m3.n:pos";"adj:sg:loc:m1.m2.m3.n:pos"] -> {empty_form with orth=orth; interp="adj:sg:inst.loc:m1.m2.m3.n:pos|adj:pl:dat:m1.m2.m3.f.n:pos"} :: forms
    | _,["adj:sg:acc:f:pos";"adj:sg:inst:f:pos"] -> {empty_form with orth=orth; interp="adj:sg:acc.inst:f:pos"} :: forms
    | _,["adj:pl:nom.voc:m1:pos";"adj:sg:acc:m3:pos";"adj:sg:nom.voc:m1.m2.m3:pos"] -> {empty_form with orth=orth; interp="adj:sg:nom.voc:m1.m2.m3:pos|adj:sg:acc:m3:pos|adj:pl:nom.voc:m1:pos"} :: forms
    | _,["adj:sg:acc:m3:pos";"adj:sg:nom:m1.m2.m3:pos"] -> {empty_form with orth=orth; interp="adj:sg:nom:m1.m2.m3:pos|adj:sg:acc:m3:pos"} :: forms
    | _,["adj:pl:acc:m2.m3.f.n:pos";"adj:pl:nom.voc:m2.m3.f.n:pos"] -> {empty_form with orth=orth; interp="adj:pl:nom.acc.voc:m2.m3.f.n:pos"} :: forms
    | _,["adj:sg:acc:n:pos";"adj:sg:nom.voc:n:pos"] -> {empty_form with orth=orth; interp="adj:sg:nom.acc.voc:n:pos"} :: forms
    | _,["adj:sg:acc:n:pos";"adj:sg:nom.voc:n:pos";"adja"] -> {empty_form with orth=orth; interp="adj:sg:nom.acc.voc:n:pos|adja"} :: forms
    | _,["adj:pl:nom:m2.m3.f.n:pos";"adj:sg:nom:n:pos"] -> {empty_form with orth=orth; interp="adj:sg:nom:n:pos|adj:pl:nom:m2.m3.f.n:pos"} :: forms
    | _,["adj:pl:acc:m2.m3.f.n:sup";"adj:pl:nom.voc:m2.m3.f.n:sup";"adj:sg:acc:n:sup";"adj:sg:nom.voc:n:sup"] -> {empty_form with orth=orth; interp="adj:sg:nom.acc.voc:n:sup|adj:pl:nom.acc.voc:m2.m3.f.n:sup"} :: forms
    | _,["adj:sg:acc:m1.m2:sup";"adj:sg:gen:m1.m2.m3.n:sup"] -> {empty_form with orth=orth; interp="adj:sg:gen:m1.m2.m3.n:sup|adj:sg:acc:m1.m2:sup"} :: forms
    | _,["adj:sg:dat:f:sup";"adj:sg:gen:f:sup";"adj:sg:loc:f:sup"] -> {empty_form with orth=orth; interp="adj:sg:gen.dat.loc:f:sup"} :: forms
    | _,["adj:sg:acc:m3:sup";"adj:sg:nom.voc:m1.m2.m3:sup"] -> {empty_form with orth=orth; interp="adj:sg:nom.voc:m1.m2.m3:sup|adj:sg:acc:m3:sup"} :: forms
    | _,["adj:pl:acc:m1:sup";"adj:pl:gen:m1.m2.m3.f.n:sup";"adj:pl:loc:m1.m2.m3.f.n:sup"] -> {empty_form with orth=orth; interp="adj:pl:gen.loc:m1.m2.m3.f.n:sup|adj:pl:acc:m1:sup"} :: forms
    | _,["adj:pl:dat:m1.m2.m3.f.n:sup";"adj:sg:inst:m1.m2.m3.n:sup";"adj:sg:loc:m1.m2.m3.n:sup"] -> {empty_form with orth=orth; interp="adj:sg:inst.loc:m1.m2.m3.n:sup|adj:pl:dat:m1.m2.m3.f.n:sup"} :: forms
    | _,["adj:sg:acc:f:sup";"adj:sg:inst:f:sup"] -> {empty_form with orth=orth; interp="adj:sg:acc.inst:f:sup"} :: forms
    | _,["adj:pl:acc:m2.m3.f.n:com";"adj:pl:nom.voc:m2.m3.f.n:com";"adj:sg:acc:n:com";"adj:sg:nom.voc:n:com"] -> {empty_form with orth=orth; interp="adj:sg:nom.acc.voc:n:com|adj:pl:nom.acc.voc:m2.m3.f.n:com"} :: forms
    | _,["adj:sg:acc:m1.m2:com";"adj:sg:gen:m1.m2.m3.n:com"] -> {empty_form with orth=orth; interp="adj:sg:gen:m1.m2.m3.n:com|adj:sg:acc:m1.m2:com"} :: forms
    | _,["adj:sg:dat:f:com";"adj:sg:gen:f:com";"adj:sg:loc:f:com"] -> {empty_form with orth=orth; interp="adj:sg:gen.dat.loc:f:com"} :: forms
    | _,["adj:sg:acc:m3:com";"adj:sg:nom.voc:m1.m2.m3:com"] -> {empty_form with orth=orth; interp="adj:sg:nom.voc:m1.m2.m3:com|adj:sg:acc:m3:com"} :: forms
    | _,["adj:pl:acc:m1:com";"adj:pl:gen:m1.m2.m3.f.n:com";"adj:pl:loc:m1.m2.m3.f.n:com"] -> {empty_form with orth=orth; interp="adj:pl:gen.loc:m1.m2.m3.f.n:com|adj:pl:acc:m1:com"} :: forms
    | _,["adj:pl:dat:m1.m2.m3.f.n:com";"adj:sg:inst:m1.m2.m3.n:com";"adj:sg:loc:m1.m2.m3.n:com"] -> {empty_form with orth=orth; interp="adj:sg:inst.loc:m1.m2.m3.n:com|adj:pl:dat:m1.m2.m3.f.n:com"} :: forms
    | _,["adj:sg:acc:f:com";"adj:sg:inst:f:com"] -> {empty_form with orth=orth; interp="adj:sg:acc.inst:f:com"} :: forms
    | _,["adj:pl:acc:m1:pos";"adj:pl:acc:m2.m3.f.n:pos";"adj:pl:dat:m1.m2.m3.f.n:pos";"adj:pl:gen:m1.m2.m3.f.n:pos";
       "adj:pl:inst:m1.m2.m3.f.n:pos";"adj:pl:loc:m1.m2.m3.f.n:pos";"adj:pl:nom.voc:m1:pos";"adj:pl:nom.voc:m2.m3.f.n:pos";
       "adj:sg:acc:f:pos";"adj:sg:acc:m1.m2:pos";"adj:sg:acc:m3:pos";"adj:sg:acc:n:pos";"adj:sg:dat:f:pos";
       "adj:sg:dat:m1.m2.m3.n:pos";"adj:sg:gen:f:pos";"adj:sg:gen:m1.m2.m3.n:pos";"adj:sg:inst:f:pos";"adj:sg:inst:m1.m2.m3.n:pos";
       "adj:sg:loc:f:pos";"adj:sg:loc:m1.m2.m3.n:pos";"adj:sg:nom.voc:f:pos";"adj:sg:nom.voc:m1.m2.m3:pos";"adj:sg:nom.voc:n:pos"] -> {empty_form with orth=orth; interp="adj:sg.pl:nom.gen.dat.acc.inst.loc.voc:m1.m2.m3.f.n:pos"} :: forms
    | _,["adj:pl:acc:m1:pos";"adj:pl:acc:m2.m3.f.n:pos";"adj:pl:dat:m1.m2.m3.f.n:pos";"adj:pl:gen:m1.m2.m3.f.n:pos";
       "adj:pl:inst:m1.m2.m3.f.n:pos";"adj:pl:loc:m1.m2.m3.f.n:pos";"adj:pl:nom.voc:m1:pos";"adj:pl:nom.voc:m2.m3.f.n:pos";
       "adj:sg:acc:f:pos";"adj:sg:acc:m1.m2:pos";"adj:sg:acc:m3:pos";"adj:sg:acc:n:pos";"adj:sg:dat:f:pos";
       "adj:sg:dat:m1.m2.m3.n:pos";"adj:sg:gen:f:pos";"adj:sg:gen:m1.m2.m3.n:pos";"adj:sg:inst:f:pos";"adj:sg:inst:m1.m2.m3.n:pos";
       "adj:sg:loc:f:pos";"adj:sg:loc:m1.m2.m3.n:pos";"adj:sg:nom.voc:f:pos";"adj:sg:nom.voc:m1.m2.m3:pos";"adj:sg:nom.voc:n:pos";"adja"] -> {empty_form with orth=orth; interp="adj:sg.pl:nom.gen.dat.acc.inst.loc.voc:m1.m2.m3.f.n:pos|adja"} :: forms
    | _,["ger:pl:nom.acc:n:imperf.perf:aff";"ger:sg:gen:n:imperf.perf:aff"] -> {empty_form with orth=orth; interp="ger:sg:gen:n:imperf.perf:aff|ger:pl:nom.acc:n:imperf.perf:aff"} :: forms
    | _,["ppas:pl:nom.acc.voc:m2.m3.f.n:imperf.perf:aff";"ppas:sg:nom.acc.voc:n:imperf.perf:aff"] -> {empty_form with orth=orth; interp="ppas:sg:nom.acc.voc:n:imperf.perf:aff|ppas:pl:nom.acc.voc:m2.m3.f.n:imperf.perf:aff"} :: forms
    | _,["ppas:sg:acc:m1.m2:imperf.perf:aff";"ppas:sg:gen:m1.m2.m3.n:imperf.perf:aff"] -> {empty_form with orth=orth; interp="ppas:sg:gen:m1.m2.m3.n:imperf.perf:aff|ppas:sg:acc:m1.m2:imperf.perf:aff"} :: forms
    | _,["ppas:sg:acc:m3:imperf.perf:aff";"ppas:sg:nom.voc:m1.m2.m3:imperf.perf:aff"] -> {empty_form with orth=orth; interp="ppas:sg:nom.voc:m1.m2.m3:imperf.perf:aff|ppas:sg:acc:m3:imperf.perf:aff"} :: forms
    | _,["ppas:pl:acc:m1:imperf.perf:aff";"ppas:pl:gen.loc:m1.m2.m3.f.n:imperf.perf:aff"] -> {empty_form with orth=orth; interp="ppas:pl:gen.loc:m1.m2.m3.f.n:imperf.perf:aff|ppas:pl:acc:m1:imperf.perf:aff"} :: forms
    | _,["ppas:pl:dat:m1.m2.m3.f.n:imperf.perf:aff";"ppas:sg:inst.loc:m1.m2.m3.n:imperf.perf:aff"] -> {empty_form with orth=orth; interp="ppas:sg:inst.loc:m1.m2.m3.n:imperf.perf:aff|ppas:pl:dat:m1.m2.m3.f.n:imperf.perf:aff"} :: forms
    | _,["pact:pl:nom.acc.voc:m2.m3.f.n:imperf:aff";"pact:sg:nom.acc.voc:n:imperf:aff"] -> {empty_form with orth=orth; interp="pact:sg:nom.acc.voc:n:imperf:aff|pact:pl:nom.acc.voc:m2.m3.f.n:imperf:aff"} :: forms
    | _,["pact:sg:acc:m1.m2:imperf:aff";"pact:sg:gen:m1.m2.m3.n:imperf:aff"] -> {empty_form with orth=orth; interp="pact:sg:gen:m1.m2.m3.n:imperf:aff|pact:sg:acc:m1.m2:imperf:aff"} :: forms
    | _,["pact:pl:nom.voc:m1:imperf:aff";"pact:sg:acc:m3:imperf:aff";"pact:sg:nom.voc:m1.m2.m3:imperf:aff"] -> {empty_form with orth=orth; interp="pact:sg:nom.voc:m1.m2.m3:imperf:aff|pact:sg:acc:m3:imperf:aff|pact:pl:nom.voc:m1:imperf:aff"} :: forms
    | _,["pact:pl:acc:m1:imperf:aff";"pact:pl:gen.loc:m1.m2.m3.f.n:imperf:aff"] -> {empty_form with orth=orth; interp="pact:pl:gen.loc:m1.m2.m3.f.n:imperf:aff|pact:pl:acc:m1:imperf:aff"} :: forms
    | _,["pact:pl:dat:m1.m2.m3.f.n:imperf:aff";"pact:sg:inst.loc:m1.m2.m3.n:imperf:aff"] -> {empty_form with orth=orth; interp="pact:sg:inst.loc:m1.m2.m3.n:imperf:aff|pact:pl:dat:m1.m2.m3.f.n:imperf:aff"} :: forms
    | _,["ger:pl:nom.acc:n:imperf.perf:neg";"ger:sg:gen:n:imperf.perf:neg"] -> {empty_form with orth=orth; interp="ger:sg:gen:n:imperf.perf:neg|ger:pl:nom.acc:n:imperf.perf:neg"} :: forms
    | _,["ppas:pl:nom.acc.voc:m2.m3.f.n:imperf.perf:neg";"ppas:sg:nom.acc.voc:n:imperf.perf:neg"] -> {empty_form with orth=orth; interp="ppas:sg:nom.acc.voc:n:imperf.perf:neg|ppas:pl:nom.acc.voc:m2.m3.f.n:imperf.perf:neg"} :: forms
    | _,["ppas:sg:acc:m1.m2:imperf.perf:neg";"ppas:sg:gen:m1.m2.m3.n:imperf.perf:neg"] -> {empty_form with orth=orth; interp="ppas:sg:gen:m1.m2.m3.n:imperf.perf:neg|ppas:sg:acc:m1.m2:imperf.perf:neg"} :: forms
    | _,["ppas:sg:acc:m3:imperf.perf:neg";"ppas:sg:nom.voc:m1.m2.m3:imperf.perf:neg"] -> {empty_form with orth=orth; interp="ppas:sg:nom.voc:m1.m2.m3:imperf.perf:neg|ppas:sg:acc:m3:imperf.perf:neg"} :: forms
    | _,["ppas:pl:acc:m1:imperf.perf:neg";"ppas:pl:gen.loc:m1.m2.m3.f.n:imperf.perf:neg"] -> {empty_form with orth=orth; interp="ppas:pl:gen.loc:m1.m2.m3.f.n:imperf.perf:neg|ppas:pl:acc:m1:imperf.perf:neg"} :: forms
    | _,["ppas:pl:dat:m1.m2.m3.f.n:imperf.perf:neg";"ppas:sg:inst.loc:m1.m2.m3.n:imperf.perf:neg"] -> {empty_form with orth=orth; interp="ppas:sg:inst.loc:m1.m2.m3.n:imperf.perf:neg|ppas:pl:dat:m1.m2.m3.f.n:imperf.perf:neg"} :: forms
    | _,["pact:pl:nom.acc.voc:m2.m3.f.n:imperf:neg";"pact:sg:nom.acc.voc:n:imperf:neg"] -> {empty_form with orth=orth; interp="pact:sg:nom.acc.voc:n:imperf:neg|pact:pl:nom.acc.voc:m2.m3.f.n:imperf:neg"} :: forms
    | _,["pact:sg:acc:m1.m2:imperf:neg";"pact:sg:gen:m1.m2.m3.n:imperf:neg"] -> {empty_form with orth=orth; interp="pact:sg:gen:m1.m2.m3.n:imperf:neg|pact:sg:acc:m1.m2:imperf:neg"} :: forms
    | _,["pact:pl:nom.voc:m1:imperf:neg";"pact:sg:acc:m3:imperf:neg";"pact:sg:nom.voc:m1.m2.m3:imperf:neg"] -> {empty_form with orth=orth; interp="pact:sg:nom.voc:m1.m2.m3:imperf:neg|pact:sg:acc:m3:imperf:neg|pact:pl:nom.voc:m1:imperf:neg"} :: forms
    | _,["pact:pl:acc:m1:imperf:neg";"pact:pl:gen.loc:m1.m2.m3.f.n:imperf:neg"] -> {empty_form with orth=orth; interp="pact:pl:gen.loc:m1.m2.m3.f.n:imperf:neg|pact:pl:acc:m1:imperf:neg"} :: forms
    | _,["pact:pl:dat:m1.m2.m3.f.n:imperf:neg";"pact:sg:inst.loc:m1.m2.m3.n:imperf:neg"] -> {empty_form with orth=orth; interp="pact:sg:inst.loc:m1.m2.m3.n:imperf:neg|pact:pl:dat:m1.m2.m3.f.n:imperf:neg"} :: forms
    | _,["ger:pl:gen:n:imperf.perf:aff";"inf:imperf.perf"] -> {empty_form with orth=orth; interp="ger:pl:gen:n:imperf.perf:aff"} :: {empty_form with orth=orth; interp="inf:imperf.perf"} :: forms
    | _,["praet:sg:m1.m2.m3:imperf.perf";"praet:sg:m1.m2.m3:imperf.perf:nagl"] -> {empty_form with orth=orth; interp="praet:sg:m1.m2.m3:imperf.perf:nagl"} :: forms
    | _,["fin:sg:ter:imperf.perf";"ger:sg:nom.acc:n:imperf.perf:aff"] -> {empty_form with orth=orth; interp="fin:sg:ter:imperf.perf"} :: {empty_form with orth=orth; interp="ger:sg:nom.acc:n:imperf.perf:aff"} :: forms
    | _,["ger:pl:gen:n:imperf.perf:aff";"impt:sg:sec:imperf.perf"] -> {empty_form with orth=orth; interp="ger:pl:gen:n:imperf.perf:aff"} :: {empty_form with orth=orth; interp="impt:sg:sec:imperf.perf"} :: forms
    | _,["fin:pl:ter:imperf.perf";"ppas:sg:acc.inst:f:imperf.perf:aff"] -> {empty_form with orth=orth; interp="fin:pl:ter:imperf.perf"} :: {empty_form with orth=orth; interp="ppas:sg:acc.inst:f:imperf.perf:aff"} :: forms
    | "a",["subst:sg:dat.loc:f";"subst:sg:gen:f"] ->
          if orth_suf = "j" then {empty_form with orth=orth; interp="subst:sg:gen.dat.loc:f"} :: forms
          else {empty_form with orth=orth; interp="subst:sg:gen:f"} :: {empty_form with orth=orth; interp="subst:sg:dat.loc:f"} :: forms
    (* | "a",["subst:pl:gen:f";"subst:sg:dat.loc:f";"subst:sg:gen:f"] -> (*print_endline lemma;*) {empty_form with orth=orth; interp="subst:pl:gen:f"} :: {empty_form with orth=orth; interp="subst:sg:dat.loc:f"} :: {empty_form with orth=orth; interp="subst:sg:gen:f"} :: forms *)
    | "a",["subst:pl:gen:f";"subst:pl:loc:f"] -> {empty_form with orth=orth; interp="subst:pl:gen.loc:f"} :: forms
    | "ε",["subst:sg:gen.acc:m1";"subst:sg:gen:m1"] -> {empty_form with orth=orth; interp="subst:sg:gen.acc:m1"} :: forms
    | "ε",["subst:sg:gen.acc:m2";"subst:sg:gen:m2"] -> {empty_form with orth=orth; interp="subst:sg:gen.acc:m2"} :: forms
    | "ε",["subst:sg:loc:m1";"subst:sg:voc:m1"]-> {empty_form with orth=orth; interp="subst:sg:loc.voc:m1"} :: forms
    | "ε",["subst:sg:loc:m2";"subst:sg:voc:m2"]-> {empty_form with orth=orth; interp="subst:sg:loc.voc:m2"} :: forms
    | "ε",["subst:sg:loc:m3";"subst:sg:voc:m3"]-> {empty_form with orth=orth; interp="subst:sg:loc.voc:m3"} :: forms
    | "ε",["subst:sg:gen:m3";"subst:sg:loc:m3";"subst:sg:voc:m3"] -> {empty_form with orth=orth; interp="subst:sg:loc.voc:m3"} :: {empty_form with orth=orth; interp="subst:sg:gen:m3"} :: forms
    | "ε",["subst:sg:acc:f";"subst:sg:nom:f"] -> {empty_form with orth=orth; interp="subst:sg:nom.acc:f"} :: forms
    | "ε",["subst:pl:gen:f";"subst:pl:nom.acc.voc:f";"subst:sg:dat.loc:f";"subst:sg:gen:f";"subst:sg:voc:f"] -> {empty_form with orth=orth; interp="subst:sg:gen.dat.loc.voc:f|subst:pl:gen:f"} :: {empty_form with orth=orth; interp="subst:pl:nom.acc.voc:f"} :: forms
    | "ε",["subst:pl:gen:f";"subst:sg:dat.loc:f";"subst:sg:gen:f";"subst:sg:voc:f"] -> {empty_form with orth=orth; interp="subst:sg:gen.dat.loc.voc:f|subst:pl:gen:f"} :: forms
    | "y",["subst:sg:nom:m1";"subst:sg:voc:m1"] -> {empty_form with orth=orth; interp="subst:sg:nom.voc:m1"} :: forms
    | "y",["subst:sg:gen.acc:m1";"subst:sg:gen:m1"] -> {empty_form with orth=orth; interp="subst:sg:gen.acc:m1"} :: forms
    | "y",["subst:pl:gen.acc:m1";"subst:pl:loc:m1"] -> {empty_form with orth=orth; interp="subst:pl:gen.acc.loc:m1"} :: forms
    | "y",["subst:pl:dat:m1";"subst:sg:inst:m1";"subst:sg:loc:m1"] -> {empty_form with orth=orth; interp="subst:sg:inst.loc:m1|subst:pl:dat:m1"} :: forms
    | "y",["subst:sg:nom:m2";"subst:sg:voc:m2"] -> {empty_form with orth=orth; interp="subst:sg:nom.voc:m2"} :: forms
    | "y",["subst:pl:nom.voc:m1";"subst:sg:nom:m1";"subst:sg:voc:m1"] -> {empty_form with orth=orth; interp="subst:sg:nom.voc:m1"} :: {empty_form with orth=orth; interp="subst:pl:nom.voc:m1"} :: forms
    | "y",["subst:sg:gen.acc:m2";"subst:sg:gen:m2"] -> {empty_form with orth=orth; interp="subst:sg:gen.acc:m2"} :: forms
    | "y",["subst:pl:gen:m2";"subst:pl:loc:m2"] -> {empty_form with orth=orth; interp="subst:pl:gen.acc.loc:m2"} :: forms
    | "y",["subst:pl:dat:m2";"subst:sg:inst:m2";"subst:sg:loc:m2"] -> {empty_form with orth=orth; interp="subst:sg:inst.loc:m2|subst:pl:dat:m2"} :: forms
    | "y",["subst:sg:nom.acc:m3";"subst:sg:voc:m3"] -> {empty_form with orth=orth; interp="subst:sg:nom.acc.voc:m3"} :: forms
    | "y",["subst:pl:gen:m3";"subst:pl:loc:m3"] -> {empty_form with orth=orth; interp="subst:pl:gen.loc:m3"} :: forms
    | "y",["subst:pl:dat:m3";"subst:sg:inst:m3";"subst:sg:loc:m3"] -> {empty_form with orth=orth; interp="subst:sg:inst.loc:m3|subst:pl:dat:m3"} :: forms
    | "y",["subst:sg:dat.loc:f";"subst:sg:gen:f";"subst:sg:nom:f";"subst:sg:voc:f"] -> {empty_form with orth=orth; interp="subst:sg:nom.gen.dat.loc.voc:f"} :: forms
(* en *)    | "y",["subst:sg:loc:m2";"subst:sg:voc:m2"]-> {empty_form with orth=orth; interp="subst:sg:loc.voc:m2"} :: forms
(* en *)    | "y",["subst:sg:gen:m3";"subst:sg:loc:m3";"subst:sg:voc:m3"] -> {empty_form with orth=orth; interp="subst:sg:loc.voc:m3"} :: {empty_form with orth=orth; interp="subst:sg:gen:m3"} :: forms
(* en *)    | "y",["subst:sg:loc:m3";"subst:sg:voc:m3"]-> {empty_form with orth=orth; interp="subst:sg:loc.voc:m3"} :: forms
    | "e",["depr:pl:nom.acc.voc:m2";"subst:sg:nom:m1";"subst:sg:voc:m1"] -> {empty_form with orth=orth; interp="subst:sg:nom.voc:m1|depr:pl:nom.acc.voc:m2"} :: forms
    | "e",["depr:pl:nom.acc.voc:m2";"subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:m1";"subst:sg:nom:m1";"subst:sg:voc:m1"] -> {empty_form with orth=orth; interp="subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:m1|depr:pl:nom.acc.voc:m2"} :: {empty_form with orth=orth; interp="subst:sg:nom.voc:m1|depr:pl:nom.acc.voc:m2"} :: forms
    | "e",["subst:sg:gen.acc:m1";"subst:sg:gen:m1"] -> {empty_form with orth=orth; interp="subst:sg:gen.acc:m1"} :: forms
    | "e",["subst:sg:inst:m1";"subst:sg:loc:m1"] -> {empty_form with orth=orth; interp="subst:sg:inst.loc:m1"} :: forms
    | "e",["subst:pl:dat:n:ncol";"subst:sg:inst:n:ncol";"subst:sg:loc:n:ncol"] -> {empty_form with orth=orth; interp="subst:sg:inst.loc:n:ncol"} :: {empty_form with orth=orth; interp="subst:pl:dat:n:ncol"} :: forms
    | "e",["subst:sg:inst:n:ncol";"subst:sg:loc:n:ncol"] -> {empty_form with orth=orth; interp="subst:sg:inst.loc:n:ncol"} :: forms
    | "e",["subst:pl:gen:n:ncol";"subst:pl:loc:n:ncol"] -> {empty_form with orth=orth; interp="subst:pl:gen.loc:n:ncol"} :: forms
    | "e",["subst:sg:dat:n:ncol";"subst:sg:loc:n:ncol"] -> {empty_form with orth=orth; interp="subst:sg:dat.loc:n:ncol"} :: forms
    | "e",["subst:pl:nom.acc.voc:n:ncol";"subst:sg:gen:n:ncol"] -> {empty_form with orth=orth; interp="subst:sg:gen:n:ncol|subst:pl:nom.acc.voc:n:ncol"} :: forms
    | "o",["subst:sg:nom.voc:m1"] -> {empty_form with orth=orth; interp="subst:sg:nom:m1"} :: {empty_form with orth=orth; interp="subst:sg:voc:m1"} :: forms
    | "o",["subst:sg:gen.acc:m1";"subst:sg:gen:m1"] -> {empty_form with orth=orth; interp="subst:sg:gen.acc:m1"} :: forms
    | "o",["depr:pl:nom.acc.voc:m2";"subst:pl:nom.voc:m1";"subst:sg:gen.acc:m1";"subst:sg:gen:m1"] -> {empty_form with orth=orth; interp="subst:sg:gen.acc:m1"} :: {empty_form with orth=orth; interp="subst:pl:nom.voc:m1"} :: {empty_form with orth=orth; interp="depr:pl:nom.acc.voc:m2"} :: forms
    | "o",["subst:sg:dat.loc:m1"] -> {empty_form with orth=orth; interp="subst:sg:dat:m1"} :: {empty_form with orth=orth; interp="subst:sg:loc:m1"} :: forms
    | "o",["subst:sg:gen.acc:m2";"subst:sg:gen:m2"] -> {empty_form with orth=orth; interp="subst:sg:gen.acc:m2"} :: forms
    | "o",["subst:pl:dat:m1:pt";"subst:pl:loc:m1:pt"] -> {empty_form with orth=orth; interp="subst:pl:dat.loc:m1:pt"} :: forms
    | "ε",["subst:sg:dat:n:ncol";"subst:sg:gen:n:ncol";"subst:sg:inst:n:ncol";"subst:sg:loc:n:ncol";"subst:sg:nom.acc.voc:n:ncol"] -> {empty_form with orth=orth; interp="subst:sg:nom.gen.dat.acc.inst.loc.voc:n:ncol"} :: forms
    | "ε",["subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:n:ncol";"subst:sg:dat:n:ncol";"subst:sg:gen:n:ncol";"subst:sg:inst:n:ncol";"subst:sg:loc:n:ncol";"subst:sg:nom.acc.voc:n:ncol"] -> {empty_form with orth=orth; interp="subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:n:ncol"} :: {empty_form with orth=orth; interp="subst:sg:nom.gen.dat.acc.inst.loc.voc:n:ncol"} :: forms
    | "ę",["subst:sg:dat:n:col";"subst:sg:loc:n:col"] -> {empty_form with orth=orth; interp="subst:sg:dat.loc:n:col"} :: forms
    | "ę",["subst:sg:dat:n:ncol";"subst:sg:loc:n:ncol"] -> {empty_form with orth=orth; interp="subst:sg:dat.loc:n:ncol"} :: forms
    | "o",["subst:sg:loc:m1";"subst:sg:voc:m1"] ->
          if orth_suf = "e" then {empty_form with orth=orth; interp="subst:sg:loc.voc:m1"} :: forms
          else {empty_form with orth=orth; interp="subst:sg:loc:m1"} :: {empty_form with orth=orth; interp="subst:sg:voc:m1"} :: forms
    | "e/ndm",["depr:pl:nom.acc.voc:m2";"subst:pl:dat:m1";"subst:pl:gen.acc:m1";"subst:pl:inst:m1";"subst:pl:loc:m1";"subst:pl:nom.voc:m1";"subst:sg:nom:m1";"subst:sg:voc:m1"] ->
        {empty_form with orth=orth; interp="subst:sg:nom.voc:m1|depr:pl:nom.acc.voc:m2"} :: {empty_form with orth=orth; interp="subst:pl:nom.gen.dat.acc.inst.loc.voc:m1"} :: forms
    | "e/ndm",["depr:pl:nom.acc.voc:m2";"subst:pl:dat:m1";"subst:pl:gen.acc:m1";"subst:pl:inst:m1";"subst:pl:loc:m1";"subst:pl:nom.voc:m1";"subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:m1";"subst:sg:nom:m1";"subst:sg:voc:m1"] ->
        {empty_form with orth=orth; interp="subst:sg:nom.voc:m1|depr:pl:nom.acc.voc:m2"} :: {empty_form with orth=orth; interp="subst:pl:nom.gen.dat.acc.inst.loc.voc:m1"} :: {empty_form with orth=orth; interp="subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:m1"} :: forms
    | "e/ndm",["subst:sg:gen.acc:m1";"subst:sg:gen:m1"] -> {empty_form with orth=orth; interp="subst:sg:gen.acc:m1"} :: forms
    | "e/ndm",["subst:sg:inst:m1";"subst:sg:loc:m1"] -> {empty_form with orth=orth; interp="subst:sg:inst.loc:m1"} :: forms
    | "e/y",["depr:pl:nom.acc.voc:m2";"subst:pl:nom.voc:m1";"subst:sg:nom:m1";"subst:sg:voc:m1"] ->
        {empty_form with orth=orth; interp="subst:pl:nom.voc:m1"} :: {empty_form with orth=orth; interp="subst:sg:nom.voc:m1|depr:pl:nom.acc.voc:m2"} :: forms
    | "e/y",["depr:pl:nom.acc.voc:m2";"subst:pl:nom.voc:m1";"subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:m1";"subst:sg:nom:m1";"subst:sg:voc:m1"] ->
        {empty_form with orth=orth; interp="subst:pl:nom.voc:m1"} :: {empty_form with orth=orth; interp="subst:sg:nom.voc:m1|depr:pl:nom.acc.voc:m2"} :: {empty_form with orth=orth; interp="subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:m1"} :: forms
    | "e/y",["depr:pl:nom.acc.voc:m2";"subst:sg:nom:m1";"subst:sg:voc:m1"] -> {empty_form with orth=orth; interp="subst:sg:nom.voc:m1|depr:pl:nom.acc.voc:m2"} :: forms
    | "e/y",["subst:pl:dat:m1";"subst:sg:inst:m1";"subst:sg:loc:m1"] -> {empty_form with orth=orth; interp="subst:sg:inst.loc:m1|subst:pl:dat:m1"} :: forms
    | "e/y",["subst:pl:gen.acc:m1";"subst:pl:loc:m1"] -> {empty_form with orth=orth; interp="subst:pl:gen.acc.loc:m1"} :: forms
    | "e/y",["subst:sg:gen.acc:m1";"subst:sg:gen:m1"] -> {empty_form with orth=orth; interp="subst:sg:gen.acc:m1"} :: forms
    | "e/y",["subst:sg:inst:m1";"subst:sg:loc:m1"] -> {empty_form with orth=orth; interp="subst:sg:inst.loc:m1"} :: forms
    | _,["depr:pl:nom.acc.voc:m2";"subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:m1"] -> {empty_form with orth=orth; interp="subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:m1|depr:pl:nom.acc.voc:m2"} :: forms
    | _,[interp] -> {empty_form with orth=orth; interp=interp} :: forms
    | _,interps ->
        (* print_endline ("merge_interps: " ^ lemma_suf ^ (*" " ^ orth ^*) " [\"" ^ String.concat "\";\"" interps ^ "\"]"); *)
        Xlist.fold interps forms (fun forms interp ->
          {empty_form with orth=orth; interp=interp} :: forms))

let process_interps dict =
  Xlist.rev_map dict (fun entry ->
    if entry.cat = "verb" then
      let aspect = get_aspect entry.lemma entry.forms in
      let forms = Xlist.rev_map entry.forms (fun f ->
        let interp = match (Xstring.split ":" f.interp) with
            ["fin";n;p;_] -> String.concat ":" ["fin";n;p;"imperf.perf"]
          | ["impt";n;p;_] -> String.concat ":" ["impt";n;p;"imperf.perf"]
          | "pcon" :: _ -> f.interp
          | "pacta" :: _ -> f.interp
          | "pact" :: _ -> f.interp
          | ["ger";n;c;g;_;a] -> String.concat ":" ["ger";n;c;g;"imperf.perf";a]
          | ["praet";n;g;_] -> String.concat ":" ["praet";n;g;"imperf.perf"]
          | ["praet";n;g;_;a] -> String.concat ":" ["praet";n;g;"imperf.perf";a]
          | ["inf";_] -> String.concat ":" ["inf";"imperf.perf"]
          | ["pant";_] -> String.concat ":" ["pant";"imperf.perf"]
          | ["imps";_] -> String.concat ":" ["imps";"imperf.perf"]
          | ["ppas";n;c;g;_;a] -> String.concat ":" ["ppas";n;c;g;"imperf.perf";a]
          | _ -> (*print_endline ("process_interps: " ^ f.interp);*) f.interp in
        {f with interp=interp}) in
      let forms = merge_interps entry.lemma forms in
      {entry with aspect=aspect; forms=forms} else
    {entry with forms=merge_interps entry.lemma entry.forms})

let process_polimorf_gender g =
  String.concat "." (Xlist.map (Xstring.split "\\." g) (function
      "n1" -> "n"
    | "n2" -> "n"
    | "p1" -> "m1"
    | "p2" -> "n"
    | "p3" -> "n"
    | s -> s))

let process_polimorf_interps dict =
  Xlist.rev_map dict (fun entry ->
    let forms = Xlist.rev_map entry.forms (fun f ->
      let interp = match (Xstring.split ":" f.interp) with
          ["ger";n;c;g;a;ac] -> String.concat ":" ["ger";n;c;process_polimorf_gender g;a;ac]
        | ["praet";n;g;a] -> String.concat ":" ["praet";n;process_polimorf_gender g;a]
        | ["praet";n;g;a;ac] -> String.concat ":" ["praet";n;process_polimorf_gender g;a;ac]
        | "praet" :: _ -> failwith "process_polimorf_interps"
        | ["ppas";n;c;g;a;ac] -> String.concat ":" ["ppas";n;c;process_polimorf_gender g;a;ac]
        | ["pact";n;c;g;a;ac] -> String.concat ":" ["pact";n;c;process_polimorf_gender g;a;ac]
        | ["adj";n;c;g;gr] -> String.concat ":" ["adj";n;c;process_polimorf_gender g;gr]
        | ["adv"] -> "adv:pos"
        | ["subst";n;c;"n1"] -> String.concat ":" ["subst";n;c;"n";"col"]
        | ["subst";n;c;"n2"] -> String.concat ":" ["subst";n;c;"n";"ncol"]
        | ["subst";n;c;"p1"] -> String.concat ":" ["subst";n;c;"m1";"pt"]
        | ["subst";n;c;"p2"] -> String.concat ":" ["subst";n;c;"n";"pt"]
        | ["subst";n;c;"p3"] -> String.concat ":" ["subst";n;c;"n";"pt"]
        | _ -> f.interp in
      {f with interp=interp}) in
    {entry with forms=forms})

(**********************************************************************************)

(*let mark_ndm dict =
  Xlist.fold dict [] (fun dict entry ->
    if entry.cat <> "noun" &&  entry.cat <> "adj" then entry :: dict else
    let map = Xlist.fold entry.forms StringMap.empty (fun map form ->
      StringMap.add_inc map form.interp (StringSet.singleton form.orth) (fun set -> StringSet.add set form.orth)) in
    let qmap = StringMap.fold map StringQMap.empty (fun qmap interp orths ->
      StringSet.fold orths qmap StringQMap.add) in
    let n = StringMap.size map in
    let found = StringQMap.fold qmap [] (fun found orth v ->
      if v = n then orth :: found else found) in
    match found with
      [] -> entry :: dict
    | [orth] ->
        let ndm,odm = Xlist.fold entry.forms ([],[]) (fun (ndm,odm) form ->
          if form.orth = orth then form :: ndm, odm else ndm, form :: odm) in
        let dict = {entry with forms=odm} :: dict in
        {entry with forms=ndm; ndm=true} :: dict
    | _ -> failwith ("mark_ndm: " ^ (String.concat " " found)))

let print_ndm filename dict =
  File.file_out filename (fun file ->
    Xlist.iter dict (fun entry ->
      if entry.ndm then
        let orth = (List.hd entry.forms).orth in
        fprintf file "%s\t%s\t%s\n" orth entry.lemma entry.cat))

let remove_ndm dict =
  Xlist.fold dict [] (fun dict entry ->
    if entry.ndm then dict
    else entry :: dict)

let remove_not_ndm dict =
  Xlist.fold dict [] (fun dict entry ->
    if not entry.ndm then dict
    else entry :: dict)*)


let kolwiek_lemmas = StringSet.of_list [
  (* adj *)
  "czyjkolwiek"; "czyjś"; "czyjże"; "jakiciś"; "jakikolwiek"; "jakisi"; "jakiś"; "jakiści";
  "jakiściś"; "jakiśkolwiek"; "jakiż"; "jakiżkolwiek"; "jakowyś"; "kijże"; "kiż"; "którykolwiek";
  "któryś"; "któryż"; "któryżkolwiek"; "niejakiś"; "takiż"; "takowyż"; "tenże"; "tyliż"; "ówże";
  (* noun *)
  "cokolwiek:s"; "cośkolwiek"; "cóżkolwiek"; "ktokolwiek"; "ktośkolwiek"; "któżkolwiek";
  "cociś"; "cosi"; "cosik"; "cosiś"; "coś:s"; "cościś"; "coże"; "cóż";
  "ktoś:s2"; "któż";
  (* adv *)
  "jakkolwiek"; "jakoś"; "małoż"; "niejakkolwiek"; "niejakoś"; (*"niemalże";*) ]

let kolwiek_suffixes = [
  "żkolwiek"; "żekolwiek"; "śkolwiek"; "kolwiek"; "ż"; "że"; "ściś"; "ciś"; "ś"; "ści"; "sik"; "si"]

let find_kolwiek_suffixes dict =
  Xlist.rev_map dict (fun entry ->
    if StringSet.mem kolwiek_lemmas entry.lemma then
      {entry with forms=Xlist.map entry.forms (fun form ->
          {form with orth=Xlist.fold kolwiek_suffixes form.orth (fun orth kolwiek_suf ->
            if Xstring.check_sufix kolwiek_suf orth then
              Xstring.cut_sufix kolwiek_suf orth
            else orth)})}
    else entry)

let exceptional_lemmata = StringSet.of_list ([
  (* wiele stemów *)
  "Apollo"; "Aujeszky"; "Białystok"; "Gózd"; "Krasnystaw"; "Różanystok"; "Wielkanoc"; "białagłowa";
  "deszcz"; "imćpan"; "iściec"; "otrząs"; "rzeczpospolita"; "wilczełyko"; "woleoczko";

  "prapraojciec"; "praojciec"; "ojciec"; "współbrat"; "spółbrat"; "półbrat"; "brat";
  "półczłowiek"; "przedczłowiek"; "praczłowiek"; "nadczłowiek"; "git-człowiek"; "człowiek";
  "półdziecko"; "+lecie"; "zimoziele"; "ziele"; "trójziele"; "nasienie";
  "ksiądz"; "dech"; "tydzień"; "roczek:s2"; "rok:s1"; "przechrzest"; "chrzest";
  "dziecko"; "ucho:s2"; "oko:s2"; "cześć:s"; "jo-jo"; "Zabłotce"; "tysiąc:s1"; "półmiesiąc"; "miesiąc"; ""; ""; "";
  "Pia"; "ręka"; "człek"; "Kozak:s1"; "bóg"; "psubrat"; "pieniądz"; ""; ""; ""; "";
  "kto"; "ktokolwiek"; "ktoś:s2"; "ktośkolwiek"; "któż"; "któżkolwiek"; "nikt"; "nic";
  "co:s"; "cociś"; "cokolwiek:s"; "cosi"; "cosik"; "cosiś"; "coś:s"; "cościś"; "cośkolwiek"; "coże"; "cóż"; "cóżkolwiek";
  "niebiosa"; "Włochy:s1"; "Niemcy"; "Węgry"; "Austro-Węgry"; ""; ""; ""; ""; ""; ""; "";
  "zając:s1"; "tysiąc:s2"; "wszyscy"; ""; ""; ""; ""; ""; ""; ""; ""; "";
(*  "ZHR"; "WAT"; "VAT"; "PAT"; "FAT"; "DAT"; "PAGART"; "PIT:s2"; "PIT:s1"; "OIT:s2"; "OIT:s1"; "CIT";
  "NOT"; "LOT"; "KRRiT"; "OIT"; ""; ""; ""; ""; ""; ""; ""; "";*)
  "bliscy"; "ojcowie"; "teściowie"; "ichmościowie"; "wujkowie"; "staruszkowie"; "pradziadkowie"; ""; ""; ""; ""; "";
  "małżonkowie"; "kochankowie"; "dziadkowie"; "rozwiedzeni"; "nieliczni"; "chrzestni"; ""; ""; ""; ""; ""; "";
  "starzy"; "wasi"; "nasi"; "najmłodsi"; "dzisiejsi"; ""; ""; ""; ""; ""; ""; "";
  "IKEA"; "stajnia"; "kuchnia:s"; "suknia"; "minisuknia"; "głównia"; "głownia"; "dźwignia"; ""; ""; ""; "";
  "workowiśnia"; "wiśnia"; "sośnia"; "laurowiśnia"; "studnia"; "idea"; "imienie"; ""; ""; ""; ""; "";
  "makao"; "macao"; "kakao"; "Akademgorodok"; "yuppi"; "hippie"; "yuppie"; ""; ""; ""; ""; "";
  "Uj"; "PIT"; "ChAT"; "podczłowiek"; "nieczłowiek"; "cześć"; "ktoś"; "ktosik"; ""; ""; ""; "";
  "+ówna"; "+yna"; "+ina"; "+anka"; "+owa"; "co"; "cokolwiek"; "coś"; "cośtam"; ""; ""; "";
  "zając"; "tysiąc"; "rok"; "roczek"; "oko"; "ucho"; "Włochy"; "niebiosy"; "wici"; ""; ""; "";
  "André"; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; "";

  "zły:a"; "dobry:a"; "przymały"; "mały:a"; "duży"; "wielki:a";
  "ppoż."; "ppanc."; "pepanc."; "midi:a"; ""; ""; ""; ""; ""; ""; ""; "";
(*  "zwać"; "wiedzieć"; "pójść"; "przejść"; "dojść"; "zsiąść"; "iść"; ""; ""; ""; ""; "";
  "być"; "zasłonić"; "słonić"; "przysłonić"; "przesłonić"; "osłonić"; "odsłonić"; ""; ""; ""; ""; "";*)

  (*
  (* błąd w słowniku *)
  "ówże";
  (* wiele stemów *)
  "twój:a"; "swój"; "mój:a"; "wszystek";
  (* oboczności w stemie *)
  "co:s"; "cociś"; "cokolwiek:s"; "cosi"; "cosik"; "cosiś"; "coś:s"; "cościś"; "cośkolwiek"; "coże"; "cóż"; "cóżkolwiek";
  "kto"; "ktokolwiek"; "ktoś:s2"; "ktośkolwiek"; "któż"; "któżkolwiek"; "nikt"; "nic";
  "Angel"; "Apollo"; "Białystok"; "Bober"; "Dzięgiel"; "Engel"; "Gołąb:s2"; "Gózd"; "Hendel"; "Herschel"; "Jastrząb";
  "Kodrąb:s2"; "Kozioł"; "Krasnystaw"; "Majcher"; "Ob"; "Omulew"; "Orzeł"; "Różanystok"; "Schuster"; "Stępień"; "Słonim";
  "Wielkanoc"; "achtel"; "archiprezbiter"; "arcydzięgiel"; "bedel"; "ber"; "białagłowa"; "białodrzew"; "ceter"; "deszcz";
  "drama"; "dziób:s1"; "dzięgiel"; "dżemper"; "falafel"; "grubodziób"; "harbajtel"; "harbejtel"; "harmider"; "imćpan";
  "iściec"; "jarząb:s2"; "kierdel"; "kimel"; "kiper:s1"; "klaster"; "kliper"; "kosodrzew"; "kureń"; "manczester";
  "nadpiersień"; "osep"; "otrząs"; "pedel"; "piksel"; "podpiersień"; "podziem"; "prezbiter"; "protokół"; "przedpiersień";
  "ratel"; "rondel:s2"; "rozpiór:s1"; "rozpiór:s2"; "rzeczpospolita"; "rzep:s2"; "rzepień"; "rzewień"; "rąb"; "sosrąb";
  "srebrnodrzew"; "swąd"; "szmermel"; "szpiegierz"; "ulster"; "wab:s2"; "wermiszel"; "wilczełyko"; "woleoczko"; "włosień:s2";
  "zew"; "złotogłów"; "świreń"; "źreb"; "żółtodziób";
  "człowiek"; "półczłowiek"; "przedczłowiek"; "praczłowiek"; "nadczłowiek"; "git-człowiek"; ""; ""; ""; ""; ""; ""; ""; "";
  "przechrzest"; "chrzest"; "półdziecko"; "roczek:s2"; "rok:s1"; "tydzień"; ""; ""; ""; ""; ""; "";
  (* oboczności w odmianie *)
  "niekażdy"; "każdy"; "niektóry:a"; "który"; "tenże"; "ten"; "tamten"; "kijże";
  "ucho:s2"; "dziecko"; "oko:s2"; "imię"; "nozdrze";
  "ZHR"; "WAT"; "VAT"; "PAT"; "FAT"; "DAT"; "PAGART"; "PIT:s2"; "PIT:s1"; "OIT:s2"; "OIT:s1"; "CIT";
  "NOT"; "LOT"; "KRRiT"; ""; ""; ""; ""; ""; ""; ""; ""; "";
  "być"; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; "";
  ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; "";*)
  (* pozostawione *)
  "czyjże"; "czyjś"; "czyjkolwiek"; "kiż"; "ów"; "ow"; "on:a"; "ki";
  "Pia"; "jo-jo"; "+lecie"; "";
(*  "zagrząźć"; "zrzeć";
  (* niepełny paradygmat *)
  "zróść"; "zląc"; "zaróść"; "zaprząc"; "zaprzysiąc"; "zanieść:v2"; "zaląc"; "wzróść"; "wyróść"; "wyprząc"; "wyprzysiąc";
  "róść"; "sprzysiąc"; "sprząc"; "ugrząźć"; "uląc"; "upiec:v2"; "uprząc"; "uróść"; "wieść:v2"; "wprząc"; "wróść"; "wyląc";
  "powieść:v2"; "posiąc"; "przeląc"; "przeprząc"; "przeróść"; "przyprząc"; "przysiąc"; "przyróść"; "prząc"; "pójść:v2"; "rozprząc"; "rozróść";
  "krzywoprzysiąc"; "ląc"; "naróść"; "obróść"; "odprzysiąc"; "odprząc"; "odróść"; "oprzysiąc"; "podróść"; "pogrząźć"; "poprzysiąc"; "poróść";
  "dojść:v2"; "doprząc"; "doróść"; "dosiąc"; "grząźć"; "iść:v2";
  (* wiele stemów *)
  "uwlec"; "wewlec"; "wlec"; "wwlec"; "wywlec"; "wyżec"; "zawlec"; "zażec"; "zewlec"; "zwlec"; "zżec"; "żec";
  "podwlec"; "podżec"; "powlec:v1"; "powlec:v2"; "przeoblec"; "przewlec"; "przeżec"; "przyoblec"; "przywlec"; "przyżec"; "rozwlec"; "rozżec";
  "dowlec"; "nawlec"; "oblec:v2"; "obwlec"; "odwlec"; "owlec"; "zeżreć";
  (* inne *)
  "liźć"; "iść:v1"; "wyniść"; "wynijść"; "wyjść"; "wniść"; "wnijść"; "wejść"; "ujść"; "rozejść"; "pójść:v1"; "przyjść"; "przejść:v2"; "przejść:v1"; "podejść"; "odejść"; "obejść:v2"; "obejść:v1"; "najść:v2"; "najść:v1"; "nadejść"; "dojść:v1";
  "roztworzyć:v2"; "przetworzyć:v2"; "otworzyć";
  "zsiąść:v2"; "zsiąść:v1"; "zesiąść"; "zasiąść"; "wysiąść"; "współposiąść"; "wsiąść"; "usiąść"; "siąść"; "rozsiąść"; "przysiąść"; "przesiąść"; "powsiąść"; "posiąść"; "podsiąść"; "osiąść"; "obsiąść"; "nasiąść"; "dosiąść";
  "źreć:v1"; "zniść"; "znijść"; "znajść"; "zejść"; "zejść"; "zajść:v2"; "zajść:v1"; "wzniść"; "wznijść"; "wzejść"*)
(*
   "moi"; "twoi";
  (*"AIDS"; "BGŻ"; "BWZ"; "BZ";*) (*"Bandtkie";*) (*"CRZZ"; "FPŻ";*) (*"Jokai"; "Jókai"; "Linde";*)(* "MSZ"; "MWGzZ"; *)
  (*"NSZ"; "OPZZ";*) "Radetzky"; "Tagore"; (*"UNZ"; "URz"; "WBZ"; "ZSZ"; "ZWZ"; "ZZ";*) "aids";
  "arcyksiężna"; "cornflakes"; "księżna"; (*"scrabble";*) "sms"; "teścina";
  "Wielkanoc"; "białagłowa"; "rzeczpospolita"; "imćpan";
  "Ob"; "podziem"; "Pia"; "woleoczko"; "wilczełyko"; "jo-jo"; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; "";
  "Omulew"; "drama"; (*"Kayah";*) "ratel"; "grubodziób"; "rozpiór:s1"; "ceter"; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; "";
  "DJ"; "FIFA"; (*"manicure"; "Greenpeace"; "Google";*) ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; "";
  "włosień:s2"; "deszcz"; "falafel"; "Krasnystaw";
  "Różanystok"; "Białystok"; "ZHR"; "rzep:s2"; ""; ""; ""; ""; ""; ""; ""; ""; ""; "";
  "IKEA"; "makao"; "macao"; "kakao"; ""; ""; ""; ""; ""; ""; ""; ""; ""; "";
  "dziecko"; "oko:s2"; "ucho:s2"; "półdziecko"; "b-cia"; ""; ""; ""; ""; ""; ""; ""; ""; "";
  "idea"; "ręka"; "cześć:s"; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; "";
  "ABBA"; "UEFA"; "FAMA"; "SABENA"; "MENA"; "APA"; "NASA"; "ANSA";
  "NAFTA"; "LETTA"; "ETA"; "ELTA"; "EFTA"; "CEFTA";
  "WAT"; "VAT"; "PAT"; "FAT"; "DAT"; "PAGART";
  "PIT:s2"; "PIT:s1"; "OIT:s2"; "OIT:s1"; "CIT"; "NOT"; "LOT"; "KRRiT";
  "człowiek"; "półczłowiek"; "przedczłowiek"; "praczłowiek"; "nadczłowiek"; "git-człowiek"; ""; ""; ""; ""; ""; ""; ""; "";
  "szwa"; "hawanna"; "butaforia"; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; "";
  "Skopie"; "Mathea"; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; "";
  "poema:s1"; "klima:s1"; "dylema"; "dilemma"; "apoftegma"; "aksjoma"; ""; ""; ""; ""; ""; ""; ""; "";
  "burgrabia"; "gograbia"; "grabia"; "hrabia"; "margrabia"; "murgrabia"; "sędzia:s1"; "wicehrabia"; "współsędzia";
  "cieśla"; "bibliopola"; "świszczypałka"; "śwircałka"; "świerczałka"; "ścierciałka"; "tatka"; "sługa:s1"; "stupajka:s1"; "stepka"; "starowinka:s2"; "skurczypałka"; "mężczyzna"; "klecha";
  ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; "";
  ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; "";
  ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; "";
  ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; "";*)
  (* "unixowy"; "unixowość"; "survivalowy"; "survivalowość"; "survivalowiec"; "software’owy"; "software’owość"; "software’owo"; "rock’n’rollowy"; "rock’n’rollowość"; "rock’n’rollowo"; "qumrańskość";
  "qumrański"; "quizowy"; "quizowość"; "queerowy"; "queerowość"; "quadowy"; "quadowiec"; "oxfordzkość"; "oxfordzki"; "novellowskość"; "novellowski"; "nieunixowy";
  "nieunixowość"; "niesurvivalowy"; "niesurvivalowość"; "niesoftware’owy"; "niesoftware’owość"; "nierock’n’rollowy"; "nierock’n’rollowość"; "nierock’n’rollowo"; "niequmrańskość";
  "niequmrański"; "niequizowy"; "niequizowość"; "niequeerowość"; "niequeerowo"; "niequadowy"; "nieoxfordzkość"; "nieoxfordzki"; "nienovellowskość"; "nienovellowski"; "nienewage'owy"; "nieliverpoolskość";
  "nieliverpoolski"; "niekickboxingowy"; "niekickboxingowość"; "nieheavymetalowy"; "nieheavymetalowość"; "nieheavymetalowo"; "nieharvardzkość"; "nieharvardzki"; "niedeveloperskość";
  "niedeveloperski"; "niedaviscupowy"; "niedaviscupowość"; "niebrexitowość"; "nieampexowy"; "nieampexowość"; "newage'owy"; "newage'owość"; "newage'owo"; "liverpoolskość"; "liverpoolski";
  "Akademgorodok"; "berceuse"; "colloquium"; "cornflakes"; "dacia"; "Dziubanii"; "epeisodion"; "facsimile"; "felicia"; "Garcia"; "Giedroyc"; "glediczia";
  "głasnost"; "hippie"; "Kodaly"; "KRRiT"; "lancia"; "Murii"; "Nagy"; "PAGART"; "paparazzo"; "Praha"; "pudźa"; "Selye";
  "welwiczia"; "yuppi"; "yuppie"; "Zápolya"; "Zrínyi"; ""; ""; ""; ""; ""; ""; ""; *)
  ] (*@ File.load_lines "../morphology/data/obce.tab" @ File.load_lines "../morphology/data/akronimy.tab" @*)
    (*@ File.fold_tab "../morphology/data/obce_langs.tab" [] (fun l x -> List.hd x :: l)*) @
    (*File.load_lines "../morphology/data/nieregularne.tab" @ File.load_lines "results/interp_validated_verb.tab" @ *)
    (*File.load_lines "results/interp_validated_noun.tab" @ File.load_lines "results/interp_validated_adj.tab" @
    File.load_lines "../morphology/data/validated_adj.tab" @ File.load_lines "../morphology/data/validated_noun.tab" @
    File.load_lines "../morphology/data/validated_verb.tab" @ File.load_lines "../morphology/data/adv_nieodprzymiotnikowe.tab" *) [])

let remove_exceptional_lemmata dict =
  Xlist.fold dict [] (fun dict entry ->
    if StringSet.mem exceptional_lemmata entry.lemma then dict
    else entry :: dict)

let remove_exceptional_lemmata_gen ex dict =
  Xlist.fold dict [] (fun dict entry ->
    if StringSet.mem ex entry.lemma then dict
    else entry :: dict)

(* let select_lemmata set dict =
  Xlist.fold dict [] (fun dict entry ->
    if StringSet.mem set entry.lemma then entry :: dict
    else dict) *)

let generate_stem dict =
  Xlist.rev_map dict (fun entry ->
    {entry with stem=
      (* if entry.ndm then (List.hd entry.forms).orth else *)
      if entry.cat = "noun" || entry.cat = "adj" || entry.cat = "adv" || entry.cat = "verb" then
        Stem.generate_stem entry
      else ""})

(*let phon_generate_stem dict =
  Xlist.rev_map dict (fun entry ->
    {entry with phon_stem=
      if entry.ndm then (List.hd entry.forms).phon_orth else
      if entry.cat = "noun" || entry.cat = "adj" || entry.cat = "adv" || entry.cat = "verb" then
        Stem.phon_generate_stem entry
      else []})*)

let generate_stem_lu dict =
  Xlist.rev_map dict (fun entry ->
    {entry with lu_stem=Stem.generate_stem_lu entry.lemma1 entry.lemma2})

let lowercase_lu dict =
  Xlist.rev_map dict (fun entry ->
    {entry with
      lemma1=Xunicode.lowercase_utf8_string entry.lemma1;
      lemma2=Xunicode.lowercase_utf8_string entry.lemma2})


let fonetic_translation_entry e =
      (* let lemma = Stem.simplify_lemma e.lemma in *)
      (* let phon_lemma = Fonetics.translate_and_check true Fonetics.rules Fonetics.rev_rules lemma in *)
      let rules = match e.cat with
          "noun" -> Fonetics.rules
        | "" -> failwith "fonetic_translation"
        | _ -> Fonetics.core_rules in
      let phon_stem = Fonetics.translate true rules e.stem in
      {e with (*phon_lemma = phon_lemma;*) phon_stem=Xlist.map phon_stem (fun s -> s.phon);
        forms = Xlist.map e.forms (fun f ->
          let phon_orth = Fonetics.translate true rules f.orth in
          {f with phon_orth = phon_orth})}

(* let fonetic_translation dict =
  Xlist.fold dict [] (fun dict e ->
    try (fonetic_translation_entry e) :: dict
    with
      Fonetics.NotFound(x,s) -> printf "NF %s %s %s\n%!" e.lemma x s; dict
    | Fonetics.NotEqual(x,s,t) -> printf "NE %s %s %s %s\n%!" e.lemma x s t; dict
    | Fonetics.MulipleSolutions(x,s,l) -> printf "MS %s %s %s: %s\n%!" e.lemma x s (String.concat " " l); dict
    | _ -> dict) *)

let select_rev_rules e =
  match e.cat with
    "noun" -> Fonetics.rev_rules
  | "" -> failwith "select_rev_rules"
  | _ -> Fonetics.core_rev_rules

let create_candidates interp_flag rules e =
  let e = try fonetic_translation_entry e with _ -> print_endline ("fonetic_translation_entry: " ^ e.lemma); {e with forms=[]} in
  let simple_lemma = Stem.simplify_lemma e.lemma in
  let phon_rev_rules = select_rev_rules e in
  let forms = Xlist.rev_map e.forms (fun f ->
    (* if f.orth = "poljom" then printf "phon_orths: \n  %s\n%!" (String.concat "\n  " (Xlist.map f.phon_orth Fonetics.string_of_phon)); *)
    let candidates = Xlist.fold f.phon_orth [] (fun candidates s ->
      Xlist.fold (MorphologyRules.CharTrees.find rules s.phon) candidates (fun candidates (stem,rule) ->
          let candidate_lemmas = Fonetics.translate(*_simple*) true phon_rev_rules (stem ^ rule.set) in
          let candidate_lemmas = Xlist.fold candidate_lemmas [] (fun candidate_lemmas candidate_lemma ->
            (* if f.orth = "poljom" then (if candidate_lemma = simple_lemma then printf "E" else printf " ");
            if f.orth = "poljom" then printf " %s %s %s %s\n%!" s.phon stem (string_of_rule rule) candidate_lemma; *)
            if candidate_lemma.phon = simple_lemma then candidate_lemma :: candidate_lemmas else candidate_lemmas) in
          if candidate_lemmas <> [] && ((not interp_flag) || f.interp = rule.interp) then (stem,rule,s,candidate_lemmas) :: candidates else candidates)) in
    {f with candidates=candidates}) in
  {e with forms=forms}

let phon_validate rules dict =
  Xlist.rev_map dict (fun entry ->
    let entry = create_candidates false rules entry in
    let forms = Xlist.rev_map entry.forms (fun form ->
      if form.candidates = [] then {form with validated=false} else {form with validated=true}) in
    {entry with forms=forms})

(*let validate rules dict =
  Xlist.rev_map dict (fun entry ->
    let simple_lemma = Stem.simplify_lemma entry.lemma in
    let forms = Xlist.rev_map entry.forms (fun form ->
      let candidates = MorphologyRules.CharTrees.find rules form.orth in
      let candidates = Xlist.fold candidates [] (fun candidates (stem,rule) ->
        if stem ^ rule.set = simple_lemma then (stem,rule) :: candidates else candidates) in
      if candidates = [] then {form with validated=false} else {form with validated=true}) in
    {entry with forms=forms})*)

let validate_lu rules dict =
  Xlist.rev_map dict (fun entry ->
    let candidates1 = MorphologyRules.CharTrees.find rules entry.lemma1 in
    let candidates2 = MorphologyRules.CharTrees.find rules entry.lemma2 in
    let b = Xlist.fold candidates1 false (fun b (stem1,rule1) ->
      Xlist.fold candidates2 b (fun b (stem2,rule2) ->
        (* Printf.printf "%s %s %s %s\n%!" stem1 stem2 (string_of_rule rule1) (string_of_rule rule1); *)
        if stem1 ^ rule1.set = stem2 ^ rule2.set then true else b)) in
    (* if b then print_endline "validated"; *)
    let b1 = Xlist.fold candidates1 false (fun b (stem1,rule1) -> if stem1 = entry.lu_stem then true else b) in
    let b2 = Xlist.fold candidates2 false (fun b (stem2,rule2) -> if stem2 = entry.lu_stem then true else b) in
    {entry with lu_validated=b; validated1=b1; validated2=b2})

let validate_interp rules dict =
  Xlist.rev_map dict (fun entry ->
    let simple_lemma = Stem.simplify_lemma entry.lemma in
    let forms = Xlist.rev_map entry.forms (fun form ->
      let candidates = Xlist.fold (MorphologyRules.CharTrees.find rules form.orth) [] (fun candidates (stem,rule) ->
        if stem ^ rule.set = simple_lemma && form.interp = rule.interp then
          (stem,rule) :: candidates else candidates) in
      if candidates = [] then ((*printf "validate_interp: %s\t%s\t%s\n" form.orth entry.lemma form.interp;*) {form with validated=false}) else {form with validated=true}) in
    {entry with forms=forms})

let interp_translation = Xlist.fold [
  "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:m3", "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:m3";
  "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:f", "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:f";
  "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:n:ncol", "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:n:ncol";
  ] StringMap.empty (fun map (k,v) -> StringMap.add_inc map k (StringSet.singleton v) (fun set -> StringSet.add set v))

let neg_interp_translation = Xlist.fold [
  "adj:sg.pl:nom.gen.dat.acc.inst.loc.voc:m1.m2.m3.f.n:pos", "subst:pl:nom.gen.dat.acc.inst.loc.voc:m1:pt";
  "adj:sg.pl:nom.gen.dat.acc.inst.loc.voc:m1.m2.m3.f.n:pos|adja", "subst:pl:nom.gen.dat.acc.inst.loc.voc:m1:pt";
  "adj:sg.pl:nom.gen.dat.acc.inst.loc.voc:m1.m2.m3.f.n:pos", "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:m3";
  "adj:sg.pl:nom.gen.dat.acc.inst.loc.voc:m1.m2.m3.f.n:pos|adja", "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:m3";
  "adj:sg.pl:nom.gen.dat.acc.inst.loc.voc:m1.m2.m3.f.n:pos", "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:f";
  "adj:sg.pl:nom.gen.dat.acc.inst.loc.voc:m1.m2.m3.f.n:pos|adja", "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:f";
  "subst:pl:nom.gen.dat.acc.inst.loc.voc:n:pt", "subst:pl:nom.gen.dat.acc.inst.loc.voc:m1:pt";
  "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:f", "subst:pl:nom.gen.dat.acc.inst.loc.voc:m1:pt";
  "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:m1", "subst:pl:nom.gen.dat.acc.inst.loc.voc:m1:pt";
  "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:m1|depr:pl:nom.acc.voc:m2", "subst:pl:nom.gen.dat.acc.inst.loc.voc:m1:pt";
  "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:m2", "subst:pl:nom.gen.dat.acc.inst.loc.voc:m1:pt";
  "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:m3", "subst:pl:nom.gen.dat.acc.inst.loc.voc:m1:pt";
  "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:n:ncol", "subst:pl:nom.gen.dat.acc.inst.loc.voc:m1:pt";
  "subst:pl:nom.gen.dat.acc.inst.loc.voc:n:pt", "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:m3";
  "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:f", "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:m3";
  "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:m1", "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:m3";
  "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:m1|depr:pl:nom.acc.voc:m2", "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:m3";
  "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:m2", "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:m3";
  "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:n:ncol", "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:m3";
  "adj:sg.pl:nom.gen.dat.acc.inst.loc.voc:m1.m2.m3.f.n:pos", "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:n:ncol";
  "adj:sg.pl:nom.gen.dat.acc.inst.loc.voc:m1.m2.m3.f.n:pos|adja", "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:n:ncol";
  "subst:pl:nom.gen.dat.acc.inst.loc.voc:n:pt", "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:n:ncol";
  "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:f", "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:n:ncol";
  "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:m1", "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:n:ncol";
  "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:m1|depr:pl:nom.acc.voc:m2", "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:n:ncol";
  "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:m2", "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:n:ncol";
  "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:m3", "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:n:ncol";
  "subst:pl:nom.gen.dat.acc.inst.loc.voc:n:pt", "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:f";
  "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:m1", "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:f";
  "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:m1|depr:pl:nom.acc.voc:m2", "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:f";
  "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:m2", "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:f";
  "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:m3", "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:f";
  "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:n:ncol", "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:f";
  "subst:sg:nom.acc:m3", "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:f";
  "subst:sg:nom:m1", "subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:f";
  "subst:pl:nom.acc.voc:f", "subst:sg:gen:f";
  "subst:sg:gen:f", "subst:pl:nom.acc.voc:f";
  ] StringMap.empty (fun map (k,v) -> StringMap.add_inc map k (StringSet.singleton v) (fun set -> StringSet.add set v))

let expand_interp interp =
  List.flatten (Xlist.map (Xstring.split "|" interp) (fun interp ->
    Xlist.map (Xlist.multiply_list (Xlist.map (Xstring.split ":" interp) (Xstring.split "\\."))) (String.concat ":")))

let is_subset rule_interps form_interps =
  let rule_interps = StringSet.of_list (expand_interp rule_interps) in
  let form_interps = StringSet.of_list (expand_interp form_interps) in
  StringSet.size (StringSet.intersection rule_interps form_interps) = StringSet.size form_interps

let validate_interp_translate rules dict =
  Xlist.rev_map dict (fun entry ->
    let simple_lemma = Stem.simplify_lemma entry.lemma in
    let forms = Xlist.rev_map entry.forms (fun form ->
      let candidates = Xlist.fold (MorphologyRules.CharTrees.find rules form.orth) [] (fun candidates (stem,rule) ->
        if stem ^ rule.set = simple_lemma && is_subset rule.interp form.interp then
          (stem,rule) :: candidates else candidates) in
      if candidates = [] then {form with validated=false} else {form with validated=true}) in
      (* let pos_candidates,neg_candidates = Xlist.fold candidates ([],[]) (fun (pos_candidates,neg_candidates) (stem,rule) ->
        let interps = try StringMap.find interp_translation rule.interp with Not_found -> StringSet.empty in
        if StringSet.mem interps form.interp then
          (stem,rule) :: pos_candidates,neg_candidates else pos_candidates,(stem,rule) :: neg_candidates) in
      if pos_candidates = [] then (
        Xlist.iter neg_candidates (fun (stem,rule) ->
          let neg_interps = try StringMap.find neg_interp_translation rule.interp with Not_found -> StringSet.empty in
          if StringSet.mem neg_interps form.interp then () else
          printf "  \"%s\", \"%s\";\n" rule.interp form.interp);
        {form with validated=false}) else {form with validated=true}) in *)
    {entry with forms=forms})

let phon_validate_interp rules dict =
  Xlist.rev_map dict (fun entry ->
    let entry = create_candidates true rules entry in
    let forms = Xlist.rev_map entry.forms (fun form ->
      if form.candidates = [] then {form with validated=false} else {form with validated=true}) in
    {entry with forms=forms})

let remove_validated_forms dict =
  Xlist.fold dict [] (fun dict entry ->
    let forms = Xlist.fold entry.forms [] (fun forms form ->
      if form.validated then forms else form :: forms) in
    if forms = [] then dict else {entry with forms=forms} :: dict)

let remove_validated_entries dict =
  Xlist.fold dict [] (fun dict entry ->
    let forms = Xlist.fold entry.forms [] (fun forms form ->
      if form.validated then forms else form :: forms) in
    if forms = [] then dict else entry :: dict)

let remove_validated_lu dict =
  Xlist.fold dict [] (fun dict entry ->
    if entry.lu_validated then dict else entry :: dict)

let remove_not_validated_forms dict =
  Xlist.fold dict [] (fun dict entry ->
    let forms = Xlist.fold entry.forms [] (fun forms form ->
      if form.validated then form :: forms else forms) in
    if forms = [] then dict else {entry with forms=forms} :: dict)

let remove_not_validated_entries dict =
  Xlist.fold dict [] (fun dict entry ->
    let forms = Xlist.fold entry.forms [] (fun forms form ->
      if form.validated then form :: forms else forms) in
    if Xlist.size forms <> Xlist.size entry.forms then dict else entry :: dict)

let print filename dict =
  File.file_out filename (fun file ->
    Xlist.iter dict (fun entry ->
      Xlist.iter entry.forms (fun form ->
        fprintf file "%s\t%s\t%s\n" form.orth entry.lemma form.interp)))

let print_lemmata filename dict =
  File.file_out filename (fun file ->
    Xlist.iter dict (fun entry ->
      fprintf file "%s\n" entry.lemma))

let remove_sup_neg_forms dict =
  Xlist.fold dict [] (fun dict entry ->
    let forms = Xlist.fold entry.forms [] (fun forms form ->
      if Xstring.check_sufix ":neg" form.interp || Xstring.check_sufix ":sup" form.interp then
        forms else form :: forms) in
    if forms = [] then dict else {entry with forms=forms} :: dict)

let find_prefix_entries prefixes dict_map simple_lemma =
  Xlist.fold prefixes [] (fun found pref ->
    if Xstring.check_prefix pref simple_lemma then
      let s = Xstring.cut_prefix pref simple_lemma in
      try
        (* print_endline ("find_prefix_entries 1: " ^ s); *)
        StringMap.find dict_map s @ found
      with Not_found -> found
    else found)

(* let check_prefix_orth prefixes l orth =
  Xlist.fold prefixes false (fun b pref ->
    if Xstring.check_prefix pref orth then
      let s = Xstring.cut_prefix pref orth in
      if Xlist.mem l s then true else b
    else b) *)

let prefixes = ["nad";"nade";"na";"do";"od";"ode";"o";"po";"prze";"roz";"roze";"s";"u";"w";"we";"wy";"współ";"za";"ob";"obe";"pod";"pode";"przy";"z";"ze";"współ"]

let remove_prefix_forms path filename out_filename =
  let dict = load_tab (path ^ filename) in
  let dict = merge_entries dict in
  (* let dict = process_interps dict in *)
  (* let dict = remove_sup_neg_forms dict in *)
  let dict_map = Xlist.fold dict StringMap.empty (fun dict_map entry ->
    let simple_lemma = Stem.simplify_lemma entry.lemma in
    StringMap.add_inc dict_map simple_lemma [entry] (fun l -> entry :: l)) in
  (* let dict_map = StringMap.map dict_map (fun l ->
    Xlist.map l (fun entry ->
      Xlist.fold entry.forms StringMap.empty (fun map f ->
        StringMap.add_inc map f.interp [f.orth] (fun l -> f.orth :: l)))) in *)
  let dict = Xlist.fold dict [] (fun dict entry ->
    (* print_endline ("remove_prefix_forms 1: " ^ entry.lemma); *)
    let simple_lemma = Stem.simplify_lemma entry.lemma in
    let cand_entries = find_prefix_entries prefixes dict_map simple_lemma in
(*    print_endline ("remove_prefix_forms 2: " ^ string_of_int (Xlist.size cand_entries));
    let b = Xlist.fold cand_entries false (fun b map ->
      let b2 = Xlist.fold entry.forms true (fun b2 form ->
        print_endline ("remove_prefix_forms 3: " ^ form.orth ^ " " ^ form.interp);
        try
          let l = StringMap.find map form.interp in
         print_endline ("remove_prefix_forms 4: " ^ string_of_int (Xlist.size l));
          b2 && (check_prefix_orth prefixes l form.orth)
        with Not_found -> false) in
      if b2 then true else b) in*)
    (* if cand_entries <> [] then print_endline entry.lemma; *)
    if cand_entries <> [] then dict else entry :: dict) in
  print out_filename dict

let generate_rules rules path filename rules_filename =
  let dict = load_tab (path ^ filename) in
  let dict = merge_entries dict in
  let dict = process_interps dict in
  (* let dict = mark_ndm dict in (* FIXME: remove_ndm? *) *)
  let dict = remove_exceptional_lemmata dict in
  let dict = find_kolwiek_suffixes dict in (* FIXME: lematy z kolwiek_suffixes nie są walidowane *)
  let dict = generate_stem dict in
  let dict = phon_validate rules dict in
  let dict = remove_validated_forms dict in
  let dict = remove_sup_neg_forms dict in (* FIXME *)
  let rules = Xlist.fold dict StringMap.empty (fun rules entry ->
    let entry = fonetic_translation_entry entry in
    Xlist.fold (RuleGenerator.phon_generate_rules_entry entry) rules (fun rules (key,rule) ->
      let rules2 = try StringMap.find rules key with Not_found -> StringMap.empty in
      let rules2 = StringMap.add_inc rules2 rule (1,[entry.lemma]) (fun (q,l) -> q+1, if q < 20 then entry.lemma :: l else l) in
      StringMap.add rules key rules2)) in
  File.file_out rules_filename (fun file ->
    StringMap.iter rules (fun interp rules2 ->
      fprintf file "\n@RULES %s\n" interp;
      StringMap.iter rules2 (fun rule (q,l) ->
        fprintf file "\t%s\t# %d %s\n" rule q (String.concat " " l))))

let generate_rules_lu rules id path rules_filename =
  let dict = load_lu [] id path in
  let dict = lowercase_lu dict in
  let dict = generate_stem_lu dict in
  let dict = validate_lu rules dict in
  let dict = remove_validated_lu dict in
  (* let dict = remove_sup_neg_forms dict in *)
  let rules = Xlist.fold dict StringMap.empty (fun rules entry ->
    Xlist.fold (RuleGenerator.generate_rules_lu_entry entry) rules (fun rules (key,rule,lemma) ->
      let rules2 = try StringMap.find rules key with Not_found -> StringMap.empty in
      let rules2 = StringMap.add_inc rules2 rule (1,[lemma]) (fun (q,l) -> q+1, if q < 20 then lemma :: l else l) in
      StringMap.add rules key rules2)) in
  File.file_out rules_filename (fun file ->
    StringMap.iter rules (fun interp rules2 ->
      fprintf file "\n@RULES %s\n" interp;
      StringMap.iter rules2 (fun rule (q,l) ->
        fprintf file "\t%s\t# %d %s\n" rule q (String.concat " " l))))

let rec get_first n l =
  if n = 0 || l = [] then [] else
  List.hd l :: (get_first (n-1) (List.tl l))

let generate_interp_rules rules interp_rules selected_tags path filename rules_filename =
  let selected_tags = StringSet.of_list selected_tags in
  let dict = load_tab (path ^ filename) in
  let dict = merge_entries dict in
  let dict = process_interps dict in
  (* let dict = mark_ndm dict in (* FIXME: remove_ndm? *) *)
  let dict = remove_exceptional_lemmata dict in
  (* let dict = find_kolwiek_suffixes dict in *)
  (* let dict = generate_stem dict in *)
  let dict = phon_validate_interp interp_rules dict in
  let dict = remove_validated_forms dict in
  let interp_rules = Xlist.fold dict StringMap.empty (fun interp_rules entry ->
    (* let simple_lemma = Stem.simplify_lemma entry.lemma in *)
    let entry = create_candidates false rules entry in
    Xlist.fold entry.forms interp_rules (fun interp_rules form ->
      let candidates = RuleGenerator.phon_generate_interp_rules (*rules*) selected_tags (*simple_lemma*) form in
      Xlist.fold candidates interp_rules (fun interp_rules (v,cand) ->
        (* StringMap.add_inc interp_rules cand (1,[entry.lemma]) (fun (q,l) -> q+1, if q < 20 then entry.lemma :: l else l)))) in *)
        StringMap.add_inc interp_rules cand (v,StringSet.singleton entry.lemma) (fun (v,set) -> v,StringSet.add set entry.lemma)))) in
  let interp_rules = List.rev (List.sort compare (StringMap.fold interp_rules [] (fun l k (v,set) ->
    (v,k,set) :: l))) in
  File.file_out rules_filename (fun file ->
    Xlist.iter interp_rules (fun (v,k,set)(*q,l*) ->
      (* fprintf file "\t%s\t# %d %s\n" k q (String.concat " " l))) *)
      (*if StringSet.size set > 1000 then*) fprintf file "\t%s\t# %d %s\n" k (StringSet.size set) (String.concat " " (get_first 20 (List.rev (StringSet.to_list set))))))

(* let generate_ndm_rules dict =
  let freq_rules = Xlist.fold dict MorphologyRules.RuleQMap.empty (fun freq_rules entry ->
    Xlist.fold entry.forms freq_rules (fun freq_rules form ->
      let rule = {id=""; freq=0; star=Ndm; pref=""; find=""; set=""; tags=[]; interp=form.interp} in
      MorphologyRules.RuleQMap.add freq_rules rule)) in
   fst (MorphologyRules.RuleQMap.fold freq_rules (MorphologyRules.RuleQMap.empty,1) (fun (freq_rules,i) rule freq ->
     MorphologyRules.RuleQMap.add_val freq_rules {rule with id = "N" ^ string_of_int i} freq, i+1)) *)

let rec get_longest_common_prefix_rec rev = function
    a :: la, b :: lb -> if a = b then get_longest_common_prefix_rec (a :: rev) (la,lb) else rev
  | _ -> rev

let get_longest_common_prefix a b =
  let rev = get_longest_common_prefix_rec [] (Xunicode.utf8_chars_of_utf8_string a, Xunicode.utf8_chars_of_utf8_string b) in
  String.concat "" (List.rev rev)

let rec set_star star = function
    [] -> star
  | {plang=""} :: l -> set_star (MorphologyRules.merge_stars (star,Productive)) l
  | {plang="core"} :: l -> set_star (MorphologyRules.merge_stars (star,Productive)) l
  | {plang="aux"} :: l -> set_star (MorphologyRules.merge_stars (star,Aux)) l
  | {plang="aux2"} :: l -> set_star (MorphologyRules.merge_stars (star,Aux2)) l
  | {plang="acro"} :: l -> set_star (MorphologyRules.merge_stars (star,Acro)) l
  | _ -> failwith "set_star"

let rec set_orth_tag rule = function
    [] -> rule
  | {plang="aux2"; pset=v} :: l -> {rule with tags=("orth",v) :: rule.tags}
  | _ :: l -> set_orth_tag rule l

let select_candidates selector candidates =
  let selected =
    Xlist.fold candidates [] (fun candidates2 (stem,rule) ->
      if selector rule then (stem,rule) :: candidates2 else candidates2) in
  if selected = [] then candidates else selected

let bucket_select_candidates tag match_fun candidates =
  let map = Xlist.fold candidates StringMap.empty (fun map (stem,rule) ->
    let k = MorphologyRules.get_tag rule.tags tag in
    StringMap.add_inc map k [stem,rule] (fun l -> (stem,rule) :: l)) in
  let groups = List.sort compare (StringMap.fold map [] (fun groups k l -> (k,l) :: groups)) in
  match groups with
    [] -> []
  | [_,l] -> l
  | _ -> match_fun groups

let group_selector = function
  | ["a",l;"e",_] -> l
  | ["J",l;"Jε",_] -> l
  | ["n",l;"ε",_] -> l
  | ["Je",l;"a",_] -> l
  | l ->
    print_endline ("group_selector: " ^ String.concat " " (Xlist.map l (fun (k,_) -> k)));
    List.flatten (Xlist.map l snd)

let agl2_selector = function
  | ["ce",l;"e",_] -> l
  | l ->
    print_endline ("agl2_selector: " ^ String.concat " " (Xlist.map l (fun (k,_) -> k)));
    List.flatten (Xlist.map l snd)

let agl_selector = function
    ["",l;"b",_;"d",_;"s",_;"t",_] -> l
  | ["g",_;"k",l] -> l
  | ["",_;"b",l] -> l
  | ["",_;"x",l] -> l
  | ["",_;"d",l] -> l
  | ["",_;"g",l] -> l
  | ["",_;"k",l] -> l
  | ["",_;"p",l] -> l
  | ["",_;"s",l] -> l
  | ["",_;"t",l] -> l
  | ["",_;"z",l] -> l
  | ["",_;"ž",l] -> l
  | l ->
    print_endline ("agl_selector: " ^ String.concat " " (Xlist.map l (fun (k,_) -> k)));
    List.flatten (Xlist.map l snd)

let lemma_selector = function
  | ["e",l;"y",_] -> l
  | ["e",l;"ε",_] -> l
  | l ->
    print_endline ("lemma_selector: " ^ String.concat " " (Xlist.map l (fun (k,_) -> k)));
    List.flatten (Xlist.map l snd)

let flex_selector = function
  | ["em",l;"ym",_] -> l
  | l ->
    print_endline ("flex_selector: " ^ String.concat " " (Xlist.map l (fun (k,_) -> k)));
    List.flatten (Xlist.map l snd)

let con_selector = function
  | ["b′",l;"j",_] -> l
  | ["f′",l;"j",_] -> l
  | ["g′",l;"j",_] -> l
  | ["j",_;"k′",l] -> l
  | ["j",_;"m′",l] -> l
  | ["j",_;"n′",l] -> l
  | ["j",_;"p′",l] -> l
  | ["j",_;"v′",l] -> l
  | ["a",l;"ʲ",_] -> l
  | ["j",l;"ʲ",_] -> l
  | ["s",l;"ʲ",_] -> l
  | ["c",l;"ʲ",_] -> l
  | ["d",l;"ʲ",_] -> l
  | ["r",l;"ʲ",_] -> l
  | ["h",_;"x",l] -> l
  | ["m",l;"m′",_] -> l
  | ["f",l;"f′",_] -> l
  | ["b",l;"b′",_] -> l
  | ["p",l;"p′",_] -> l
  | ["v",l;"v′",_] -> l
  | ["k",l;"k′",_] -> l
  | ["g",l;"g′",_] -> l
  | ["j",_;"k",l;"k′",_] -> l
  | ["g",l;"g′",_;"j",_;"ʲ",_] -> l
  | ["g",l;"g′",_;"ʲ",_] -> l
  | ["c",_;"z",l] -> l
  | ["v",l;"v′",_;"ł",_] -> l
  | ["x",l;"š",_] -> l
  | ["v",l;"ł",_] -> l
  | ["t",_;"ž",l] -> l
  | ["s",l;"z",_] -> l
  | ["c",l;"k",_] -> l
  | ["ž",_;"ǯ",l] -> l
  (* | ["",_;"",_] -> l
  | ["",_;"",_] -> l
  | ["",_;"",_] -> l
  | ["",_;"",_] -> l
  | ["",_;"",_] -> l
  | ["",_;"",_] -> l *)
  | l ->
    print_endline ("con_selector: " ^ String.concat " " (Xlist.map l (fun (k,_) -> k)));
    List.flatten (Xlist.map l snd)

let con2_selector = function
  | ["",l;"r",_] -> l
  | ["j",l;"ʲ",_] -> l
  | l ->
    print_endline ("con2_selector: " ^ String.concat " " (Xlist.map l (fun (k,_) -> k)));
    List.flatten (Xlist.map l snd)

let lcon2_selector = function
  | ["e",l;"′e",_] -> l
  | l ->
    print_endline ("lcon2_selector: " ^ String.concat " " (Xlist.map l (fun (k,_) -> k)));
    List.flatten (Xlist.map l snd)


let generate_rule_frequencies2 rules dict rules_filename =
  let dict = merge_entries dict in
  let dict = process_interps dict in
  let dict = remove_cat "cond" dict in
  let dict = remove_exceptional_lemmata dict in
  let freq_rules = Xlist.fold dict MorphologyRules.RuleQMap.empty (fun freq_rules entry ->
    let entry = create_candidates true rules entry in
    let simple_lemma = Stem.simplify_lemma entry.lemma in
    (* print_endline simple_lemma; *)
    Xlist.fold entry.forms freq_rules (fun freq_rules form ->
      (* print_endline form.orth; *)
      let candidates = Xlist.fold form.candidates [] (fun candidates (stem,rule,s,tl) ->
            let rule,short_stem = match rule.pref with
                "naj" -> rule, Xstring.cut_prefix "naj" (Fonetics.get_short_stem "" ("naj" ^ stem) s.mapping)
              | "n′e" -> {rule with pref="nie"}, Xstring.cut_prefix "nie" (Fonetics.get_short_stem "" ("n′e" ^ stem) s.mapping)
              | "" -> rule, Fonetics.get_short_stem "" stem s.mapping
              | _ -> failwith "generate_rule_frequencies" in
            let short_stem = get_longest_common_prefix short_stem simple_lemma in
            let pref_stem = rule.pref ^ short_stem in
            (* printf "%s %s %s\n%!" simple_lemma stem pref_stem; *)
            let rule = {rule with
              find = Xstring.cut_prefix pref_stem form.orth;
              set = Xstring.cut_prefix short_stem simple_lemma} in
            Xlist.fold tl candidates (fun candidates t ->
            try
              (* print_endline (MorphologyRules.string_of_star rule.star);
              let rule = {rule with star=Productive} in *)
              let star = set_star rule.star s.mapping in
              let star = set_star star t.mapping in
              let rule = set_orth_tag rule t.mapping in
              (stem,{rule with star=star}) :: candidates
            with MorphologyRules.MergeStars(a,b) ->
              (* printf "contradicting stars %s %s %s\n" (MorphologyRules.string_of_star a) (MorphologyRules.string_of_star b) form.orth; *)
              candidates)) in
      (* printf "%s %s %d\n%!" simple_lemma form.orth (Xlist.size candidates); *)
      let candidates = select_candidates (fun r -> r.star <> Aux2) candidates in
      let candidates = select_candidates (fun r -> r.star <> Star) candidates in
      (* let candidates2 =
          Xlist.fold candidates [] (fun candidates2 (stem,rule) ->
            if rule.star = Aux2 then candidates2 else (stem,rule) :: candidates2) in
      let candidates = if candidates2 = [] then candidates else candidates2 in
      let candidates2 =
          Xlist.fold candidates [] (fun candidates2 (stem,rule) ->
            if rule.star = Star then candidates2 else (stem,rule) :: candidates2) in
      let candidates = if candidates2 = [] then candidates else candidates2 in *)
      let _,candidates = Xlist.fold candidates (max_int,[]) (fun (min_n,min_l) (stem,rule) ->
        let n = Xstring.size rule.find in
        if n < min_n then n,[stem,rule] else
        if n > min_n then min_n,min_l else
        min_n, (stem,rule) :: min_l) in
      (* let candidades = select_candidates (fun r -> MorphologyRules.get_tag r.tags "agl" <> "") candidates in *)
      let candidates = select_candidates (fun r -> MorphologyRules.get_tag r.tags "agl2" <> "") candidates in
      let candidates = bucket_select_candidates "group" group_selector candidates in
      let candidates = bucket_select_candidates "agl2" agl2_selector candidates in
      let candidates = bucket_select_candidates "agl" agl_selector candidates in
      let candidates = bucket_select_candidates "lemma" lemma_selector candidates in
      let candidates = bucket_select_candidates "con" con_selector candidates in
      let candidates = bucket_select_candidates "con2" con2_selector candidates in
      let candidates = bucket_select_candidates "lcon2" lcon2_selector candidates in
      let candidates = bucket_select_candidates "flex" flex_selector candidates in
      let map = Xlist.fold candidates StringMap.empty (fun map (_,r) -> StringMap.add map (string_of_rule r) r) in
      match StringMap.fold map [] (fun l s r -> (s,MorphologyRules.get_tag r.tags "agl",MorphologyRules.get_tag r.tags "agl2",MorphologyRules.get_tag r.tags "con",MorphologyRules.get_tag r.tags "flex",MorphologyRules.get_tag r.tags "group",MorphologyRules.get_tag r.tags "lemma",r) :: l) with
      (* match MorphologyRules.RuleSet.to_list (Xlist.fold candidates MorphologyRules.RuleSet.empty (fun set (_,r) -> MorphologyRules.RuleSet.add set r)) with *)
        [] -> freq_rules
      | [_,_,_,_,_,_,_,r] -> MorphologyRules.RuleQMap.add freq_rules r
      | l ->
         printf "%s %s\n  %s\n" form.orth entry.lemma (String.concat "\n  " (Xlist.map l (fun (s,_,_,_,_,_,_,_) -> s)));
         let _,_,_,_,_,_,_,r = List.hd l in
         MorphologyRules.RuleQMap.add freq_rules r)) in
  let freq_rules2 = MorphologyRules.RuleQMap.fold freq_rules MorphologyRules.RuleQMap.empty (fun freq_rules2 r freq ->
    let b = try let _ = MorphologyRules.RuleQMap.find freq_rules {r with star=Productive} in true with Not_found -> false in
    if b then MorphologyRules.RuleQMap.add_val freq_rules2 {r with star=Productive} freq
    else MorphologyRules.RuleQMap.add_val freq_rules2 r freq) in
  File.file_out rules_filename (fun file ->
    ignore (MorphologyRules.RuleQMap.fold freq_rules2 0 (fun id rule freq ->
      fprintf file "%s\n" (MorphologyRules.string_of_freq_rule {rule with id=string_of_int id; freq=freq}); id+1)))

let generate_rule_frequencies rules path filename rules_filename =
  let dict = load_tab (path ^ filename) in
  generate_rule_frequencies2 rules dict rules_filename

let generate_rule_frequencies_list rules sources rules_filename =
  let dict = Xlist.fold sources [] (fun dict (path,filename) ->
    load_tab_incr (path ^ filename) dict) in
  generate_rule_frequencies2 rules dict rules_filename

let generate_stem_dict rules_filename path filename out_filename =
  let rules = MorphologyRules.load_freq_rules rules_filename in
  let rules = MorphologyRules.CharTrees.create rules in
  let dict = load_tab (path ^ filename) in
  let dict = merge_entries dict in
  let dict = process_interps dict in
  let dict = remove_cat "cond" dict in
  (* let dict = mark_ndm dict in *)
  let stems = Xlist.fold dict StringMap.empty (fun stems entry ->
    let entry = fonetic_translation_entry entry in
    let simple_lemma,lemma_suf = Stem.simplify_lemma_full entry.lemma in
    Xlist.fold entry.forms stems (fun stems form ->
      let candidates = MorphologyRules.CharTrees.find rules form.orth in
      let candidates = Xlist.fold candidates [] (fun candidates (stem,rule) ->
        (* if rule.star = Ndm && not entry.ndm then candidates else
        if rule.star <> Ndm && entry.ndm then candidates else *)
        if stem ^ rule.set = simple_lemma && form.interp = rule.interp then
          (stem,rule) :: candidates else candidates) in
      if candidates = [] then stems else
      let stem,rule = List.hd candidates in
      StringMap.add_inc stems (stem ^ "\t" ^ lemma_suf ^ "\t" ^ entry.aspect) [rule.id] (fun l -> rule.id :: l))) in
  File.file_out out_filename (fun file ->
    StringMap.iter stems (fun stem ids ->
      fprintf file "%s\t%s\n" stem (String.concat " " ids)))

(**********************************************************************************)

exception BadForm of string

let get_con1 = function
    "h" :: "c" :: l -> "ch",l
  | "z" :: "c" :: l -> "cz",l
  | "z" :: "d" :: l -> "dz",l
  | "ź" :: "d" :: l -> "dź",l
  | "ż" :: "d" :: l -> "dż",l
  | "z" :: "r" :: l -> "rz",l
  | "z" :: "s" :: l -> "sz",l
  | "a" :: l -> "a",l
  | "ą" :: l -> "ą",l
  | "b" :: l -> "b",l
  | "c" :: l -> "c",l
  | "ć" :: l -> "ć",l
  | "d" :: l -> "d",l
  | "e" :: l -> "e",l
  | "ę" :: l -> "ę",l
  | "f" :: l -> "f",l
  | "g" :: l -> "g",l
  | "h" :: l -> "h",l
  | "j" :: l -> "j",l
  | "k" :: l -> "k",l
  | "l" :: l -> "l",l
  | "ł" :: l -> "ł",l
  | "m" :: l -> "m",l
  | "n" :: l -> "n",l
  | "ń" :: l -> "ń",l
  | "o" :: l -> "o",l
  | "ó" :: l -> "ó",l
  | "p" :: l -> "p",l
  | "q" :: l -> "q",l
  | "r" :: l -> "r",l
  | "s" :: l -> "s",l
  | "ś" :: l -> "ś",l
  | "t" :: l -> "t",l
  | "u" :: l -> "u",l
  | "v" :: l -> "v",l
  | "w" :: l -> "w",l
  | "y" :: l -> "y",l
  | "x" :: l -> "x",l
  | "z" :: l -> "z",l
  | "ź" :: l -> "ź",l
  | "ż" :: l -> "ż",l
  | "i" :: "h" :: "c" :: l -> "chi",l
  | "i" :: "z" :: "c" :: l -> "czi",l
  | "i" :: "z" :: "d" :: l -> "dzi",l
  | "i" :: "ź" :: "d" :: l -> "dźi",l
  | "i" :: "ż" :: "d" :: l -> "dżi",l
  | "i" :: "z" :: "r" :: l -> "rzi",l
  | "i" :: "z" :: "s" :: l -> "szi",l
  | "i" :: "a" :: l -> "i","a" :: l
  | "i" :: "b" :: l -> "bi",l
  | "i" :: "c" :: l -> "ci",l
  | "i" :: "d" :: l -> "di",l
  | "i" :: "f" :: l -> "fi",l
  | "i" :: "g" :: l -> "gi",l
  | "i" :: "h" :: l -> "hi",l
  | "i" :: "j" :: l -> "ji",l
  | "i" :: "k" :: l -> "ki",l
  | "i" :: "l" :: l -> "li",l
  | "i" :: "m" :: l -> "mi",l
  | "i" :: "n" :: l -> "ni",l
  | "i" :: "p" :: l -> "pi",l
  | "i" :: "r" :: l -> "ri",l
  | "i" :: "s" :: l -> "si",l
  | "i" :: "t" :: l -> "ti",l
  | "i" :: "v" :: l -> "vi",l
  | "i" :: "w" :: l -> "wi",l
  | "i" :: "x" :: l -> "xi",l
  | "i" :: "z" :: l -> "zi",l
  | "i" :: "ż" :: l -> "żi",l
  | "-" :: l -> raise (BadForm "acronym")
  | "’" :: l -> raise (BadForm "acronym")
  | l -> raise (BadForm ("get_con1: '" ^ String.concat "" (List.rev l) ^ "'"))
  (* | l -> failwith ("get_con1: '" ^ String.concat "" (List.rev l) ^ "'") *)

let get_con2 = function
    "h" :: "c" :: l -> "ch",l
  | "z" :: "c" :: l -> "cz",l
  | "z" :: "d" :: l -> "dz",l
  | "ź" :: "d" :: l -> "dź",l
  | "ż" :: "d" :: l -> "dż",l
  | "z" :: "r" :: l -> "rz",l
  | "z" :: "s" :: l -> "sz",l
  | "a" :: l -> "a",l
  | "ą" :: l -> "ą",l
  | "b" :: l -> "b",l
  | "c" :: l -> "c",l
  | "ć" :: l -> "ć",l
  | "d" :: l -> "d",l
  | "e" :: l -> "e",l
  | "ę" :: l -> "ę",l
  | "f" :: l -> "f",l
  | "g" :: l -> "g",l
  | "h" :: l -> "h",l
  | "i" :: l -> "i",l
  | "j" :: l -> "j",l
  | "k" :: l -> "k",l
  | "l" :: l -> "l",l
  | "ł" :: l -> "ł",l
  | "m" :: l -> "m",l
  | "n" :: l -> "n",l
  | "ń" :: l -> "ń",l
  | "o" :: l -> "o",l
  | "ó" :: l -> "ó",l
  | "p" :: l -> "p",l
  | "q" :: l -> "q",l
  | "r" :: l -> "r",l
  | "s" :: l -> "s",l
  | "ś" :: l -> "ś",l
  | "t" :: l -> "t",l
  | "u" :: l -> "u",l
  | "v" :: l -> "v",l
  | "w" :: l -> "w",l
  | "y" :: l -> "y",l
  | "x" :: l -> "x",l
  | "z" :: l -> "z",l
  | "ź" :: l -> "ź",l
  | "ż" :: l -> "ż",l
  | "-" :: l -> raise (BadForm "acronym")
  | "A" :: l -> "A",l
  | "E" :: l -> "E",l
  | "I" :: l -> "I",l
  | "L" :: l -> "L",l
  | "N" :: l -> "N",l
  | "O" :: l -> "O",l
  | "P" :: l -> "P",l
  | "U" :: l -> "U",l
  | "W" :: l -> "W",l
  | "á" :: l -> "á",l
  | "é" :: l -> "é",l
  | "ü" :: l -> "ü",l
  | l -> raise (BadForm ("get_con2: '" ^ String.concat "" (List.rev l) ^ "'"))

let analyze_om_form s =
  if Xstring.check_sufix "-etom" s then raise (BadForm "acronym") else
  if Xstring.check_sufix "-otom" s then raise (BadForm "acronym") else
  if Xstring.check_sufix "om" s then
    let l = List.rev (Xunicode.utf8_chars_of_utf8_string (Xstring.cut_sufix "om" s)) in
    let b,l = get_con1 l in
    let a,l = get_con2 l in
    String.concat "" (List.rev l), a, b
  else raise (BadForm "suffix other than om")

let analyze_nom_form s a b =
  let l = List.rev (Xunicode.utf8_chars_of_utf8_string s) in
  if s = a ^ b && List.hd l <> "i" then true,s else
  match l with
    "a" :: _ -> false,""
  | "ą" :: _ -> false,""
  | "e" :: _ -> false,""
  | "ę" :: _ -> false,""
  | "i" :: _ -> false,""
  | "o" :: _ -> false,""
  | "ó" :: _ -> false,""
  | "u" :: _ -> false,""
  | "y" :: _ -> false,""
  | "m" :: "u" :: _ -> false,""
  | _ -> true,s

let analyze_nom_gen nom gen =
  let s = get_longest_common_prefix nom gen in
  let l = List.rev (Xunicode.utf8_chars_of_utf8_string gen) in
  match l with
    "i" :: _ -> raise (BadForm "suffix other than wyglos")
  | "y" :: _ -> raise (BadForm "suffix other than wyglos")
  | "u" :: _ -> raise (BadForm "suffix other than wyglos")
  | "a" :: _ -> raise (BadForm "suffix other than wyglos")
  | "h" :: "c" :: _ -> raise (BadForm "suffix other than wyglos")
  | "w" :: "ó" :: _ ->
      (match Xstring.cut_prefix s nom, Xstring.cut_prefix s gen with
        | "owa", "ów" -> s ^ "ow", gen
        | "owie", "ów" -> s ^ "owi", gen
        | "owo", "ów" -> s ^ "ow", gen
        | _ -> raise (BadForm "suffix other than wyglos"))
  | _ ->
      (match List.rev (Xunicode.utf8_chars_of_utf8_string nom) with
        "a" :: l -> String.concat "" (List.rev l), gen
      | "A" :: l -> String.concat "" (List.rev l), gen
      | "e" :: l -> String.concat "" (List.rev l), gen (* nie ma 'kie' i 'gie' *)
      | "ę" :: l -> String.concat "" (List.rev l), gen
      | "i" :: l -> nom, gen
      | "o" :: l -> String.concat "" (List.rev l), gen
      | "y" :: l -> String.concat "" (List.rev l), gen
      | "n" :: "i" :: l -> String.concat "" (List.rev l), gen
      | _ -> raise (BadForm "analyze_nom_gen"))

let rec merge_diftongs = function
    "c" :: "h" :: l -> "ch" :: merge_diftongs l
  | "c" :: "z" :: l -> "cz" :: merge_diftongs l
  | "d" :: "z" :: l -> "dz" :: merge_diftongs l
  | "d" :: "ź" :: l -> "dź" :: merge_diftongs l
  | "d" :: "ż" :: l -> "dż" :: merge_diftongs l
  | "r" :: "z" :: l -> "rz" :: merge_diftongs l
  | "s" :: "z" :: l -> "sz" :: merge_diftongs l
  | s :: l -> s :: merge_diftongs l
  | [] -> []

let compare_con = function
    "","" -> true
  | "bi","b" -> true
  | "b","bi" -> true
  | "ci","ć" -> true
  | "ci","c" -> true
  | "ć","ci" -> true
  | "di","d" -> true
  | "dzi","dź" -> true
  | "dź","dzi" -> true
  | "d","dzi" -> true
  | "ę","ą" -> true
  | "f","fi" -> true
  | "gi","g" -> true
  | "g","gi" -> true
  | "ki","k" -> true
  | "k","ki" -> true
  | "mi","m" -> true
  | "m","mi" -> true
  | "ni","ń" -> true
  | "ni","n" -> true
  | "ń","ni" -> true
  | "n","ni" -> true
  | "o","ó" -> true
  | "pi","p" -> true
  | "p","pi" -> true
  | "r","rz" -> true
  | "ri","r" -> true
  | "si","ś" -> true
  | "s","si" -> true
  | "ś","si" -> true
  | "t","ci" -> true
  | "ti","t" -> true
  | "wi","w" -> true
  | "w","wi" -> true
  | "zi","ź" -> true
  | "z","zi" -> true
  | "ź","zi" -> true
  | a,b -> if a = b then true else raise (BadForm ("compare_con:" ^ a ^ " " ^ b))

let manage_wyglos a b s =
  match merge_diftongs (Xunicode.utf8_chars_of_utf8_string s) with
    [a2;b2] -> if compare_con (a,a2) && compare_con (b,b2) then s,"" else raise (BadForm ("manage_wyglos:" ^ a ^ b ^ " " ^ s))
  | [a2;"e";b2] -> if compare_con (a,a2) && compare_con (b,b2) then s,"e" else raise (BadForm ("manage_wyglos:" ^ a ^ b ^ " " ^ s))
  | [a2;"i";"e";b2] -> if compare_con (a,a2^"i") && compare_con (b,b2) then s,"e" else raise (BadForm ("manage_wyglos:" ^ a ^ b ^ " " ^ s))
  | [a2;"y";b2] -> if compare_con (a,a2) && compare_con (b,b2) then raise (BadForm "y form") else raise (BadForm ("manage_wyglos:" ^ a ^ b ^ " " ^ s))
  | [a2;b2;"i";"j"] -> if compare_con (a,a2) && compare_con (b,b2^"i") then raise (BadForm "ij form") else raise (BadForm ("manage_wyglos:" ^ a ^ b ^ " " ^ s))
  | [a2;b2;"y";"j"] -> if compare_con (a,a2) && compare_con (b,b2) then raise (BadForm "yj form") else raise (BadForm ("manage_wyglos:" ^ a ^ b ^ " " ^ s))
  | [a2;b2;"i";"ą";"t"] -> if compare_con (a,a2) && compare_con (b,b2^"i") then raise (BadForm "wyjątek3") else raise (BadForm ("manage_wyglos:" ^ a ^ b ^ " " ^ s))
  | [a2;b2;"i";"n"] -> if compare_con (a,a2) && compare_con (b,b2^"i") then raise (BadForm "wyjątek3") else raise (BadForm ("manage_wyglos:" ^ a ^ b ^ " " ^ s))
  | [a2;b2;"ą";"t"] -> if compare_con (a,a2) && compare_con (b,b2) then raise (BadForm "wyjątek3") else raise (BadForm ("manage_wyglos:" ^ a ^ b ^ " " ^ s))
  | [a2;b2;"a";"h"] -> if compare_con (a,a2) && compare_con (b,b2) then raise (BadForm "wyjątek3") else raise (BadForm ("manage_wyglos:" ^ a ^ b ^ " " ^ s))
  | [a2;b2;"i";"o";"n"] -> if compare_con (a,a2) && compare_con (b,b2^"i") then raise (BadForm "wyjątek3") else raise (BadForm ("manage_wyglos:" ^ a ^ b ^ " " ^ s))
  | [a2;b2;"u";"s"] -> if compare_con (a,a2) && compare_con (b,b2) then raise (BadForm "wyjątek3") else raise (BadForm ("manage_wyglos:" ^ a ^ b ^ " " ^ s))
  | ["x"] -> if a = "k" && b = "s" then s,"" else raise (BadForm ("manage_wyglos:" ^ a ^ b ^ " " ^ s))
  | _ -> raise (BadForm ("manage_wyglos:" ^ a ^ b ^ " " ^ s))

let generate_wyglos path filename out_filename =
  let dict = load_tab (path ^ filename) in
  let dict_dat = Xlist.fold dict [] (fun dict_dat entry ->
    let form = get_form entry in
    if Xstring.check_prefix "subst:pl:dat" form.interp then entry :: dict_dat else dict_dat) in
  let dict_gen = Xlist.fold dict [] (fun dict_gen entry ->
    let form = get_form entry in
    if Xstring.check_prefix "subst:pl:gen" form.interp then entry :: dict_gen else dict_gen) in
  (* let dict_dat = load_tab "results/sgjp_dat.tab" in
  let dict_gen = load_tab "results/sgjp_gen.tab" in *)
  let dict = Xlist.fold dict_dat [] (fun dict e ->
    let lemma = Stem.simplify_lemma e.lemma in
    let form = (get_form e).orth in
    try
      let stem,a,b = analyze_om_form form in
      if Xstring.check_prefix stem lemma then
        let f,s = analyze_nom_form (Xstring.cut_prefix stem lemma) a b in
        if f then (manage_wyglos a b s, a, b) :: dict else dict
      else (printf "wyjątek: %s %s\n" lemma form; dict)
    with
      BadForm "suffix other than om" -> dict
    | BadForm "acronym" -> dict
    | BadForm s -> printf "%s: %s %s\n" s lemma form; dict) in
  let dict = Xlist.fold dict_gen dict (fun dict e ->
    let lemma = Stem.simplify_lemma e.lemma in
    let form = (get_form e).orth in
    try
      let nom,gen = analyze_nom_gen lemma form in
      let l = List.rev (Xunicode.utf8_chars_of_utf8_string nom) in
      let b,l = get_con1 l in
      let a,l = get_con2 l in
      let stem = String.concat "" (List.rev l) in
      if Xstring.check_prefix stem gen then
        (manage_wyglos a b (Xstring.cut_prefix stem gen), a, b) :: dict
      else (printf "wyjątek2: %s %s\n" lemma form; dict)
    with
      BadForm "suffix other than wyglos" -> dict
    | BadForm "acronym" -> dict
    | BadForm s -> printf "%s: %s %s\n" s lemma form; dict) in
  let qmap = Xlist.fold dict StringQMap.empty (fun qmap ((s,c), a, b) ->
    StringQMap.add qmap (c ^ "\t" ^ s ^ "\t" ^ a ^ b ^ "\t" ^ a ^ "\t" ^ b)) in
  File.file_out out_filename (fun file ->
    StringQMap.iter qmap (fun k v ->
      fprintf file "%d\t%s\n" v k));
  ()


let latex_of_wyglos2 a =
  MorphologyRules.latex_escape_string a

let make_wyglos_line2 alts =
  let l,alts = Xlist.fold alts ([],[]) (fun (l,alts) -> function
      [] -> "" :: l,[] :: alts
    | alt :: a -> latex_of_wyglos2 alt :: l, a :: alts) in
  String.concat " & " (List.rev l), List.rev alts

let rec print_wyglos2 name alts =
  if Xlist.fold alts true (fun b -> function [] -> b | _ -> false) then print_endline "\\hline" else (
  let s,alts = make_wyglos_line2 alts in
  print_endline (MorphologyRules.latex_escape_string name ^ " & " ^ s ^ "\\\\");
  print_wyglos2 "" alts)

let print_wyglos names phons wyglos =
  print_endline ("\\begin{longtable}{p{1mm}|" ^ String.concat "" (Xlist.map names (fun _ -> "p{1mm}")) ^ "}");
  print_endline (" & " ^ String.concat " & " names(*Xlist.map names (fun name ->
    try Xlist.assoc alt_names name with Not_found -> failwith ("print_wyglos: " ^ name))*) ^ "\\\\");
  print_endline "\\hline";
  (* print_endline "\\endhead\n\\hline\\\\"; *)
  let wyglos = Xlist.map names (fun name ->
    try StringMap.find wyglos name with Not_found -> failwith ("print_wyglos: " ^ name)) in
  (* let wyglos = Xlist.map wyglos (fun alternation ->
    Xlist.fold alternation StringMap.empty (fun alternation a ->
      StringMap.add_inc alternation a.aphone [a] (fun l -> a :: l))) in *)
  Xlist.iter phons (fun phon ->
    print_wyglos2 phon (Xlist.map wyglos (fun a -> try List.rev (StringMap.find a phon) with Not_found -> [])));
  print_endline "\\end{longtable}\n"


let first1 = ["a";"ą";"e";"ę";"i";"o";"ó";"u";"y";]
let first2 = ["c";"ć";"cz";"dz";"dź";"dż";"j";"l";"ń";"rz";"sz";"ś";"ź";"ż"]
let first3 = ["b";"ch";"d";"f";"g";"h";"k";"ł";"m";"n";"p";"r";"s";"t";"v";"w";"x";"z";]
let second1 = ["b";"bi";"c";"ch";"ci";"cz";"dz";"dzi";"dż";"f";"g";"gi";"h";"j";"k";"ki";"l";"ł";
  "m";"mi";"n";"ni";"p";"pi";"q";"r";"rz";"s";"si";"sz";"t";"u";"v";"w";"wi";"y";"z";"zi";"ż"]
let second2 = ["b";"c";"ch";"ci";"cz";"dzi";"dż";"f";"g";"h";"k";"ki";"l";"ł";
  "m";"n";"ni";"p";"q";"r";"s";"sz";"t";"v";"w";"wi";"y";"z";"ż"]
let second3 = ["b";"bi";"c";"ch";"ci";"cz";"dz";"dzi";"dż";"f";"g";"gi";"h";"k";"ki";"l";"ł";
  "m";"n";"ni";"p";"pi";"r";"rz";"s";"si";"sz";"t";"w";"wi";"y";"z";"ż"]

let latex_of_wyglos filename =
  let wyglos = File.fold_tab filename StringMap.empty (fun map -> function
    [freq; con; s; t; a; b] ->
        let map2 = try StringMap.find map a with Not_found -> StringMap.empty in
        let map2 = StringMap.add_inc map2 b [s] (fun l -> s :: l) in
        StringMap.add map a map2
    | line -> failwith ("latex_of_wyglos: " ^ (String.concat "\t" line))) in
  print_wyglos first1 second1 wyglos;
  print_wyglos first2 second2 wyglos;
  print_wyglos first3 second3 wyglos;
  ()

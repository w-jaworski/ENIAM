(*
 *  ENIAMmorphology, a morphological analyser and a guesser for Polish
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

open MorphologyTypes
open Xstd

let load_stems filename =
  File.fold_tab filename StringMap.empty (fun stems -> function
      [stem; lemma_suf; aspect; ids] ->
        let ids = StringSet.of_list (Xstring.split " " ids) in
        StringMap.add_inc stems stem [lemma_suf,aspect,ids] (fun l -> (lemma_suf,aspect,ids) :: l)
    | l -> failwith ("load_stems: " ^ String.concat " " l))

let load_tab filename =
  File.load_tab filename (function
      orth :: lemma :: interp :: _ ->
        {empty_entry with lemma=lemma; forms=[{empty_form with orth=orth; interp=interp}]}
    | line -> failwith ("load_tab: " ^ (String.concat "\t" line)))

let simplify_lemma s =
  match Xstring.split ":" s with
    [s] -> s,""
  | [s;t] -> s,t
  | _ -> failwith ("simplify_lemma: " ^ s)

type status = LemmaVal | LemmaAlt | LemmNotVal | TokNotFound

let string_of_status = function
    LemmaVal -> "LemmaVal"
  | LemmaAlt -> "LemmaAlt"
  | LemmNotVal -> "LemmNotVal"
  | TokNotFound -> "TokNotFound"

type t = {lemma: string; lemma_suf: string; interp: string; freq: int; status: status; star: star; tags: (string * string) list; ntype_freq: StringQMap.t; find: string}

let string_of_interpretation t =
  let lemma = if t.lemma_suf = "" then t.lemma else t.lemma ^ ":" ^ t.lemma_suf in
  Printf.sprintf "%s\t%s\t%d\t%s\t%s\t%s\t%s" lemma t.interp t.freq (string_of_status t.status)
    (MorphologyRules.string_of_star t.star) t.find (String.concat " " (Xlist.map t.tags (fun (k,v) -> k ^ "=" ^ v)))

let string_of_interpretations l =
  String.concat "\n" (Xlist.map l string_of_interpretation)

let xml_of_interpretation t =
  Xml.Element("t",["lemma",t.lemma; "lemma_suf",t.lemma_suf; "interp",t.interp;
    "freq",string_of_int t.freq; "status",string_of_status t.status;
    "star",MorphologyRules.string_of_star t.star],
    Xlist.map t.tags (fun (k,v) ->
      Xml.Element("attr",["name",k;"value",v],[])))

let xml_of_interpretations l msg =
  if msg = "" then Xml.Element("data",[],Xlist.map l xml_of_interpretation)
  else Xml.Element("error",[],[Xml.PCData msg])

let html_of_interpretation t =
  let lemma = if t.lemma_suf = "" then t.lemma else t.lemma ^ ":" ^ t.lemma_suf in
  Printf.sprintf "<tr><td>%s</td><td>%s</td><td>%d</td><td>%s</td><td>%s</td><td>%s</td></tr>"
    lemma t.interp t.freq (string_of_status t.status)
    (MorphologyRules.string_of_star t.star)
    (String.concat " " (Xlist.map t.tags (fun (k,v) -> k ^ "=" ^ v)))

let html_of_interpretations l msg =
  if msg = "" then
    "<table><tr><td><b>lemma</b></td><td><b>interp</b></td><td><b>freq</b></td><td><b>status</b></td><td><b>star</b></td></td><td><b>attrs</b></td></tr>" ^
    String.concat "\n" (List.rev (Xlist.rev_map l html_of_interpretation)) ^
  "</table>"
  else msg

let html_of_interpretations2 l msg =
  if msg = "" then
    "<table><tr><td><b>form</b></td><td><b>interp</b></td><td><b>freq</b></td><td><b>status</b></td><td><b>star</b></td></td><td><b>attrs</b></td></tr>" ^
    String.concat "\n" (List.rev (Xlist.rev_map l html_of_interpretation)) ^
  "</table>"
  else msg

let prepare_alt alt alt_filename =
  let alt2 = load_tab alt_filename in
  let alt = Xlist.fold alt2 alt (fun alt entry ->
    Xlist.fold entry.forms alt (fun alt form ->
      let simple_lemma,lemma_suf = simplify_lemma entry.lemma in
      let v = true, {lemma=simple_lemma; lemma_suf=lemma_suf; interp=form.interp; freq=1; ntype_freq=StringQMap.empty; find=form.orth; status=LemmaAlt; star=Star; tags=[]} in
      StringMap.add_inc alt form.orth [v] (fun l -> v :: l))) in
  alt

let prepare_rev_alt alt alt_filename =
  let alt2 = load_tab alt_filename in
  let alt = Xlist.fold alt2 alt (fun alt entry ->
    Xlist.fold entry.forms alt (fun alt form ->
      let simple_lemma,lemma_suf = simplify_lemma entry.lemma in
      let v = {lemma=form.orth; lemma_suf=lemma_suf; interp=form.interp; freq=1; ntype_freq=StringQMap.empty; find=simple_lemma; status=LemmaAlt; star=Star; tags=[]} in
      StringMap.add_inc alt simple_lemma [v] (fun l -> v :: l))) in
  alt

let prepare_rules rules_filename =
  let rules = MorphologyRules.load_freq_rules rules_filename in
  let rules = MorphologyRules.CharTrees.create rules in
  rules

let prepare_rev_rules rules_filename =
  let rules = MorphologyRules.load_rev_freq_rules rules_filename in
(*   Xlist.iter rules (fun r -> print_endline (MorphologyRules.string_of_freq_rule r)); *)
  let rules = MorphologyRules.CharTrees.create rules in
  rules

let prepare_wyglos wyglos_filename =
  let wyglos = File.fold_tab wyglos_filename [] (fun wyglos -> function
      [freq; con; s; t; a; b] ->
        {empty_rule with freq=int_of_string freq; find=s; interp=con} :: wyglos
    | line -> failwith ("prepare_wyglos: " ^ (String.concat "\t" line))) in
  let wyglos = MorphologyRules.CharTrees.create wyglos in
  wyglos

let alt = ref (StringMap.empty : (bool * t) list StringMap.t)
let rev_alt = ref (StringMap.empty : t list StringMap.t)
let stems = ref (StringMap.empty : (string * string * StringSet.t) list StringMap.t)
let rules = ref ([] : (StringMap.key * MorphologyRules.CharTrees.t) list)
let rev_rules = ref ([] : (StringMap.key * MorphologyRules.CharTrees.t) list)
let wyglos = ref ([] : (StringMap.key * MorphologyRules.CharTrees.t) list)
let lemmata = ref StringSet.empty

let initialize () =
  alt := prepare_alt StringMap.empty alt_filename;
  alt := prepare_alt !alt alt_supplement_filename;
  rev_alt := prepare_rev_alt StringMap.empty alt_filename;
  rev_alt := prepare_rev_alt !rev_alt alt_supplement_filename;
  stems := load_stems stem_filename;
  rules := prepare_rules rules_filename;
  rev_rules := prepare_rev_rules rules_filename;
  wyglos := prepare_wyglos wyglos_filename;
  lemmata := StringSet.of_list (File.load_lines lemmata_filename)

(* let initialize () =
  alt := prepare_alt StringMap.empty "resources/alt.tab";
  stems := load_stems "resources/stem.tab";
  rules := prepare_rules "resources/freq_rules.tab";
  wyglos := prepare_wyglos "resources/wyglos.tab" *)

let get_tag l tag =
  try Xlist.assoc l tag with Not_found -> ""

let manage_aspect aspect interp =
  let l = Xstring.split_delim "imperf\\.perf" interp in
  String.concat aspect l
  (* match Xstring.split_delim "imperf\\.perf" interp with
    [s] -> s
  | [s;t] -> s ^ aspect ^ t
  | _ -> failwith ("manage_aspect: " ^ interp) *)

(* let has_vovel_sufix s =
  let n = String.length s in
  let a = String.get s (n-1) in
  if a = 'a' || a = 'e' || a = 'i' || a = 'o' || a = 'u' || a = 'y' then true else
  let a = String.sub s (n-2) 2 in
  if a = "ó" || a = "ą" || a = "ę" then true else
  false *)

(* let has_vovel_sufix = function
    "a" :: _ -> true
  | "ą" :: _ -> true
  | "e" :: _ -> true
  | "ę" :: _ -> true
  | "i" :: _ -> true
  | "o" :: _ -> true
  | "ó" :: _ -> true
  | "u" :: _ -> true
  | "y" :: _ -> true
  | _ -> false *)

let check_diftongs s t =
  if t = "" || s = "" then true else
  let n = String.length s in
  let a = String.get s (n-1) in
  let b = String.get t 0 in
  if (a = 'c' || a = 'd' || a = 'r' || a = 's') && b = 'z' then false else
  if a = 'c' && b = 'h' then false else
  if String.length t < 2 then true else
  let b = String.sub t 0 2 in
  if a = 'd' && (b = "ź" || b = "ż") then false else
  true

(* let check_patal s t = *)
  
(* let check_diftongs = function
    "c" :: _, "z" :: _ -> false
  | "d" :: _, "z" :: _ -> false
  | "r" :: _, "z" :: _ -> false
  | "s" :: _, "z" :: _ -> false
  | "d" :: _, "ź" :: _ -> false
  | "d" :: _, "ż" :: _ -> false
  | "c" :: _, "h" :: _ -> false
  | _ -> true *)

let char_tree_mem tree lcon2 s =
(*   Printf.printf "char_tree_mem: lcon2=%s s=%s " lcon2 s; *)
  let l = MorphologyRules.CharTrees.find tree s in
  let l = if lcon2 = "e" || lcon2 = "′e" then
    Xlist.fold l [] (fun l (stem,rule) -> if rule.interp = "e" then (stem,rule) :: l else l) else
    Xlist.fold l [] (fun l (stem,rule) -> if rule.interp = "" then (stem,rule) :: l else l) in
(*   print_endline (if l <> [] then "true" else "false"); *)
  l <> []

let is_uppercase s =
  if s = "" then false else
  let c = String.get s 0 in
  if 'A' <= c && 'Z' >= c then true else
  if Xstring.check_prefix "Ć" s || Xstring.check_prefix "Ś" s || Xstring.check_prefix "Ń" s ||
     Xstring.check_prefix "Ż" s || Xstring.check_prefix "Ł" s then true else
  false

let check_fluency flag stem (rule : MorphologyTypes.rule) =
(*   Printf.printf "check_fluency %s\t%s\n%!" stem (MorphologyRules.string_of_freq_rule rule); *)
  (* let rev_stem = List.rev (Xunicode.utf8_chars_of_utf8_string stem) in
  let rule_find = Xunicode.utf8_chars_of_utf8_string rule.find in *)
  (* let rule_set = Xunicode.utf8_chars_of_utf8_string rule.set in *)
  (* if not (check_diftongs (rev_stem,rule_find)) then false else *)
  if not (check_diftongs stem rule.find) then false else
(*   if not (check_patal stem rule.set) then false else *)
  if rule.MorphologyTypes.set = rule.MorphologyTypes.find then true else
  let cat = MorphologyRules.get_tag rule.tags "cat" in
  let lemma = MorphologyRules.get_tag rule.tags "lemma" in
  let flex = MorphologyRules.get_tag rule.tags "flex" in
  let lcon2 = MorphologyRules.get_tag rule.tags "lcon2" in
  let con2 = MorphologyRules.get_tag rule.tags "con2" in
  if flag && cat = "noun" && lemma = "ε" then
    if (rule.star = Aux || rule.star = Aux2 || rule.star = Acro) && lcon2 <> "" then false else
    if (rule.star = Aux || rule.star = Aux2 || rule.star = Acro || is_uppercase rule.find) && lcon2 = "" then true else
    (* if (*String.get rule.set 0 = 'e' &&*) has_vovel_sufix rev_stem then false else *)
    (* true else *)
    char_tree_mem !wyglos lcon2 (stem ^ rule.set) else
  if not flag && cat = "noun" && flex = "ε" then
    if (rule.star = Aux || rule.star = Aux2 || rule.star = Acro) && con2 <> "" then false else
    if (rule.star = Aux || rule.star = Aux2 || rule.star = Acro || is_uppercase rule.find) && con2 = "" then true else
    (* if (*String.get rule.set 0 = 'e' &&*) has_vovel_sufix rev_stem then false else *)
    (* true else *)
    char_tree_mem !wyglos con2 (stem ^ rule.set) else
  true

let select_fluent candidates =
  let selected =
    Xlist.fold candidates [] (fun candidates2 (b,x) ->
      if b then x :: candidates2 else candidates2) in
  if selected = [] then Xlist.map candidates snd else selected

let rec no_lemma_found = function
    [] -> true
  | {interp="brev:pun"} :: l -> no_lemma_found l
  | {interp="brev:npun"} :: l -> no_lemma_found l
  | _ -> false
  
let get_interpretations orth =
  let orth = String.concat "’" (Xstring.split "'" orth) in (* zaślepka w celu zamiany apostrofów *)
  let candidates = MorphologyRules.CharTrees.find !rules orth in
  let found = try StringMap.find !alt orth with Not_found -> [] in
  let found = Xlist.fold candidates found (fun found (stem,rule) ->
    (* Printf.printf "%s\t%s\n%!" stem (MorphologyRules.string_of_freq_rule rule); *)
    let fluency = check_fluency true stem rule in
    let l = try StringMap.find !stems stem with Not_found -> [] in
    let l = Xlist.fold l [] (fun l (lemma_suf,aspect,ids) ->
      if StringSet.mem ids rule.id then (lemma_suf,aspect) :: l else l) in
    if l = [] then
      if rule.star = Star then found else
      (fluency,{lemma=stem ^ rule.set; lemma_suf=""; interp=rule.interp; freq=rule.freq; ntype_freq=rule.ntype_freq; find=rule.find; status=LemmNotVal; star=rule.star; tags=rule.tags}) :: found else
    Xlist.fold l found (fun found (lemma_suf,aspect) ->
      (true,{lemma=stem ^ rule.set; lemma_suf=lemma_suf; interp=manage_aspect aspect rule.interp; freq=rule.freq; ntype_freq=rule.ntype_freq; find=rule.find; status=LemmaVal; star=rule.star; tags=rule.tags}) :: found)) in
  let found = select_fluent found in
  if no_lemma_found found then {lemma=orth; lemma_suf=""; interp="unk"; freq=1; ntype_freq=StringQMap.empty; find=""; status=TokNotFound; star=Star; tags=[]} :: found else found

let catch_get_interpretations form =
  try
    let result = get_interpretations form in result,""
  with e -> [], Printexc.to_string e

let suffix_lemmata = Xlist.fold [
  "em",{lemma="być"; lemma_suf=""; interp="aglt:sg:pri:imperf:wok"; freq=1; ntype_freq=StringQMap.empty; status=LemmaAlt; star=Star; find="em"; tags=[]};
  "eś",{lemma="być"; lemma_suf=""; interp="aglt:sg:sec:imperf:wok"; freq=1; ntype_freq=StringQMap.empty; status=LemmaAlt; star=Star; find="eś"; tags=[]};
  "eście",{lemma="być"; lemma_suf=""; interp="aglt:pl:sec:imperf:wok"; freq=1; ntype_freq=StringQMap.empty; status=LemmaAlt; star=Star; find="eście"; tags=[]};
  "eśmy",{lemma="być"; lemma_suf=""; interp="aglt:pl:pri:imperf:wok"; freq=1; ntype_freq=StringQMap.empty; status=LemmaAlt; star=Star; find="eśmy"; tags=[]};
  "m",{lemma="być"; lemma_suf=""; interp="aglt:sg:pri:imperf:nwok"; freq=1; ntype_freq=StringQMap.empty; status=LemmaAlt; star=Star; find="m"; tags=[]};
  "ś",{lemma="być"; lemma_suf=""; interp="aglt:sg:sec:imperf:nwok"; freq=1; ntype_freq=StringQMap.empty; status=LemmaAlt; star=Star; find="ś"; tags=[]};
  "ście",{lemma="być"; lemma_suf=""; interp="aglt:pl:sec:imperf:nwok"; freq=1; ntype_freq=StringQMap.empty; status=LemmaAlt; star=Star; find="ście"; tags=[]};
  "śmy",{lemma="być"; lemma_suf=""; interp="aglt:pl:pri:imperf:nwok"; freq=1; ntype_freq=StringQMap.empty; status=LemmaAlt; star=Star; find="śmy"; tags=[]};
  "by",{lemma="by"; lemma_suf=""; interp="qub"; freq=1; ntype_freq=StringQMap.empty; status=LemmaAlt; star=Star; find="by"; tags=[]};
  "ń",{lemma="on"; lemma_suf=""; interp="ppron3:sg:gen.acc:m1.m2.m3:ter:nakc:praep"; freq=1; ntype_freq=StringQMap.empty; status=LemmaAlt; star=Star; find="ń"; tags=[]};
  "że",{lemma="że"; lemma_suf=""; interp="qub"; freq=1; ntype_freq=StringQMap.empty; status=LemmaAlt; star=Star; find="że"; tags=[]};
  "ż",{lemma="że"; lemma_suf=""; interp="qub"; freq=1; ntype_freq=StringQMap.empty; status=LemmaAlt; star=Star; find="ż"; tags=[]};
  ] StringMap.empty (fun map (suf,lemma) -> StringMap.add map suf lemma)

let get_suf_interpretations orth =
  try [StringMap.find suffix_lemmata orth] with Not_found -> []
  
let rec check_interp_rec = function
    [],[] -> true
  | [],_ -> false
  | _,[] -> false
  | "_" :: l1, tag2 :: l2 -> check_interp_rec (l1, l2)
  | tag1 :: l1, "_" :: l2 -> check_interp_rec (l1, l2)
  | tag1:: l1, tag2 :: l2 -> 
      let set1 = StringSet.of_list (Xstring.split "\\." tag1) in
      let set2 = StringSet.of_list (Xstring.split "\\." tag2) in
      if StringSet.is_empty (StringSet.intersection set1 set2) then false
      else check_interp_rec (l1, l2)
  
let check_interp interp1 interp2 =
  if interp1 = "" then true else
  Xlist.fold (Xstring.split "|" interp2) false (fun b interp2 ->
    check_interp_rec (Xstring.split ":" interp1, Xstring.split ":" interp2) || b)
    
let synthetize lemma interp =
  let lemma,lemma_suf = simplify_lemma lemma in
  let candidates = MorphologyRules.CharTrees.find_ignore_prefix !rev_rules lemma in
  let found = try StringMap.find !rev_alt lemma with Not_found -> [] in
  let found = Xlist.fold found [] (fun found t ->
    if lemma_suf <> "" && lemma_suf <> t.lemma_suf then found else
    if not (check_interp interp t.interp) then found else
    (true,{t with lemma_suf=""}) :: found) in
  let found = Xlist.fold candidates found (fun found (stem,rule) ->
(*     Printf.printf "%s\t%s\n%!" stem (MorphologyRules.string_of_freq_rule rule); *)
(*     let cat2 = MorphologyRules.get_tag rule.tags "cat" in *)
    if not (check_interp interp rule.interp) then found else
    let pref = MorphologyRules.get_tag rule.tags "pref" in
    let pref = if pref = "ε" then "" else pref in
    let fluency = check_fluency false stem rule in
    let l = try StringMap.find !stems stem with Not_found -> [] in
    let b = Xlist.fold l false (fun b (lemma_suf2,aspect,ids) ->
      if lemma_suf <> "" && lemma_suf <> lemma_suf2 then b else
      if StringSet.mem ids rule.id then true else b) in
	(fluency,{lemma=pref ^ stem ^ rule.set; lemma_suf=""; interp=rule.interp; freq=rule.freq; ntype_freq=rule.ntype_freq; find=rule.find; status=if b then LemmaVal else LemmNotVal; star=rule.star; tags=rule.tags}) :: found) in
  let found = select_fluent found in
  if no_lemma_found found then {lemma=lemma; lemma_suf=""; interp="unk"; freq=1; ntype_freq=StringQMap.empty; find=""; status=TokNotFound; star=Star; tags=[]} :: found else found
(*  Xlist.iter candidates (fun (stem,rule) ->
    Printf.printf "%s " stem;
    MorphologyRules.print_rule stdout rule)*)

let disambiguate featured_stati excluded_stati l =
  let l = Xlist.fold l [] (fun l t ->
    let cat = MorphologyRules.get_tag t.tags "cat" in
    let prior = match t.status with
        LemmaAlt -> 100
	  | LemmaVal -> 100 (* gdy jest 200 dobry:adj:_:_:_:_ generuje tylko formy com i sup *)
	  | LemmNotVal -> 300 + (if t.star = Star then 1000 else 0)
	  | TokNotFound -> 400 + (if t.star = Star then 1000 else 0) in
	let prior2 = 
	  if Xlist.mem featured_stati t.star then -10 else
	  if Xlist.mem featured_stati Ndm && cat = "ndm" then -10 else
	  if Xlist.mem excluded_stati t.star then 10 else 
	  if Xlist.mem excluded_stati Ndm && cat = "ndm" then 10 else 0 in
	let prior3 = match MorphologyRules.get_tag t.tags "lemma" with
	    "ować" -> -1
	  | "ać" -> 1
	  | _ -> 0 in
    (prior+prior2+prior3,t) :: l) in 
  let _,l = Xlist.select_min_priority l in
  l
    
let catch_synthetize_disambiguate form =
  try
    let lemma,interp = match Xstring.split ":" form with
        lemma :: l -> lemma, String.concat ":" l
	  | _ -> failwith ("catch_synthetize_disambiguate: bad form " ^ form) in
    let result = disambiguate [] [Acro;Aux;Aux2;Ndm;Dial] (synthetize lemma interp) in
    result,""
  with e -> [], Printexc.to_string e

let int_of_status = function
    LemmaVal -> 0
  | LemmaAlt -> 1
  | LemmNotVal -> 2
  | TokNotFound -> 3

let compare_status s t =
  compare (int_of_status s) (int_of_status t)

let int_of_cat = function
    "verb" -> 0
  | "adv" -> 1
  | "adj" -> 2
  | "adj:grad" -> 3
  | "noun" -> 4
  | "ndm" -> 5
  | _ -> 6

let compare_cat s t =
  compare (int_of_cat s) (int_of_cat t)

let int_of_star = function
    Productive -> 0
  | Star -> 1
  | Ndm -> 2
  | Dial -> 6
  | Acro -> 5
  | Aux2 -> 4
  | Aux -> 3

let compare_star s t =
  compare (int_of_star s) (int_of_star t)

let int_of_interp s = 
  Xlist.fold (Xstring.split "\\." s) max_int (fun v s ->
    let v2 = match s with
         "" -> 0
       | "sg" -> 1
       | "pl" -> 2
       | "m1" -> 1
       | "m2" -> 2
       | "m3" -> 3
       | "n" -> 4
       | "f" -> 5
       | "nom" -> 1
       | "gen" -> 2
       | "dat" -> 3
       | "acc" -> 4
       | "inst" -> 5
       | "loc" -> 6
       | "voc" -> 7
       | "pos" -> 1
       | "com" -> 2
       | "sup" -> 3
       | "pri" -> 1
       | "sec" -> 2
       | "ter" -> 3
       | "imperf" -> 1
       | "perf" -> 2
       | "col" -> 1
       | "ncol" -> 2
       | "pt" -> 3
       | "ack" -> 1
       | "nack" -> 2
       | "aff" -> 1
       | "neg" -> 2
       | "praep" -> 1
       | "npraep" -> 2
       | "wok" -> 1
       | "nwok" -> 2
       | "congr" -> 1
       | "rec" -> 2
       | "agl" -> 1
       | "nagl" -> 2
       | _ -> 9 in
    min v v2)
  
let split_interp s =
  let s = List.hd (Xstring.split "|" s) in
  match Xstring.split ":" s with
      cat :: tag1 :: tag2 :: tag3 :: tag4 :: tag5 :: _ -> cat, tag1, tag2, tag3, tag4, tag5
    | [cat;tag1;tag2;tag3;tag4] -> cat, tag1, tag2, tag3, tag4, ""
    | [cat;tag1;tag2;tag3] -> cat, tag1, tag2, tag3, "", ""
    | [cat;tag1;tag2] -> cat, tag1, tag2, "", "", ""
    | [cat;tag1] -> cat, tag1, "", "", "", ""
    | [cat] -> cat, "", "", "", "", ""
    | [] -> "", "", "", "", "", ""
  
let compare_interp s t =
  let scat, stag1, stag2, stag3, stag4, stag5 = split_interp s in
  let tcat, ttag1, ttag2, ttag3, ttag4, ttag5 = split_interp t in
  let c = compare scat tcat in
  if c <> 0 then c else
  compare 
    (10000 * int_of_interp stag5 + 1000 * int_of_interp stag4 + 100 * int_of_interp stag3 + 10 * int_of_interp stag1 + int_of_interp stag2)
    (10000 * int_of_interp ttag5 + 1000 * int_of_interp ttag4 + 100 * int_of_interp ttag3 + 10 * int_of_interp ttag1 + int_of_interp ttag2)

let compare_results s t =
  let c = compare_status s.status t.status in
  if c <> 0 then c else
  let c = compare_cat (MorphologyRules.get_tag s.tags "cat") (MorphologyRules.get_tag t.tags "cat") in
  if c <> 0 then c else
  let c = compare_star s.star t.star in
  if c <> 0 then c else
  compare s.lemma t.lemma

let sort_results l =
  Xlist.sort l compare_results

let compare_results2 s t =
(*  let c = compare_cat (MorphologyRules.get_tag s.tags "cat") (MorphologyRules.get_tag t.tags "cat") in
  if c <> 0 then c else*)
  let c = compare_interp s.interp t.interp in
  if c <> 0 then c else
  let c = compare_status s.status t.status in
  if c <> 0 then c else
  let c = compare_star s.star t.star in
  if c <> 0 then c else
  compare s.lemma t.lemma

let sort_results2 l =
  Xlist.sort l compare_results2

  

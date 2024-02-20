(*
 *  ENIAMmorphology, a morphological analyser and a guesser for Polish
 *  Copyright (C) 2016 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
 *  Copyright (C) 2016 Institute of Computer Science Polish Academy of Sciences
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
open MorphologyTypes
open Printf

type tags =
    T of string * string
  | A of string

let parse_name s =
  if s = "" then failwith "parse_name: empty name" else
  if String.get s 0 = '@' then String.sub s 1 (String.length s - 1)
  else failwith ("parse_name: invalid name " ^ s)

let parse_tags s =
  Xlist.map (Xstring.split " " s) (fun t ->
    match Xstring.split "=" t with
      [k] -> A k
    | [k;v] -> T(k,v)
    | _ -> failwith "parse_tags")

let parse_ntype_freq s =
  Xlist.fold (Xstring.split " " s) StringQMap.empty (fun map t ->
    match Xstring.split "=" t with
      [k;v] -> StringQMap.add_val map k (try int_of_string v with _ -> failwith "parse_ntype_freq")
    | _ -> failwith "parse_ntype_freq")

let parse_star = function
    "" -> Productive
  | "*" -> Star
  | "ndm" -> Ndm
  | "D" -> Dial
  | "C" -> Acro
  | "B" -> Aux2
  | "A" -> Aux
  | s -> failwith ("parse_star: " ^ s)

exception MergeStars of star * star

let merge_stars = function
    Productive,Productive -> Productive
  | Star,Star -> Star
  | Dial,Dial -> Dial
  | Acro,Acro -> Acro
  | Aux,Aux -> Aux
  | Aux2,Aux2 -> Aux2
  | Star,Productive -> Star
  | Productive,Star -> Star
  | Dial,Productive -> Dial
  | Productive,Dial -> Dial
  | Acro,Productive -> Acro
  | Productive,Acro -> Acro
  | Aux,Productive -> Aux
  | Productive,Aux -> Aux
  | Aux2,Productive -> Aux2
  | Productive,Aux2 -> Aux2
  | Aux2,Aux -> Aux2
  | Aux,Aux2 -> Aux2
  | a,b -> raise (MergeStars(a,b))

let string_of_star = function
    Productive -> ""
  | Star -> "*"
  | Ndm -> "ndm"
  | Dial -> "D"
  | Acro -> "C"
  | Aux2 -> "B"
  | Aux -> "A"

let print_rule file rule =
  Printf.fprintf file "%s\t%d\t%s\t%s\t%s\t%s\t%s\n" rule.id rule.freq (string_of_star rule.star)
    rule.pref rule.find rule.set rule.interp

(**********************************************************************************************)

type alternation = {astar: star; aphone: string; aphone2: string; afind: string; aset: string}

let load_alternations filename =
  let alternations,name,alts = File.fold_tab filename ([],"",[]) (fun (found,name,alts) -> function
      [alt_name] ->
        let alt_name = parse_name alt_name in
        if name = "" then found,alt_name,[] else (name,List.rev alts) :: found,alt_name,[]
    | [star;d;a;b;c] -> found,name,{astar=parse_star star; aphone=a; aphone2=d; afind=b; aset=c} :: alts
    | line ->  failwith ("load_alternations:" ^ String.concat "\t" line)) in
  (name,List.rev alts) :: alternations

let alternations () = load_alternations "../morphology/data/alternations.dic"

let revert_alternations l =
  Xlist.map l (fun a -> {a with afind=a.aset; aset=a.afind})

let alternation_map alternations = Xlist.fold alternations StringMap.empty (fun map (k,v) ->
  StringMap.add map k v)

let rev_alternation_map alternations = Xlist.fold alternations StringMap.empty (fun map (k,v) ->
  StringMap.add map k (revert_alternations v))

type suf_rule = {sstar: star; salt_name: string; ssufix: string; stags: tags list}
type pref_rule = {pstar: star; pprefix: string; ptags: tags list}

let load_suf_rules filename =
  let suf_rules,name,rules = File.fold_tab filename ([],"",[]) (fun (found,name,rules) -> function
      [rules_name] ->
        let rules_name = parse_name rules_name in
        if name = "" then found,rules_name,[] else (name,List.rev rules) :: found,rules_name,[]
    | [star;alt_name;sufix;tags] -> found,name,{sstar=parse_star star; salt_name=alt_name; ssufix=sufix; stags=parse_tags tags} :: rules
    | line -> failwith ("load_suf_rules: " ^ String.concat "\t" line)) in
  (name,List.rev rules) :: suf_rules

let load_pref_rules filename =
  let pref_rules,name,rules = File.fold_tab filename ([],"",[]) (fun (found,name,rules) -> function
      [rules_name] ->
        let rules_name = parse_name rules_name in
        if name = "" then found,rules_name,[] else (name,List.rev rules) :: found,rules_name,[]
    | [star;prefix;tags] -> found,name,{pstar=parse_star star; pprefix=prefix; ptags=parse_tags tags} :: rules
    | _ -> failwith "load_pref_rules") in
  (name,List.rev rules) :: pref_rules

let rules () = load_suf_rules "../morphology/data/rules.dic"
let rev_rules () = load_suf_rules "../morphology/data/rev_rules.dic"
let pref_rules () = load_pref_rules "../morphology/data/pref_rules.dic"

let expand_tags x y l =
  List.flatten (Xlist.map l (function
      T(k,v) -> [k,v]
    | A k ->
        (if x = "" then [] else [k,x]) @
        (if y = "" then [] else [k ^ "2",y])))

let expand_tags_simple l =
  Xlist.map l (function
      T(k,v) -> k,v
    | A k -> failwith ("expand_tags_simple: " ^ k))

let load_freq_rules filename =
  File.fold_tab filename [] (fun rules -> function
    [id; freq; star; pref; find; set; interp; tags] ->
       {id=id; freq=int_of_string freq; star=parse_star star; pref=pref; find=find; set=set;
        tags=expand_tags_simple (parse_tags tags); interp=interp; ntype_freq=StringQMap.empty} :: rules
  | [id; freq; star; pref; find; set; interp; tags; ntype] ->
       {id=id; freq=int_of_string freq; star=parse_star star; pref=pref; find=find; set=set;
        tags=expand_tags_simple (parse_tags tags); interp=interp; ntype_freq=parse_ntype_freq ntype} :: rules
  | _ -> failwith "load_freq_rules")

let load_rev_freq_rules filename =
  File.fold_tab filename [] (fun rules -> function
    [id; freq; star; pref; find; set; interp; tags] ->
       {id=id; freq=int_of_string freq; star=parse_star star; pref=pref; find=set; set=find;
        tags=expand_tags_simple (parse_tags tags); interp=interp; ntype_freq=StringQMap.empty} :: rules
  | [id; freq; star; pref; find; set; interp; tags; ntype] ->
       {id=id; freq=int_of_string freq; star=parse_star star; pref=pref; find=set; set=find;
        tags=expand_tags_simple (parse_tags tags); interp=interp; ntype_freq=parse_ntype_freq ntype} :: rules
  | _ -> failwith "load_rev_freq_rules")

let prepare_rules alternation_map suf_rules =
  Xlist.fold suf_rules [] (fun rules s ->
    let alternation = try StringMap.find alternation_map s.salt_name with Not_found -> failwith ("prepare_rules: " ^ s.salt_name) in
    Xlist.fold alternation rules (fun rules a ->
        try
          {star=merge_stars (s.sstar,a.astar); pref=""; find=a.afind ^ s.ssufix; set=a.aset;
           tags=expand_tags a.aphone a.aphone2 s.stags; interp=""; id=""; freq=0; ntype_freq=StringQMap.empty} :: rules
        with MergeStars _ -> rules))

let prepare_rev_rules rev_alternation_map suf_rules =
  Xlist.fold suf_rules [] (fun rules s ->
    let alternation = try StringMap.find rev_alternation_map s.salt_name with Not_found -> failwith ("prepare_rev_rules: " ^ s.salt_name) in
    Xlist.fold alternation rules (fun rules a ->
        try
          {star=merge_stars (s.sstar,a.astar); pref=""; find=a.afind; set=a.aset ^ s.ssufix;
           tags=expand_tags a.aphone a.aphone2 s.stags; interp=""; id=""; freq=0; ntype_freq=StringQMap.empty} :: rules
        with MergeStars _ -> rules))

let prepare_pref_rules pref_rules =
  Xlist.fold pref_rules [] (fun rules p ->
    {star=p.pstar; pref=p.pprefix; find=""; set=""; tags=expand_tags "" "" p.ptags; interp=""; id=""; freq=0; ntype_freq=StringQMap.empty} :: rules)

let rule_map alternation_map rev_alternation_map rules rev_rules pref_rules =
  let map = Xlist.fold rules StringMap.empty (fun map (k,v) -> StringMap.add map k (prepare_rules alternation_map v)) in
(*  StringMap.iter map (fun k rules ->
    print_endline k;
    Xlist.iter rules (print_rule stdout));
  print_endline "---------------------";*)
  let map = Xlist.fold rev_rules map (fun map (k,v) -> StringMap.add map k (prepare_rev_rules rev_alternation_map v)) in
  Xlist.fold pref_rules map (fun map (k,v) -> StringMap.add map k (prepare_pref_rules v))

let schemata () = File.load_tab "../morphology/data/schemata.dic" (fun l -> l)

(**********************************************************************************************)

let rec extract_tag s rev = function
    [] -> "", List.rev rev
  | (k,v) :: l -> if s = k then v, List.rev rev @ l else extract_tag s ((k,v) :: rev) l

let get_tag l tag =
  try Xlist.assoc l tag with Not_found -> ""

let create_compound_rules schemata rule_map =
  let found = Xlist.fold schemata [] (fun found schema ->
    let compounds = Xlist.fold schema [{star=Productive;pref="";find="";set="";tags=[];interp=""; id=""; freq=0; ntype_freq=StringQMap.empty}] (fun compounds rule_set_name ->
      let rules = try StringMap.find rule_map rule_set_name with Not_found -> failwith ("create_compound_rules: " ^ rule_set_name) in
      Xlist.fold compounds [] (fun compounds compound ->
        Xlist.fold rules compounds (fun compounds rule ->
          (* printf "compound.find=%s; compound.set=%s\n" compound.find compound.set;
          printf "rule.find=%s; rule.set=%s\n" rule.find rule.set;  *)
          try
          if rule.find = "" && rule.set = "" then
            {compound with star=merge_stars (compound.star, rule.star);
                           pref=compound.pref ^ rule.pref; tags=rule.tags@compound.tags} :: compounds
          else if Xstring.check_sufix compound.set rule.find then
            {compound with star=merge_stars (compound.star, rule.star);
                           find=Xstring.cut_sufix compound.set rule.find ^ compound.find; set=rule.set; tags=rule.tags@compound.tags} :: compounds
          else if Xstring.check_sufix rule.find compound.set then
            {compound with star=merge_stars (compound.star, rule.star);
                           find=compound.find; set=Xstring.cut_sufix rule.find compound.set ^ rule.set; tags=rule.tags@compound.tags} :: compounds
          else compounds
          with MergeStars _ -> compounds))) in
    compounds @ found) in
  let found = Xlist.rev_map found (fun rule ->
    let suf, tags = extract_tag "suf" [] rule.tags in
    {rule with set=rule.set ^ suf; tags=tags}) in
  found

let make_compound_rules () =
  let schemata = schemata () in
  let alternations = alternations () in
  let alternation_map = alternation_map alternations in
  let rev_alternation_map = rev_alternation_map alternations in
  let rule_map = rule_map alternation_map rev_alternation_map (rules ()) (rev_rules ()) (pref_rules ()) in
  create_compound_rules schemata rule_map

(**********************************************************************************************)

let tag_value = function
    "cat" -> 1
  | "pref" -> 2
  | "con" -> 3
  | "con2" -> 4
  | "grad" -> 5
  | "group" -> 6
  | "flex2" -> 7
  | "flex" -> 8
  | "lemma" -> 9
  | "lcon" -> 10
  | "lcon2" -> 11
  | "palat" -> 12
  | "velar" -> 13
  | "agl" -> 14
  | "agl2" -> 15
  | "orth" -> 16
  | s -> failwith ("tag_value: " ^ s)

let tag_value2 = function
    "cat" -> 1
  | "flex" -> 2
  | "flex2" -> 3
  | "grad" -> 4
  | "pref" -> 5
  | "lemma" -> 6
  | "con" -> 7
  | "con2" -> 8
  | "lcon" -> 9
  | "lcon2" -> 10
  | "group" -> 11
  | "palat" -> 12
  | "velar" -> 13
  | "agl" -> 14
  | "agl2" -> 15
  | "orth" -> 16
  | s -> failwith ("tag_value2: " ^ s)

let compare_tag (a,_) (b,_) =
  compare (tag_value a) (tag_value b)

let compare_tag2 (a,_) (b,_) =
  compare (tag_value2 a) (tag_value2 b)

let load_interp_rules filename =
  File.load_tab filename (function
      star :: tags :: interp :: comment :: [] ->
        {star=parse_star star;
          pref=""; find=""; set="";
          tags=expand_tags_simple (parse_tags tags); interp=interp; (*comment=comment;*) id=""; freq=0; ntype_freq=StringQMap.empty}
    | line -> failwith ("load_tab: " ^ (String.concat "\t" line)))

module InterpTree = struct

  type t =
      N of string * t StringMap.t * rule list
    | L of rule list

  let empty = L []

  let rec create_rec rule = function
      [],N(key,map,rules) -> N(key,map,rule :: rules)
    | [],L rules -> L(rule :: rules)
    | (k,v) :: tags,N(key,map,rules) ->
        if k <> key then failwith ("create_rec: " ^ k ^ " " ^ key) else
        let tree = try StringMap.find map v with Not_found -> empty in
        let tree = create_rec rule (tags,tree) in
        N(key,StringMap.add map v tree,rules)
    | (k,v) :: tags,L rules ->
        let tree = create_rec rule (tags,empty) in
        N(k,StringMap.add StringMap.empty v tree,rules)

  let create interp_rules =
    Xlist.fold interp_rules empty (fun interp_tree rule ->
      let tags = Xlist.sort rule.tags compare_tag2 in
      create_rec rule (tags,interp_tree))

  let rec find_rec = function
      [],N(_,_,rules) -> rules
    | _,L rules -> rules
    | (k,v) :: tags,N(key,map,rules) ->
        if k <> key then find_rec (tags,N(key,map,rules)) else
        try rules @ (find_rec (tags,StringMap.find map v))
        with Not_found -> rules

  let find interp_tree tags =
    find_rec (Xlist.sort tags compare_tag2,interp_tree)

end

let interp_tree () = InterpTree.create (load_interp_rules "../morphology/data/interp_rules.dic")

(**********************************************************************************************)

let create_interp_compound_rules interp_tree compound_rules =
  Xlist.fold compound_rules [] (fun interp_compound_rules rule ->
    let interp_rules = InterpTree.find interp_tree rule.tags in
    Xlist.fold interp_rules interp_compound_rules (fun interp_compound_rules interp_rule ->
      try {rule with interp=interp_rule.interp; star=merge_stars (rule.star, interp_rule.star)} :: interp_compound_rules
      with MergeStars _ -> interp_compound_rules))

let assign_ids rules =
  fst (Xlist.fold rules ([],1) (fun (rules,id) rule ->
    {rule with id=string_of_int id} :: rules, id+1))

let interp_compound_rules compound_rules = assign_ids (create_interp_compound_rules (interp_tree ()) compound_rules)

(**********************************************************************************************)

module CharTrees = struct

  type t = M of t CharMap.t * rule list

  let empty = M(CharMap.empty,[])

  let rec add_path_rules rule orth_suf i (M(map,rules)) =
    if i = -1 then M(map,rule :: rules) else
    let tree = try CharMap.find map (String.get orth_suf i) with Not_found -> empty in
    let tree = add_path_rules rule orth_suf (i-1) tree in
    M(CharMap.add map (String.get orth_suf i) tree,rules)

  let create_char_tree rules =
    let tree = Xlist.fold rules empty (fun tree rule ->
      add_path_rules rule rule.find (String.length rule.find - 1) tree) in
    tree

  let create rules =
    let prefix_map = Xlist.fold rules StringMap.empty (fun prefix_map rule ->
      StringMap.add_inc prefix_map rule.pref [rule] (fun l -> rule :: l)) in
    StringMap.fold prefix_map [] (fun trees prefix rules -> (prefix, create_char_tree rules) :: trees)

  let rec find_rec l i orth (M(map,rules)) =
    if i = 0 then Xlist.fold rules l (fun l rule -> ("", rule) :: l) else
    let l = try find_rec l (i-1) orth (CharMap.find map (String.get orth (i-1))) with Not_found -> l in
    Xlist.fold rules l (fun l rule -> (String.sub orth 0 i, rule) :: l)

  let find trees orth =
(*     print_endline "find"; *)
    Xlist.fold trees [] (fun found (pref,tree) ->
(*       print_endline pref; *)
      if Xstring.check_prefix pref orth then (
        let orth = Xstring.cut_prefix pref orth in
(*         printf "%s %d " orth (Xlist.size found); *)
        let found = find_rec found (String.length orth) orth tree in
(*         printf "%d\n%!" (Xlist.size found); *)
(*         Xlist.iter found (fun (stem,rule) -> printf "F %s\t%s\n" stem (string_of_rule rule)); *)
        found)
      else found)

  let find_ignore_prefix trees orth =
(*     print_endline "find"; *)
    Xlist.fold trees [] (fun found (pref,tree) ->
(*         printf "%s %d " orth (Xlist.size found); *)
        let found = find_rec found (String.length orth) orth tree in
(*         printf "%d\n%!" (Xlist.size found); *)
(*         Xlist.iter found (fun (stem,rule) -> printf "F %s\t%s\n" stem (string_of_rule rule)); *)
        found)

  let add_char c rule =
    let s = String.make 1 c in
    {rule with find=s ^ rule.find; set=s ^ rule.set}

  let rec disjoint_rec super (M(map,rules)) =
    let rules = rules @ super in
    if CharMap.is_empty map then M(map,rules) else
    M(CharMap.mapi map (fun c tree ->
      disjoint_rec (Xlist.rev_map rules (add_char c)) tree),[])

  let disjoint trees =
    Xlist.rev_map trees (fun (pref,tree) ->
      pref, disjoint_rec [] tree)

  let rec print_rules_rec file (M(map,rules)) =
    Xlist.iter rules (print_rule file);
    CharMap.iter map (fun _ tree -> print_rules_rec file tree)

  let print_rules filename trees =
    File.file_out filename (fun file ->
      Xlist.iter trees (fun (_,tree) ->
        print_rules_rec file tree))

end

let compound_rule_trees compound_rules = CharTrees.create compound_rules
let interp_compound_rule_trees interp_compound_rules = CharTrees.create interp_compound_rules

let make_compound_rule_trees = compound_rule_trees

let make_interp_compound_rule_trees compound_rules =
  interp_compound_rule_trees (interp_compound_rules compound_rules)

(**********************************************************************************************)

module OrderedRule = struct

  type t = rule

  let compare = compare

end

module RuleQMap = Xmap.MakeQ(OrderedRule)
module RuleMap = Xmap.Make(OrderedRule)

let string_of_ntype_freq map = 
  String.concat " " (StringQMap.fold map [] (fun l k v -> (k ^ "=" ^ string_of_int v) :: l))

let string_of_freq_rule rule =
  sprintf "%s\t%d\t%s\t%s\t%s\t%s\t%s\t%s\t%s" rule.id rule.freq 
    (string_of_star rule.star) rule.pref rule.find rule.set rule.interp (string_of_tags rule.tags) (string_of_ntype_freq rule.ntype_freq)

(**********************************************************************************************)

let latex_escape_char = function
    "′" -> "$'$"
  | "ʲ" -> "\\textipa{\\super{j}}"
  | "ʒ" -> "\\textipa{Z}"
  | "ǯ" -> "\\textipa{\\v{Z}}"
  | "ε" -> "$\\varepsilon$"
  | c -> c

let latex_escape_string s =
  String.concat "" (Xlist.map (Xunicode.utf8_chars_of_utf8_string s) latex_escape_char)

let latex_of_alternation a =
  (if a.astar = Star then "$\\star$" else "") ^
  latex_escape_string a.afind ^ " $\\rightarrow$ " ^ latex_escape_string a.aset

let make_alternation_line phons alternation =
  let l,map = Xlist.fold phons ([],StringMap.empty) (fun (l,map) phon ->
    if not (StringMap.mem alternation phon) then "" :: l,map else
    let alts = StringMap.find alternation phon in
    latex_of_alternation (List.hd alts) :: l,
    if List.tl alts = [] then map else (StringMap.add map phon (List.tl alts))) in
  String.concat " & " (List.rev l), map

let rec print_alternation name phons map =
  if StringMap.is_empty map then print_endline "\\hline\\\\" else (
  let s,map = make_alternation_line phons map in
  print_endline (latex_escape_string name ^ " & " ^ s ^ "\\\\");
  print_alternation "" phons map)

let make_alternation_line2 alts =
  let l,alts = Xlist.fold alts ([],[]) (fun (l,alts) -> function
      [] -> "" :: l,[] :: alts
    | alt :: a -> latex_of_alternation alt :: l, a :: alts) in
  String.concat " & " (List.rev l), List.rev alts

let rec print_alternation2 name alts =
  if Xlist.fold alts true (fun b -> function [] -> b | _ -> false) then print_endline "\\hline" else (
  let s,alts = make_alternation_line2 alts in
  print_endline (latex_escape_string name ^ " & " ^ s ^ "\\\\");
  print_alternation2 "" alts)

let alt_names = [
  "funkcjonalnie_miekkie_iy","\\boldmath$\\alpha'${\\bf y}";
  "funkcjonalnie_miekkie_ae","\\boldmath$\\alpha'$";
  "funkcjonalnie_miekkie_wyglos","\\boldmath$\\alpha'\\varepsilon$";
  "funkcjonalnie_twarde_y","\\boldmath$\\alpha${\\bf y}";
  "funkcjonalnie_twarde_e","\\boldmath$\\alpha${\\bf e}";
  "funkcjonalnie_twarde_a","\\boldmath$\\alpha$";
  "funkcjonalnie_twarde_i","\\boldmath$\\alpha${\\bf i}";
  "funkcjonalnie_twarde_ie","\\boldmath$\\alpha${\\bf ie}";
  "funkcjonalnie_twarde_wyglos","\\boldmath$\\alpha\\varepsilon$";
  "funkcjonalnie_twarde_u1","\\boldmath$\\alpha_1$";
  "funkcjonalnie_twarde_u2","\\boldmath$\\alpha_2$";
  "dowolne","";
  "funkcjonalnie_twarde_ie1","\\boldmath$\\alpha${\\bf ie}$_1$";
  "funkcjonalnie_twarde_ie2","\\boldmath$\\alpha${\\bf ie}$_2$";
  "adj_grad_miekkie_sz","\\boldmath$\\kappa'$";
  "adj_grad_miekkie_iejsz","\\boldmath$\\lambda'$";
  "adj_grad_twarde_sz","\\boldmath$\\kappa$";
  "adj_grad_twarde_iejsz","\\boldmath$\\lambda$";
  "adv_grad_miekkie","\\boldmath$\\xi'$";
  "adv_grad_twarde","\\boldmath$\\xi$";
  "sz","{\\bf š}";
  "sz_i","{\\bf ši}";
  "c_cz","{\\bf č}";
  "aiy","\\boldmath$\\iota$";
(*  "ger_t","{\\bf t\\boldmath$'$}";
  "ger_n","{\\bf n\\boldmath$'$}";
  "pact","{\\bf c}";
  "ppas_t","{\\bf t}";
  "ppas_n","{\\bf n}";
  "ppas_ti","{\\bf t\\boldmath$'$i}";
  "ppas_ni","{\\bf n\\boldmath$'$i}";
  "praet","{\\bf ł}";*)
  "praet_i","\\boldmath$\\beta${\\bf li}";
  "ae","{\\bf a}";
  "verb_n","\\boldmath$\\eta$";
  "verb_impt","\\boldmath$\\gamma\\varepsilon$";
  "verb_fin","\\boldmath$\\gamma$";
  "verb_łszy","\\boldmath$\\beta${\\bf ł}";
  "verb_inf_ć","\\boldmath$\\beta${\\bf t}$'$";
  "ppas_e","\\boldmath$\\nu$";
  "verb_owa","";
  "verb_ywa","y";
  "verb_awa","";
  "verb_a","";
  "verb_u","";
  "verb_y","";
  "verb_palat","\\boldmath$\\alpha'${\\bf y}";
  "verb_palat_e","\\boldmath$\\alpha'$";
  "prefix_e","{\\bf }";
  "verb_ą2","{\\bf }";
  "verb_ą1","{\\bf }";
  "verb_ą3","{\\bf }";
  "funkcjonalnie_miekkie_ae2","\\boldmath$\\alpha'$";
  "funkcjonalnie_miekkie_iy2","\\boldmath$\\alpha'${\\bf y}";
  "palat_j","\\boldmath$\\zeta$";
  "kapitaliki_y","{\\bf }";
  "kapitaliki_e","{\\bf }";
  "kapitaliki_a","{\\bf }";
  "kapitaliki_ie","{\\bf }";
  "kapitaliki_wyglos","{\\bf }";
  "obce_ais","{\\bf }";
  "obce_apostrof","{\\bf }";
  "obce_funkcjonalnie_miekkie_ae","{\\bf }";
  "obce_funkcjonalnie_twarde_iy","{\\bf }";
  "obce_funkcjonalnie_twarde_ie","{\\bf }";
  "obce_funkcjonalnie_twarde_e","{\\bf }";
  "obce_funkcjonalnie_twarde_a","{\\bf }";
  "obce_funkcjonalnie_twarde_i","{\\bf }";
  "obce_funkcjonalnie_twarde_wyglos","{\\bf }";
  "obce_funkcjonalnie_miekkie_iy","{\\bf }";
  "obce_funkcjonalnie_miekkie_wyglos","{\\bf }";
  "obce_eu","{\\bf }";
  "obce_ech","{\\bf }";
  ]

let rec print_alternations names phons alternations =
  print_endline ("\\begin{center}\n\\begin{tabular}{r" ^ String.concat "" (Xlist.map phons (fun _ -> "r")) ^ "}");
  print_endline (" & " ^ String.concat " & " (Xlist.map phons latex_escape_string) ^ "\\\\");
  print_endline "\\hline\\\\";
  Xlist.iter names (fun name ->
    let name2 = Xlist.assoc alt_names name in
    let alternation = StringMap.find alternations name in
    let alternation = Xlist.fold alternation StringMap.empty (fun alternation a ->
      StringMap.add_inc alternation a.aphone [a] (fun l -> a :: l)) in
    let alternation = StringMap.map alternation List.rev in
    print_alternation name2 phons alternation);
  print_endline "\\end{tabular}\n\\end{center}\n"

let rec print_alternations2 names phons alternations =
  print_endline ("\\begin{longtable}{r|" ^ String.concat "" (Xlist.map names (fun _ -> "r")) ^ "}");
  print_endline (" & " ^ String.concat " & " (Xlist.map names (fun name ->
    try Xlist.assoc alt_names name with Not_found -> failwith ("print_alternations2: " ^ name))) ^ "\\\\");
  print_endline "\\hline";
  (* print_endline "\\endhead\n\\hline\\\\"; *)
  let alternations = Xlist.map names (fun name ->
    try StringMap.find alternations name with Not_found -> failwith ("print_alternations2: " ^ name)) in
  let alternations = Xlist.map alternations (fun alternation ->
    Xlist.fold alternation StringMap.empty (fun alternation a ->
      StringMap.add_inc alternation a.aphone [a] (fun l -> a :: l))) in
  Xlist.iter phons (fun phon ->
    print_alternation2 phon (Xlist.map alternations (fun a -> try List.rev (StringMap.find a phon) with Not_found -> [])));
  print_endline "\\end{longtable}\n"

let palat_alts = [
  "funkcjonalnie_miekkie_iy";
  "funkcjonalnie_miekkie_ae";
  "funkcjonalnie_miekkie_wyglos";]

let npalat_alts = [
  "funkcjonalnie_twarde_y";
  "funkcjonalnie_twarde_e";
  "funkcjonalnie_twarde_a";
  "funkcjonalnie_twarde_i";
  "funkcjonalnie_twarde_ie";
  "funkcjonalnie_twarde_wyglos";]

let palat_grad = [
  "adj_grad_miekkie_sz";
  "adj_grad_miekkie_iejsz";
  "adv_grad_miekkie";
  ]

let npalat_grad = [
  "adj_grad_twarde_sz";
  "adj_grad_twarde_iejsz";
  "adv_grad_twarde";
  ]

let other = [
  "sz";
  "sz_i";
  "c_cz";
  "aiy";
  "ppas_e";
  "ae"
  ]

let verb_flex = [
  "verb_łszy";
  "praet_i";
  "verb_inf_ć";
  "verb_impt";
  "verb_fin";
  "verb_n";
  "palat_j";
  ]

let palat_phons = ["b′";"d′";"f′";"m′";"n′";"p′";"s′";"t′";"v′";"z′";"l";"c";"č";"ʒ";"ǯ";"ř";"š";"ž";"ʲ";"j";"g′";"k′";"a";"e"]

let palat_phons1 = ["b′";"d′";"f′";"m′";"n′"]
let palat_phons2 = ["p′";"s′";"t′";"v′";"z′";"l"]
let palat_phons3 = ["c";"č";"ʒ";"ǯ";"ř";"š";"ž"]
let palat_phons4 = ["ʲ";"j";"g′";"k′";"a";"e"]

let npalat_phons = ["b";"x";"d";"f";"h";"ł";"m";"n";"p";"r";"s";"t";"v";"z";"g";"k";"o";"u"]

let npalat_phons1 = ["b";"x";"d";"f";"h";"ł"]
let npalat_phons2 = ["m";"n";"p";"r";"s"]
let npalat_phons3 = ["t";"v";"z";"g";"k";"o";"u"]

let palat_grad_phons = ["m′";"n′";"p′";"c";"č";"ž"]
let npalat_grad_phons = ["b";"x";"d";"h";"ł";"m";"n";"p";"r";"t";"v";"g";"k"]

let other_phons = ["c";"š";"a";"e";"i";"o";"y"]

let all_phons = ["b′";"d′";"f′";"m′";"n′";"p′";"s′";"t′";"v′";"z′";"l";"c";"č";"ʒ";"ǯ";"ř";"š";"ž";"ʲ";"j";"g′";"k′";"a";"e";
  "b";"x";"d";"f";"h";"ł";"m";"n";"p";"r";"s";"t";"v";"z";"g";"k";"o";"u";"i";"y";"ą";"ę"]

let verb_flex_phons = ["";"l";"c";"č";"ř";"ž";"j";"a";
  "b";"x";"d";"f";"h";"ł";"m";"n";"p";"r";"s";"t";"v";"z";"g";"k"]

let merge_ie alternations =
  let ie1 = StringMap.find alternations "funkcjonalnie_twarde_ie1" in
  let ie2 = StringMap.find alternations "funkcjonalnie_twarde_ie2" in
  let ie = ie1 @ ie2 in
  StringMap.add alternations "funkcjonalnie_twarde_ie" ie

let latex_of_alternations filename =
  let alternations = alternation_map (load_alternations filename) in
  let alternations = merge_ie alternations in
  (* print_alternations palat_alts palat_phons1 alternations;
  print_alternations palat_alts palat_phons2 alternations;
  print_alternations palat_alts palat_phons3 alternations;
  print_alternations palat_alts palat_phons4 alternations;
  print_alternations npalat_alts npalat_phons1 alternations;
  print_alternations npalat_alts npalat_phons2 alternations;
  print_alternations npalat_alts npalat_phons3 alternations; *)
  print_alternations2 palat_alts palat_phons alternations;
  print_alternations2 npalat_alts npalat_phons alternations;
  print_alternations2 palat_grad palat_grad_phons alternations;
  print_alternations2 npalat_grad npalat_grad_phons alternations;
  print_alternations2 other other_phons alternations;
  print_alternations2 verb_flex verb_flex_phons alternations;
  ()

let latex_math_text s =
  if s = "" then "\\varepsilon" else
  "\\text{" ^ latex_escape_string s ^ "}"

let latex_prepare_tags tags =
  String.concat ", " (List.rev (Xlist.fold tags [] (fun l -> function
    T("cat",v) -> (latex_math_text v) :: l
  | T("palat","t") -> "\\uparrow" :: l
  | T("palat","n") -> "\\downarrow" :: l
  | T("palat","ę") -> l
  | T("palat","anin") -> l
  | T("palat","mię") -> l
  | T("palat","o") -> l
  | T("palat","stwo") -> l
  | T("palat","ni") -> l
  | T("velar","t") -> "\\rightarrow" :: l
  | T("velar","n") -> "\\leftarrow" :: l
  | T(k,v) -> (latex_math_text k ^ ":=" ^ latex_math_text v) :: l
  | A _ -> l)))

let latex_prepare_tags2 tags =
  String.concat ", " (List.rev (Xlist.fold tags [] (fun l -> function
    ("cat",v) -> (latex_escape_string v) :: l
  | ("palat","t") -> "$\\uparrow$" :: l
  | ("palat","n") -> "$\\downarrow$" :: l
  | ("palat","ę") -> l
  | ("palat","anin") -> l
  | ("palat","mię") -> l
  | ("palat","o") -> l
  | ("palat","stwo") -> l
  | ("palat","ni") -> l
  | ("velar","t") -> "$\\rightarrow$" :: l
  | ("velar","n") -> "$\\leftarrow$" :: l
  | (k,v) -> (k ^ ":=" ^ latex_escape_string v) :: l)))

let latex_prepare_rule r =
  (if r.sstar = Star then "\\star" else if r.sstar = Dial then "D" else "") ^
  "-" ^ latex_math_text ((try Xlist.assoc alt_names r.salt_name with Not_found -> print_endline r.salt_name; "???") ^ r.ssufix) ^
  " & " ^ (latex_prepare_tags r.stags)

let latex_prepare_rev_rule r =
  (if r.sstar = Star then "\\star" else if r.sstar = Dial then "D" else "") ^
  "+" ^ latex_math_text ((try Xlist.assoc alt_names r.salt_name with Not_found -> print_endline r.salt_name; "???") ^ r.ssufix) ^
  " & " ^ (latex_prepare_tags r.stags)

let latex_prepare_pref_rule r =
  (if r.pstar = Star then "\\star" else if r.pstar = Dial then "D" else "") ^
  latex_math_text r.pprefix ^ "- & " ^ (latex_prepare_tags r.ptags)

let latex_prepare_rules prepare_fun rule_map (name,rules) =
  let rules = Xlist.map rules prepare_fun in
  let rules = "\\left[\\begin{array}{ll}\n" ^ String.concat "\\\\\n" rules ^ "\n\\end{array}\\right]" in
  StringMap.add rule_map name rules

let latex_of_schemata () =
  let rule_map = Xlist.fold (rules ()) StringMap.empty (latex_prepare_rules latex_prepare_rule) in
  let rule_map = Xlist.fold (rev_rules ()) rule_map (latex_prepare_rules latex_prepare_rev_rule) in
  let rule_map = Xlist.fold (pref_rules ()) rule_map (latex_prepare_rules latex_prepare_pref_rule) in
  Xlist.iter (schemata ()) (fun schema ->
    print_endline "\\begin{scriptsize}\\[";
    let schema = Xlist.map schema (fun rule_set_name ->
      try StringMap.find rule_map rule_set_name with Not_found -> failwith ("create_compound_rules: " ^ rule_set_name)) in
    print_endline (String.concat " \\otimes " schema);
    print_endline "\\]\\end{scriptsize}\n")

let make_rule_key r =
  let cat = get_tag r.tags "cat" in
  let lemma = get_tag r.tags "lemma" in
  let group = get_tag r.tags "group" in
  let gender =
      match Xstring.split ":" (List.hd (Xstring.split "|" r.interp)) with
        "subst" :: n :: c :: g -> (String.concat ":" g)
      | "depr" :: _ -> "m1"
      | _ -> "" in
  if cat = "adj" || cat = "adv" || cat = "adj:grad" || cat = "ndm" then cat, cat else
  if cat = "verb" then
    if lemma = "" then cat, "verb:general" else
    cat ^ " " ^ lemma, cat else
  cat ^ " " ^ lemma ^ " " ^ group ^ " " ^ gender, cat

let case_number c =
  match List.hd (Xstring.split "\\." c) with
    "nom" -> 1
  | "gen" -> 2
  | "dat" -> 3
  | "acc" -> 4
  | "inst" -> 5
  | "loc" -> 6
  | "voc" -> 7
  | _ -> failwith "case_number"

let key_of_noun_nc s =
  match Xstring.split ":" s with
    ["sg";c] -> case_number c
  | ["pl";c] -> 7 + case_number c
  | ["depr"] -> 15
  | _ -> failwith "key_of_noun_nc"

let rec get_gender = function
    s :: l ->
      let ll = Xstring.split "|" s in
      (match Xstring.split ":" (List.hd ll) with
        "subst" :: _ :: _ :: g -> String.concat ":" g
      | _ -> get_gender (List.tl ll @ l))
  | [] -> failwith "get_gender"

let latex_of_noun_interp_rules l =
  let cat = get_tag (List.hd l).tags "cat" in
  let lemma = get_tag (List.hd l).tags "lemma" in
  let gender = get_gender (Xlist.map l (fun r -> r.interp)) in
  let l = Xlist.map l (fun r -> {r with tags = snd (extract_tag "cat" [] r.tags)}) in
  let l = Xlist.map l (fun r -> {r with tags = snd (extract_tag "lemma" [] r.tags)}) in
  print_endline ("cat=" ^ cat ^ " lemma=" ^ latex_escape_string lemma ^ " gender:=" ^ gender ^ "\\\\");
  let map = Xlist.fold l StringMap.empty (fun map r ->
    StringMap.add_inc map r.interp [r] (fun l -> r :: l)) in
  let l = List.sort compare (StringMap.fold map [] (fun l interp rules ->
    let interp = String.concat "|" (Xlist.map (Xstring.split "|" interp) (fun interp ->
      match Xstring.split ":" interp with
        "subst" :: n :: c :: _ -> n ^ ":" ^ c
      | "depr" :: _ -> "depr"
      | _ -> failwith "latex_of_noun_interp_rules: interp")) in
    let key = key_of_noun_nc (List.hd (Xstring.split "|" interp)) in
    (key, interp, Xlist.map rules (fun r ->
      let flex,tags = extract_tag "flex" [] r.tags in
      let palat,tags = extract_tag "palat" [] tags in
      let velar,tags = extract_tag "velar" [] tags in
      let palat_velar =
        (match palat with
          "t" -> "\\uparrow"
        | "n" -> "\\downarrow"
        | "" -> ""
        | _ -> failwith "latex_of_noun_interp_rules: palat") ^
        (match velar with
          "t" -> "\\rightarrow"
        | "n" -> "\\leftarrow"
        | "" -> ""
        | _ -> failwith "latex_of_noun_interp_rules: velar") in
      if tags <> [] then failwith "latex_of_noun_interp_rules: tags" else
      (if r.star = Star then "$\\star$" else "") ^
      latex_escape_char flex ^
      (if palat_velar = "" then "" else "$" ^ palat_velar ^ "$"))) :: l)) in
  print_endline "\\begin{tabular}{l|l}";
  Xlist.iter l (fun (_,interp,l) ->
    print_endline (interp ^ " & " ^ (String.concat " " l) ^ "\\\\"));
  print_endline "\\end{tabular}\\\\"

let latex_of_interp_rules_table l tags =
  print_endline ("\\begin{longtable}{p{7cm}|" ^ String.concat "|" (Xlist.map tags (fun _ -> "l")) ^ "}");
  print_endline ("interpretation & " ^ String.concat " & " tags ^ "\\\\\n\\hline");
  Xlist.iter (List.rev l) (fun (r: rule) ->
    let interp = String.concat "" (Xstring.split ":imperf\\.perf" r.interp) in
    let interp = String.concat " " (Xstring.split "|" interp) in
    print_endline ((if r.star = Star then "$\\star$" else "") ^ interp ^ " & " ^
      String.concat " & " (Xlist.map tags (fun tag ->
        latex_escape_string (get_tag r.tags tag))) ^ "\\\\"));
  print_endline "\\end{longtable}"


let latex_of_interp_rules () =
  let map = Xlist.fold (load_interp_rules "../morphology/data/interp_rules.dic") StringMap.empty (fun map r ->
    let key, cat = make_rule_key r in
    StringMap.add_inc map key (cat,[r]) (fun (_,l) -> cat, r :: l)) in
  StringMap.iter map (fun _ (cat,l) ->
    if cat = "noun" then latex_of_noun_interp_rules l else
    if cat = "adj" then latex_of_interp_rules_table l ["cat";"flex";"lemma"] else
    if cat = "adj:grad" then latex_of_interp_rules_table l ["cat";"pref";"flex";"lemma"] else
    if cat = "ndm" then latex_of_interp_rules_table l ["cat"] else
    if cat = "adv" then latex_of_interp_rules_table l ["cat";"flex";"lemma"] else
    if cat = "verb" then latex_of_interp_rules_table l ["cat";"pref";"group";"flex";"flex2";"lemma"] else
    if cat = "verb:general" then latex_of_interp_rules_table l ["cat";"pref";"flex";"flex2"] else
    Xlist.iter l (fun r ->
      print_endline ((if r.star = Star then "$\\star$" else "") ^ r.interp ^ " $\\leftarrow$ " ^ (latex_prepare_tags2 r.tags) ^ "\\\\")))

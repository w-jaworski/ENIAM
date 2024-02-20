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

type status = Idle | Symbols | Rules | RevSymbols | RevRules

let string_of_phon p =
  Printf.sprintf "%s  %s" p.phon (String.concat " " (Xlist.map p.mapping (fun r -> r.pfind ^ "->" ^ r.pset))) (*fun (a,b) -> a ^ "->" ^ b))*)

module CharTree = struct

  type t = M of t CharMap.t * phon_rule list

  let empty = M(CharMap.empty,[])

  let rec add_path_rules rule orth i (M(map,rules)) =
    if i = String.length orth then M(map,rule :: rules) else
    let tree = try CharMap.find map (String.get orth i) with Not_found -> empty in
    let tree = add_path_rules rule orth (i+1) tree in
    M(CharMap.add map (String.get orth i) tree,rules)

  let create rules =
    let tree = Xlist.fold rules empty (fun tree rule ->
      add_path_rules rule (rule.pfind ^ rule.psuf) 0 tree) in
    tree

  let rec find_rec l i orth (M(map,rules)) =
    if i = String.length orth then Xlist.fold rules l (fun l rule -> ("", rule) :: l) else
    let l = try find_rec l (i+1) orth (CharMap.find map (String.get orth i)) with Not_found -> l in
    Xlist.fold rules l (fun l rule -> (String.sub orth i (String.length orth - i), rule) :: l)

  let find tree orth =
    let found = find_rec [] 0 orth tree in
    (* printf "%d\n%!" (Xlist.size found); *)
    (* Xlist.iter found (fun (stem,rule) -> printf "F %s\t%s\n" stem (string_of_rule rule)); *)
    found

end

let load_rules filename =
  let status,symbol_defs,rev_symbol_defs,rules,rev_rules =
    File.fold_tab filename (Idle,StringMap.empty,StringMap.empty,[],[]) (fun (status,symbol_defs,rev_symbol_defs,rules,rev_rules) -> function
        ["@symbols"] -> Symbols,symbol_defs,rev_symbol_defs,rules,rev_rules
      | ["@rev_symbols"] -> RevSymbols,symbol_defs,rev_symbol_defs,rules,rev_rules
      | ["@rules"] -> Rules,symbol_defs,rev_symbol_defs,rules,rev_rules
      | ["@rev_rules"] -> RevRules,symbol_defs,rev_symbol_defs,rules,rev_rules
      | [key;vals] ->
           (match status with
             Symbols -> status, StringMap.add symbol_defs key (Xstring.split " " vals), rev_symbol_defs, rules, rev_rules
           | RevSymbols -> status, symbol_defs, StringMap.add rev_symbol_defs key (Xstring.split " " vals), rules, rev_rules
           | _ -> failwith ("Fonetics.load_rules status 1: " ^ key ^ "\t" ^ vals))
      | [lang;v;r;s] ->
           (match status with
             Rules -> status, symbol_defs, rev_symbol_defs, {pset=v; pfind=r; psuf=s; plang=lang} :: rules, rev_rules
           | RevRules -> status, symbol_defs, rev_symbol_defs, rules, {pset=r; pfind=v; psuf=s; plang=lang} :: rev_rules
           | _ -> failwith "Fonetics.load_rules: status 2")
      | line -> failwith ("load_rules: " ^ (String.concat "\t" line))) in
  if status <> Rules && status <> RevRules then failwith "Fonetics.load_rules: status 3" else
  symbol_defs, rev_symbol_defs, rules, rev_rules

let prepare_rules symbol_defs rules =
  let rules = List.flatten (Xlist.rev_map rules (fun r ->
    let suf = Xunicode.utf8_chars_of_utf8_string r.psuf in
    let suf = Xlist.map suf (fun s ->
      try StringMap.find symbol_defs s with Not_found -> [s]) in
    Xlist.rev_map (Xlist.multiply_list suf) (fun l ->
      {r with psuf=String.concat "" l}))) in
  CharTree.create rules

let select_rules lang rules =
  Xlist.fold rules [] (fun rules r ->
    if r.plang = lang then r :: rules else rules)

let deselect_rules lang rules =
  Xlist.fold rules [] (fun rules r ->
    if r.plang <> lang || String.get r.pset 0 = '{' || String.get r.pfind 0 = '{' then r :: rules else rules)

let rules, rev_rules, core_rules, core_rev_rules =
  let symbol_defs,rev_symbol_defs,rules,rev_rules = load_rules "data/fonetics.dic" in
  let core_rules = select_rules "core" rules in
  let core_rev_rules = select_rules "core" rev_rules in
  prepare_rules symbol_defs rules,
  prepare_rules rev_symbol_defs rev_rules,
  prepare_rules symbol_defs core_rules,
  prepare_rules rev_symbol_defs core_rev_rules

let sufs = [
  ["ω"; "iκ"; "ρ"; "δ"; "λ"; "i"](*; "zi"*);
  ["γ"; "eά"; "owieε"; "β"]; ["ε"; ""];
  ["α"; "ά"; "’"; "’eά"; "iβ"; "g"];
  ["e"; "y"; "è"; "a"; "h"; "u"; "n"; "é"; "cieε"]]

let make_key s =
  match Xunicode.utf8_chars_of_utf8_string s with
    ["ʒ"] -> "d"
  | ["ř"] -> "r"
  | [c] -> c
  | ["d";"j"] -> "dj"
  | ["d";"ʲ"] -> "dj"
  | ["t";"j"] -> "tj"
  | ["t";"ʲ"] -> "tj"
  | [c;"′"] -> c
  | [c;"j"] -> c
  | [c;"ʲ"] -> c
  | [c;"ʲ";"j"] -> c
  | [c;"′";"j"] -> c
  | [] -> ""
  | "{" :: c :: "}" :: _ -> "{" ^ c
  | "{" :: c :: d :: "}" :: _ -> "{" ^ c ^ d
  | _ -> (*failwith ("make_key: '" ^ s ^ "'")*) s

let latex_escape_char = function
    "′" -> "$'$"
  | "ʲ" -> "\\textipa{\\super{j}}"
  | "ʒ" -> "\\textipa{Z}"
  | "ǯ" -> "\\textipa{\\v{Z}}"
  | "{" -> "\\{"
  | "}" -> "\\}"
  | "ε" -> "$\\varepsilon$"
  | c -> c

let latex_escape_utf8_string s =
  String.concat "" (Xlist.map (Xunicode.utf8_chars_of_utf8_string s) latex_escape_char)

let latex_of_rule rule =
  latex_escape_utf8_string rule.pset ^ " $\\leftarrow$ " ^ latex_escape_utf8_string rule.pfind
  (* String.concat "" (Xlist.map (Xunicode.utf8_chars_of_utf8_string rule.pset) latex_escape_char) ^ " $\\leftarrow$ " ^ rule.pfind *)

let latex_of_rev_rule rule =
  latex_escape_utf8_string rule.pfind ^ " $\\rightarrow$ " ^ latex_escape_utf8_string rule.pset

let is_excluded r =
  match r.pset, r.pfind, r.psuf with
    "r", "r", "zi" -> true
  | "mar", "mar", "z" -> true
  | "m′er", "mier", "z" -> true
  | "n′e", "nie", "i" -> true
  | _ -> false


let latex_of_core_rules filename =
  let symbol_defs,rev_symbol_defs,rules,rev_rules =  load_rules filename in
  let core_rules = select_rules "core" rules in
  let map =  Xlist.fold core_rules StringMap.empty (fun map rule ->
    if is_excluded rule then map else (
    (* print_endline (latex_of_rule rule); *)
    let key = make_key rule.pset in
    let map2 = try StringMap.find map key with Not_found -> StringMap.empty in
    if not (Xlist.mem (List.flatten sufs) rule.psuf) then print_endline ("latex_of_rules 1: " ^ rule.psuf);
    let map2 = StringMap.add_inc map2 rule.psuf rule (fun rule2 -> print_endline(*failwith*) ("latex_of_rules 2: " ^ key); rule) in
    StringMap.add map key map2)) in
  Xlist.iter sufs (fun sufs ->
    print_endline ("\\begin{longtable}{" ^ String.concat "|" (Xlist.map sufs (fun _ -> "l")) ^ "}");
    StringMap.iter map (fun _ map2 ->
      let line = Xlist.map sufs (fun suf ->
        try latex_of_rule (StringMap.find map2 suf) with Not_found -> "") in
      if String.concat "" line <> "" then
        print_endline (String.concat " & " line ^ "\\\\"));
    print_endline "\\end{longtable}\n"
        )

let latex_of_non_core_rules filename =
  let symbol_defs,rev_symbol_defs,rules,rev_rules =  load_rules filename in
  let non_core_rules = deselect_rules "core" rules in
  let map =  Xlist.fold non_core_rules StringMap.empty (fun map rule ->
    if is_excluded rule then map else (
    let key = make_key rule.pset in
    let map2 = try StringMap.find map key with Not_found -> StringMap.empty in
    let map2 = StringMap.add_inc map2 rule.psuf [rule] (fun rules -> rule :: rules) in
    StringMap.add map key map2)) in
    print_endline ("\\begin{longtable}{lp{10cm}}");
    StringMap.iter map (fun key map2 ->
      StringMap.iter map2 (fun _ rules ->
        Xlist.iter rules (fun rule ->
          let suf = Xunicode.utf8_chars_of_utf8_string rule.psuf in
          let suf = Xlist.map suf (fun s ->
            try StringMap.find symbol_defs s with Not_found -> [s]) in
          let suf = String.concat " " (List.sort compare (Xlist.rev_map (Xlist.multiply_list suf) (String.concat ""))) in
          let suf = latex_escape_utf8_string suf in
          printf "%s & %s\\\\\n" (latex_of_rule rule) suf)));
    print_endline "\\end{longtable}\n"

let latex_of_non_core_rev_rules filename =
  let symbol_defs,rev_symbol_defs,rules,rev_rules =  load_rules filename in
  let non_core_rules = deselect_rules "core" rev_rules in
  let map =  Xlist.fold non_core_rules StringMap.empty (fun map rule ->
    if is_excluded rule then map else (
    let key = make_key rule.pset in
    let map2 = try StringMap.find map key with Not_found -> StringMap.empty in
    let map2 = StringMap.add_inc map2 rule.psuf [rule] (fun rules -> rule :: rules) in
    StringMap.add map key map2)) in
    print_endline ("\\begin{longtable}{lp{10cm}}");
    StringMap.iter map (fun key map2 ->
      StringMap.iter map2 (fun _ rules ->
        Xlist.iter rules (fun rule ->
          let suf = Xunicode.utf8_chars_of_utf8_string rule.psuf in
          let suf = Xlist.map suf (fun s ->
            try StringMap.find rev_symbol_defs s with Not_found -> [s]) in
          let suf = String.concat " " (List.sort compare (Xlist.rev_map (Xlist.multiply_list suf) (String.concat ""))) in
          let suf = latex_escape_utf8_string suf in
          printf "%s & %s\\\\\n" (latex_of_rev_rule rule) suf)));
    print_endline "\\end{longtable}\n"


let rec translate_rec closure found found_maping rules s =
  if s = "ε" then [List.rev found,List.rev found_maping] else
  let l = CharTree.find rules s in
  (* Xlist.iter l (fun (t,r) ->
      printf "s=%s t=%s set=%s find=%s suf=%s\n%!" s t r.set r.find r.suf); *)
  let l = if l = [] && closure then
    let c,s = Xunicode.first_utf8_char_of_utf8_string s in
    [s,{pfind=c; pset=c; psuf=""; plang=""}] else l in
  List.flatten (Xlist.rev_map l (fun (t,r) ->
    translate_rec closure (r.pset :: found) (r(*r.pfind,r.pset*) :: found_maping) rules (r.psuf ^ t)))

let translate closure rules s =
  (* printf "translate 1: %s\n%!" s; *)
  let ll = translate_rec closure [] [] rules (s ^ "ε") in
  Xlist.rev_map ll (fun (phon,mapping) -> {phon=String.concat "" phon; mapping=mapping})

exception NotFound of string * string
exception NotEqual of string * string * string
exception MulipleSolutions of string * string * string list

let translate_simple closure rules s =
  let ll = translate_rec closure [] [] rules (s ^ "ε") in
  if ll = [] then raise (NotFound(s,"")) else
  Xlist.rev_map ll (fun (phon,mapping) -> String.concat "" phon)

let print_phon p = print_endline (string_of_phon p)

(*let _ = translate rules "blafickie"
let _ = translate rules "blafiacki"
let _ = translate rules "dudzia"*)
(*let _ = Xlist.iter (translate true rules "rzódża") print_phon
let _ = Xlist.iter (translate true rules "Mia") print_phon
let _ = Xlist.iter (translate true rules "mia") print_phon
let _ = Xlist.iter (translate true rules "łódź") print_phon
let _ = Xlist.iter (translate true rules "Łódź") print_phon
let _ = Xlist.iter (translate true rules "PTTK-owsko") print_phon*)
(* let _ = Xlist.iter (translate true core_rules "duzi") print_phon *)
(* let _ = translate true rules "izolował" *)

let translate_and_check closure rules rev_rules orth =
  let l = translate closure rules orth in
  Xlist.iter l (fun s ->
    let y = translate closure rev_rules s.phon in
    let y = Xlist.map y (fun s -> s.phon) in
    match StringSet.to_list (StringSet.of_list y) with
      [] -> raise (NotFound(orth,s.phon))
    | [t] -> if t <> orth then raise (NotEqual(orth,s.phon,t))
    | l -> raise (MulipleSolutions(orth,s.phon,l)))


(*let translate_single closure rules x =
  let y = translate closure rev_rules x in
  let y = Xlist.map y (fun s -> s.phon) in
  match StringSet.to_list (StringSet.of_list y) with
    [] -> raise (NotFound(x,""))
  | [t] -> t
  | l ->
     Printf.printf "%s  %s\n" x (String.concat " " l);
     raise (MulipleSolutions(x,"",l))

let rec rev_translate_rec x s = function
    [] -> x,s,[]
  | (_,"") :: m -> rev_translate_rec x s m
  | (a,b) :: m ->
      if Xstring.check_prefix b s then rev_translate_rec (x^a) (Xstring.cut_prefix b s) m
      else x,s,m

let rev_translate closure rev_rules s m =
  let x,s,_ = rev_translate_rec "" s m in
  if s = "" then x else
  x ^ (translate_single closure rev_rules s)*)

(*let rev_translate2 closure rev_rules s m =
  let x,s,_ = rev_translate_rec "" s m in
  if s = "" then [x] else
  let l = translate closure rev_rules s in
  if l = [] then raise (NotFound(s,"")) else
  Xlist.rev_map l (fun y -> x ^ y.phon)*)

let rec get_short_stem x s = function
    [] -> if s = "" then x else failwith "get_short_stem"
  | r :: m ->
      if Xstring.check_prefix r.pset s then get_short_stem (x^r.pfind) (Xstring.cut_prefix r.pset s) m
      else x

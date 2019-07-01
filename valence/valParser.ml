(*
 *  ENIAMvalence is a library that assigns tokens with lexicosemantic information.
 *  Copyright (C) 2017-2018 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
 *  Copyright (C) 2017-2018 Institute of Informatics, University of Warsaw
 *  Copyright (C) 2017-2018 SELIDOR - T. Puza, Ł. Wasilewski Sp.J.
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
(*open LCGtypes*)
open LCGlexiconTypes
open CategoriesPL
open WalTypes

exception ParseError of string * string * int

(*let rec get_first n = function
    [] -> []
  | s :: l -> if n = 0 then [] else s :: (get_first (n-1) l)

let print_prefix n l =
  print_endline (String.concat " " (get_first n l))*)

let remove_comments line =
  try
    let n = String.index line '#' in
    String.sub line 0 n
  with Not_found -> line

let rec parse_param_names_rec i0 rev = function
    (i,"@ROLE_NAMES") :: tokens -> i, List.rev rev, (i,"@ROLE_NAMES") :: tokens
  | (i,"@SELPREF_NAMES") :: tokens -> i, List.rev rev, (i,"@SELPREF_NAMES") :: tokens
  | (i,"@LEXICON") :: tokens -> i, List.rev rev, (i,"@LEXICON") :: tokens
  | (i,t) :: tokens -> parse_param_names_rec i0 ((i,t) :: rev) tokens
  | [] -> raise (ParseError("parse_param_names_rec", "unexpexted end of input", i0))

let parse_param_names i0 = function
    (i,"@PARAM_NAMES") :: tokens -> parse_param_names_rec i [] tokens
  | (i,s) :: _ -> raise (ParseError("parse_param_names", "'@PARAM_NAMES' expected while '" ^ s ^ "' found", i))
  | [] -> raise (ParseError("parse_param_names", "unexpexted end of input", i0))

let rec parse_selpref_names_rec i0 rev = function
    (i,"@ROLE_NAMES") :: tokens -> i, List.rev rev, (i,"@ROLE_NAMES") :: tokens
  | (i,"@LEXICON") :: tokens -> i, List.rev rev, (i,"@LEXICON") :: tokens
  | (i,t) :: tokens -> parse_selpref_names_rec i0 ((i,t) :: rev) tokens
  | [] -> raise (ParseError("parse_selpref_names_rec", "unexpexted end of input", i0))

let parse_selpref_names i0 = function
    (i,"@SELPREF_NAMES") :: tokens -> parse_selpref_names_rec i [] tokens
  | (i,s) :: _ -> raise (ParseError("parse_selpref_names", "'@SELPREF_NAMES' expected while '" ^ s ^ "' found", i))
  | [] -> raise (ParseError("parse_selpref_names", "unexpexted end of input", i0))

let rec parse_role_names_rec i0 rev = function
    (i,"@LEXICON") :: tokens -> i, List.rev rev, (i,"@LEXICON") :: tokens
  | (i,t) :: tokens -> parse_role_names_rec i0 ((i,t) :: rev) tokens
  | [] -> raise (ParseError("parse_role_names_rec", "unexpexted end of input", i0))

let parse_role_names i0 = function
    (i,"@ROLE_NAMES") :: tokens -> parse_role_names_rec i [] tokens
  | (i,"@LEXICON") :: tokens -> i, [], (i,"@LEXICON") :: tokens
  | (i,s) :: _ -> raise (ParseError("parse_role_names", "'@ROLE_NAMES' expected while '" ^ s ^ "' found", i))
  | [] -> raise (ParseError("parse_role_names", "unexpexted end of input", i0))

let rec split_semic i0 found rev = function
    (i1,"lemma") :: (i2,"=") :: (i3,";") :: l -> split_semic (if rev = [] then i1 else i0) found ((i1,";") :: (i2,"=") :: (i3,"lemma") :: rev) l
  | (i,";") :: l -> split_semic i ((i0, List.rev rev) :: found) [] l
  | (i,s) :: l -> split_semic (if rev = [] then i else i0) found ((i,s) :: rev) l
  | [] -> if rev = [] then List.rev found else List.rev ((i0, List.rev rev) :: found)

let rec split_comma i0 found rev = function
    (i1,"lemma") :: (i2,"=") :: (i3,",") :: l -> split_comma (if rev = [] then i1 else i0) found ((i1,",") :: (i2,"=") :: (i3,"lemma") :: rev) l
  | (i,",") :: l -> split_comma i ((i0, List.rev rev) :: found) [] l
  | (i,s) :: l -> split_comma (if rev = [] then i else i0) found ((i,s) :: rev) l
  | [] -> if rev = [] then List.rev found else List.rev ((i0, List.rev rev) :: found)

let rec split_star i0 found rev = function
    (i,"*") :: l -> split_star i ((i0, List.rev rev) :: found) [] l
  | (i,s) :: l -> split_star (if rev = [] then i else i0) found ((i,s) :: rev) l
  | [] -> if rev = [] then List.rev found else List.rev ((i0, List.rev rev) :: found)

let rec split_plus i0 found rev = function
    (i,"+") :: l -> split_plus i ((i0, List.rev rev) :: found) [] l
  | (i,s) :: l -> split_plus (if rev = [] then i else i0) found ((i,s) :: rev) l
  | [] -> if rev = [] then List.rev found else List.rev ((i0, List.rev rev) :: found)

let catch_selector_of_string i proc s =
  try selector_of_string s
  with _ -> raise (ParseError(proc, "unknown selector: " ^ s, i))

let match_selectors = function
    i0,(i,s) :: l -> i,catch_selector_of_string i "match_selectors" s,l
  | i0,[] -> raise (ParseError("match_selectors", "empty", i0))

let match_relation = function
  (* cat,"=" :: "=" :: l -> cat,StrictEq,l *)
  | i,cat,(_,"!") :: (_,"=") :: l -> i,cat,Neq,l
  | i,cat,(_,"=") :: l -> i,cat,Eq,l
  | _,cat,(i,s) :: l -> raise (ParseError("match_relation", "relation symbol not found: " ^ String.concat " " (s :: Xlist.map l snd), i))
  | i,cat,[] -> raise (ParseError("match_relation", "empty", i))

let rec split_mid i0 rev = function
    (i1,s) :: (i2,"|") :: l -> split_mid i2 ((i1,s) :: rev) l
  | (i1,s1) :: (i2,s2) :: (i3,"|") :: l -> split_mid i3 ((i1,s1^" "^s2) :: rev) l
  | (i1,s1) :: (i2,s2) :: (i3,s3) :: (i4,"|") :: l -> split_mid i4 ((i1,s1^" "^s2^" "^s3) :: rev) l
  | (i1,s1) :: (i2,s2) :: (i3,s3) :: (i4,s4) :: (i5,"|") :: l -> split_mid i5 ((i1,s1^" "^s2^" "^s3^" "^s4) :: rev) l
  | [i,s] -> List.rev ((i,s) :: rev)
  | [i1,s1;i2,s2] -> List.rev ((i1,s1^" "^s2) :: rev)
  | [i1,s1;i2,s2;i3,s3] -> List.rev ((i1,s1^" "^s2^" "^s3) :: rev)
  | [i1,s1;i2,s2;i3,s3;i4,s4] -> List.rev ((i1,s1^" "^s2^" "^s3^" "^s4) :: rev)
  | [] -> raise (ParseError("split_mid", "empty", i0))
  | (i,s) :: l -> raise (ParseError("split_mid", "delimiter not found: " ^ String.concat " " (s :: Xlist.map l snd), i))

let rec check_value i0 selector l =
  let vals = try SelectorMap.find selector_values selector
    with Not_found -> raise (ParseError("check_value", "invalid selector: " ^ string_of_selector selector, i0)) in
  if vals = [] then () else
    Xlist.iter l (fun (i,s) ->
        if not (Xlist.mem vals s) then
          raise (ParseError("check_value", "invalid selector: " ^ string_of_selector selector ^ "=" ^ s, i)));
  Xlist.map l snd

let match_value = function
    i,cat,rel,[s] -> {sel=cat;rel=rel;values=check_value i cat [s]}
  | i,cat,rel,[] -> raise (ParseError("match_value", "empty", i))
  | i,cat,rel,l -> {sel=cat;rel=rel;values=check_value i cat (split_mid i [] l)}

let parse_selectors i0 l =
  (* print_endline s; *)
  (* let l = Xlist.map (Str.full_split (Str.regexp "|\\|,\\|=\\|!") s) (function
        Str.Text s -> s
      | Str.Delim s -> s) in *)
  let ll = split_comma i0 [] [] l in
  let l = Xlist.rev_map ll match_selectors in
  let l = Xlist.rev_map l match_relation in
  let l = Xlist.rev_map l match_value in
  l

let manage_lemmata = function
    (i1,"lemma") :: (i2,"=") :: (i3,":") :: (i4,",") :: tokens -> [i1,"lemma";i2,"=";i3,":";i4,","],tokens
  | (i1,"lemma") :: (i2,"=") :: (i3,":") :: (i4,s) :: (i5,",") :: tokens -> [i1,"lemma";i2,"=";i3,":"^s;i5,","],tokens
  | (i1,"lemma") :: (i2,"=") :: (i3,"<") :: (i4,"/") :: (i5,s) :: (i6,",") :: tokens -> [i1,"lemma";i2,"=";i3,"</"^s;i6,","],tokens
  | tokens -> [],tokens

let parse_case i0 = function
      "nom" -> Case "nom"
    | "gen" -> Case "gen"
    | "dat" -> Case "dat"
    | "acc" -> Case "acc"
    | "inst" -> Case "inst"
    | "loc" -> Case "loc"
    | "voc" -> Case "voc"
    | "str" -> Str
    | "pred" -> Case "pred"
    | "part" -> Part
    | "postp" -> Case "postp"
    | "agr" -> CaseAgr
    | "nomagr" -> NomAgr
    | "_" -> CaseUndef
    | s -> raise (ParseError("parse_case", "unknown value: '"^s^"'", i0))

let parse_grad i0 = function
      "pos" -> Grad "pos"
    | "com" -> Grad "com"
    | "sup" -> Grad "sup"
    | "agr" -> GradAgr
    | "_" -> GradUndef
    | s -> raise (ParseError("parse_grad", "unknown value: '"^s^"'", i0))

(* let parse_psem i0 = function
      "sem" -> Psem
    | "nosem" -> Pnosem
    | s -> raise (ParseError("parse_psem", "unknown value: '"^s^"'", i0)) *)

let parse_ctype = function
    "int" -> Int
  | "rel" -> Rel
  | "_" -> CompTypeUndef
  | s -> failwith ("parse_ctype: " ^ s)

let parse_comp = function
    | "gdy" -> Gdy (* adv; gdyby: qub comp *)
    | "żeby2" -> Zeby
    | "_" -> CompUndef
    | s -> Comp s
(*    | "co" -> Comp "co" (* subst qub prep comp *)
    | "kto" -> Comp "kto" (* subst *)
    | "ile" -> Comp "ile" (* num adv *)
    | "jaki" -> Comp "jaki" (* adj *)
    | "który" -> Comp "który" (* adj *)
    | "czyj" -> Comp "czyj" (* adj *)
    | "jak" -> Comp "jak" (* prep conj adv *)
    | "kiedy" -> Comp "kiedy" (* comp adv *)
    | "gdzie" -> Comp "gdzie" (* qub adv *)
    | "odkąd" -> Comp "odkąd" (* adv *)
    | "skąd" -> Comp "skąd" (* adv *)
    | "dokąd" -> Comp "dokąd" (* adv *)
    | "którędy" -> Comp "którędy" (* adv *)
    | "dlaczego" -> Comp "dlaczego" (* adv *)
    | "czemu" -> Comp "czemu" (* adv *)
    | "czy" -> Comp "czy" (* qub conj *)
    | "jakby" -> Comp "jakby" (* qub comp *)
    | "jakoby" -> Comp "jakoby" (* qub comp *)
    | "dopóki" -> Comp "dopóki" (* comp *)
    | "zanim" -> Comp "zanim" (* comp *)
    | "jeśli" -> Comp "jeśli" (* comp *)
    | "żeby" -> Comp "żeby" (* qub comp *)
    | "że" -> Comp "że" (* qub comp *)
    | "aż" -> Comp "aż" (* qub comp *)
    | "bo" -> Comp "bo" (* qub comp *)
    | "niczym" -> Comp "niczym"
    | "aby" -> Comp "aby"
    | "w tym" -> Comp "w tym"
    | s -> failwith ("parse_comp: " ^ s)*)

let parse_morf i0 params = function
    [_,"null"] -> Null
  | [_,"pro"] -> Pro
  | [_,"np";_,"(";i,case;_,")"] -> NP(parse_case i case)
  | [_,"npa";_,"(";i,case;_,")"] -> NPA(parse_case i case)
  | [_,"infp"] -> InfP AspectUndef
  | [_,"prepnp";_,"(";i2,prep;_,",";i3,case;_,")"] ->
      if not (StringSet.mem params prep) then raise (ParseError("parse_morf", "unknown param: '"^prep^"'", i2)) else
      PrepNP(prep,parse_case i3 case)
  | [_,"prepnp";_,"(";i1,prep1;i2,prep2;_,",";i3,case;_,")"] ->
      if not (StringSet.mem params prep1) || not (StringSet.mem params prep2) then raise (ParseError("parse_morf", "unknown param: '"^prep1^" "^prep2^"'", i1)) else
      PrepNP(prep1^" "^prep2,parse_case i3 case)
  | [_,"prepnp";_,"(";i1,prep1;i2,prep2;i3,prep3;_,",";i4,case;_,")"] ->
      if not (StringSet.mem params prep1) || not (StringSet.mem params prep2) || not (StringSet.mem params prep3) then raise (ParseError("parse_morf", "unknown param: '"^prep1^" "^prep2^" "^prep3^"'", i1)) else
      PrepNP(prep1^" "^prep2^" "^prep3,parse_case i4 case)
  | [_,"prepnp";_,"(";i1,prep1;i2,prep2;i3,prep3;i4,prep4;_,",";i5,case;_,")"] ->
      if not (StringSet.mem params prep1) || not (StringSet.mem params prep2) || not (StringSet.mem params prep3) || not (StringSet.mem params prep4) then raise (ParseError("parse_morf", "unknown param: '"^prep1^" "^prep2^" "^prep3^" "^prep4^"'", i1)) else
      PrepNP(prep1^" "^prep2^" "^prep3^" "^prep4,parse_case i5 case)
(*  | [_,"prepnp";_,"(";i1,psem;_,",";i2,prep;_,",";i3,case;_,")"] ->
      if not (StringSet.mem params prep) then raise (ParseError("parse_morf", "unknown param: '"^prep^"'", i2)) else
      PrepNP((*parse_psem i1 psem,*)prep,parse_case i3 case)
  | [_,"prepnp";_,"(";i1,psem;_,",";_,prep1;i2,prep2;_,",";i3,case;_,")"] ->
      if not (StringSet.mem params prep1) || not (StringSet.mem params prep1) then raise (ParseError("parse_morf", "unknown param: '"^prep1^" "^prep2^"'", i1)) else
      PrepNP((*parse_psem i1 psem,*)prep1^" "^prep2,parse_case i3 case)*)
  | [_,"prepncp";_,"(";i1,psem;_,",";i2,prep;_,",";i3,case;_,",";i4,ctype;_,",";i5,comp;_,")"] ->
      if not (StringSet.mem params prep) then raise (ParseError("parse_morf", "unknown param: '"^prep^"'", i2)) else
      if not (StringSet.mem params comp) then raise (ParseError("parse_morf", "unknown param: '"^comp^"'", i5)) else
      PrepNCP((*parse_psem i1 psem,*)prep,parse_case i3 case,parse_ctype ctype,parse_comp comp)
  | [_,"prepncp";_,"(";i2,prep;_,",";i3,case;_,",";i4,ctype;_,",";i5,comp;_,")"] ->
      if not (StringSet.mem params prep) then raise (ParseError("parse_morf", "unknown param: '"^prep^"'", i2)) else
      if not (StringSet.mem params comp) then raise (ParseError("parse_morf", "unknown param: '"^comp^"'", i5)) else
      PrepNCP(prep,parse_case i3 case,parse_ctype ctype,parse_comp comp)
  | [_,"prepadjp";_,"(";i2,prep;_,",";i3,case;_,")"] ->
      if not (StringSet.mem params prep) then raise (ParseError("parse_morf", "unknown param: '"^prep^"'", i2)) else
      PrepAdjP(prep,parse_case i3 case)
  | [_,"prepfixed";_,"(";i2,prep;_,")"] ->
      if not (StringSet.mem params prep) then raise (ParseError("parse_morf", "unknown param: '"^prep^"'", i2)) else
      PrepFixed(prep)
  | [_,"cp";_,"(";i1,ctype;_,",";i2,comp;_,")"] ->
      if not (StringSet.mem params comp) then raise (ParseError("parse_morf", "unknown param: '"^comp^"'", i2)) else
      CP(parse_ctype ctype,parse_comp comp)
  | [_,"cp";_,"(";i1,ctype;_,",";i2,comp1;i3,comp2;_,")"] ->
      if not (StringSet.mem params comp1) then raise (ParseError("parse_morf", "unknown param: '"^comp1^"'", i2)) else
      if not (StringSet.mem params comp2) then raise (ParseError("parse_morf", "unknown param: '"^comp2^"'", i2)) else
      CP(parse_ctype ctype, Comp (comp1 ^ " " ^ comp2))
  | [_,"adjp";_,"(";i,case;_,")"] -> AdjP(parse_case i case)
  | [_,"adja"] -> AdjA
  | [_,"xp"] -> XP
  | [_,"ip"] -> IP
  | [_,"refl"] -> SimpleLexArg("się",QUB)
  | [_,"advp"] -> AdvP "misc"
  | [_,"advp";_,"(";i,grad;_,")"] -> AdvP grad
  | [_,"padvp"] -> PadvP
  | [_,"admod";_,"(";i,grad;_,")"] -> AdMod(parse_grad i grad)
  | [_,"qubp"] -> Qub
  | [_,"colonp"] -> ColonP
  | [_,"symbolp"] -> SymbolP
  | [_,"fixed"] -> FixedP ""
  | [_,"head"] -> Head
  | [_,"comparp";_,"(";i1,psem;_,",";i2,prep;_,",";i3,case;_,")"] ->
      if not (StringSet.mem params prep) then raise (ParseError("parse_morf", "unknown param: '"^prep^"'", i2)) else
      ComparP((*parse_psem i1 psem,*)prep,parse_case i3 case)
  | [_,"comparp";_,"(";i2,prep;_,",";i3,case;_,")"] ->
      if not (StringSet.mem params prep) then raise (ParseError("parse_morf", "unknown param: '"^prep^"'", i2)) else
      ComparP(prep,parse_case i3 case)
  | [_,"negp"] -> SimpleLexArg("nie",QUB)
  | [_,"inclusion"] -> Inclusion
  | [_,"rp"] -> RP
  | [_,"nump";_,"(";i,case;_,")"] -> NumP(parse_case i case)
  | [_,"interjp"] -> InterjP
  | [] -> raise (ParseError("parse_morf", "empty", i0))
  | l -> raise (ParseError("parse_morf", "unknown: " ^ String.concat " " (Xlist.map l snd), i0))

let parse_catprefs i0 sefprefs tokens =
  if tokens = [] then raise (ParseError("parse_catprefs", "empty", i0)) else
  let ll = split_comma i0 [] [] tokens in
  Xlist.map ll (function
      i0,[] -> raise (ParseError("parse_catprefs", "empty selpref", i0))
    | _,[i,s] -> if not (StringSet.mem sefprefs s) then raise (ParseError("parse_catprefs", "unknown sel pref: '"^s^"'", i)) else s
    | i0,l -> raise (ParseError("parse_catprefs", "bad syntax: " ^ String.concat " " (Xlist.map l snd), i0)))

let rec parse_position2 i0 rev = function
    (_,"}") :: l -> List.rev rev, l
  | (i,s) :: l -> parse_position2 i0 ((i,s) :: rev) l
  | [] -> raise (ParseError("parse_position2", "'}' not found", i0))

let rec parse_position3 i0 rev = function
    [_,"]"] -> List.rev rev
  | (i,"]") :: l -> raise (ParseError("parse_position3", "bad syntax: " ^ String.concat " " (Xlist.map l snd), i))
  | (i,s) :: l -> parse_position3 i0 ((i,s) :: rev) l
  | [] -> raise (ParseError("parse_position3", "']' not found", i0))

let rec parse_position i0 params sefprefs roles p = function
    (_,",") :: l -> parse_position i0 params sefprefs roles p l
  | (i,"subj") :: l -> if p.gf=ARG then parse_position i0 params sefprefs roles {p with gf=SUBJ} l else raise (ParseError("parse_position", "multiple grammar functions", i))
  | (i,"obj") :: l -> if p.gf=ARG then parse_position i0 params sefprefs roles {p with gf=OBJ} l else raise (ParseError("parse_position", "multiple grammar functions", i))
  | (i,"ger") :: l -> if p.gf=ARG then parse_position i0 params sefprefs roles {p with gf=GER} l else raise (ParseError("parse_position", "multiple grammar functions", i))
  | (i,"nger") :: l -> if p.gf=ARG then parse_position i0 params sefprefs roles {p with gf=NGER} l else raise (ParseError("parse_position", "multiple grammar functions", i))
(*   | (i,"core") :: l -> if p.gf=ARG then parse_position i0 params sefprefs roles {p with gf=CORE} l else raise (ParseError("parse_position", "multiple grammar functions", i)) *)
(*   | (i,"pers") :: l -> if p.gf=PERS then parse_position i0 params sefprefs roles {p with gf=OBJ} l else raise (ParseError("parse_position", "multiple grammar functions", i)) *)
  | (i,"controller") :: l -> parse_position i0 params sefprefs roles {p with cr=["1"]} l
  | (i,"controllee") :: l -> parse_position i0 params sefprefs roles {p with ce=["1"]} l
  | (i,"controller2") :: l -> parse_position i0 params sefprefs roles {p with cr=["2"]} l
  | (i,"controllee2") :: l -> parse_position i0 params sefprefs roles {p with ce=["2"]} l
  | (i,"controller3") :: l -> parse_position i0 params sefprefs roles {p with cr=["3"]} l
  | (i,"controllee3") :: l -> parse_position i0 params sefprefs roles {p with ce=["3"]} l
  | (i,"sit") :: l -> parse_position i0 params sefprefs roles {p with node="sit"} l
  | (i,"dot") :: l -> parse_position i0 params sefprefs roles {p with node="dot"} l
  | (i,"local") :: l -> parse_position i0 params sefprefs roles {p with range=Local} l
  | (i,"distant") :: l -> parse_position i0 params sefprefs roles {p with range=Distant} l
  | (i,"core") :: l -> parse_position i0 params sefprefs roles {p with range=Local} l
  | (i,"/") :: l -> parse_position i0 params sefprefs roles {p with dir=Forward_} l
  | (i,"\\") :: l -> parse_position i0 params sefprefs roles {p with dir=Backward_} l
  | (i,"?") :: l -> parse_position i0 params sefprefs roles {p with is_necessary=Multi} l
  | (i,"{") :: l ->
      let morfs,l = parse_position2 i [] l in
      let p =
        match l with
          (i1,":") :: (i2,role) :: (i3,"[") :: l ->
            let p = if StringSet.mem roles role then {p with role=role} else raise (ParseError("parse_position", "unknown role '"^role^"'", i2)) in
            let l = parse_position3 i [] l in
            {p with cat_prefs=parse_catprefs i3 sefprefs l}
        | _ -> raise (ParseError("parse_position", "bad semantic role specification", i)) in
      let morfs = split_plus i [] [] morfs in
      {p with morfs = Xlist.map morfs (fun (i,morf) -> parse_morf i params morf)}
  | (i,s) :: l -> raise (ParseError("parse_position", "unknown token '"^s^"'", i))
  | [] -> raise (ParseError("parse_position", "'{' not found", i0))

let rec find_colon i0 rev = function
    (i1,"lemma") :: (i2,"=") :: (i3,":") :: l -> find_colon i0 ((i1,":") :: (i2,"=") :: (i3,"lemma") :: rev) l
  | (i,":") :: l -> i,List.rev rev, l
  | (i,s) :: l -> find_colon i0 ((i,s) :: rev) l
  | [] -> raise (ParseError("find_colon", "':' not found", i0))

let parse_cat i0 sefprefs = function
    [i,s] -> if not (StringSet.mem sefprefs s) then raise (ParseError("parse_cat", "unknown sel pref: '"^s^"'", i)) else s, ""
  | [i,s;_,"(";_,a1;_,")"] -> if not (StringSet.mem sefprefs s) then raise (ParseError("parse_cat", "unknown sel pref: '"^s^"'", i)) else s, a1
  | [i,s;_,"(";_,a1;_,a2;_,")"] -> if not (StringSet.mem sefprefs s) then raise (ParseError("parse_cat", "unknown sel pref: '"^s^"'", i)) else s, a1 ^ " " ^ a2
  | [i,s;_,"(";_,a1;_,a2;_,a3;_,")"] -> if not (StringSet.mem sefprefs s) then raise (ParseError("parse_cat", "unknown sel pref: '"^s^"'", i)) else s, a1 ^ " " ^ a2 ^ " " ^ a3
  | [i,s;_,"(";_,a1;_,a2;_,a3;_,a4;_,")"] -> if not (StringSet.mem sefprefs s) then raise (ParseError("parse_cat", "unknown sel pref: '"^s^"'", i)) else s, a1 ^ " " ^ a2 ^ " " ^ a3 ^ " " ^ a4
  | [] -> raise (ParseError("parse_cat", "empty", i0))
  | l -> raise (ParseError("parse_cat", "bad syntax: " ^ String.concat " " (Xlist.map l snd), i0))

let parse_entry i0 params sefprefs roles tokens =
  let prefix,tokens = manage_lemmata tokens in
  let i1,selectors,tokens = find_colon i0 [] tokens in
  let i2,cat,schema = find_colon i1 [] tokens in
  let selectors = parse_selectors i1 (prefix @ selectors) in
  let cat,sense = parse_cat i2 sefprefs cat in
  let schema = split_star i2 [] [] schema in
  let schema = Xlist.map schema (fun (i,l) -> parse_position i params sefprefs roles empty_position l) in
  i0, selectors, cat, sense, schema

let string_of_parse_error filename proc s i line =
  Printf.sprintf "Valence dictionary error\nin file %s\nin line %d: %s\n%s: %s" filename i line proc s

let parse_lexicon filename i0 a params sefprefs roles = function
    (i,"@LEXICON") :: tokens ->
    let entries = split_semic i [] [] tokens in
    Xlist.fold entries ([],true) (fun (entries,is_correct) (i,entry) ->
      try (parse_entry i params sefprefs roles entry) :: entries, is_correct
      with ParseError(proc,s,i) ->
        print_endline (string_of_parse_error filename proc s i a.(i-1));
        entries,false)
  | (i,s) :: _ -> raise (ParseError("parse_lexicon", "'@LEXICON' expected while '" ^ s ^ "' found", i))
  | [] -> raise (ParseError("parse_lexicon", "unexpexted end of input", i0))

let load_lexicon filename =
  let lines = Xstring.split "\n" (File.load_file filename) in
  let a = Array.of_list lines in
  let lines,no_lines = Xlist.fold lines ([],1) (fun (lines,i) line -> (i,line) :: lines, i+1) in
  let lines = Xlist.rev_map lines (fun (i,line) -> i, remove_comments line) in
  let tokens = List.flatten (Xlist.rev_map lines (fun (i,line) ->
      Xlist.rev_map (Str.full_split
                       (Str.regexp "\\]\\| \\|\t\\|\r\\|\\?\\|:\\|;\\|&\\|!\\|=\\|}\\|{\\|,\\|\\*\\|/\\|\\+\\|)\\|(\\||\\|\\[\\|\\") line) (function
            Str.Text s -> i,s
          | Str.Delim s -> i,s))) in
  let tokens = Xlist.fold tokens [] (fun tokens -> function
        _," " -> tokens
      | _,"\t" -> tokens
      | _,"\r" -> tokens
      | i,t -> (i,t) :: tokens) in
  try
    let i,param_names,tokens = parse_param_names 1 tokens in
    let params = Xlist.fold param_names (StringSet.singleton "_") (fun params (_,param) -> StringSet.add params param) in
    let i,selpref_names,tokens = parse_selpref_names i tokens in
    let sefprefs = Xlist.fold selpref_names StringSet.empty (fun sefprefs (_,sefpref) -> StringSet.add sefprefs sefpref) in
    let i,role_names,tokens = parse_role_names i tokens in
    let roles = Xlist.fold role_names (StringSet.singleton "ADJUNCT") (fun roles (_,role) -> StringSet.add roles role) in
    let lexicon,is_correct = parse_lexicon filename i a params sefprefs roles tokens in
    if is_correct then List.rev lexicon else exit 0
  with ParseError(proc,s,i) ->
    print_endline (string_of_parse_error filename proc s i a.(i-1));
    exit 0

let rec extract_selector i s rev = function
    {rel=Eq} as c :: l -> if c.sel = s then c.values, List.rev rev @ l else extract_selector i s (c :: rev) l
  | c :: l -> if c.sel = s then failwith "extract_selector 1" else extract_selector i s (c :: rev) l
  | [] -> failwith ("extract_selector 2: " ^ string_of_selector s ^ " in line " ^ string_of_int i)

let rec check_extract_selector i s rev = function
    {rel=Eq} as c :: l -> if c.sel = s then c.values, List.rev rev @ l else check_extract_selector i s (c :: rev) l
  | c :: l -> if c.sel = s then failwith "check_extract_selector 1" else check_extract_selector i s (c :: rev) l
  | [] -> raise Not_found

let load_include_lemmata filename i data_path = function
    [filename2] ->
      (try
(*         print_endline filename; *)
        let lines = List.flatten (File.load_tab (data_path ^ "/" ^ filename2 ^ ".tab") (function x :: _ -> [x] | [] -> [])) in
        List.flatten (Xlist.rev_map lines (Xstring.split "|"))
      with Unix.Unix_error(Unix.ENOENT, "stat", filename2) ->
        print_endline (string_of_parse_error filename "load_include_lemmata" ("File " ^ filename2 ^ " not found") i "");
        exit 0)
  | l ->
        print_endline (string_of_parse_error filename "load_include_lemmata" ("Invalid filename: " ^ String.concat "|" l) i "");
        exit 0

let load_connected entries pros path filename =
  let l = load_lexicon filename in
  Xlist.fold l (entries,pros) (fun (entries,pros) (i, selectors, cat, sense, schema) ->
      let poss,selectors = extract_selector i Pos2 [] selectors in
      let lemmata,selectors =
        try
          let filename2,selectors = check_extract_selector i IncludeLemmata [] selectors in
          load_include_lemmata filename i path filename2,selectors
        with Not_found -> (try check_extract_selector i Lemma [] selectors with Not_found -> [""],selectors) in
      (* let snode,selectors = extract_selector i SNode [] selectors in *)
      (* if selectors <> [] then failwith "load_connected: ni" else *)
      let pro_lemma,selectors = try check_extract_selector i ProLemma [] selectors with Not_found -> [],selectors in
      if pro_lemma <> [] && lemmata <> [""] then failwith "load_connected: both lemma and pro-lemma defined" else
      if pro_lemma = [] then
        Xlist.fold poss entries (fun entries pos ->
          Xlist.fold lemmata entries (fun entries lemma ->
            let sense = if sense = "" then lemma else sense in
(*           Printf.printf "load_connected: %s %s\n" cat sense; *)
            let entry = selectors,sense,cat,(*snode,*)schema in
            Entries.add_inc entries pos lemma entry)),pros else
      if Xlist.size pro_lemma <> 1 then failwith ("load_connected: invalid pro-lemma " ^ String.concat "|" pro_lemma) else
      if Xlist.size poss <> 1 then failwith ("load_connected: invalid pos2 " ^ String.concat "|" poss) else
      entries,(List.hd poss,List.hd pro_lemma,selectors,sense,cat,schema) :: pros)

let valence = ref (StringMap.empty,[])

(* let _ =
  load_lexicon user_valence_filename *)

let initialize () =
(*   print_endline "initialize"; *)
  valence := load_connected Entries.empty [] data_path user_valence_filename;
  valence := Xlist.fold !SubsyntaxTypes.theories !valence (fun (entries,pros) theory ->
(*     print_endline theory; *)
    load_connected entries pros (SubsyntaxTypes.theories_path ^ theory) (SubsyntaxTypes.theories_path ^ theory ^ "/valence.dic"));
  valence := Xlist.fold !SubsyntaxTypes.user_theories !valence (fun (entries,pros) theory ->
(*     print_endline theory; *)
    load_connected entries pros (SubsyntaxTypes.user_theories_path ^ theory) (SubsyntaxTypes.user_theories_path ^ theory ^ "/valence.dic"));
  ()

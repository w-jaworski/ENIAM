(*
 *  LCGlexicon is a library that provides LCG lexicon form Polish
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
open LCGtypes
open LCGlexiconTypes
open CategoriesPL

exception ParseError of string * string * int

let rec get_first n = function
    [] -> []
  | s :: l -> if n = 0 then [] else s :: (get_first (n-1) l)

let print_prefix n l =
  print_endline (String.concat " " (get_first n l))

let remove_comments line =
  try
    let n = String.index line '#' in
    String.sub line 0 n
  with Not_found -> line

let rec parse_phrase_names_rec i0 rev = function
    (i,"@WEIGHTS") :: tokens -> i, List.rev rev, (i,"@WEIGHTS") :: tokens
  | (i,"@LEXICON") :: tokens -> i, List.rev rev, (i,"@LEXICON") :: tokens
  | (i,t) :: tokens -> parse_phrase_names_rec i0 ((i,t) :: rev) tokens
  | [] -> raise (ParseError("parse_phrase_names_rec", "unexpexted end of input", i0))

let parse_phrase_names i0 = function
    (i,"@PHRASE_NAMES") :: tokens -> parse_phrase_names_rec i [] tokens
  | (i,s) :: _ -> raise (ParseError("parse_phrase_names", "'@PHRASE_NAMES' expected while '" ^ s ^ "' found", i))
  | [] -> raise (ParseError("parse_phrase_names", "unexpexted end of input", i0))

let rec parse_weights_rec i0 weights = function
    (i,"@LEXICON") :: tokens -> i, weights, (i,"@LEXICON") :: tokens
  | (_,w) :: (_,"=") :: (i,n) :: tokens -> parse_weights_rec i (StringMap.add weights w (float_of_string n)) tokens
  | (i,s) :: _ -> raise (ParseError("parse_weights_rec", "unexpexted token '" ^ s ^ "'", i))
  | [] -> raise (ParseError("parse_weights_rec", "unexpexted end of input", i0))

let parse_weights i0 = function
    (i,"@WEIGHTS") :: tokens -> parse_weights_rec i StringMap.empty tokens
  | (i,"@LEXICON") :: tokens -> i, StringMap.empty, (i,"@LEXICON") :: tokens
  | (i,s) :: _ -> raise (ParseError("parse_weights", "'@WEIGHTS' expected while '" ^ s ^ "' found", i))
  | [] -> raise (ParseError("parse_weights", "unexpexted end of input", i0))

let rec split_semic i0 found rev = function
    (i1,"lemma") :: (i2,"=") :: (i3,";") :: l -> split_semic (if rev = [] then i1 else i0) found ((i1,";") :: (i2,"=") :: (i3,"lemma") :: rev) l
  | (i,";") :: l -> split_semic i ((i0, List.rev rev) :: found) [] l
  | (i,s) :: l -> split_semic (if rev = [] then i else i0) found ((i,s) :: rev) l
  | [] -> if rev = [] then List.rev found else List.rev ((i0, List.rev rev) :: found)

let rec split_colon found rev = function
    (i1,"lemma") :: (i2,"=") :: (i3,":") :: l -> split_colon found ((i1,":") :: (i2,"=") :: (i3,"lemma") :: rev) l
  | (_,":") :: l -> split_colon (List.rev rev :: found) [] l
  | (i,s) :: l -> split_colon found ((i,s) :: rev) l
  | [] -> if rev = [] then List.rev found else List.rev ((List.rev rev) :: found)

let rec split_comma i0 found rev = function
    (i1,"lemma") :: (i2,"=") :: (i3,",") :: l -> split_comma (if rev = [] then i1 else i0) found ((i1,",") :: (i2,"=") :: (i3,"lemma") :: rev) l
  | (i,",") :: l -> split_comma i ((i0, List.rev rev) :: found) [] l
  | (i,s) :: l -> split_comma (if rev = [] then i else i0) found ((i,s) :: rev) l
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
    [i,s] -> List.rev ((i,s) :: rev)
  | (i1,s) :: (i2,"|") :: (i3,"|") :: l -> raise (ParseError("split_mid", "duplicated delimeter found", i2))
  | (i1,s) :: (i2,"|") :: l -> split_mid i2 ((i1,s) :: rev) l
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
    i,cat,rel,[s] -> {sel=cat; rel=rel; values=check_value i cat [s]}
  | i,cat,rel,[] -> raise (ParseError("match_value", "empty", i))
  | i,cat,rel,l -> {sel=cat; rel=rel; values=check_value i cat (split_mid i [] l)}

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


type syntax =
    A of string
  | B of internal_grammar_symbol
  | C of grammar_symbol
  | D of direction * grammar_symbol
  | E of (direction * grammar_symbol) list

let make_atoms phrase_names =
  SelectorMap.fold selector_values (StringSet.of_list (Xlist.rev_map phrase_names snd)) (fun atoms _ l ->
      Xlist.fold l atoms StringSet.add)

let rec find_right_bracket i0 rev = function
    (_,"]") :: l -> List.rev rev, l
  | (i,s) :: l -> find_right_bracket i ((i,s) :: rev) l
  | [] -> raise (ParseError("find_right_bracket", "empty", i0))

let operators = StringSet.of_list [
    "*"; "+"; "/"; "|"; "\\"; "("; ")"; ","; "{"; "}"; "?"]

let find_internal_grammar_symbols atoms = function
  | i,"T" -> i,B Top
  | i,"1" -> i,C One
  | i,"schema" -> i,D(Both,Tensor[AVar "schema"])
  | i,"local-schema" -> i,D(Both,Tensor[AVar "local-schema"])
  | i,"distant-schema" -> i,D(Both,Tensor[AVar "distant-schema"])
  | i,"adjuncts" -> i,D(Both,Tensor[AVar "adjuncts"])
  | i,s -> if StringSet.mem selector_names s then i,B (AVar s) else
    if StringSet.mem atoms s then i,B (Atom s) else
    if StringSet.mem operators s then i,A s else
      raise (ParseError("find_internal_grammar_symbols", "unknown symbol " ^ s, i))

let rec find_tensor2 rev = function
    (_,B s1) :: (_,A "*") :: (i,B s2) :: l -> find_tensor2 (s1 :: rev) ((i,B s2) :: l)
  | (_,B s1) :: l -> List.rev (s1 :: rev), l
  | (i,t) :: l -> raise (ParseError("find_tensor2", "", i))
  | [] -> failwith "find_tensor2"

let rec find_tensor = function
    (* B s1 :: A "*" :: B s2 :: A "*" :: B s3 :: A "*" :: B s4 :: A "*" :: B s5 :: A "*" :: B s6 :: A "*" :: B s7 :: A "*" :: B s8 :: l -> failwith "find_tensor 1"
  | B s1 :: A "*" :: B s2 :: A "*" :: B s3 :: A "*" :: B s4 :: A "*" :: B s5 :: A "*" :: B s6 :: A "*" :: B s7 :: l -> C (Tensor[s1;s2;s3;s4;s5;s6;s7]) :: find_tensor l
  | B s1 :: A "*" :: B s2 :: A "*" :: B s3 :: A "*" :: B s4 :: A "*" :: B s5 :: A "*" :: B s6 :: l -> C (Tensor[s1;s2;s3;s4;s5;s6]) :: find_tensor l
  | B s1 :: A "*" :: B s2 :: A "*" :: B s3 :: A "*" :: B s4 :: A "*" :: B s5 :: l -> C (Tensor[s1;s2;s3;s4;s5]) :: find_tensor l
  | B s1 :: A "*" :: B s2 :: A "*" :: B s3 :: A "*" :: B s4 :: l -> C (Tensor[s1;s2;s3;s4]) :: find_tensor l
  | B s1 :: A "*" :: B s2 :: A "*" :: B s3 :: l -> C (Tensor[s1;s2;s3]) :: find_tensor l
  | B s1 :: A "*" :: B s2 :: l -> C (Tensor[s1;s2]) :: find_tensor l *)
  | (i,B s1) :: l -> let sl,l = find_tensor2 [] ((i,B s1) :: l) in (i,C (Tensor sl)) :: find_tensor l
  | (i,A "*") :: _ -> raise (ParseError("find_tensor", "unexpected '*'", i))
  | t :: l -> t :: find_tensor l
  | [] -> []

let rec find_plus2 rev = function
    (_,C s1) :: (_,A "+") :: (i,C s2) :: l -> find_plus2 (s1 :: rev) ((i,C s2) :: l)
  | (_,C s1) :: l -> List.rev (s1 :: rev), l
  | (i,t) :: l -> raise (ParseError("find_plus2", "", i))
  | [] -> failwith "find_plus2"

let rec find_plus = function
    (* C s1 :: A "+" :: C s2 :: A "+" :: C s3 :: A "+" :: C s4 :: A "+" :: C s5 :: A "+" :: C s6 :: A "+" :: C s7 :: l -> failwith "find_plus 1"
  | C s1 :: A "+" :: C s2 :: A "+" :: C s3 :: A "+" :: C s4 :: A "+" :: C s5 :: A "+" :: C s6 :: l -> C (Plus[s1;s2;s3;s4;s5;s6]) :: find_plus l
  | C s1 :: A "+" :: C s2 :: A "+" :: C s3 :: A "+" :: C s4 :: A "+" :: C s5 :: l -> C (Plus[s1;s2;s3;s4;s5]) :: find_plus l
  | C s1 :: A "+" :: C s2 :: A "+" :: C s3 :: A "+" :: C s4 :: l -> C (Plus[s1;s2;s3;s4]) :: find_plus l
  | C s1 :: A "+" :: C s2 :: A "+" :: C s3 :: l -> C (Plus[s1;s2;s3]) :: find_plus l *)
  | (i1,C s1) :: (i2,A "+") :: (i3,C s2) :: l -> let sl,l = find_plus2 [] ((i1,C s1) :: (i2,A "+") :: (i3,C s2) :: l) in (i1,C (Plus sl)) :: find_plus l
  | (i,A "+") :: _ -> raise (ParseError("find_plus 2", "unexpected '+'", i))
  | t :: l -> t :: find_plus l
  | [] -> []

let rec find_paren = function
    (_,A "(") :: (i,C s) :: (_,A ")") :: l -> (i,C s) :: find_paren l
  | (i,s) :: l -> (i,s) :: find_paren l
  | [] -> []

let rec find_imp = function
  | (i,C s1) :: (_,A "/") :: (_,C s2) :: l -> (i,C (Imp(s1,Forward,s2))) :: find_imp l
  | (i,C s1) :: (_,A "|") :: (_,C s2) :: l -> (i,C (Imp(s1,Both,s2))) :: find_imp l
  | (i,C s1) :: (_,A "\\") :: (_,C s2) :: l -> (i,C (Imp(s1,Backward,s2))) :: find_imp l
  | (i,s) :: l -> (i,s) :: find_imp l
  | [] -> []

let rec find_maybe = function
  | (i,A "?") :: (_,C s2) :: l -> (i,C (Maybe s2)) :: find_maybe l
  | (i,A "?") :: _ -> raise (ParseError("find_maybe 1", "unexpected '?'", i))
  | (i,s) :: l -> (i,s) :: find_maybe l
  | [] -> []

let rec find_mult_imp = function
  | (i1,A "{") :: (i2,A "/") :: (_,C s2) :: l -> (i1,A "{") :: (i2,D (Forward,s2)) :: find_mult_imp l
  | (i1,A "{") :: (i2,A "|") :: (_,C s2) :: l -> (i1,A "{") :: (i2,D (Both,s2)) :: find_mult_imp l
  | (i1,A "{") :: (i2,A "\\") :: (_,C s2) :: l -> (i1,A "{") :: (i2,D (Backward,s2)) :: find_mult_imp l
  | (i1,A ",") :: (i2,A "/") :: (_,C s2) :: l -> (i1,A ",") :: (i2,D (Forward,s2)) :: find_mult_imp l
  | (i1,A ",") :: (i2,A "|") :: (_,C s2) :: l -> (i1,A ",") :: (i2,D (Both,s2)) :: find_mult_imp l
  | (i1,A ",") :: (i2,A "\\") :: (_,C s2) :: l -> (i1,A ",") :: (i2,D (Backward,s2)) :: find_mult_imp l
  | (i,A "/") :: _ -> raise (ParseError("find_mult_imp 1", "unexpected '/'", i))
  | (i,A "|") :: _ -> raise (ParseError("find_mult_imp 2", "unexpected '|'", i))
  | (i,A "\\") :: _ -> raise (ParseError("find_mult_imp 3", "unexpected '\\'", i))
  | (i,A "(") :: _ -> raise (ParseError("find_mult_imp 4", "unexpected '('", i))
  | (i,A ")") :: _ -> raise (ParseError("find_mult_imp 5", "unexpected ')'", i))
  | (i,s) :: l -> (i,s) :: find_mult_imp l
  | [] -> []

let rec find_mult2 rev = function
    (_,D(s1,t1)) :: (_,A ",") :: (i,D(s2,t2)) :: l -> find_mult2 ((s1,t1) :: rev) ((i,D(s2,t2)) :: l)
  | (_,D(s1,t1)) :: (_,A "}") :: l -> List.rev ((s1,t1) :: rev), l
  | (i,t) :: l -> raise (ParseError("find_mult2", "", i))
  | [] -> failwith "find_mult2"

let rec find_mult = function
    (* A "{" :: D(s1,t1) :: A "," :: D(s2,t2) :: A "," :: D(s3,t3) :: A "," :: D(s4,t4) :: A "," :: D(s5,t5) :: A "," :: D(s6,t6) :: A "," :: D(s7,t7) :: A "," :: D(s8,t8) :: A "," :: D(s9,t9) :: A "," :: D(s10,t10) :: A "," :: D _ :: l -> failwith "find_mult 1: to many elements in { }"
  | A "{" :: D(s1,t1) :: A "," :: D(s2,t2) :: A "," :: D(s3,t3) :: A "," :: D(s4,t4) :: A "," :: D(s5,t5) :: A "," :: D(s6,t6) :: A "," :: D(s7,t7) :: A "," :: D(s8,t8) :: A "," :: D(s9,t9) :: A "," :: D(s10,t10) :: A "}" :: l -> E[s1,t1;s2,t2;s3,t3;s4,t4;s5,t5;s6,t6;s7,t7;s8,t8;s9,t9;s10,t10] :: find_mult l
  | A "{" :: D(s1,t1) :: A "," :: D(s2,t2) :: A "," :: D(s3,t3) :: A "," :: D(s4,t4) :: A "," :: D(s5,t5) :: A "," :: D(s6,t6) :: A "," :: D(s7,t7) :: A "," :: D(s8,t8) :: A "," :: D(s9,t9) :: A "}" :: l -> E[s1,t1;s2,t2;s3,t3;s4,t4;s5,t5;s6,t6;s7,t7;s8,t8;s9,t9] :: find_mult l
  | A "{" :: D(s1,t1) :: A "," :: D(s2,t2) :: A "," :: D(s3,t3) :: A "," :: D(s4,t4) :: A "," :: D(s5,t5) :: A "," :: D(s6,t6) :: A "," :: D(s7,t7) :: A "," :: D(s8,t8) :: A "}" :: l -> E[s1,t1;s2,t2;s3,t3;s4,t4;s5,t5;s6,t6;s7,t7;s8,t8] :: find_mult l
  | A "{" :: D(s1,t1) :: A "," :: D(s2,t2) :: A "," :: D(s3,t3) :: A "," :: D(s4,t4) :: A "," :: D(s5,t5) :: A "," :: D(s6,t6) :: A "," :: D(s7,t7) :: A "}" :: l -> E[s1,t1;s2,t2;s3,t3;s4,t4;s5,t5;s6,t6;s7,t7] :: find_mult l
  | A "{" :: D(s1,t1) :: A "," :: D(s2,t2) :: A "," :: D(s3,t3) :: A "," :: D(s4,t4) :: A "," :: D(s5,t5) :: A "," :: D(s6,t6) :: A "}" :: l -> E[s1,t1;s2,t2;s3,t3;s4,t4;s5,t5;s6,t6] :: find_mult l
  | A "{" :: D(s1,t1) :: A "," :: D(s2,t2) :: A "," :: D(s3,t3) :: A "," :: D(s4,t4) :: A "," :: D(s5,t5) :: A "}" :: l -> E[s1,t1;s2,t2;s3,t3;s4,t4;s5,t5] :: find_mult l
  | A "{" :: D(s1,t1) :: A "," :: D(s2,t2) :: A "," :: D(s3,t3) :: A "," :: D(s4,t4) :: A "}" :: l -> E[s1,t1;s2,t2;s3,t3;s4,t4] :: find_mult l
  | A "{" :: D(s1,t1) :: A "," :: D(s2,t2) :: A "," :: D(s3,t3) :: A "}" :: l -> E[s1,t1;s2,t2;s3,t3] :: find_mult l
  | A "{" :: D(s1,t1) :: A "," :: D(s2,t2) :: A "}" :: l -> E[s1,t1;s2,t2] :: find_mult l *)
  | (_,A "{") :: (i,D(s1,t1)) :: l -> let sl,l = find_mult2 [] ((i,D(s1,t1)) :: l) in (i,E sl) :: find_mult l
  | (i,A "{") :: _ -> raise (ParseError("find_mult 2", "unexpected '{'", i))
  | (i,A "}") :: _ -> raise (ParseError("find_mult 3", "unexpected '}'", i))
  | (i,A ",") :: _ -> raise (ParseError("find_mult 4", "unexpected ','", i))
  | t :: l -> t :: find_mult l
  | [] -> []

let rec apply_mult i0 = function
    (i1,C s) :: (i2,E t) :: l -> apply_mult i2 ((i1,C (ImpSet(s,t))) :: l)
  | [i,C s] -> i,C s
  | _ -> raise (ParseError("apply_mult","",i0))

let parse_syntax i0 atoms l =
  (* print_endline s; *)
  (* let l = Xlist.map (Str.full_split (Str.regexp "?\\|}\\|{\\|,\\|*\\|/\\|+\\|)\\|(\\||\\|\\") s) (function
        Str.Text s -> s
      | Str.Delim s -> s) in *)
  let l = List.rev (Xlist.rev_map l (find_internal_grammar_symbols atoms)) in
  let l = find_tensor l in
  let l = find_plus l in
  let l = find_paren l in
  let l = find_maybe l in
  let l = find_imp l in
  let l = find_paren l in
  let l = find_imp l in
  let l = find_paren l in
  let l = find_imp l in
  let l = find_paren l in
  let l = find_mult_imp l in
  let l = find_mult l in
  match apply_mult i0 l with
    _,C s -> s
  | i,_ -> raise (ParseError("parse_syntax","",i))

let check_quant_range i0 cat l =
  let set = StringSet.of_list (
      try SelectorMap.find selector_values cat
      with Not_found -> raise (ParseError("check_quant_range", string_of_selector cat, i0))) in
  if StringSet.is_empty set then () else
    Xlist.iter l (fun v ->
        if not (StringSet.mem set v) then
          raise (ParseError("check_quant_range", string_of_selector cat ^ "=" ^ v, i0)))

let parse_quant_range i0 = function
    _,[_,"0"] -> Zero
  | _,[_,"T"] -> Top
  | _,[_,"all_numbers"] -> LCGrenderer.make_quant_restriction all_numbers
  | _,[_,"all_cases"] -> LCGrenderer.make_quant_restriction all_cases
  | _,[_,"all_genders"] -> LCGrenderer.make_quant_restriction all_genders
  | _,[_,"all_persons"] -> LCGrenderer.make_quant_restriction all_persons
  | cat,l ->
    let l = Xstring.split "&" (String.concat "" (Xlist.map l snd)) in
    check_quant_range i0 cat l;
    LCGrenderer.make_quant_restriction l

let parse_quantifiers i0 tokens =
  Xlist.map (split_comma i0 [] [] tokens) (function
        _,(i,cat) :: (_,"=") :: [] -> raise (ParseError("parse_quantifiers", "empty range", i))
      | _,(i,cat) :: (_,"=") :: tokens ->
            let cat = catch_selector_of_string i "parse_quantifiers" cat in
            cat, parse_quant_range i (cat,tokens)
      | _,(i,t) :: _ -> raise (ParseError("parse_quantifiers", "unexpected token '" ^ t ^ "'", i))
      | i0,[] -> raise (ParseError("parse_quantifiers", "no token", i0)))

let parse_raised i0 tokens =
  Xlist.map (split_comma i0 [] [] tokens) (function
        _,[i,cat] -> catch_selector_of_string i "parse_raised" cat
      | _,(i,t) :: _ -> raise (ParseError("parse_raised", "unexpected token '" ^ t ^ "'", i))
      | i0,[] -> raise (ParseError("parse_raised", "no token", i0)))

let rec find_syntax_end rev = function
    ((_,"BRACKET") :: _) as tokens -> List.rev rev, tokens
  | ((_,"COORD") :: _) as tokens -> List.rev rev, tokens
  | ((_,"PRECOORD") :: _) as tokens -> List.rev rev, tokens
  | ((_,"SINGLE-COST") :: _) as tokens -> List.rev rev, tokens
  | ((_,"DOUBLE-COST") :: _) as tokens -> List.rev rev, tokens
  | ((_,"QUANT") :: (_,"[") :: _) as tokens -> List.rev rev, tokens
  | ((_,"RAISED") :: (_,"[") :: _) as tokens -> List.rev rev, tokens
  | ((_,"SEM") :: (_,"[") :: _) as tokens -> List.rev rev, tokens
  | s :: tokens -> find_syntax_end (s :: rev) tokens
  | [] -> List.rev rev, []

let parse_sem_term sem_term = String.concat "" (Xlist.map sem_term snd)

let rec parse_rule atoms = function
    (_,"BRACKET") :: tokens -> Bracket :: parse_rule atoms tokens
  | (_,"QUANT") :: (i,"[") :: tokens ->
    let quant,tokens = find_right_bracket i [] tokens in
    Quant(parse_quantifiers i quant) :: parse_rule atoms tokens
  | (_,"COORD") :: tokens -> Coord :: parse_rule atoms tokens
  | (_,"PRECOORD") :: tokens -> PreCoord :: parse_rule atoms tokens
  | (_,"SINGLE-COST") :: tokens -> Cost 1 :: parse_rule atoms tokens
  | (_,"DOUBLE-COST") :: tokens -> Cost 2 :: parse_rule atoms tokens
  | (_,"RAISED") :: (i,"[") :: tokens ->
    let raised,tokens = find_right_bracket i [] tokens in
    Raised(parse_raised i raised) :: parse_rule atoms tokens
  | (_,"SEM") :: (i,"[") :: tokens ->
    let sem_term,tokens = find_right_bracket i [] tokens in
    Sem(parse_sem_term sem_term) :: parse_rule atoms tokens
  | [] -> []
  | tokens ->
    let i = fst (List.hd tokens) in
    let syntax,tokens = find_syntax_end [] tokens in
    (* print_prefix 100 tokens; *)
    Syntax(parse_syntax i atoms syntax) :: parse_rule atoms tokens

let parse_entry i0 atoms weights tokens =
  let prefix,tokens = manage_lemmata tokens in
  let selectors, rule, weight =
    match split_colon [] [] tokens with
      [selectors;rule] -> selectors, rule, 0.
    | [selectors;rule;[i,weight]] -> selectors, rule,
                                   (try StringMap.find weights weight
                                    with Not_found -> raise (ParseError("parse_entry", "unknown weight symbol '" ^ weight ^ "'", i)))
    | _ -> raise (ParseError("parse_entry", "invalid number of ':' in entry " ^ String.concat " " (Xlist.map tokens snd), i0)) in
  let selectors = parse_selectors i0 (prefix @ selectors) in
  let rule = parse_rule atoms rule in
  {empty_entry with selectors=selectors; rule=rule; weight=weight}

let string_of_parse_error filename proc s i line =
  Printf.sprintf "LCG lexicon error\nin file %s\nin line %d: %s\n%s: %s" filename i line proc s

let parse_lexicon filename i0 a atoms weights = function
    (i,"@LEXICON") :: tokens ->
    let entries = split_semic i [] [] tokens in
    Xlist.fold entries ([],true) (fun (entries,is_correct) (i,entry) ->
      try (parse_entry i atoms weights entry) :: entries, is_correct
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
    let i,phrase_names,tokens = parse_phrase_names 1 tokens in
    let atoms = make_atoms phrase_names in
    let i,weights,tokens = parse_weights i tokens in
    let lexicon,is_correct = parse_lexicon filename i a atoms weights tokens in
    if is_correct then List.rev lexicon else exit 0
  with ParseError(proc,s,i) ->
    print_endline (string_of_parse_error filename proc s i a.(i-1));
    exit 0

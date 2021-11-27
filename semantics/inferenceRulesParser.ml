(*
 *  ENIAMsemantics implements semantic processing for ENIAM
 *  Copyright (C) 2018 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
 *  Copyright (C) 2018 LekSeek Sp. z o.o. sp. k.
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

let rec parse_cat_names_rec i0 rev = function
    (i1,"@") :: (i2,"ROLE_NAMES") :: tokens -> i1, List.rev rev, (i1,"@") :: (i2,"ROLE_NAMES") :: tokens
  | (i1,"@") :: (i2,"RULES") :: tokens -> i1, List.rev rev, (i1,"@") :: (i2,"RULES") :: tokens
  | (i1,"@") :: (i2,"OPERATORS") :: tokens -> i1, List.rev rev, (i1,"@") :: (i2,"OPERATORS") :: tokens
  | (i,t) :: tokens -> parse_cat_names_rec i0 ((i,t) :: rev) tokens
  | [] -> raise (ParseError("parse_cat_names_rec", "unexpexted end of input", i0))

let parse_cat_names i0 = function
    (i,"@") :: (_,"CAT_NAMES") :: tokens -> parse_cat_names_rec i [] tokens
  | (i,s) :: _ -> raise (ParseError("parse_cat_names", "'@CAT_NAMES' expected while '" ^ s ^ "' found", i))
  | [] -> raise (ParseError("parse_cat_names", "unexpexted end of input", i0))

let rec parse_role_names_rec i0 rev = function
    (i1,"@") :: (i2,"RULES") :: tokens -> i1, List.rev rev, (i1,"@") :: (i2,"RULES") :: tokens
  | (i1,"@") :: (i2,"OPERATORS") :: tokens -> i1, List.rev rev, (i1,"@") :: (i2,"OPERATORS") :: tokens
  | (i,t) :: tokens -> parse_role_names_rec i0 ((i,t) :: rev) tokens
  | [] -> raise (ParseError("parse_role_names_rec", "unexpexted end of input", i0))

let parse_role_names i0 = function
    (i,"@") :: (_,"ROLE_NAMES") :: tokens -> parse_role_names_rec i [] tokens
  | (i,s) :: _ -> raise (ParseError("parse_role_names", "'@ROLE_NAMES' expected while '" ^ s ^ "' found", i))
  | [] -> raise (ParseError("parse_role_names", "unexpexted end of input", i0))

let rec parse_operator_names_rec i0 rev = function
    (i1,"@") :: (i2,"RULES") :: tokens -> i1, List.rev rev, (i1,"@") :: (i2,"RULES") :: tokens
  | (i,t) :: tokens -> parse_operator_names_rec i0 ((i,t) :: rev) tokens
  | [] -> raise (ParseError("parse_operator_names_rec", "unexpexted end of input", i0))

let parse_operator_names i0 = function
    (i,"@") :: (_,"OPERATORS") :: tokens -> parse_operator_names_rec i [] tokens
  | (i,s) :: _ -> raise (ParseError("parse_operator_names", "'@OPERATORS' expected while '" ^ s ^ "' found", i))
  | [] -> raise (ParseError("parse_operator_names", "unexpexted end of input", i0))

let rec split_semic i0 found rev = function
(*     (i1,"lemma") :: (i2,"=") :: (i3,";") :: l -> split_semic (if rev = [] then i1 else i0) found ((i1,";") :: (i2,"=") :: (i3,"lemma") :: rev) l *)
  | (i,";") :: l -> split_semic i ((i0, List.rev rev) :: found) [] l
  | (i,s) :: l -> split_semic (if rev = [] then i else i0) found ((i,s) :: rev) l
  | [] -> if rev = [] then List.rev found else List.rev ((i0, List.rev rev) :: found)

let rec split_colon found rev = function
(*     (i1,"lemma") :: (i2,"=") :: (i3,":") :: l -> split_colon found ((i1,":") :: (i2,"=") :: (i3,"lemma") :: rev) l *)
  | (_,":") :: l -> split_colon (List.rev rev :: found) [] l
  | (i,s) :: l -> split_colon found ((i,s) :: rev) l
  | [] -> if rev = [] then List.rev found else List.rev ((List.rev rev) :: found)

let rec split_comma i0 found rev = function
(*     (i1,"lemma") :: (i2,"=") :: (i3,",") :: l -> split_comma (if rev = [] then i1 else i0) found ((i1,",") :: (i2,"=") :: (i3,"lemma") :: rev) l *)
  | (i,",") :: l -> split_comma i ((i0, List.rev rev) :: found) [] l
  | (i,s) :: l -> split_comma (if rev = [] then i else i0) found ((i,s) :: rev) l
  | [] -> if rev = [] then List.rev found else List.rev ((i0, List.rev rev) :: found)

let rec split_arrow i0 found rev = function
(*     (i1,"lemma") :: (i2,"=") :: (i3,",") :: l -> split_arrow (if rev = [] then i1 else i0) found ((i1,",") :: (i2,"=") :: (i3,"lemma") :: rev) l *)
  | (i,"=") :: (_,"=") :: (_,">") :: l -> split_arrow i ((i0, List.rev rev) :: found) [] l
  | (i,s) :: l -> split_arrow (if rev = [] then i else i0) found ((i,s) :: rev) l
  | [] -> if rev = [] then List.rev found else List.rev ((i0, List.rev rev) :: found)

(*type selector = Sense | Cat | Quant | Rel | Attr

let selector_of_string = function
    "sense" -> Sense
  | "cat" -> Cat
  | "quant" -> Quant
  | "rel" -> Rel
  | "attr" -> Attr
  | s -> failwith ("selector_of_string: " ^ s)

let string_of_selector = function
    Sense -> "sense"
  | Cat -> "cat"
  | Quant -> "quant"
  | Rel -> "rel"
  | Attr -> "attr"

let catch_selector_of_string i proc s =
  try selector_of_string s
  with _ -> raise (ParseError(proc, "unknown selector: " ^ s, i))

let match_selectors = function
    i0,(i,s) :: l -> i,catch_selector_of_string i "match_selectors" s,l
  | i0,[] -> raise (ParseError("match_selectors", "empty", i0))

type eq = Eq | Neq

let match_relation = function
  (* cat,"=" :: "=" :: l -> cat,StrictEq,l *)
  | i,cat,(_,"!") :: (_,"=") :: l -> i,cat,Neq,l
  | i,cat,(_,"=") :: l -> i,cat,Eq,l
  | _,cat,(i,s) :: l -> raise (ParseError("match_relation", "relation symbol not found: " ^ String.concat " " (s :: Xlist.map l snd), i))
  | i,cat,[] -> raise (ParseError("match_relation", "empty", i))

let rec split_mid i0 rev = function
    [i,s] -> List.rev ((i,s) :: rev)
  | (i1,s) :: (i2,"|") :: (i3,"|") :: l -> raise (ParseError("split_mid", "duplicated delimeter found", i2))
  | (i1,s) :: (i0,"|") :: l -> split_mid i0 ((i1,s) :: rev) l
  | (i1,s1) :: (i2,s2) :: (i0,"|") :: l -> split_mid i0 ((i1,s1 ^ " " ^ s2) :: rev) l
  | (i1,s1) :: (i2,s2) :: (i3,s3) :: (i0,"|") :: l -> split_mid i0 ((i1,s1 ^ " " ^ s2 ^ " " ^ s3) :: rev) l
  | [i1,s1;i2,s2] -> List.rev ((i1,s1 ^ " " ^ s2) :: rev)
  | [i1,s1;i2,s2;i3,s3] -> List.rev ((i1,s1 ^ " " ^ s2 ^ " " ^ s3) :: rev)
  | [] -> raise (ParseError("split_mid", "empty", i0))
  | (i,s) :: l -> raise (ParseError("split_mid", "delimiter not found: " ^ String.concat " " (s :: Xlist.map l snd), i))

let rec check_value i0 role_names selector l =
  let vals = match selector with
      Sense -> StringSet.empty
    | Cat -> StringSet.empty
    | Quant -> StringSet.of_list ["sg";"pl";"pro";]
    | Attr -> StringSet.empty
    | Rel -> role_names in
  if StringSet.is_empty vals then () else
    Xlist.iter l (fun (i,s) ->
        if not (StringSet.mem vals s) then
          raise (ParseError("check_value", "invalid selector: " ^ string_of_selector selector ^ "=" ^ s, i)));
  Xlist.map l snd
 *)


type syntax =
    C of string
  | R of string
  | S of string
  | O of string
  | X of string
  | V1 of int
  | V2 of int
  | B of string * string * (int * syntax) list

let operators = StringSet.of_list [
    "@"; "*"; "+"; "="; "/"; "|"; "\\"; "("; ")"; "["; "]"; ","; "{"; "}"; "?"; ">"; "„"; "”"; ":"]

let render_names = StringSet.of_list [
    "subst"; "adj"; "sg"; "pl"; "nom"; "gen"; "$g"; "$n"; "lemma"; "orth"; "fixed"; "pos"; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; ]
    
let rec string_of_syntax = function
    C s -> s
  | R s -> s
  | S s -> "„" ^ s ^ "”"
  | O s -> s
  | X s -> s
  | V1 v -> "*" ^ string_of_int v
  | V2 v -> "?" ^ string_of_int v
  | B(s,t,l) -> s ^ string_of_syntax_list l ^ t

and string_of_syntax_list l =
  String.concat "" (Xlist.map l (fun (_,t) -> string_of_syntax t))

let rec find_atomic_symbols cat_names role_names rev l =
  if l = [] then List.rev rev else
  let symbol,l = match l with
(*       (i,"=") :: (_,"=") :: (_,">") :: l -> (i,O "==>"), l *)
	  (i,"...") :: l -> (i,O "..."), l
    | (i,"{") :: (_,"}") :: l -> (i,O "{}"), l
    | (i,"„") :: (_,"”") :: l -> (i,S ""), l
    | (i,"„") :: (_,s1) :: (_,"”") :: l -> (i,S s1), l
(*    | (i,"„") :: (_,s1) :: (_,s2) :: (_,"”") :: l -> (i,S (s1^" "^s2)), l
    | (i,"„") :: (_,s1) :: (_,s2) :: (_,s3) :: (_,"”") :: l -> (i,S (s1^" "^s2^" "^s3)), l
    | (i,"„") :: (_,s1) :: (_,s2) :: (_,s3) :: (_,s4) :: (_,"”") :: l -> (i,S (s1^" "^s2^" "^s3^" "^s4)), l*)
    | (i,"*") :: (i2,n) :: l ->
         if StringSet.mem role_names n then (i,O "*"), (i2,n) :: l else
         let n = try int_of_string n with _ -> raise (ParseError("find_atomic_symbols", "number expected " ^ n, i2)) in
         if n < 0 then raise (ParseError("find_atomic_symbols", "variable ids must be greater then zero " ^ string_of_int n, i2)) else
         (i,V1 n), l
    | (i,"?") :: (i2,n) :: l ->
         if StringSet.mem role_names n then (i,O "?"), (i2,n) :: l else
         let n = try int_of_string n with _ -> raise (ParseError("find_atomic_symbols", "number expected " ^ n, i2)) in
         if n < 0 then raise (ParseError("find_atomic_symbols", "variable ids must be greater then zero " ^ string_of_int n, i2)) else
         (i,V2 n), l
    | (i,"render") :: l -> (i,O "render"), l
    | (i,s) :: l ->
         let symbol =
           if StringSet.mem cat_names s then i,C s else
		   if StringSet.mem role_names s then i,R s else
		   if StringSet.mem operators s then i,O s else
		   if StringSet.mem render_names s then i,X s else
           raise (ParseError("find_atomic_symbols", "unknown symbol " ^ s, i)) in
		 symbol, l
	| [] -> failwith "find_atomic_symbols" in
  find_atomic_symbols cat_names role_names (symbol :: rev) l

let rec find_brackets brackets rev = function
    (i,O s) :: l ->
       (try
         let t = Xlist.assoc brackets s in
         let found,l = find_rbracket i t brackets [] l in
         find_brackets brackets ((i,B(s,t,found)) :: rev) l
       with Not_found -> find_brackets brackets ((i,O s) :: rev) l)
  | (i,B _) :: _ -> raise (ParseError("find_brackets", "unexpected brackets", i))
  | (i,t) :: l -> find_brackets brackets ((i,t) :: rev) l
  | [] -> List.rev rev

and find_rbracket i0 rb brackets rev = function
    (i,O s) :: l ->
       if s = rb then List.rev rev, l else
       (try
         let t = Xlist.assoc brackets s in
         let found,l = find_rbracket i t brackets [] l in
         find_rbracket i0 rb brackets ((i,B(s,t,found)) :: rev) l
       with Not_found -> find_rbracket i rb brackets ((i,O s) :: rev) l)
  | (i,B _) :: _ -> raise (ParseError("find_brackets", "unexpected brackets", i))
  | (i,t) :: l -> find_rbracket i rb brackets ((i,t) :: rev) l
  | [] -> raise (ParseError("find_rbracket", "empty", i0))

let rec split_op_comma i0 found rev = function
    (i,O ",") :: l -> split_op_comma i ((i0, List.rev rev) :: found) [] l
  | (i,s) :: l -> split_op_comma (if rev = [] then i else i0) found ((i,s) :: rev) l
  | [] -> if rev = [] then List.rev found else List.rev ((i0, List.rev rev) :: found)

type req = Opt | Multi |Req

type rule =
    Concept of string * string * int * rule * rule
  | Context of rule
  | Role of req * string * rule
  | Attr of req * string
  | Dots
  | Args of rule list
  | NoArgs
  | Contents of rule list
  | NoContents
  | Render of string * (int * string * string list) list

let string_of_req = function
    Opt -> "Opt"
  | Multi -> "Multi"
  | Req -> "Req"

let rec string_of_rule spaces = function
    Concept(a,b,n,c,r) -> Printf.sprintf "Concept(%s,%s,%d,%s,%s)" a b n (string_of_rule spaces c) (string_of_rule spaces r)
  | Context r -> Printf.sprintf "Context(%s)" (string_of_rule spaces r)
  | Role(a,b,r) -> Printf.sprintf "Role(%s,%s,%s)" (string_of_req a) b (string_of_rule spaces r)
  | Attr(a,b) -> Printf.sprintf "Attr(%s,%s)" (string_of_req a) b
  | Dots -> "Dots"
  | Args [] -> "Args[]"
  | Args l -> Printf.sprintf "Args[\n%s%s]" spaces (String.concat (";\n" ^ spaces) (Xlist.map l (string_of_rule ("  " ^ spaces))))
  | NoArgs -> "NoArgs"
  | Contents l -> Printf.sprintf "Contents[\n%s%s]" spaces (String.concat (";\n" ^ spaces) (Xlist.map l (string_of_rule ("  " ^ spaces))))
  | NoContents -> "NoContents"
  | Render(c,l) -> Printf.sprintf "Render(%s,[%s])" c (String.concat ";" (Xlist.map l (fun (n,x,interp) -> String.concat ":" (string_of_int n :: x :: interp))))
  
let rec parse_render_arg_rec = function
      (_,O ":") :: (_,C s) :: interp -> s :: parse_render_arg_rec interp
    | (_,O ":") :: (_,R s) :: interp -> s :: parse_render_arg_rec interp
    | (_,O ":") :: (_,X s) :: interp -> s :: parse_render_arg_rec interp
    | (i,_) :: _ as l -> raise (ParseError("parse_render_arg_rec", "invalid syntax " ^ string_of_syntax_list l, i))
    | [] -> []

let parse_render_arg (i0,l) =
  let n, interp = match l with
      (_,V2 n) :: interp -> n, interp
	| _ -> raise (ParseError("parse_render_arg", "invalid syntax " ^ string_of_syntax_list l, i0)) in
  let x, interp = match interp with
      (_,O ":") :: (_,C "lemma") :: interp -> "lemma", interp
    | (_,O ":") :: (_,R "lemma") :: interp -> "lemma", interp
    | (_,O ":") :: (_,X "lemma") :: interp -> "lemma", interp
    | (_,O ":") :: (_,C "orth") :: interp -> "orth", interp
    | (_,O ":") :: (_,R "orth") :: interp -> "orth", interp
    | (_,O ":") :: (_,X "orth") :: interp -> "orth", interp
    | _ -> "", interp in
  let interp = parse_render_arg_rec interp in
  n, x, interp
  
let rec parse_role i0 = function
    [_,O "..."] -> Dots
  | l ->
  let o,l = match l with
      (_,O "?") :: l -> Opt,l
	| (_,O "*") :: l -> Multi,l
    | _ -> Req,l in
  let b,r,l = match l with
      (_,R r) :: (_,O ":") :: l -> true,r,l
	| [_,R r] -> false,r,[]
    | (i,t) :: l -> raise (ParseError("parse_role", "invalid syntax " ^ string_of_syntax_list ((i,t) :: l), i))
    | [] -> raise (ParseError("parse_role", "empty", i0)) in
  if b then Role(o,r,parse_concept l) else Attr(o,r)

and parse_concept = function
	[_,O "..."] -> Dots
  | [_,C c;i1,O "render";i2,B("(",")",l)] ->
       Render(c,List.rev (Xlist.rev_map (split_op_comma i2 [] [] l) parse_render_arg))
(*       Render(c,List.rev (Xlist.rev_map (split_op_comma i2 [] [] l) (function
           (_,V2 n) :: interp -> parse_render_arg n 
		 | _ -> raise (ParseError("parse_concept", "invalid syntax " ^ string_of_syntax_list a, i2)))))*)
  | [i,B("[","]",l)] -> Context(parse_concept l)
  | l ->
  let c,l = match l with
      (_,C c) :: l -> c,l
    | _ -> "",l in
  let s,l = match l with
      (_,S s) :: l -> s,l
    | _ -> "",l in
  let n,l = match l with
      (_,V1 n) :: l -> n,l
	| (_,V2 n) :: l -> n,l
    | _ -> -1,l in
  let contents,l = match l with
      (i,B("[","]",l2)) :: l -> Contents(List.rev (Xlist.rev_map (split_op_comma i [] [] l2) (fun (i,t) -> parse_concept t))), l
    | _ -> NoContents,l in
  let args = match l with
      [i,B("{","}",l)] -> Args(List.rev (Xlist.rev_map (split_op_comma i [] [] l) (fun (i,t) -> parse_role i t)))
	| [i,O "{}"] -> Args[]
	| [] -> NoArgs
	| (i,t) :: l -> raise (ParseError("parse_concept", "invalid syntax " ^ string_of_syntax_list ((i,t) :: l), i)) in
  Concept(c,s,n,contents,args)

let rec parse_rule cat_names role_names (i0,tokens) =
  let l = find_atomic_symbols cat_names role_names [] tokens in
  let l = find_brackets ["{","}";"(",")";"[","]"] [] l in
  parse_concept l

let parse_entry i0 cat_names role_names tokens =
  (* let prefix,tokens = manage_lemmata tokens in *)
  let prior,tokens = match List.rev tokens with
      (i2,n) :: (i1,":") :: l ->
         let n = try int_of_string n with _ -> raise (ParseError("parse_entry", "number expected " ^ n, i2)) in
         if n < 0 then raise (ParseError("parse_entry", "rule priority must be greater then zero " ^ string_of_int n, i2)) else
         n, List.rev l
	| _ -> raise (ParseError("parse_entry", "no priority provided " ^ String.concat " " (Xlist.map tokens snd), i0)) in
  let rule1, rule2 =
    match split_arrow i0 [] [] tokens with
      [rule1;rule2] -> rule1, rule2
    | _ -> raise (ParseError("parse_entry", "invalid number of '==>' in entry " ^ String.concat " " (Xlist.map tokens snd), i0)) in
  let rule1 = parse_rule cat_names role_names rule1 in
  let rule2 = parse_rule cat_names role_names rule2 in
  rule1, rule2, prior

let string_of_parse_error proc s i line =
  Printf.sprintf "Inference rules lexicon error in line %d: %s\n%s: %s" i line proc s

let parse_lexicon i0 a cat_names lexicon role_names = function
    (_,"@") :: (i,"RULES") :: tokens ->
    let entries = split_semic i [] [] tokens in
    Xlist.fold entries (lexicon,true) (fun (entries,is_correct) (i,entry) ->
      try (parse_entry i cat_names role_names entry) :: entries, is_correct
      with ParseError(proc,s,i) ->
        print_endline (string_of_parse_error proc s i a.(i-1));
        entries,false)
  | (i,s) :: _ -> raise (ParseError("parse_lexicon", "'@RULES' expected while '" ^ s ^ "' found", i))
  | [] -> raise (ParseError("parse_lexicon", "unexpexted end of input", i0))

let rec merge_quoted2 i0 rev = function
    [] -> raise (ParseError("merge_quoted2", "unexpexted end of input", i0))
  | (i,"”") :: tokens -> (i0, String.concat "" (List.rev rev)), (i,"”"), tokens
  | (i,"\t") :: _ -> raise (ParseError("merge_quoted2", "unexpexted TAB", i))
  | (i,"\n") :: _ -> raise (ParseError("merge_quoted2", "unexpexted NEWLINE", i))
  | (_,s) :: tokens -> merge_quoted2 i0 (s :: rev) tokens

let rec merge_quoted rev = function
    [] -> List.rev rev
  | (i,"„") :: tokens -> let s, q, tokens = merge_quoted2 i [] tokens in merge_quoted (q :: s :: (i,"„") :: rev) tokens
  | (i,"”") :: _  ->  raise (ParseError("merge_quoted", "unexpexted ”", i))
  | x :: tokens -> merge_quoted (x :: rev) tokens

let load_lexicon filename (lexicon, role_names) =
  let lines = Xstring.split "\n" (File.load_file filename) in
  let a = Array.of_list lines in
  let lines,no_lines = Xlist.fold lines ([],1) (fun (lines,i) line -> (i,line) :: lines, i+1) in
  let lines = Xlist.rev_map lines (fun (i,line) -> i, remove_comments line) in
  let tokens = List.rev (List.flatten (Xlist.rev_map lines (fun (i,line) ->
      Xlist.rev_map (Str.full_split
                       (Str.regexp "\\]\\| \\|\t\\|\r\\|\\?\\|:\\|;\\|&\\|!\\|=\\|}\\|{\\|,\\|\\*\\|@\\|/\\|\\+\\|)\\|(\\||\\|\\[\\|\\\\|<\\|>\\|„\\|”") line) (function
            Str.Text s -> i,s
          | Str.Delim s -> i,s)))) in
  try
    let tokens = merge_quoted [] tokens in
    let tokens = List.rev (Xlist.fold tokens [] (fun tokens -> function
        _," " -> tokens
      | _,"\t" -> tokens
      | _,"\r" -> tokens
      | i,t -> (i,t) :: tokens)) in
    let i,cat_names,tokens = parse_cat_names 1 tokens in
    let cat_names = Xlist.fold cat_names StringSet.empty (fun params (_,param) -> StringSet.add params param) in
    let i,role_names_list,tokens = parse_role_names i tokens in
    let role_names = Xlist.fold role_names_list role_names (fun params (_,param) -> StringSet.add params param) in
    let i,operators,tokens = parse_operator_names i tokens in
    let role_names2 = Xlist.fold operators role_names (fun params (_,param) -> StringSet.add params param) in
    let lexicon,is_correct = parse_lexicon i a cat_names (List.rev lexicon) role_names2 tokens in
    if is_correct then List.rev lexicon, role_names else exit 0
  with ParseError(proc,s,i) ->
    print_endline (string_of_parse_error proc s i a.(i-1));
    exit 0

(*let rec extract_selector sel rev = function
    (s,Eq,l) :: selectors ->
      if s = sel then l, List.rev rev @ selectors else
      extract_selector sel ((s,Eq,l) :: rev) selectors
  | t :: selectors -> extract_selector sel (t :: rev) selectors
  | [] -> [], List.rev rev

let find_roles rule = (* FIXME: trzeba by dodać sprawdzanie rozłączności *)
  let req_single_rel = ref StringSet.empty in
  let opt_single_rel = ref StringSet.empty in
  let multi_rel = ref StringSet.empty in
  let rec find_roles_rec = function
    WithRecord(s,l) ->
      req_single_rel := StringSet.add !req_single_rel s;
      Xlist.iter l (fun (_,t) -> find_roles_rec t)
  | Record l -> Xlist.iter l (fun (_,t) -> find_roles_rec t)
  | List l -> Xlist.iter l find_roles_rec
  | Constant s -> ()
  | NConstant s -> ()
  | LConstant s -> ()
  | RoleRef s -> req_single_rel := StringSet.add !req_single_rel s
  | RoleRefDefault(s,t) ->
      opt_single_rel := StringSet.add !opt_single_rel s;
      find_roles_rec t
  | MultiRoleRef s -> multi_rel := StringSet.add !multi_rel s in
  find_roles_rec rule;
  !req_single_rel,!opt_single_rel,!multi_rel*)

let rec find_sense = function
    Concept(c,s,n,contents,args) -> s
  | Context r -> find_sense r
  | _ -> failwith "find_sense"

let rec find_cat = function
    Concept(c,s,n,contents,args) -> c
  | Context r -> find_cat r
  | _ -> failwith "find_cat"

(*let rec map_sense_cat = function
    Concept(c,s,n,contents,args) -> Concept((if c = "_" then "" else c),(if s = "_" then "" else s),n,contents,args)
  | Context r -> Context(map_sense_cat r)
  | _ -> failwith "map_sense_cat"*)

let create_rule_dict l =
  let map = Xlist.fold l IntMap.empty (fun map (rule1,rule2,prior) ->
    IntMap.add_inc map prior [rule1,rule2] (fun l -> (rule1,rule2) :: l)) in
  let l = IntMap.fold map [] (fun l prior rules ->
    let senses,cats = Xlist.fold rules (StringMap.empty,StringMap.empty) (fun (senses,cats) (rule1,rule2) ->
      let sense = find_sense rule1 in
(*       if sense = "_" then StringMap.add_inc senses "" [map_sense_cat rule1,rule2] (fun l -> (map_sense_cat rule1,rule2) :: l), cats else *)
      if sense <> "" then StringMap.add_inc senses sense [rule1,rule2] (fun l -> (rule1,rule2) :: l), cats else
      let cat = find_cat rule1 in
(*       if cat = "_" then senses, StringMap.add_inc cats cat [map_sense_cat rule1,rule2] (fun l -> (map_sense_cat rule1,rule2) :: l) else *)
      if cat <> "" then senses, StringMap.add_inc cats cat [rule1,rule2] (fun l -> (rule1,rule2) :: l) else
      failwith "create_rule_dict: rule without sense and cat") in
	(prior,senses,cats) :: l) in
  Xlist.sort l (fun (p1,_,_) (p2,_,_) -> compare p1 p2)

let rules = ref []
let can_combine = ref StringSet.empty

(*let _ =
  print_endline "loading inference.dic";
  let l = load_lexicon (WalTypes.data_path ^ "/inference.dic") in
  Xlist.iter l (fun (rule1, rule2, prior) ->
    Printf.printf "%s\n==>\n%s, %d\n\n%!" (string_of_rule "  " rule1) (string_of_rule "  " rule2) prior)*)


let initialize () =
  let lexicon, role_names = load_lexicon (WalTypes.data_path ^ "/inference.dic") ([], StringSet.empty) in
  let lexicon, role_names = 
    Xlist.fold !SubsyntaxTypes.theories (lexicon, role_names) (fun (lexicon, role_names) theory ->
      File.catch_no_file (load_lexicon (SubsyntaxTypes.theories_path ^ theory ^ "/inference.dic")) (lexicon, role_names)) in
  let lexicon, role_names = 
    Xlist.fold !SubsyntaxTypes.user_theories (lexicon, role_names) (fun (lexicon, role_names) theory ->
      File.catch_no_file (load_lexicon (SubsyntaxTypes.user_theories_path ^ theory ^ "/inference.dic")) (lexicon, role_names)) in
  rules := create_rule_dict lexicon;
  can_combine := role_names;
  ()

(*
 *  ENIAMsubsyntax: tokenization, lemmatization, MWE and sentence detecion for Polish
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

open Xstd
open SubsyntaxTypes

type prod_lemma =
    Str of string
  | Concat
  | ConcatInt
  | ConcatSpace
  
type prod =
    MakeLemma of prod_lemma * string * sel list * int list (* lemma * pos * interp * args *)
  | MakeIdeogram of prod_lemma * string * int list (* lemma * mode * args *)
  | MakeInterp of prod_lemma * int list (* lemma * args *)
  
let create_fixed_dict path filename dict =
  let valence = DataLoader.extract_valence_lemmata path filename StringMap.empty in
  StringMap.fold valence dict (fun dict lemma map ->
(*     print_endline ("create_fixed_dict 1: " ^ lemma); *)
    if StringMap.mem map "fixed" then
      try
        let orths = List.flatten (List.rev (Xlist.rev_map (Patterns.parse lemma) Tokenizer.get_orth_list)) in
(*       print_endline ("create_fixed_dict 2: " ^ String.concat " " orths); *)
	    let s = List.hd orths in
        let orths = Xlist.map orths (fun s -> O s) in
        let prod = MakeLemma(Str lemma,"fixed",[],[]) in
        StringMap.add_inc dict s [orths,prod] (fun l -> (orths,prod) :: l)
	  with Failure e -> failwith (e ^ ": " ^ lemma)
    else dict)
  
let process_interp interp =
  match Tagset.parse interp with  
    [pos,interp] -> pos,Xlist.map interp (function
        ["$c"] -> S "c"
      | ["$n"] -> S "n"
      | ["$g"] -> S "g"
      | ["$d"] -> S "d"
      | ["$C"] -> S "C"
      | ["_"] -> G
      | l -> Xlist.iter l (fun s -> if Xstring.check_prefix "$" s then failwith ("process_interp: " ^ s) else ()); V l)
  | _ -> failwith "process_interp"
  
let load_mwe_dict filename dict =
  File.fold_tab filename dict (fun dict -> function
      [orths; lemma; interp] ->
        let orths = Xstring.split " " orths in
        if orths = [] then failwith "load_mwe_dict" else
        let s = List.hd orths in
        let orths = Xlist.map orths (function "." -> N "." | s -> O s) in
        let pos,interp = process_interp interp in
        let prod = MakeLemma(Str lemma,pos,interp,[]) in
        StringMap.add_inc dict s [orths,prod] (fun l -> (orths,prod) :: l)
(*        let lemma,cat,interp = match process_interp lemma interp with
            Lem(lemma,cat,interp) -> lemma,cat,interp
          | _ -> failwith "load_mwe_dict2" in
        StringMap.add_inc dict s [orths,lemma,cat,interp] (fun l -> (orths,lemma,cat,interp) :: l)*)
    | l -> failwith ("load_mwe_dict '" ^ String.concat "\t" l ^ "'"))

let process_orth = function
    [Lexer.T "*"; Lexer.B("(",")",[Lexer.T interp])] -> 
      let pos,interp = process_interp interp in
      LemStar(pos,interp)
  | [Lexer.T lemma; Lexer.B("(",")",[Lexer.T interp])] -> 
      let pos,interp = process_interp interp in
      Lem(lemma,pos,interp)
  | [Lexer.T "."] -> N "."
  | [Lexer.T orth] -> 
      if orth = "%letters" then Letters else
      if orth = "%smallet" then SmallLet else
      if orth = "%caplet" then CapLet else
      if Xstring.check_prefix "%" orth then I(Xstring.cut_prefix "%" orth) else
      if Xstring.check_prefix "^" orth then C(Xstring.cut_prefix "^" orth) else
      if orth = "\\(" then O "(" else
      if orth = "\\)" then O ")" else
      if orth = "\\{" then O "{" else
      if orth = "\\}" then O "}" else
      if orth = "\\ " then O " " else
      if orth = "\\\\" then O "\\" else
      O orth
  | [Lexer.B("{","}",l); Lexer.B("(",")",[Lexer.T interp])] -> 
      let pos,interp = process_interp interp in
      Lem(Lexer.string_of_token_list l,pos,interp)  (* FIXME: czy tu nie ma problemu ze spacjami przed znakami interpunkcyjnymi? *)
(*   process_interp (Lexer.string_of_token_list l) interp *)
  | [Lexer.B("{","}",l)] -> O(Lexer.string_of_token_list l)
  | tokens -> failwith ("process_orth 1: " ^ Lexer.string_of_token_list tokens)

let process_lemma = function
    "%concat" -> Concat
  | "%concat-int" -> ConcatInt
  | "%concat-sp" -> ConcatSpace
  | s -> Str s
  
let rec proces_args args = 
  Xlist.map (Xstring.split ";" args) (fun s -> try int_of_string s with _ -> failwith "proces_args")
  
let make_prod lemma (pos,interp) args =
  if Xstring.check_prefix "%" pos then 
    if interp <> [] then failwith "make_prod" else
    if pos = "%interp" then MakeInterp(lemma,args) else
    MakeIdeogram(lemma,Xstring.cut_prefix "%" pos,args)
  else MakeLemma(lemma,pos,interp,args)

let rec manage_space_markes = function
    I("nsp") :: _ -> failwith "manage_space_markes"
  | I("sp") :: _ -> failwith "manage_space_markes"
  | pat :: I("sp") :: l -> SP(pat) :: manage_space_markes l 
  | pat :: I("nsp") :: l -> NSP(pat) :: manage_space_markes l 
  | pat :: l -> pat :: manage_space_markes l 
  | [] -> []
   
let process_prod = function
    [Lexer.T lemma; Lexer.B("(",")",[Lexer.T interp])] -> 
      make_prod (process_lemma lemma) (process_interp interp) []
  | [Lexer.T lemma; Lexer.B("(",")",[Lexer.T interp]);Lexer.B("[","]",[Lexer.T args])] -> 
      make_prod (process_lemma lemma) (process_interp interp) (proces_args args)
  | tokens -> failwith ("process_prod: " ^ Lexer.string_of_token_list tokens)

let rec process_escaped = function
    Lexer.T "\\" :: Lexer.T "(" :: l -> Lexer.T "\\(" :: process_escaped l 
  | Lexer.T "\\" :: Lexer.T ")" :: l -> Lexer.T "\\)" :: process_escaped l 
  | Lexer.T "\\" :: Lexer.T "{" :: l -> Lexer.T "\\{" :: process_escaped l 
  | Lexer.T "\\" :: Lexer.T "}" :: l -> Lexer.T "\\}" :: process_escaped l 
  | Lexer.T "\\" :: Lexer.T " " :: l -> Lexer.T "\\ " :: process_escaped l 
  | Lexer.T "\\" :: Lexer.T "\\" :: l -> Lexer.T "\\\\" :: process_escaped l 
  | s :: l -> s :: process_escaped l 
  | [] -> []
  
let load_mwe_dict2 filename (dict,dict2) =
  File.fold_tab filename (dict,dict2) (fun (dict,dict2) -> function
      [orths; lemma] ->
(*         print_endline ("load_mwe_dict2: " ^ orths ^ "\t" ^ lemma); *)
        let tokens = Lexer.split "(\\|)\\|{\\|}\\| \\|\\" orths in
        let tokens = process_escaped tokens in
        (* print_endline ("load_dict2 1: " ^ Lexer.string_of_token_list tokens); *)
        let tokens = Lexer.find_brackets ["{","}";"(",")"] [] tokens in
        (* print_endline ("load_dict2 2: " ^ Lexer.string_of_token_list tokens); *)
        let orths = List.rev (Xlist.rev_map (Lexer.split_symbol (Lexer.T " ") [] tokens) process_orth) in
        let orths = manage_space_markes orths in
        let tokens = Lexer.split "(\\|)\\|{\\|}\\|\\]\\|\\[" lemma in
        (* print_endline ("load_dict2 3: " ^ Lexer.string_of_token_list tokens); *)
        let tokens = Lexer.find_brackets ["{","}";"(",")";"[","]"] [] tokens in
        (* print_endline ("load_dict2 4: " ^ Lexer.string_of_token_list tokens); *)
        let prod = process_prod tokens in
        if orths = [] then failwith "load_mwe_dict2" else
        (match List.hd orths with
            Lem(s,_,_) -> dict, StringMap.add_inc dict2 s [orths,prod] (fun l -> (orths,prod) :: l)
          | O s -> StringMap.add_inc dict s [orths,prod] (fun l -> (orths,prod) :: l), dict2
          | _ -> dict, StringMap.add_inc dict2 "" [orths,prod] (fun l -> (orths,prod) :: l))
    | l -> failwith ("load_mwe_dict2 '" ^ String.concat "\t" l ^ "'"))

let add_known_orths_and_lemmata dict =
  let a = {number=""; gender=""; no_sgjp=true; poss_ndm=false; exact_case=false; ont_cat="MWEcomponent"} in
  let orths,lemmata = StringMap.fold dict (!known_orths,!known_lemmata) (fun (orth_set,lemma_map) _ l ->
    Xlist.fold l (orth_set,lemma_map) (fun (orth_set,lemma_map) (orths,prod) ->
      Xlist.fold orths (orth_set,lemma_map) (fun (orth_set,lemma_map) -> function
          O s -> 
            StringSet.add orth_set s, lemma_map
        | Lem(lemma,pos,_) -> orth_set, 
            let map2 = try StringMap.find lemma_map lemma with Not_found -> StringMap.empty in
            let map2 = StringMap.add_inc map2 (Tagset.simplify_pos pos) (OntSet.singleton a) (fun set -> OntSet.add set a) in
            StringMap.add lemma_map lemma map2
        | _ -> orth_set, lemma_map))) in
  known_orths := orths;
  known_lemmata := lemmata

    
let load_mwe_dicts () =
  let dict = StringMap.empty in
  let dict2 = StringMap.empty in
  let dict = File.catch_no_file (load_mwe_dict mwe_filename) dict in
  let dict,dict2 = File.catch_no_file (load_mwe_dict2 mwe2_filename) (dict,dict2) in
  let dict = File.catch_no_file (create_fixed_dict data_path "/valence.dic") dict in
  let dict =
    Xlist.fold !theories dict (fun dict theory ->
      File.catch_no_file (load_mwe_dict (theories_path ^ theory ^ "/mwe.tab")) dict) in
  let dict,dict2 =
    Xlist.fold !theories (dict,dict2) (fun (dict,dict2) theory ->
      File.catch_no_file (load_mwe_dict2 (theories_path ^ theory ^ "/mwe2.tab")) (dict,dict2)) in
  let dict =
    Xlist.fold !theories dict (fun dict theory ->
      File.catch_no_file (create_fixed_dict (theories_path ^ theory) "/valence.dic") dict) in
  add_known_orths_and_lemmata dict;
  add_known_orths_and_lemmata dict2;
  let cdict,cdict2 = File.catch_no_file (load_mwe_dict2 (data_path ^ "/coordination.tab")) (StringMap.empty,StringMap.empty) in
  add_known_orths_and_lemmata cdict;
  add_known_orths_and_lemmata cdict2;
  dict,dict2

let mwe_dict = ref (StringMap.empty : (pat list * prod) list StringMap.t)
let mwe_dict2 = ref (StringMap.empty : (pat list * prod) list StringMap.t)

let get_orths paths =
  IntMap.fold paths StringSet.empty (fun orths _ map ->
    IntMap.fold map orths (fun orths _ l ->
      TokenEnvSet.fold l orths (fun orths t ->
        Xlist.fold (Tokenizer.get_orths t.token) orths StringSet.add)))

let get_lemmas paths =
  IntMap.fold paths StringSet.empty (fun orths _ map ->
    IntMap.fold map orths (fun orths _ l ->
      TokenEnvSet.fold l orths (fun orths t ->
        StringSet.add orths (Tokenizer.get_lemma t.token))))

let preselect orths lemmas rules l =
  Xlist.fold l rules (fun rules (match_list,prod) ->
    (* print_endline ("preselect: " ^ lemma); *)
    let b = Xlist.fold match_list true (fun b -> function
        O s -> StringSet.mem orths s && b
      | Lem(s,_,_) -> StringSet.mem lemmas s && b
      | _ -> b) in
    if b then (Xlist.size match_list > 1,match_list,prod) :: rules else rules)

let preselect_dict orths lemmas dict rules =
  StringSet.fold orths rules (fun rules orth ->
    try
      (* print_endline ("preselect_dict: " ^ orth); *)
      preselect orths lemmas rules (StringMap.find dict orth)
    with Not_found -> rules)

let preselect_dict2 orths lemmas dict2 rules =
  let rules = try preselect orths lemmas rules (StringMap.find dict2 "") with Not_found -> rules in
  StringSet.fold lemmas rules (fun rules lemma ->
    try
      preselect orths lemmas rules (StringMap.find dict2 lemma)
    with Not_found -> rules)

let select_rules paths mwe_dict mwe_dict2 =
  let orths = get_orths paths in
  (* print_endline ("MWE.select_rules 1 orths=[" ^ String.concat ";" (StringSet.to_list orths) ^ "]"); *)
  let lemmas = get_lemmas paths in
  let rules = preselect_dict orths lemmas mwe_dict [] in
  (* print_endline ("MWE.select_rules 1 |rules|=" ^ string_of_int (Xlist.size rules)); *)
  (* Xlist.iter rules (fun (is_mwe,match_list,lemma,cat,interp) -> print_endline lemma); *)
  let rules = preselect_dict2 orths lemmas mwe_dict2 rules in
  (* print_endline ("MWE.select_rules 2 |rules|=" ^ string_of_int (Xlist.size rules)); *)
  (* let rules = add_ordnum_rules intnum_orths rules in *)
  (* print_endline ("MWE.select_rules 5 |rules|=" ^ string_of_int (Xlist.size rules) ^ " |year_orths|=" ^ string_of_int (StringSet.size year_orths) ^ " |letter_orths|=" ^ string_of_int (StringSet.size letter_orths)); *)
  rules

let rec match_path_rec map found (t:token_env) sels rev = function
    [] -> (t :: rev, sels) :: found
  | s :: l ->
     let map2 = try IntMap.find map t.next with Not_found -> IntMap.empty in
     let found2 = IntMap.fold map2 [] (fun found2 _ l ->
       TokenEnvSet.fold l found2 (fun found2 new_t ->
         let sels = try Patterns.match_token_env sels (s,new_t) with Not_found -> [] in
		 Xlist.fold sels found2 (fun found2 sels -> (new_t,sels) :: found2))) in
     Xlist.fold found2 found (fun found (new_t,sels) -> match_path_rec map found new_t sels (t :: rev) l)

let match_path map = function
    [] -> failwith "match_path"
  | s :: l ->
     let found = IntMap.fold map [] (fun found i map2 ->
       IntMap.fold map2 found (fun found j l ->
         TokenEnvSet.fold l found (fun found t ->
         let sels = try Patterns.match_token_env [] (s,t) with Not_found -> [] in
		 Xlist.fold sels found (fun found sels -> (t,sels) :: found)))) in
     Xlist.fold found [] (fun found (t,sels) -> match_path_rec map found t sels [] l)

let concat_orths l =
  let s = String.concat "" (Xlist.map l (fun t -> t.orth ^ (if t.beg+t.len=t.next then "" else " "))) in
  if Xstring.check_sufix " " s then Xstring.cut_sufix " " s else s
  
let rec match_args n = function
    t :: l, i :: args ->
      if i = n then t :: (match_args (n+1) (l,args))
      else match_args (n+1) (l,i :: args)
  | _, [] -> []
  | _ -> failwith "match_args"
  
let create_token_env is_mwe matching args =
  let l = List.rev matching in
  let args = match_args 1 (l,args) in
  let beg = (List.hd l).beg in
  let t = List.hd matching in
  let len = t.beg + t.len - beg in
  {empty_token_env with
    orth=concat_orths l;
    beg=beg;
    len=len;
    next=t.next;
    args=args;
    attrs=(if is_mwe then [MWE] else []) @ Tokenizer.merge_attrs l}
  
let create_lemma matching = function
    Str s -> s
  | Concat -> Patterns.concat_orths2 matching
  | ConcatSpace -> Patterns.concat_orths_space matching
  | ConcatInt -> Patterns.concat_intnum matching
  
let create_token is_mwe (matching:token_env list) sels = function
    MakeLemma(prod_lemma, pos, interp, args) ->
      let t = create_token_env is_mwe matching args in
      let lemma = create_lemma (List.rev matching) prod_lemma in
      let tags = Tagset.validate lemma pos [Xlist.map interp (function
          S s -> (try Xlist.assoc sels s with Not_found -> ["_"])
        | V s -> s
        | G -> ["_"])] in
    List.flatten (Xlist.map tags (fun tags ->
      Xlist.map (Lemmatization.get_ontological_category lemma pos tags) (fun (is_in_lexicon,has_no_sgjp_tag,has_poss_ndm_tag,has_exact_case_tag,cat,tags) ->
        {t with token = Lemma(lemma,pos,[tags],cat)})))
  | MakeIdeogram(prod_lemma, mode, args) ->
      let t = create_token_env is_mwe matching args in
      let lemma = create_lemma (List.rev matching) prod_lemma in
      [{t with token = Ideogram(lemma,mode)}]
  | MakeInterp(prod_lemma, args) ->
      let t = create_token_env is_mwe matching args in
      let lemma = create_lemma (List.rev matching) prod_lemma in
      [{t with token = Interp lemma}]
  
let is_lemma t =
  match t.token with Lemma _ -> true | _ -> false
    
let add_token2 paths t =
 let map = try IntMap.find paths t.beg with Not_found -> IntMap.empty in
  let map = IntMap.add_inc map t.next (TokenEnvSet.singleton t) (fun set -> TokenEnvSet.add set t) in
  IntMap.add paths t.beg map

let add_token (paths,l) t =
(*   if is_lemma t && Tokenizer.get_cat t.token <> "MWEcomponent" then paths, t :: l else *) (* kłóci się z LemStar *)
  add_token2 paths t, l

let apply_rule paths (is_mwe,match_list,prod) =
  (* print_endline ("apply_rule: " ^ lemma); *)
  let matchings_found = match_path paths match_list in
  Xlist.fold matchings_found paths (fun paths (matching,sels) ->
    try
      Xlist.fold (create_token is_mwe matching sels prod) paths add_token2
    with Not_found -> paths)

let count_path_size paths =
  IntMap.fold paths 0 (fun n _ map2 ->
    IntMap.fold map2 n (fun n _ set ->
      TokenEnvSet.size set + n))

let process (paths,last) =
  (* print_endline ("MWE.process 1 |paths|=" ^ string_of_int (Xlist.size paths)); *)
  let paths,rest = Xlist.fold paths (IntMap.empty,[]) add_token in
  (* print_endline ("MWE.process 2 |paths|=" ^ string_of_int (count_path_size paths)); *)
  let rules = select_rules paths !mwe_dict !mwe_dict2 in
  (* print_endline ("MWE.process 3 |rules|=" ^ string_of_int (Xlist.size rules)); *)
  let paths = Xlist.fold rules paths apply_rule in
  (* print_endline ("MWE.process 4 |paths|=" ^ string_of_int (count_path_size paths)); *)
  let rules = select_rules paths !mwe_dict !mwe_dict2 in
  (* print_endline ("MWE.process 5 |rules|=" ^ string_of_int (Xlist.size rules)); *)
  let paths = Xlist.fold rules paths apply_rule in
  (* print_endline ("MWE.process 6 |paths|=" ^ string_of_int (count_path_size paths)); *)
  let rules = select_rules paths !mwe_dict !mwe_dict2 in
  (* print_endline ("MWE.process 7 |rules|=" ^ string_of_int (Xlist.size rules)); *)
  let paths = Xlist.fold rules paths apply_rule in
  (* print_endline "MWE.process 8"; *)
  let rules = select_rules paths !mwe_dict !mwe_dict2 in
  (* print_endline "MWE.process 9"; *)
  let paths = Xlist.fold rules paths apply_rule in
  (* print_endline "MWE.process 10"; *)
  let paths = IntMap.fold paths rest (fun paths _ map ->
    IntMap.fold map paths (fun paths _ l ->
      TokenEnvSet.fold l paths (fun paths t ->
        t :: paths))) in
  (* print_endline "MWE.process 11"; *)
  Patterns.sort (paths,last)

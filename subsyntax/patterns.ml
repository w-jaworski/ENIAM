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
open Printf
open SubsyntaxTypes

let rec flatten_tokens rev_variants = function
  | [] -> rev_variants
  | Token t :: l -> flatten_tokens (Xlist.map rev_variants (fun rev_variant -> Token t :: rev_variant)) l
  | Seq seq :: l -> flatten_tokens rev_variants (seq @ l)
  | Variant variants :: l -> flatten_tokens (List.flatten (Xlist.map variants (fun variant -> flatten_tokens rev_variants [variant]))) l

let rec normalize_tokens rev = function
    [] -> List.rev rev
  | Token t :: l -> normalize_tokens (Token t :: rev) l
  | Seq seq :: l -> normalize_tokens rev (seq @ l)
  | Variant[t] :: l -> normalize_tokens rev (t :: l)
  | Variant variants :: l ->
      let variants = flatten_tokens [[]] [Variant variants] in
      let variants = Xlist.map variants (fun rev_seq ->
        match List.rev rev_seq with
          [] -> failwith "normalize_tokens"
        | [t] -> t
        | seq -> Seq seq) in
      let t = match variants with
          [] -> failwith "normalize_tokens"
        | [t] -> t
        | variants -> Variant variants in
      normalize_tokens (t :: rev) l

let concat_orths l =
  String.concat "" (Xlist.map l (fun t -> t.orth))

let concat_orths2 l =
  String.concat "" (Xlist.map l (fun t -> Tokenizer.get_orth t.token))

let concat_intnum l = 
  String.concat "" (Xlist.map l (function
      {token=Ideogram(v,_)} -> v
	| {token=Interp "-"} -> "-"
	| _ -> ""))
(*function
    [{token=Ideogram(v4,_)};_;{token=Ideogram(v3,_)};_;{token=Ideogram(v2,_)};_;{token=Ideogram(v1,_)}] -> v4^v3^v2^v1
  | [{token=Ideogram(v3,_)};_;{token=Ideogram(v2,_)};_;{token=Ideogram(v1,_)}] -> v3^v2^v1
  | [{token=Ideogram(v2,_)};_;{token=Ideogram(v1,_)}] -> v2^v1
  | [{token=Ideogram(v1,_)}] -> v1
  | [{token=Interp "-"};{token=Ideogram(v4,_)};_;{token=Ideogram(v3,_)};_;{token=Ideogram(v2,_)};_;{token=Ideogram(v1,_)}] -> "-"^v4^v3^v2^v1
  | [{token=Interp "-"};{token=Ideogram(v3,_)};_;{token=Ideogram(v2,_)};_;{token=Ideogram(v1,_)}] -> "-"^v3^v2^v1
  | [{token=Interp "-"};{token=Ideogram(v2,_)};_;{token=Ideogram(v1,_)}] -> "-"^v2^v1
  | [{token=Interp "-"};{token=Ideogram(v1,_)}] -> "-"^v1
  | _ -> failwith "concat_intnum"*)

let dig_value t =
  match t.token with
    Ideogram(v,_) -> v
  | _ -> failwith "dig_value"

(*let rec make_tys n t = 
  match t.token,t.args with
    Ideogram(v,"intnum"),[] -> Ideogram(v ^ String.make n '0',"intnum"),[]
  | Ideogram(v,"realnum"),[] ->
     (* print_endline ("make_tys: v='" ^ v ^ "'"); *)
     let a,b = match Xstring.split "," v with [a;b] -> a,b | [a] -> a,"" | _ -> failwith "make_tys" in
     (* print_endline ("make_tys: '" ^ a ^ "' '" ^ b ^ "'"); *)
     let a = if a = "0" then "" else if a = "-0" then "-" else a in
     if String.length b > n then Ideogram(a ^ String.sub b 0 n ^ "," ^ String.sub b n (String.length b - n),"realnum"),[] else
     Ideogram(a ^ b ^ String.make (n-String.length b) '0',"intnum"),[]
  | Ideogram(s,"intnum-interval"),[x;y] -> Ideogram(s,"intnum-interval"),[{t with token=fst (make_tys n x)}; {t with token=fst (make_tys n y)}]  (* FIXME: orth dla inverval *)
  | Ideogram(s,"realnum-interval"),[x;y] -> Ideogram(s,"realnum-interval"),[{t with token=fst (make_tys n x)}; {t with token=fst (make_tys n y)}]  (* FIXME: orth dla inverval *)
  | _ -> failwith "make_tys"*)

let html_patterns = [ (* FIXME: Poniższe nie zadziałają zwn. na usuwanie nielematyzowanych napisów *)
  [N "<"; Letters; N ">"], (function l -> Ideogram(concat_orths2 l,"html-tag"),[]);
  [N "<"; N "/"; Letters; N ">"], (function l -> Ideogram(concat_orths2 l,"html-tag"),[]);
  [N "<"; Letters; N "/"; N ">"], (function l -> Ideogram(concat_orths2 l,"html-tag"),[]);
]


type matching = {
  prefix: tokens list;
  matched: token_env list;
  suffix: tokens list;
  pattern: pat list;
  command: token_env list -> token * token_env list;
  command_abr: token_env list -> tokens list;
  }

let execute_command matching =
  let l = List.rev matching.matched in
  let len = Xlist.fold l 0 (fun len t -> t.len + len) in
  let token,args = matching.command l in
  Seq((List.rev matching.prefix) @ [Token{empty_token_env with
    orth=concat_orths l;
    beg=(List.hd l).beg;
    len=len;
    next=(List.hd l).beg+len;
    token=token;
    args=args;
    weight=0.; (* FIXME: dodać wagi do konkretnych reguł i uwzględnić wagi maczowanych tokenów *)
    attrs=Tokenizer.merge_attrs l}] @ matching.suffix)

let execute_abr_command matching =
  let l = List.rev matching.matched in
  Seq((List.rev matching.prefix) @ (matching.command_abr l) @ matching.suffix)

let rec check_interp sels = function
    [],[] -> true
  | s :: interp, ["_"] :: interp2 -> check_interp sels (interp,interp2)
  | V l :: interp, l2 :: interp2 -> 
      let b = Xlist.fold l false (fun b s -> b || Xlist.mem l2 s) in
      if b then check_interp sels (interp,interp2) else false
  | S s :: interp, l2 :: interp2 ->
      (try
        let l = Xlist.assoc sels s in
        let b = Xlist.fold l false (fun b s -> Xlist.mem l2 s || b) in
        if b then check_interp sels (interp,interp2) else false
      with Not_found -> check_interp sels (interp,interp2))
  | G :: interp, l2 :: interp2 -> check_interp sels (interp,interp2)
  | [],l -> failwith ("check_interp 1: " ^ Tagset.render [l])
  | _,l -> failwith ("check_interp 2 (possible bug in mwe dict): " ^ Tagset.render [l])

let rec get_sels sels = function
    [],[] -> sels
  | s :: interp, ["_"] :: interp2 -> get_sels sels (interp,interp2)
  | V l :: interp, l2 :: interp2 -> get_sels sels (interp,interp2)
  | S s :: interp, l2 :: interp2 ->
      (try
        let l = Xlist.assoc sels s in
        let sels = List.remove_assoc s sels in
        let l = Xlist.fold l [] (fun l s -> if Xlist.mem l2 s then s :: l else l) in
        get_sels ((s,l) :: sels) (interp,interp2)
      with Not_found -> get_sels ((s,l2) :: sels) (interp,interp2))
  | G :: interp, l2 :: interp2 -> get_sels sels (interp,interp2)
  | _ -> failwith "get_sels"

let match_token sels = function
    I m, Ideogram(_,m2) -> if m = m2 then [sels] else raise Not_found
  | C c, Lemma(_,_,_,c2) -> (*print_endline "match_token 1";*) if c = c2 then [sels] else raise Not_found
(*   | C c, t -> print_endline ("match_token 2: " ^ (SubsyntaxStringOf.string_of_token t)); raise Not_found *)
  | Sym s, Symbol s2 -> if s = s2 then [sels] else raise Not_found
  | O pat, Ideogram(s,"dig") -> if pat = s then [sels] else raise Not_found
  | O pat, Interp s -> if pat = s then [sels] else raise Not_found
  | O pat, SmallLetter(uc,lc) -> if pat = lc then [sels] else raise Not_found
  | O pat, CapLetter(uc,lc) -> if pat = uc then [sels] else raise Not_found
  | O pat, AllSmall(uc,fc,lc) -> if pat = lc then [sels] else raise Not_found
  | O pat, AllCap(uc,fc,lc) -> if pat = uc then [sels] else raise Not_found
  | O pat, FirstCap(uc,fc,lc) -> if pat = fc then [sels] else raise Not_found
  | O pat, SomeCap(uc,orth,lc) -> if pat = orth then [sels] else raise Not_found
  | T pat, Ideogram(s,"dig") -> if pat = s then [sels] else raise Not_found
  | T pat, Interp s -> if pat = s then [sels] else raise Not_found
  | T pat, SmallLetter(uc,lc) -> if pat = uc || pat = lc then [sels] else raise Not_found
  | T pat, CapLetter(uc,lc) -> if pat = uc || pat = lc then [sels] else raise Not_found
  | T pat, AllSmall(uc,fc,lc) -> if pat = uc || pat = fc || pat = lc then [sels] else raise Not_found
  | T pat, AllCap(uc,fc,lc) -> if pat = uc || pat = fc || pat = lc then [sels] else raise Not_found
  | T pat, FirstCap(uc,fc,lc) -> if pat = uc || pat = fc || pat = lc then [sels] else raise Not_found
  | T pat, SomeCap(uc,orth,lc) -> if pat = uc || pat = orth || pat = lc then [sels] else raise Not_found
  | N ".", Symbol "." -> [sels]
  | N ".", _ -> raise Not_found
  | N " ", Symbol " " -> [sels]
  | N " ", _ -> raise Not_found
  | N pat, Interp s -> if pat = s then [sels] else raise Not_found
  | Lem(lemma,pos,interp), Lemma(lemma2,pos2,interps2,_) ->
      let found = Xlist.fold interps2 [] (fun found interp2 ->
        if lemma=lemma2 && pos=pos2 && check_interp sels (interp,interp2) then
          (get_sels sels (interp,interp2)) :: found else found) in
	  if found = [] then raise Not_found else found (* FIXME *)
  | Letters, SmallLetter _ -> [sels]
  | Letters, CapLetter _ -> [sels]
  | Letters, AllSmall _ -> [sels]
  | Letters, AllCap _ -> [sels]
  | Letters, FirstCap _ -> [sels]
  | Letters, SomeCap _ -> [sels]
  | CapLet, CapLetter _ -> [sels]
  | SmallLet, SmallLetter _ -> [sels]
  | _ -> raise Not_found

let match_token_env sels = function
    NSP pat, t -> if t.beg + t.len = t.next then match_token sels (pat,t.token) else raise Not_found
  | SP pat, t -> if t.beg + t.len = t.next then raise Not_found else match_token sels (pat,t.token) 
  | pat, t -> match_token sels (pat, t.token)
  
let rec find_first_token matching pat = function
    Token t -> (try let _ = match_token [] (pat,t.token) in [{matching with matched = t :: matching.matched}] with Not_found -> [])
  | Seq l -> Xlist.map (find_first_token matching pat (List.hd (List.rev l))) (fun matching -> {matching with prefix = matching.prefix @ (List.tl (List.rev l))})
  | Variant l -> List.flatten (Xlist.map l (find_first_token matching pat))

let rec find_middle_token matching pat = function
    Token t -> (try let _ = match_token [] (pat,t.token) in [{matching with matched = t :: matching.matched}] with Not_found -> [])
  | Seq _ -> []
  | Variant l -> List.flatten (Xlist.map l (find_middle_token matching pat))

let rec find_last_token matching pat = function
    Token t -> (try let _ = match_token [] (pat,t.token) in [{matching with matched = t :: matching.matched}] with Not_found -> [])
  | Seq l -> Xlist.map (find_last_token matching pat (List.hd l)) (fun matching -> {matching with suffix = matching.suffix @ (List.tl l)})
  | Variant l -> List.flatten (Xlist.map l (find_last_token matching pat))

let rec find_pattern_tail matchings = function
    [] -> raise Not_found
  | token :: l ->
      let found,finished = Xlist.fold matchings ([],[]) (fun (found,finished) matching ->
        match matching.pattern with
          [pat] -> found, (find_last_token {matching with pattern=[]} pat token) @ finished
        | pat :: pattern -> (find_middle_token {matching with pattern=pattern} pat token) @ found, finished
        | _ -> failwith "find_pattern: ni") in
      (try
        if found = [] then raise Not_found else
        find_pattern_tail found l
      with Not_found ->
        let finished = List.flatten (Xlist.map finished (fun matching -> try [execute_command matching] with Not_found -> [])) in
        if finished = [] then raise Not_found else Variant finished,l)

(* wzorce nie mogą mieć długości 1 *)
let rec find_pattern matchings rev = function
    token :: l ->
      let found = Xlist.fold matchings [] (fun found matching ->
        match matching.pattern with
          pat :: pattern -> (find_first_token {matching with pattern=pattern} pat token) @ found
        | [] -> failwith "find_pattern: empty pattern") in
      if found = [] then find_pattern matchings (token :: rev) l else
      (try
        let token,l = find_pattern_tail found l in
        find_pattern matchings (token :: rev) l
      with Not_found -> find_pattern matchings (token :: rev) l)
  | [] -> List.rev rev

let find_patterns patterns tokens =
  find_pattern (Xlist.map patterns (fun (pattern,command) ->
    {prefix=[]; matched=[]; suffix=[]; pattern=pattern; command=command; command_abr=(fun _ -> [])})) [] tokens

let rec find_abr_pattern_tail matchings = function
    [] -> raise Not_found
  | token :: l ->
      (* print_endline ("find_abr_pattern_tail 1: " ^ SubsyntaxStringOf.string_of_tokens 0 token); *)
      let found,finished = Xlist.fold matchings ([],[]) (fun (found,finished) matching ->
        match matching.pattern with
          [pat] -> (*print_endline ("find_abr_pattern_tail 2a:");*) (find_last_token {matching with pattern=[]} pat token) @ found, finished
        | pat :: pattern -> (*print_endline ("find_abr_pattern_tail 2b:");*) (find_middle_token {matching with pattern=pattern} pat token) @ found, finished
        | [] -> (*print_endline "find_abr_pattern_tail 2c: []";*) found, matching :: finished) in
      (try
        (* print_endline "find_abr_pattern_tail 3"; *)
        if found = [] then raise Not_found else
        find_abr_pattern_tail found l
      with Not_found ->
        (* print_endline "find_abr_pattern_tail 4"; *)
        let finished = List.flatten (Xlist.map finished (fun matching -> try [execute_abr_command matching] with Not_found -> [])) in
        if finished = [] then raise Not_found else Variant finished,token :: l)

let rec find_abr_pattern matchings rev = function
    token :: l ->
      (* print_endline ("find_abr_pattern 1: " ^ SubsyntaxStringOf.string_of_tokens 0 token); *)
      let found = Xlist.fold matchings [] (fun found matching ->
        match matching.pattern with
          pat :: pattern -> (find_first_token {matching with pattern=pattern} pat token) @ found
        | [] -> failwith "find_abr_pattern: empty pattern") in
      if found = [] then find_abr_pattern matchings (token :: rev) l else
      (try
        let token,l = find_abr_pattern_tail found l in
        find_abr_pattern matchings (token :: rev) l
      with Not_found -> find_abr_pattern matchings (token :: rev) l)
  | [] -> List.rev rev

let find_abr_patterns patterns tokens =
  (* Xlist.iter tokens (fun token -> print_endline ("A " ^ SubsyntaxStringOf.string_of_tokens 0 token)); *)
  let tokens = find_abr_pattern (Xlist.map patterns (fun (pattern,command) ->
    {prefix=[]; matched=[]; suffix=[]; pattern=pattern; command=(fun _ -> Symbol "",[]); command_abr=command})) [] tokens in
  (* Xlist.iter tokens (fun token -> print_endline ("B " ^ SubsyntaxStringOf.string_of_tokens 0 token)); *)
  tokens


(*exception PatternFound

let query_beg_patterns = [
  [I "<query>";I "<sentence>"];
  [I "<query>";I "„s";I "<sentence>"];
  [I "<query>";I "<or>";I "<sentence>"];
  ]

let query_end_patterns = [
  [I "</sentence>";I "</query>"];
  [I "</sentence>";I "”s";I "</query>"];
  ]

let find_beg_pattern pattern tokens =
  try
    let _ = find_pattern_tail [{prefix=[]; matched=[]; suffix=[];
         pattern=pattern; command=(fun _ -> raise PatternFound);
         command_abr=(fun _ -> [])}] tokens in false
  with PatternFound -> true | Not_found -> false

let replace_beg_pattern pattern command tokens =
  try
    let t,l = find_abr_pattern_tail [{prefix=[]; matched=[]; suffix=[];
         pattern=pattern; command=(fun _ -> Symbol "");
         command_abr=command}] tokens in
    t :: l
  with Not_found -> failwith "replace_beg_pattern"

(* let s_beg i = {empty_token_env with beg=i;len=1;next=i+1; token=Interp "<sentence>"}
let c_beg i = {empty_token_env with beg=i;len=1;next=i+1; token=Interp "<clause>"} *)
let s_end i = Token{empty_token_env with beg=i;len=1;next=i+1; token=Interp "</sentence>"}
let c_end i = Token{empty_token_env with beg=i;len=1;next=i+1; token=Interp "</clause>"}

let add_sentence_beg = function
    [q;t] -> let next=t.next in [Token q;Token{t with len=t.len-2;next=next-2};Tokenizer.s_beg (next-2);Tokenizer.c_beg (next-1)]
  | [q] -> let next=q.next in [Token{q with len=q.len-2;next=next-2};Tokenizer.s_beg (next-2);Tokenizer.c_beg (next-1)]
  | _ -> failwith "add_sentence_beg"

let add_sentence_end = function
    [q;t] -> let beg=t.beg in [Token q;Token{t with len=t.len-2;beg=beg+2};s_end (beg+1);c_end beg]
  | [q] -> let beg=q.beg in [Token{q with len=q.len-2;beg=beg+2};s_end (beg+1);c_end beg]
  | _ -> failwith "add_sentence_end"

let rec revert_tokens = function
    Token t -> Token t
  | Seq l -> Seq(Xlist.rev_map l revert_tokens)
  | Variant l -> Variant(Xlist.map l revert_tokens)

let manage_query_boundaries tokens =
  (* let b =
    try
      let _ = find_pattern_tail (Xlist.map query_beg_patterns (fun pattern ->
        {prefix=[]; matched=[]; suffix=[];
         pattern=pattern; command=(fun _ -> raise PatternFound);
         command_abr=(fun _ -> [])})) tokens in false
    with PatternFound -> true | Not_found -> false in
  (if b then print_endline "sentence beg found" else print_endline "sentence beg not found"); *)
  let tokens =
    if find_beg_pattern [I "<query>";I "„s"] tokens then
      if find_beg_pattern [I "<query>";I "„s";I "<sentence>"] tokens then tokens else
      replace_beg_pattern [I "<query>";I "„s"] add_sentence_beg tokens else
    if find_beg_pattern [I "<query>";I "<or>"] tokens then
      if find_beg_pattern [I "<query>";I "<or>";I "<sentence>"] tokens then tokens else
      replace_beg_pattern [I "<query>";I "<or>"] add_sentence_beg tokens else
    if find_beg_pattern [I "<query>";I "(s";I "<sentence>"] tokens then tokens else
    if find_beg_pattern [I "<query>";I "<sentence>"] tokens then tokens else
    replace_beg_pattern [I "<query>"] add_sentence_beg tokens in
  (* let b =
    try
      let _ = find_pattern (Xlist.map query_end_patterns (fun pattern ->
        {prefix=[]; matched=[]; suffix=[];
         pattern=pattern; command=(fun _ -> raise PatternFound);
         command_abr=(fun _ -> [])})) [] tokens in false
    with PatternFound -> true in
  (if b then print_endline "sentence end found" else print_endline "sentence end not found"); *)
  let tokens = Xlist.rev_map tokens revert_tokens in
  let tokens =
    if find_beg_pattern [I "</query>";I "</sentence>"] tokens then tokens else
    if find_beg_pattern [I "</query>";I "”s";I "</sentence>"] tokens then tokens else
    if find_beg_pattern [I "</query>";I "”s"] tokens then
      replace_beg_pattern [I "</query>";I "”s"] add_sentence_end tokens else
    if find_beg_pattern [I "</query>";I ")s"(*;I "</sentence>"*)] tokens then tokens else
    replace_beg_pattern [I "</query>"] add_sentence_end tokens in
  let tokens = Xlist.rev_map tokens revert_tokens in
  tokens*)


let rec set_next_id n = function
    Token t -> Token{t with next=n}
  | Seq l ->
      (match List.rev l with
        t :: l -> Seq(List.rev ((set_next_id n t) :: l))
      | [] -> failwith "set_next_id n")
  | Variant l -> Variant(Xlist.map l (set_next_id n))

let rec remove_spaces rev = function
    [] -> List.rev rev
  | x :: Token{token=Symbol " "; next=n} ::  l -> remove_spaces rev ((set_next_id n x) :: l)
  | x :: Token{token=Symbol "\t"; next=n} ::  l -> remove_spaces rev ((set_next_id n x) :: l)
  | x :: Token{token=Symbol "\n"; next=n} ::  l -> remove_spaces rev ((set_next_id n x) :: l)
  | x :: Token{token=Symbol "\r"; next=n} ::  l -> remove_spaces rev ((set_next_id n x) :: l)
  | x :: Token{token=Ideogram("<b>","html-tag"); next=n} ::  l -> remove_spaces rev ((set_next_id n x) :: l)
  | x :: Token{token=Ideogram("</b>","html-tag"); next=n} ::  l -> remove_spaces rev ((set_next_id n x) :: l)
  | x :: Token{token=Ideogram("<i>","html-tag"); next=n} ::  l -> remove_spaces rev ((set_next_id n x) :: l)
  | x :: Token{token=Ideogram("</i>","html-tag"); next=n} ::  l -> remove_spaces rev ((set_next_id n x) :: l)
  | x :: Token{token=Ideogram("<u>","html-tag"); next=n} ::  l -> remove_spaces rev ((set_next_id n x) :: l)
  | x :: Token{token=Ideogram("</u>","html-tag"); next=n} ::  l -> remove_spaces rev ((set_next_id n x) :: l)
  | x :: Token{token=Ideogram("<br/>","html-tag"); next=n} ::  l -> remove_spaces rev ((set_next_id n x) :: l)
  | Token{token=Symbol " "} :: l -> remove_spaces rev l
  | Token{token=Symbol "\t"} :: l -> remove_spaces rev l
  | Token{token=Symbol "\n"} :: l -> remove_spaces rev l
  | Token{token=Symbol "\r"} :: l -> remove_spaces rev l
  | Token{token=Ideogram("<b>","html-tag")} :: l -> remove_spaces rev l
  | Token{token=Ideogram("</b>","html-tag")} :: l -> remove_spaces rev l
  | Token{token=Ideogram("<i>","html-tag")} :: l -> remove_spaces rev l
  | Token{token=Ideogram("</i>","html-tag")} :: l -> remove_spaces rev l
  | Token{token=Ideogram("<u>","html-tag")} :: l -> remove_spaces rev l
  | Token{token=Ideogram("</u>","html-tag")} :: l -> remove_spaces rev l
  | Token{token=Ideogram("<br/>","html-tag")} :: l -> remove_spaces rev l
  | x :: l -> remove_spaces (x :: rev) l

(*let create_sentence_end_beg i len next orth =
  Seq[Token{empty_token_env with beg=i;len=20;next=i+20;token=Interp "</clause>"};
      Token{empty_token_env with orth=orth;beg=i+20;len=20;next=i+40;token=Interp "</sentence>"};
      Token{empty_token_env with beg=i+40;len=20;next=i+60;token=Interp "<sentence>"};
      Token{empty_token_env with beg=i+60;len=len-60;next=next;token=Interp "<clause>"}]

let create_clause_end_beg i len next orth =
  Seq[Token{empty_token_env with beg=i;len=60;next=i+60;token=Interp "</clause>"};
      Token{empty_token_env with beg=i+60;len=len-60;next=next;token=Interp "<clause>"}]

let process_interpunction_token t = function
    Interp "." -> Variant [create_sentence_end_beg t.beg t.len t.next t.orth; Token t]
  | Interp "," -> Variant [create_clause_end_beg t.beg t.len t.next t.orth; Token t]
  | Interp ":" -> Variant [create_clause_end_beg t.beg t.len t.next t.orth; create_sentence_end_beg t.beg t.len t.next t.orth; Token t]
  | Interp ";" -> Variant [create_sentence_end_beg t.beg t.len t.next t.orth; Token t]
  | Interp "¶" -> Variant [create_clause_end_beg t.beg t.len t.next t.orth; create_sentence_end_beg t.beg t.len t.next t.orth]
  | Interp "<query>" -> Seq[
      Token{empty_token_env with orth=t.orth; beg=t.beg;len=60;next=t.beg+60;token=Interp "<query>"};
      Token{empty_token_env with beg=t.beg+60;len=20;next=t.beg+80;token=Interp "<sentence>"};
      Token{empty_token_env with beg=t.beg+80;len=20;next=t.next;token=Interp "<clause>"}]
  | Interp "</query>" -> Seq[
      Token{empty_token_env with beg=t.beg;len=20;next=t.beg+20;token=Interp "</clause>"};
      Token{empty_token_env with beg=t.beg+20;len=20;next=t.beg+40;token=Interp "</sentence>"};
      Token{empty_token_env with orth=t.orth; beg=t.beg+40;len=60;next=t.next;token=Interp "</query>"}]
  | _ -> Token t

let rec process_interpunction rev = function
    [] -> List.rev rev
  | Token t :: l -> process_interpunction ((process_interpunction_token t t.token) :: rev) l
  | Seq seq :: l -> process_interpunction (Seq(process_interpunction [] seq) :: rev) l
  | Variant variants :: l ->
      process_interpunction (Variant(process_interpunction [] variants) :: rev) l*)
  
let parse query = 
  let l = Xunicode.classified_chars_of_utf8_string query in
  let l = Tokenizer.tokenize l in
  let l = normalize_tokens [] l in
(*  Xlist.iter l (fun t -> print_endline (SubsyntaxStringOf.string_of_tokens 0 t));*)
  let l = find_patterns html_patterns l in
  let l = normalize_tokens [] l in
(*   Xlist.iter l (fun t -> print_endline (SubsyntaxStringOf.string_of_tokens 0 t)); *)
  let l = normalize_tokens [] l in
  let l = remove_spaces [] l in
  l
  
(**********************************************************************************)

let compare_token_record p r =
  let v = compare p.beg r.beg in
  if v <> 0 then v else
  let v = compare p.next r.next in
  if v <> 0 then v else
  compare p r

let sort (paths,last) =
  Xlist.sort paths compare_token_record, last

let rec uniq_rec rev = function
    [] -> List.rev rev
  | [p] -> List.rev (p :: rev)
  | p :: r :: l -> 
(*      Printf.printf "uniq_rec 1: %s\n" (SubsyntaxStringOf.string_of_token_env p);
      Printf.printf "uniq_rec 2: %s\n" (SubsyntaxStringOf.string_of_token_env r);
      if p = r then Printf.printf "uniq_rec eq %d\n" (compare p r) else Printf.printf "uniq_rec neq %d\n" (compare p r);*)
      if p = r then uniq_rec rev (r :: l) else uniq_rec (p :: rev) (r :: l)

let uniq (paths,last) =
  uniq_rec [] paths, last

let rec translate_into_paths_rec paths = function
    Token t -> t :: paths
  | Seq l -> Xlist.fold l paths translate_into_paths_rec
  | Variant l -> Xlist.fold l paths translate_into_paths_rec

let translate_into_paths tokens =
  let paths = Xlist.fold tokens [] (fun paths token ->
    translate_into_paths_rec paths token) in
  let last = if paths = [] then 0 else (List.hd paths).next in
  let paths = sort (paths,last) in
  let paths = uniq paths in
  paths

let remove_inaccessible_tokens paths beg last =
  let set = Xlist.fold paths (IntSet.singleton beg) (fun set t ->
    if IntSet.mem set t.beg then IntSet.add set t.next else set) in
  if not (IntSet.mem set last) then raise (SubsyntaxTypes.BrokenPaths(beg,last,IntSet.max_elt set,paths)) else
  Xlist.fold paths [] (fun paths t ->
    if IntSet.mem set t.beg then t :: paths else paths)

let remove_category cat paths =
  List.rev (Xlist.fold paths [] (fun paths t ->
    if Tokenizer.get_cat t.token = cat then paths else t :: paths))
  

(**********************************************************************************)

let insert_left_list quant t l =
  let l,beg = Xlist.fold l ([],t.beg) (fun (l,beg) s ->
    {empty_token_env with beg=beg; len=quant; next=beg+quant; token=Interp s} :: l, beg+quant) in
  List.rev ({t with beg=beg; len=t.len-beg+t.beg} :: l)

let insert_right_list quant t l =
  let l,next = Xlist.fold (List.rev l) ([],t.beg+t.len) (fun (l,next) s ->
    {empty_token_env with beg=next-quant; len=quant; next=next; token=Interp s} :: l, next-quant) in
  let l = List.rev ({t with len=next-t.beg; next=next} :: l) in
  List.rev ({(List.hd l) with next=t.next} :: List.tl l)
  
let insert_both_list quant t ll rl = 
  let l = insert_right_list quant t rl in
  (insert_left_list quant (List.hd l) ll) @ (List.tl l)
  
let create_sentence_end_beg t =
  insert_both_list quant1 {t with token=Interp "</sentence>"} ["</clause>"] ["<sentence>"; "<clause>"]

let create_clause_end_beg t =
  insert_right_list quant1 {t with token=Interp "</clause>"} ["<clause>"]

let process_interpunction_token beg next t = 
  if t.beg = beg then 
    if t.next = next then insert_both_list quant1 t ["<sentence>"; "<clause>"] ["</clause>"; "</sentence>"]
    else 
      if t.token = Interp "<query>" then insert_right_list quant1 t  ["<sentence>"; "<clause>"]
      else insert_left_list quant1 t ["<sentence>"; "<clause>"]
  else 
    if t.next = next then match t.token with
        Interp "." -> insert_left_list quant1 {t with token=Interp "</sentence>"} ["</clause>"]
      | Interp "</query>" -> insert_left_list quant1 t ["</clause>"; "</sentence>"]
      | _ -> insert_right_list quant1 t ["</clause>"; "</sentence>"]
    else match t.token with 
        Interp "." -> t :: (create_sentence_end_beg t)
      | Lemma(",","conj",[[]],_) -> t :: (create_clause_end_beg t)
      | Interp ":" -> t :: (create_clause_end_beg t) @ (create_sentence_end_beg t)
      | Interp ";" -> t :: (create_sentence_end_beg t)
      | Interp "¶" -> t :: (create_clause_end_beg t) @ (create_sentence_end_beg t)
      | _ -> [t]

let insert_tokens map paths =
  List.flatten (Xlist.rev_map paths (fun t ->
    let l = Xlist.fold (try IntMap.find (IntMap.find map t.beg) t.next with Not_found -> []) [] (fun l (pat,ll,rl) ->
      if t = pat then (ll,rl) :: l else l) in
    if l = [] then [t] else
    let ll,rl = List.hd l in (* FIXME: potencjalny problem przy niejednoznaczności *)
    if t.len < 2 * quant2 then failwith "insert_tokens" else
    insert_both_list quant2 t ll rl))
      
(*let create_sentence_end_beg i len next orth =
  [{empty_token_env with beg=i;len=20;next=i+20;token=Interp "</clause>"};
   {empty_token_env with orth=orth;beg=i+20;len=20;next=i+40;token=Interp "</sentence>"};
   {empty_token_env with beg=i+40;len=20;next=i+60;token=Interp "<sentence>"};
   {empty_token_env with beg=i+60;len=len-60;next=next;token=Interp "<clause>"}]

let create_clause_end_beg i len next orth =
  [{empty_token_env with orth=orth;beg=i;len=60;next=i+60;token=Interp "</clause>"};
   {empty_token_env with beg=i+60;len=len-60;next=next;token=Interp "<clause>"}]*)

(*let process_interpunction_token beg next t = 
  if t.beg = beg then 
    if t.next = next then [
      {empty_token_env with beg=t.beg;len=20;next=t.beg+20;token=Interp "<sentence>"};
      {empty_token_env with beg=t.beg+20;len=20;next=t.beg+40;token=Interp "<clause>"};
      {t with beg=t.beg+40;len=t.len-80;next=t.beg+t.len-40};
      {empty_token_env with beg=t.beg+t.len-40;len=20;next=t.beg+t.len-20;token=Interp "</clause>"};
      {empty_token_env with beg=t.beg+t.len-20;len=20;next=t.next;token=Interp "</sentence>"}]
    else 
      if t.token = Interp "<query>" then [
        {t with len=t.len-40;next=t.beg+t.len-40};
        {empty_token_env with beg=t.beg+t.len-40;len=20;next=t.beg+t.len-20;token=Interp "<sentence>"};
        {empty_token_env with beg=t.beg+t.len-20;len=20;next=t.next;token=Interp "<clause>"}]
      else [
        {empty_token_env with beg=t.beg;len=20;next=t.beg+20;token=Interp "<sentence>"};
        {empty_token_env with beg=t.beg+20;len=20;next=t.beg+40;token=Interp "<clause>"};
        {t with beg=t.beg+40;len=t.len-40;next=t.next}]
  else 
    if t.next = next then match t.token with
        Interp "." -> [
            {empty_token_env with beg=t.beg;len=20;next=t.beg+20;token=Interp "</clause>"};
            {t with beg=t.beg+20;len=t.len-20;token=Interp "</sentence>"}]
      | Interp "</query>" -> [
            {empty_token_env with beg=t.beg;len=20;next=t.beg+20;token=Interp "</clause>"};
            {empty_token_env with beg=t.beg+20;len=20;next=t.beg+40;token=Interp "</sentence>"};
			{t with beg=t.beg+40;len=t.len-40}]
      | _ -> [
            {t with len=t.len-40;next=t.beg+t.len-40};
            {empty_token_env with beg=t.beg+t.len-40;len=20;next=t.beg+t.len-20;token=Interp "</clause>"};
            {empty_token_env with beg=t.beg+t.len-20;len=20;next=t.next;token=Interp "</sentence>"}]
    else match t.token with 
        Interp "." -> t :: (create_sentence_end_beg t.beg t.len t.next t.orth)
      | Lemma(",","conj",[[]],_) -> t :: (create_clause_end_beg t.beg t.len t.next t.orth)
      | Interp ":" -> t :: (create_clause_end_beg t.beg t.len t.next t.orth) @ (create_sentence_end_beg t.beg t.len t.next t.orth)
      | Interp ";" -> t :: (create_sentence_end_beg t.beg t.len t.next t.orth)
      | Interp "¶" -> t :: (create_clause_end_beg t.beg t.len t.next t.orth) @ (create_sentence_end_beg t.beg t.len t.next t.orth)
      | _ -> [t]*)

let rec process_interpunction beg next paths = 
  List.flatten (List.rev (Xlist.rev_map paths (fun t -> 
    process_interpunction_token beg next t)))

(**********************************************************************************)

(* Korzystamy z tego, że istnieje wierzchołek najmniejszy i największy *)
let rec biconnected_compontents_rec next found rev = function
    [] -> if rev = [] then found else ((*List.rev*) rev) :: found
  | t :: paths -> 
      if t.beg > next then failwith "biconnected_compontents_rec" else
      if t.beg = next then  
        biconnected_compontents_rec t.next (if rev = [] then found else ((*List.rev*) rev) :: found) [t] paths else
      biconnected_compontents_rec (max next t.next) found (t :: rev) paths

let biconnected_compontents paths =
  List.rev (biconnected_compontents_rec 0 [] [] paths)



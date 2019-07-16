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

open Printf
open SubsyntaxTypes
open Xstd
open Xunicode

let rec get_orth_list = function
    Token{orth=""} -> []
  | Token{orth="&alpha;"} -> ["α"]
  | Token{orth="&beta;"} -> ["β"]
  | Token{orth="&gamma;"} -> ["γ"]
  | Token{orth="&kappa;"} -> ["κ"]
  | Token{orth="&Delta;"} -> ["Δ"]
  | Token{orth="&mu;"} -> ["µ"]
  | Token{orth="&ge;"} -> ["≥"]
  | Token{orth="&le;"} -> ["≤"]
  | Token{orth="&gt;"} -> [">"]
  | Token{orth="&lt;"} -> ["<"]
  | Token{orth="&uarr;"} -> ["↑"]
  | Token{orth="&darr;"} -> ["↓"]
  | Token{orth="&deg;"} -> ["°"]
  | Token{orth="&tau;"} -> ["τ"]
  | Token{orth="&prop;"} -> ["∝"]
  | Token{orth="&iota;"} -> ["ι"]
  | Token{orth="&harr;"} -> ["↔"]
  | Token{orth="&larr;"} -> ["←"]
  | Token{orth="&infin;"} -> ["∞"]
  | Token{orth="&plusmn;"} -> ["±"]
  | Token{orth="&omicron;"} -> ["ο"]
  | Token{orth="&asymp;"} -> ["≈"]
  | Token t -> if Xlist.mem t.attrs HasAglSuffix then raise Not_found else [t.orth]
  | Variant l -> 
     let l = Xlist.fold l [] (fun l t -> 
       try get_orth_list t :: l with Not_found -> l) in
	 if l = [] then failwith "get_orth_list" else List.hd l
  | Seq l -> List.flatten (Xlist.map l get_orth_list)
  
let get_orth = function
    SmallLetter(uc,lc) -> lc
  | CapLetter(uc,lc) -> uc
  | AllSmall(uc,fc,lc) -> lc
  | AllCap(uc,fc,lc) -> uc
  | FirstCap(uc,fc,lc) -> fc
  | SomeCap(uc,orth,lc) -> orth
  | Symbol orth  -> orth
(*  | Ideogram(v,"roman") -> ""
  | Ideogram(v,"roman-month") -> ""*)
  | Ideogram(v,_) -> v
  | Other orth  -> orth
  | Interp orth  -> orth
  | Lemma(orth,_,_,_) -> orth
  | Tokens _ -> failwith "get_orth"

let get_orths = function
    SmallLetter(uc,lc) -> [uc;lc]
  | CapLetter(uc,lc) -> [uc;lc]
  | AllSmall(uc,fc,lc) -> [uc;fc;lc]
  | AllCap(uc,fc,lc) -> [uc;fc;lc]
  | FirstCap(uc,fc,lc) -> [uc;fc;lc]
  | SomeCap(uc,orth,lc) -> [uc;orth;lc]
  | Symbol orth  -> [orth]
(*  | Ideogram(v,"roman") -> []
  | Ideogram(v,"roman-month") -> []*)
  | Ideogram(v,_) -> [v]
  | Other orth  -> [orth]
  | Interp orth  -> [orth]
  | Lemma(orth,_,_,_) -> [orth]
  | Tokens _ -> failwith "get_orths"

let rec get_lemma = function
    Interp orth -> orth
  | Lemma(lemma,_,_,_) -> lemma
  | _ -> ""

let rec get_pos = function
    Interp _ -> "interp"
  | Lemma(lemma,pos,_,_) -> pos
  | _ -> ""

let rec get_cat = function
    Lemma(_,_,_,cat) -> cat
  | _ -> ""

let rec get_interp = function
    Interp orth -> ""
  | Lemma(lemma,cat,interp,_) -> Tagset.render interp
  | _ -> ""

let months = StringSet.of_list ["1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "01"; "02"; "03"; "04"; "05"; "06"; "07"; "08"; "09"; "10"; "11"; "12"]
let hours = StringSet.of_list ["0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "00"; "01"; "02"; "03"; "04"; "05"; "06"; "07"; "08"; "09";
   "10"; "11"; "12"; "13"; "14"; "15"; "16"; "17"; "18"; "19"; "20"; "21"; "22"; "23"; "24"]
let days = StringSet.of_list ["1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "01"; "02"; "03"; "04"; "05"; "06"; "07"; "08"; "09";
   "10"; "11"; "12"; "13"; "14"; "15"; "16"; "17"; "18"; "19"; "20"; "21"; "22"; "23"; "24"; "25"; "26"; "27"; "28"; "29"; "30"; "31"]
let romanmonths = StringSet.of_list ["I"; "II"; "III"; "IV"; "V"; "VI"; "VII"; "VIII"; "IX"; "X"; "XI"; "XII"]

let dig_token orth i digs token =
  Token{empty_token_env with orth=orth;beg=i;len=Xlist.size digs * factor;next=i+Xlist.size digs * factor;
    token=token; attrs=[MaybeCS]}

let dig_tokens orth i digs v cat =
  [Token{empty_token_env with orth=orth;beg=i;len=Xlist.size digs * factor;next=i+Xlist.size digs * factor;
    token=Ideogram(v,cat); attrs=[MaybeCS]}]

let merge_digits i digs =
  let orth = String.concat "" digs in
  let t = dig_tokens orth i digs in
  let v = try Printf.sprintf "%02d" (int_of_string orth) with _ -> failwith "merge_digits" in
  let variants =
    (t orth "dig") @
    (if List.hd digs <> "0" then (t orth "posnum") else []) @
    (if digs = ["0"] || List.hd digs <> "0" then (t orth "natnum") else []) @
    (if StringSet.mem months orth then (t v "month") else []) @
    (if StringSet.mem hours orth then (t v "hour") else []) @
    (if StringSet.mem days orth then (t v "day") else []) @
    (if Xlist.size digs = 2 && List.hd digs < "6" then (t v "minute") else []) @
    (t orth (string_of_int (Xlist.size digs) ^ "dig")) @
    (if Xlist.size digs <= 3 && List.hd digs <> "0" then (t orth "pref3dig") else []) in
  Variant variants

let recognize_roman_I v = function
    Capital("I",_) :: Capital("I",_) :: Capital("I",_) :: [] -> v+3,false
  | Capital("I",_) :: Capital("I",_) :: [] -> v+2,false
  | Capital("I",_) :: [] -> v+1,false
  | [] -> v,false
  | Capital("I",_) :: Capital("I",_) :: Capital("I",_) :: Small(_,"w") :: [] -> v+3,true
  | Capital("I",_) :: Capital("I",_) :: Small(_,"w") :: [] -> v+2,true
  | Capital("I",_) :: Small(_,"w") :: [] -> v+1,true
  | Small(_,"w") :: [] -> v,true
  | _ -> 0,false

let recognize_roman_V v = function
    Capital("I",_) :: ForeignCapital("V",_) :: [] -> v+4,false
  | ForeignCapital("V",_) :: l -> recognize_roman_I (v+5) l
  | Capital("I",_) :: ForeignCapital("X",_) :: [] -> v+9,false
  | Capital("I",_) :: ForeignCapital("V",_) :: Small(_,"w") :: [] -> v+4,true
  | Capital("I",_) :: ForeignCapital("X",_) :: Small(_,"w") :: [] -> v+9,true
  | l -> recognize_roman_I v l

let recognize_roman_X v = function
  | ForeignCapital("X",_) :: ForeignCapital("X",_) :: ForeignCapital("X",_) :: l -> recognize_roman_V (v+30) l
  | ForeignCapital("X",_) :: ForeignCapital("X",_) :: l -> recognize_roman_V (v+20) l
  | ForeignCapital("X",_) :: l -> recognize_roman_V (v+10) l
  | l -> recognize_roman_V v l

let recognize_roman_L v = function
    ForeignCapital("X",_) :: Capital("L",_) :: l -> recognize_roman_V (v+40) l
  | Capital("L",_) :: l -> recognize_roman_X (v+50) l
  | ForeignCapital("X",_) :: Capital("C",_) :: l -> recognize_roman_V (v+90) l
  | l -> recognize_roman_X v l

let recognize_roman_C v = function
  | Capital("C",_) :: Capital("C",_) :: Capital("C",_) :: l -> recognize_roman_L (v+300) l
  | Capital("C",_) :: Capital("C",_) :: l -> recognize_roman_L (v+200) l
  | Capital("C",_) :: l -> recognize_roman_L (v+100) l
  | l -> recognize_roman_L v l

let recognize_roman_D v = function
    Capital("C",_) :: Capital("D",_) :: l -> recognize_roman_L (v+400) l
  | Capital("D",_) :: l -> recognize_roman_C (v+500) l
  | Capital("C",_) :: Capital("M",_) :: l -> recognize_roman_L (v+900) l
  | l -> recognize_roman_C v l

let recognize_roman_M v = function
  | Capital("M",_) :: Capital("M",_) :: Capital("M",_) :: l -> recognize_roman_D (v+3000) l
  | Capital("M",_) :: Capital("M",_) :: l -> recognize_roman_D (v+2000) l
  | Capital("M",_) :: l -> recognize_roman_D (v+1000) l
  | l -> recognize_roman_D v l

let rec merge l =
  String.concat "" (Xlist.map l (function
      Capital(uc,lc) -> uc
    | ForeignCapital(uc,lc) -> uc
    | Small(uc,lc) -> lc
    | ForeignSmall(uc,lc) -> lc
    | _ -> failwith "merge"))

let lowercase_first = function
    [] -> []
  | Capital(uc,lc) :: l -> Small(uc,lc) :: l
  | ForeignCapital(uc,lc) :: l -> ForeignSmall(uc,lc) :: l
  | Small(uc,lc) :: l -> Small(uc,lc) :: l
  | ForeignSmall(uc,lc) :: l -> ForeignSmall(uc,lc) :: l
  | _ -> failwith "lowercase_first"

let rec lowercase_all = function
    [] -> []
  | Capital(uc,lc) :: l -> Small(uc,lc) :: lowercase_all l
  | ForeignCapital(uc,lc) :: l -> ForeignSmall(uc,lc) :: lowercase_all l
  | Small(uc,lc) :: l -> Small(uc,lc) :: lowercase_all l
  | ForeignSmall(uc,lc) :: l -> ForeignSmall(uc,lc) :: lowercase_all l
  | _ -> failwith "lowercase_all"

let rec uppercase_all = function
    [] -> []
  | Capital(uc,lc) :: l -> Capital(uc,lc) :: uppercase_all l
  | ForeignCapital(uc,lc) :: l -> ForeignCapital(uc,lc) :: uppercase_all l
  | Small(uc,lc) :: l -> Capital(uc,lc) :: uppercase_all l
  | ForeignSmall(uc,lc) :: l -> ForeignCapital(uc,lc) :: uppercase_all l
  | _ -> failwith "uppercase_all"

let make_first_cap = function
    [] -> []
  | Capital(uc,lc) :: l -> Capital(uc,lc) :: lowercase_all l
  | ForeignCapital(uc,lc) :: l -> ForeignCapital(uc,lc) :: lowercase_all l
  | Small(uc,lc) :: l -> Capital(uc,lc) :: lowercase_all l
  | ForeignSmall(uc,lc) :: l -> ForeignCapital(uc,lc) :: lowercase_all l
  | _ -> failwith "make_first_cap"

let recognize_stem has_sufix i letters =
  let orth = merge letters in
  let lowercase = merge (lowercase_all letters) in
  let uppercase = merge (uppercase_all letters) in
  let first_cap = merge (make_first_cap letters) in
  let token = if Xlist.size letters = 1 then
    if orth = lowercase then SmallLetter(uppercase,lowercase) else
    if orth = uppercase then CapLetter(uppercase,lowercase) else
    failwith "recognize_stem"
  else
    if orth = lowercase then AllSmall(uppercase,first_cap,lowercase) else
    if orth = uppercase then AllCap(uppercase,first_cap,lowercase) else
    if orth = first_cap then FirstCap(uppercase,first_cap,lowercase) else
    SomeCap(uppercase,orth,lowercase) in
  Token{empty_token_env with orth=orth;beg=i;len=Xlist.size letters * factor;next=i+Xlist.size letters * factor; token=token;
    attrs=if has_sufix then [HasAglSuffix] else []}

let make_lemma (lemma,interp) cat =
  match Tagset.parse_and_validate lemma interp with
    [pos,tags] -> Lemma(lemma,pos,tags,cat)
  | [pos1,tags1;pos2,tags2] -> if pos1=pos2 then Lemma(lemma,pos1,tags1 @ tags2,cat) else failwith "make_lemma 1"
  | _ -> failwith "make_lemma 2"

let merge_attrs l =
(*   print_endline (String.concat " " (Xlist.map l (fun token -> "[" ^ token.orth ^ " " ^ String.concat ";" token.attrs ^ "]"))); *)
  let len = Xlist.size l in
  let attrs = Xlist.fold l AttrQMap.empty (fun attrs token ->
    Xlist.fold token.attrs attrs AttrQMap.add) in
  let n_cs = try AttrQMap.find attrs CS with Not_found -> 0 in
  let n_maybe_cs = try AttrQMap.find attrs MaybeCS with Not_found -> 0 in
  let new_attrs =
    (if n_cs > 0 then
      if n_cs + n_maybe_cs = len then [CS] else raise Not_found
    else
      if n_maybe_cs = len then [MaybeCS] else []) @
    (AttrQMap.fold attrs [] (fun attrs attr _ -> if attr = CS || attr = MaybeCS then attrs else attr :: attrs)) in
(*   print_endline (String.concat " " new_attrs); *)
  new_attrs

let recognize_suffix i letters =
  match recognize_stem false i letters with
    Token t -> Token {t with attrs=[AglSuffix]}
  | _ -> failwith "recognize_suffix"
(*  let orth = merge letters in
  Token{empty_token_env with orth=orth;beg=i;len=Xlist.size letters * factor;next=i+Xlist.size letters * factor; token=Ideogram(orth,"suffix"); attrs=[AglSuffix]}*)


let recognize_romandig i letters =
  let roman,w = recognize_roman_M 0 letters in
  if roman > 0 then
    let letters,w = if w then let l = List.rev letters in List.rev (List.tl l), [List.hd l] else letters,[] in
    let orth = merge letters in
    let roman = string_of_int roman in
    let t = {empty_token_env with orth=orth;beg=i;len=Xlist.size letters * factor;next=i+Xlist.size letters * factor} in
    if w = [] then 
      if StringSet.mem romanmonths orth then [
        Token{t with token=Ideogram(roman,"roman"); attrs=MaybeCS :: t.attrs};
        Token{t with token=Ideogram(roman,"roman-month"); attrs=MaybeCS :: t.attrs}]
      else [
        Token{t with token=Ideogram(roman,"roman"); attrs=MaybeCS :: t.attrs}]
	else
      let beg = i + Xlist.size letters * factor in
      let w = [Token{empty_token_env with orth=merge w; beg=beg; len=factor; next=beg+factor; token=SmallLetter(merge (uppercase_all w),merge w)}] in
(*      [Variant[Token{empty_token_env with orth=merge w; beg=beg; len=factor; next=beg+factor; token=SmallLetter(merge (uppercase_all w),merge w)};
               Token{empty_token_env with orth=merge w; beg=beg; len=factor; next=beg+factor; token=make_lemma ("wiek","subst:sg:_:m3")}]] in*)
      [Seq(Token{t with token=Ideogram(roman,"roman"); attrs=MaybeCS :: t.attrs}::w)]
  else []

let sufixes1 = Xlist.map [
  ["m"];
  ["e"; "m"];
  ["ś"];
  ["e"; "ś"];
  ["ś"; "m"; "y"];
  ["e"; "ś"; "m"; "y"];
  ["ś"; "c"; "i"; "e"];
  ["e"; "ś"; "c"; "i"; "e"];
  ["ń"];
  ["ż"; "e"];
  ["ż"];
  ] List.rev

let sufixes2 = Xlist.map [
  ["b"; "y"];
  ] List.rev

let rec find_suffix rev = function
    _, [] -> raise Not_found
  | [], l -> rev, l
  | s :: pat, Capital(uc,lc) :: l -> if s = lc then find_suffix (Capital(uc,lc) :: rev) (pat,l) else raise Not_found
  | s :: pat, Small(uc,lc) :: l -> if s = lc then find_suffix (Small(uc,lc) :: rev) (pat,l) else raise Not_found
  | _,_ -> raise Not_found

let find_suffixes2 sufixes letters sufs =
  Xlist.fold sufixes [] (fun l suf ->
    try
      let suf,rev_stem = find_suffix [] (suf,letters) in
      (rev_stem,suf :: sufs) :: l
    with Not_found -> l)

let find_suffixes i letters =
  let letters = List.rev letters in
  let l = (letters,[]) :: find_suffixes2 sufixes1 letters [] in
  let l = Xlist.fold l l (fun l (letters,sufs) ->
    (find_suffixes2 sufixes2 letters sufs) @ l) in
  Xlist.map l (fun (rev_stem, sufs) ->
    List.rev (fst (Xlist.fold (List.rev rev_stem :: sufs) ([],i) (fun (seq,i) letters ->
      (letters,i) :: seq, i + factor * Xlist.size letters))))

let merge_letters2 i letters =
  let l = find_suffixes i letters in
  let roman = recognize_romandig i letters in
  let variants = Xlist.fold l roman (fun variants -> function
      [] -> failwith "merge_letters"
    | [stem,i] -> (recognize_stem false i stem) :: variants
    | (stem,i) :: suffixes ->
        (try (Seq((recognize_stem true i stem) :: Xlist.map suffixes (fun (suf,i) -> recognize_suffix i suf))) :: variants
         with Not_found -> variants)) in
  Variant variants
  
let merge_letters i = function
    [letter] -> merge_letters2 i [letter]
  | (ForeignSmall("X","x")) :: letters -> Variant[
      Seq[recognize_stem false i [Small("X","x")]; merge_letters2 (i+factor) letters]; 
      merge_letters2 i ((Small("X","x")) :: letters)]
  | (ForeignCapital("X","x")) :: letters -> Variant[
      Seq[recognize_stem false i [Capital("X","x")]; merge_letters2 (i+factor) letters]; 
      merge_letters2 i ((Capital("X","x")) :: letters)]
  | letters -> merge_letters2 i letters

let rec group_digits rev = function
    [] -> List.rev rev, []
  | Digit s :: l -> group_digits (s :: rev) l
  | x :: l -> List.rev rev, x :: l

let rec group_letters rev = function
    [] -> List.rev rev, []
  | Capital(uc,lc) :: l -> group_letters ((Capital(uc,lc)) :: rev) l
  | ForeignCapital(uc,lc) :: l -> group_letters ((ForeignCapital(uc,lc)) :: rev) l
  | Small(uc,lc) :: l -> group_letters ((Small(uc,lc)) :: rev) l
  | ForeignSmall(uc,lc) :: l -> group_letters ((ForeignSmall(uc,lc)) :: rev) l
  | x :: l -> List.rev rev, x :: l

let rec group_others rev = function
    [] -> List.rev rev, []
  | Other(s,_) :: l -> group_others (s :: rev) l
  | x :: l -> List.rev rev, x :: l

let create_sign_token i signs l token =
  let orth = String.concat "" (Xlist.map signs (function Sign s -> s | Small(uc,lc) -> lc | _ -> failwith "create_sign_token")) in
  let len = Xlist.size signs * factor in
  Token{empty_token_env with orth=orth;beg=i;len=len;next=i+len;token=token; attrs=[]},i+len,l

let create_opening_double_quot i signs l =
  create_sign_token i signs l (Interp "„")

let create_closing_double_quot i signs l =
  create_sign_token i signs l (Interp "”")

let create_both_double_quot i signs l =
  let orth = String.concat "" (Xlist.map signs (function Sign s -> s | Small(uc,lc) -> lc | _ -> failwith "create_sign_token")) in
  let len = Xlist.size signs * factor in
  Variant[
    Token{empty_token_env with orth=orth;beg=i;len=len;next=i+len;token=Interp "„"};
    Token{empty_token_env with orth=orth;beg=i;len=len;next=i+len;token=Interp "”"}],i+len,l
  
let create_hyphen i signs l =
  create_sign_token i signs l (Interp "-")

let create_multidot i signs l =
  let orth = String.concat "" (Xlist.map signs (function Sign s -> s | _ -> failwith "create_multidot")) in
  let len = Xlist.size signs * factor in
  Variant[Seq[Token{empty_token_env with           beg=i;       len=quant1;    next=i+quant1;token=Symbol "."; attrs=[]};
              Token{empty_token_env with orth=orth;beg=i+quant1;len=len-quant1;next=i+len;   token=Interp "…"}];
          Token{empty_token_env with orth=orth;beg=i;len=len;next=i+len;token=Interp "…"; attrs=[]}],i+len,l

let create_quot i signs l =
  create_sign_token i signs l (Interp "’")

let recognize_sign_group i = function
  | (Sign "&") :: (Small(u1,"n")) :: (Small(u2,"b")) :: (Small(u3,"s")) :: (Small(u4,"p")) :: (Sign ";") :: l -> create_sign_token i ((Sign "&") :: (Small(u1,"n")) :: (Small(u2,"b")) :: (Small(u3,"s")) :: (Small(u4,"p")) :: (Sign ";") :: []) l (Symbol " ")
  | (Sign "&") :: (Small(u1,"a")) :: (Small(u2,"l")) :: (Small(u3,"p")) :: (Small(u4,"h")) :: (Small(u5,"a")) :: (Sign ";") :: l -> create_sign_token i ((Sign "&") :: (Small(u1,"a")) :: (Small(u2,"l")) :: (Small(u3,"p")) :: (Small(u4,"h")) :: (Small(u5,"a")) :: (Sign ";") :: []) l (Interp "α")
  | (Sign "&") :: (Small(u1,"b")) :: (Small(u2,"e")) :: (Small(u3,"t")) :: (Small(u4,"a")) :: (Sign ";") :: l -> create_sign_token i ((Sign "&") :: (Small(u1,"b")) :: (Small(u2,"e")) :: (Small(u3,"t")) :: (Small(u4,"a")) :: (Sign ";") :: []) l (Interp "β")
  | (Sign "&") :: (Small(u1,"g")) :: (Small(u2,"a")) :: (Small(u3,"m")) :: (Small(u4,"m")) :: (Small(u5,"a")) :: (Sign ";") :: l -> create_sign_token i ((Sign "&") :: (Small(u1,"g")) :: (Small(u2,"a")) :: (Small(u3,"m")) :: (Small(u4,"m")) :: (Small(u5,"a")) :: (Sign ";") :: []) l (Interp "γ")
  | (Sign "&") :: (Small(u1,"k")) :: (Small(u2,"a")) :: (Small(u3,"p")) :: (Small(u4,"p")) :: (Small(u5,"a")) :: (Sign ";") :: l -> create_sign_token i ((Sign "&") :: (Small(u1,"k")) :: (Small(u2,"a")) :: (Small(u3,"p")) :: (Small(u4,"p")) :: (Small(u5,"a")) :: (Sign ";") :: []) l (Interp "κ")
  | (Sign "&") :: (Small(u1,"D")) :: (Small(u2,"e")) :: (Small(u3,"l")) :: (Small(u4,"t")) :: (Small(u5,"a")) :: (Sign ";") :: l -> create_sign_token i ((Sign "&") :: (Small(u1,"D")) :: (Small(u2,"e")) :: (Small(u3,"l")) :: (Small(u4,"t")) :: (Small(u5,"a")) :: (Sign ";") :: []) l (Interp "Δ")
  | (Sign "&") :: (Small(u1,"m")) :: (Small(u2,"u")) :: (Sign ";") :: l -> create_sign_token i ((Sign "&") :: (Small(u1,"m")) :: (Small(u2,"u")) :: (Sign ";") :: []) l (Interp "µ")
  | (Sign "&") :: (Small(u1,"g")) :: (Small(u2,"e")) :: (Sign ";") :: l -> create_sign_token i ((Sign "&") :: (Small(u1,"g")) :: (Small(u2,"e")) :: (Sign ";") :: []) l (Interp "≥")
  | (Sign "&") :: (Small(u1,"l")) :: (Small(u2,"e")) :: (Sign ";") :: l -> create_sign_token i ((Sign "&") :: (Small(u1,"l")) :: (Small(u2,"e")) :: (Sign ";") :: []) l (Interp "≤")
  | (Sign "&") :: (Small(u1,"g")) :: (Small(u2,"t")) :: (Sign ";") :: l -> create_sign_token i ((Sign "&") :: (Small(u1,"g")) :: (Small(u2,"t")) :: (Sign ";") :: []) l (Interp ">")
  | (Sign "&") :: (Small(u1,"l")) :: (Small(u2,"t")) :: (Sign ";") :: l -> create_sign_token i ((Sign "&") :: (Small(u1,"l")) :: (Small(u2,"t")) :: (Sign ";") :: []) l (Interp "<")
  | (Sign "&") :: (Small(u1,"u")) :: (Small(u2,"a")) :: (Small(u3,"r")) :: (Small(u4,"r")) :: (Sign ";") :: l -> create_sign_token i ((Sign "&") :: (Small(u1,"u")) :: (Small(u2,"a")) :: (Small(u3,"r")) :: (Small(u4,"r")) :: (Sign ";") :: []) l (Interp "↑")
  | (Sign "&") :: (Small(u1,"d")) :: (Small(u2,"a")) :: (Small(u3,"r")) :: (Small(u4,"r")) :: (Sign ";") :: l -> create_sign_token i ((Sign "&") :: (Small(u1,"d")) :: (Small(u2,"a")) :: (Small(u3,"r")) :: (Small(u4,"r")) :: (Sign ";") :: []) l (Interp "↓")
  | (Sign "&") :: (Small(u1,"d")) :: (Small(u2,"e")) :: (Small(u3,"g")) :: (Sign ";") :: l -> create_sign_token i ((Sign "&") :: (Small(u1,"d")) :: (Small(u2,"e")) :: (Small(u3,"g")) :: (Sign ";") :: []) l (Interp "°")
  | (Sign "&") :: (Small(u1,"t")) :: (Small(u2,"a")) :: (Small(u3,"u")) :: (Sign ";") :: l -> create_sign_token i ((Sign "&") :: (Small(u1,"t")) :: (Small(u2,"a")) :: (Small(u3,"u")) :: (Sign ";") :: []) l (Interp "τ")
  | (Sign "&") :: (Small(u1,"p")) :: (Small(u2,"r")) :: (Small(u3,"o")) :: (Small(u4,"p")) :: (Sign ";") :: l -> create_sign_token i ((Sign "&") :: (Small(u1,"p")) :: (Small(u2,"r")) :: (Small(u3,"o")) :: (Small(u4,"p")) :: (Sign ";") :: []) l (Interp "∝")
  | (Sign "&") :: (Small(u1,"i")) :: (Small(u2,"o")) :: (Small(u3,"t")) :: (Small(u4,"a")) :: (Sign ";") :: l -> create_sign_token i ((Sign "&") :: (Small(u1,"i")) :: (Small(u2,"o")) :: (Small(u3,"t")) :: (Small(u4,"a")) :: (Sign ";") :: []) l (Interp "ι")
  | (Sign "&") :: (Small(u1,"h")) :: (Small(u2,"a")) :: (Small(u3,"r")) :: (Small(u4,"r")) :: (Sign ";") :: l -> create_sign_token i ((Sign "&") :: (Small(u1,"h")) :: (Small(u2,"a")) :: (Small(u3,"r")) :: (Small(u4,"r")) :: (Sign ";") :: []) l (Interp "↔")
  | (Sign "&") :: (Small(u1,"l")) :: (Small(u2,"a")) :: (Small(u3,"r")) :: (Small(u4,"r")) :: (Sign ";") :: l -> create_sign_token i ((Sign "&") :: (Small(u1,"l")) :: (Small(u2,"a")) :: (Small(u3,"r")) :: (Small(u4,"r")) :: (Sign ";") :: []) l (Interp "←")
  | (Sign "&") :: (Small(u1,"i")) :: (Small(u2,"n")) :: (Small(u3,"f")) :: (Small(u4,"i")) :: (Small(u5,"n")) :: (Sign ";") :: l -> create_sign_token i ((Sign "&") :: (Small(u1,"i")) :: (Small(u2,"n")) :: (Small(u3,"f")) :: (Small(u4,"i")) :: (Small(u5,"n")) :: (Sign ";") :: []) l (Interp "∞")
  | (Sign "&") :: (Small(u1,"p")) :: (Small(u2,"l")) :: (Small(u3,"u")) :: (Small(u4,"s")) :: (Small(u5,"m")) :: (Small(u6,"n")) :: (Sign ";") :: l -> create_sign_token i ((Sign "&") :: (Small(u1,"p")) :: (Small(u2,"l")) :: (Small(u3,"u")) :: (Small(u4,"s")) :: (Small(u5,"m")) :: (Small(u6,"n")) :: (Sign ";") :: []) l (Interp "±")
  | (Sign "&") :: (Small(u1,"o")) :: (Small(u2,"m")) :: (Small(u3,"i")) :: (Small(u4,"c")) :: (Small(u5,"r")) :: (Small(u6,"o")) :: (Small(u7,"n")) :: (Sign ";") :: l -> create_sign_token i ((Sign "&") :: (Small(u1,"o")) :: (Small(u2,"m")) :: (Small(u3,"i")) :: (Small(u4,"c")) :: (Small(u5,"r")) :: (Small(u6,"o")) :: (Small(u7,"n")) :: (Sign ";") :: []) l (Interp "ο")
  | (Sign "&") :: (Small(u1,"a")) :: (Small(u2,"s")) :: (Small(u3,"y")) :: (Small(u4,"m")) :: (Small(u5,"p")) :: (Sign ";") :: l -> create_sign_token i ((Sign "&") :: (Small(u1,"a")) :: (Small(u2,"s")) :: (Small(u3,"y")) :: (Small(u4,"m")) :: (Small(u5,"p")) :: (Sign ";") :: []) l (Interp "≈")
  | (Sign " ") :: l -> create_sign_token i [Sign " "] l (Symbol " ")
  | (Sign "﻿") :: l -> create_sign_token i [Sign "﻿"] l (Symbol " ")
  | (Sign " ") :: l -> create_sign_token i [Sign " "] l (Symbol " ")
  | (Sign "\t") :: l -> create_sign_token i [Sign "\t"] l (Symbol "\t")
  | (Sign "\r") :: l -> create_sign_token i [Sign "\r"] l (Symbol "\r")
  | (Sign "\n") :: l -> create_sign_token i [Sign "\n"] l (Symbol "\n")
  | (Sign "#") :: l -> create_sign_token i [Sign "#"] l (Interp "#")
  | (Sign "\"") :: l -> create_both_double_quot i [Sign "\""] l
  | (Sign "˝") :: l -> create_both_double_quot i [Sign "˝"] l
  | (Sign "„") :: l -> create_opening_double_quot i [Sign "„"] l
  | (Sign "”") :: l -> create_closing_double_quot i [Sign "”"] l
  | (Sign "“") :: l -> create_both_double_quot i [Sign "“"] l
  | (Sign ",") :: (Sign ",") :: l -> create_opening_double_quot i [Sign ",";Sign ","] l
  | (Sign "`") :: (Sign "`") :: l -> create_opening_double_quot i [Sign "`";Sign "`"] l
  | (Sign "'") :: (Sign "'") :: l -> create_closing_double_quot i [Sign "'";Sign "'"] l
  | (Sign "’") :: (Sign "’") :: l -> create_closing_double_quot i [Sign "’";Sign "’"] l
  | (Sign "<") :: (Sign "<") :: l -> create_sign_token i [Sign "<";Sign "<"] l (Interp "«") (* prawy cudzysłów *)
  | (Sign ">") :: (Sign ">") :: l -> create_sign_token i [Sign ">";Sign ">"] l (Interp "»") (* lewy cudzysłów *)
  | (Sign "«") :: l -> create_sign_token i [Sign "«"] l (Interp "«")
  | (Sign "»") :: l -> create_sign_token i [Sign "»"] l (Interp "»")
  | (Sign "'") :: l -> create_quot i [Sign "'"] l
  | (Sign "’") :: l -> create_quot i [Sign "’"] l
  | (Sign "´") :: l -> create_quot i [Sign "´"] l
  | (Sign "`") :: l -> create_quot i [Sign "`"] l
  | (Sign "‘") :: l -> create_quot i [Sign "‘"] l
  | (Sign "-") :: (Sign "-") :: (Sign "-") :: l -> create_hyphen i [Sign "-";Sign "-";Sign "-"] l
  | (Sign "-") :: (Sign "-") :: l -> create_hyphen i [Sign "-";Sign "-"] l
  | (Sign "-") :: l -> create_hyphen i [Sign "-"] l
  | (Sign "‐") :: l -> create_hyphen i [Sign "‐"] l
  | (Sign "‑") :: l -> create_hyphen i [Sign "‑"] l
  | (Sign "‒") :: l -> create_hyphen i [Sign "‒"] l
  | (Sign "−") :: l -> create_hyphen i [Sign "−"] l
  | (Sign "–") :: l -> create_hyphen i [Sign "–"] l
  | (Sign "—") :: l -> create_hyphen i [Sign "—"] l
  | (Sign "…") :: l -> create_multidot i ((Sign "…") :: []) l
  | (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: l -> (* Różne natęrzenia wypunktowania *)
        create_sign_token i [Sign ".";Sign ".";Sign ".";Sign ".";Sign ".";Sign ".";Sign ".";Sign "."] l (Interp "……")
  | (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: l ->
        create_sign_token i [Sign ".";Sign ".";Sign ".";Sign ".";Sign ".";Sign ".";Sign "."] l (Interp "……")
  | (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: l ->
        create_sign_token i [Sign ".";Sign ".";Sign ".";Sign ".";Sign ".";Sign "."] l (Interp "……")
  | (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: l ->
        create_sign_token i [Sign ".";Sign ".";Sign ".";Sign ".";Sign "."] l (Interp "……")
  | (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: l ->
        create_sign_token i [Sign ".";Sign ".";Sign ".";Sign "."] l (Interp "……")
  | (Sign ".") :: (Sign ".") :: (Sign ".") :: l -> create_multidot i ((Sign ".") :: (Sign ".") :: (Sign ".") :: []) l
  | (Sign ".") :: (Sign ".") :: l -> create_multidot i ((Sign ".") :: (Sign ".") :: []) l
  | (Sign ".") :: l ->
        Variant[Seq[Token{empty_token_env with          beg=i;       len=quant1;       next=i+quant1;token=Symbol "."; attrs=[]};
                    Token{empty_token_env with orth=".";beg=i+quant1;len=factor-quant1;next=i+factor;token=Interp "."}];
                Token{empty_token_env with orth=".";beg=i;len=factor;next=i+factor;token=Symbol "."; attrs=[]};
                Token{empty_token_env with orth=".";beg=i;len=factor;next=i+factor;token=Interp "."; attrs=[]}],i+factor,l
  | (Sign s) :: l -> create_sign_token i [Sign s] l (Interp s)
  | l ->  failwith "recognize_sign_group"

let merge_url i len orth cat =
    Token{empty_token_env with orth=orth;beg=i;len=len*factor;next=i+len*factor;token=Ideogram(orth,cat)}

let rec group_chars i rev = function
    [] -> List.rev ((Token{empty_token_env with beg=i;len=factor;next=i+factor;token=Interp "</query>"}) :: rev)
  | Digit s :: l -> let x,l = group_digits [] ((Digit s) :: l) in group_chars (i + Xlist.size x * factor) ((merge_digits i x) :: rev) l
  | Sign "<" :: Sign "/" :: Small("S","s") :: Small("U","u") :: Small("B","b") :: Sign ">" :: Sign ">" :: l ->
      group_chars i rev (Sign "<" :: Sign "/" :: Small("S","s") :: Small("U","u") :: Small("B","b") :: Sign ">" :: Other(">",1) :: l)
  | Sign s :: l -> let x,i,l = recognize_sign_group i ((Sign s) :: l) in group_chars i (x :: rev) l
  | Capital(s,t) :: l -> let x,l = group_letters [] ((Capital(s,t)) :: l) in group_chars (i + Xlist.size x * factor) ((merge_letters i x) :: rev) l
  | ForeignCapital(s,t) :: l -> let x,l = group_letters [] ((ForeignCapital(s,t)) :: l) in group_chars (i + Xlist.size x * factor) ((merge_letters i x) :: rev) l
  | Small(uc,lc) :: l -> let x,l = group_letters [] ((Small(uc,lc)) :: l) in group_chars (i + Xlist.size x * factor) ((merge_letters i x) :: rev) l
  | ForeignSmall(uc,lc) :: l -> let x,l = group_letters [] ((ForeignSmall(uc,lc)) :: l) in group_chars (i + Xlist.size x * factor) ((merge_letters i x) :: rev) l
  | Emoticon s :: l -> group_chars (i + factor) ((Token{empty_token_env with orth=s;beg=i;len=factor;next=i+factor;token=Ideogram(s,"emoticon")}) :: rev) l
  | Other(">",1) :: l -> group_chars i rev (Sign ">" :: l)
  | Other("url",len) :: Sign s :: l -> group_chars (i + len * factor) ((merge_url i len s "url") :: rev) l
  | Other("email",len) :: Sign s :: l -> group_chars (i + len * factor) ((merge_url i len s "email") :: rev) l
  | Other(s,x) :: l ->
        let x,l = group_others [] ((Other(s,x)) :: l) in
        group_chars (i + Xlist.size x * factor)
          ((Token{empty_token_env with orth=String.concat "" x;beg=i;len=Xlist.size x * factor;next=i+Xlist.size x * factor;token=Other(String.concat "" x)}) :: rev) l

let tokenize l =
  (Token{empty_token_env with beg=0;len=factor;next=factor;token=Interp "<query>"}) :: (group_chars factor [] (Url.find l))

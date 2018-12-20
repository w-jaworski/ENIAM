(*
 *  ENIAMtokenizer, a tokenizer for Polish
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

open TokenizerTypes

let compose_lemma t lemma_suf interp =
  Tokens.make_lemma (Tokens.get_orth t.token ^ lemma_suf, interp)

let compose_lemma3 t1 t2 t3 lemma_suf interp =
  Tokens.make_lemma (Tokens.get_orth t1.token ^ Tokens.get_orth t2.token ^ Tokens.get_orth t3.token ^ lemma_suf, interp)

let concat_orths l =
  String.concat "" (Xlist.map l (fun t -> t.orth))

let ct l lemma interp =
  let beg = (List.hd l).beg in
  let t = List.hd (List.rev l) in
  let len = t.beg + t.len - beg in
  Token{empty_token_env with
    orth=concat_orths l;
    beg=beg;
    len=len;
    next=t.next;
    token=Tokens.make_lemma (lemma,interp) "X"; (* FIXME: nieokreślona kategoria semantyczna *)
    attrs=Tokens.merge_attrs l}

let rec get_orth_prefix i l =
  if i = 0 then "",l else
  match l with
    c :: l -> let s,l = get_orth_prefix (i-1) l in c ^ s, l
  | [] -> failwith "get_orth_prefix"

let make_sub_tokens t l =
  let n = Xlist.fold l 0 (fun n (i,_,_) -> n + i) in
  let orth = Xunicode.utf8_chars_of_utf8_string t.orth in
  if Xlist.size orth <> n then failwith "make_sub_tokens: invalid orth length" else
  let l,_,_,_ = Xlist.fold l ([],t.beg,t.len,orth) (fun (l,beg,remaining_len,orth) (i,lemma,interp) ->
    let orth,remaining_orth = get_orth_prefix i orth in
    let len = if beg mod factor = 0 then i * factor else ((i-1) * factor) + (beg mod factor) in
    if remaining_len = 0 then failwith "make_sub_tokens: invalid remaining_len" else
    let len = if len > remaining_len then remaining_len else len in
    Token{empty_token_env with
      orth=orth;
      beg=beg;
      len=len;
      next=beg+len;
      token=Tokens.make_lemma (lemma,interp) "X"; (* FIXME: nieokreślona kategoria semantyczna *)
      attrs=t.attrs} :: l,
    beg+len, remaining_len-len, remaining_orth) in
  l

let st t l =
  let l = make_sub_tokens t l in
  match l with
    Token s :: l -> List.rev (Token{s with next=t.next} :: l)
  | _ -> failwith "st"

let std t d l =
  let l = make_sub_tokens t l in
  match l with
    Token s :: l -> List.rev (Token{s with orth=s.orth^d.orth; len=d.beg+d.len-s.beg; next=d.next} :: l)
  | _ -> failwith "std"

(* FIXME: BR-owi interpretowany jako bieżący rok *)
let abr_patterns = [
  [T "b"; Sym "."; T "u"; Sym "."],			(function [a;b;c;d] -> 	[ct [a;b] "bez" "prep:gen:nwok"; ct [c;d] "uwaga" "subst:pl:gen:f"] | _ -> failwith "abr_patterns");
  [T "b"; Sym "."; T "zm"; Sym "."],		(function [a;b;c;d] -> 	[ct [a;b] "bez" "prep:gen:nwok"; ct [c;d] "zmiana" "subst:pl:gen:f"] | _ -> failwith "abr_patterns");
  [T "blm"],					(function [a] -> 	st a [1,"bez","prep:gen:nwok";1,"liczba","subst:sg:gen:f";1,"mnogi","adj:sg:gen:f:pos"] | _ -> failwith "abr_patterns");
  [T "blp"],					(function [a] -> 	st a [1,"bez","prep:gen:nwok";1,"liczba","subst:sg:gen:f";1,"pojedynczy","adj:sg:gen:f:pos"] | _ -> failwith "abr_patterns");
  [T "błp"; Sym "."],				(function [a;b] -> 	std a b [2,"błogosławiony","adj:sg:gen:f:pos";1,"pamięć","subst:sg:gen:f"] | _ -> failwith "abr_patterns");
  [T "bm"],					(function [a] -> 	st a [1,"bieżący","adj:sg:$c:m3:pos";1,"miesiąc","subst:sg:$c:m3"] | _ -> failwith "abr_patterns");
  [T "bm"; Sym "."],				(function [a;b] -> 	std a b [1,"bieżący","adj:sg:$c:m3:pos";1,"miesiąc","subst:sg:$c:m3"] | _ -> failwith "abr_patterns");
  [T "bp"; Sym "."],				(function [a;b] -> 	std a b [1,"błogosławiony","adj:sg:gen:f:pos";1,"pamięć","subst:sg:gen:f"] | _ -> failwith "abr_patterns");
  [T "br"],					(function [a] -> 	st a [1,"bieżący","adj:sg:$c:m3:pos";1,"rok","subst:sg:$c:m3"] | _ -> failwith "abr_patterns");
  [T "br"; Sym "."],				(function [a;b] -> 	std a b [1,"bieżący","adj:sg:$c:m3:pos";1,"rok","subst:sg:$c:m3"] | _ -> failwith "abr_patterns");
  [T "c"; Sym "."; T "d"; Sym "."; T "n"; Sym "."],	(function [a;b;c;d;e;f] -> [ct [a;b] "ciąg" "subst:sg:nom:m3"; ct [c;d] "daleki" "adj:sg:nom:m3:com"; ct [e;f] "nastąpić" "fin:sg:ter:perf"] | _ -> failwith "abr_patterns");
  [T "ccm"],					(function [a] -> 	st a [1,"sześcienny","adj:_:$c:m3:pos";2,"centymetr","subst:_:$c:m3"] | _ -> failwith "abr_patterns");
  [T "cd"; Sym "."],				(function [a;b] -> 	std a b [1,"ciąg","subst:sg:nom:m3";1,"daleki","adj:sg:nom:m3:com"] | _ -> failwith "abr_patterns");
  [T "cdn"; Sym "."],				(function [a;b] -> 	std a b [1,"ciąg","subst:sg:nom:m3";1,"daleki","adj:sg:nom:m3:com";1,"nastąpić","fin:sg:ter:perf"] | _ -> failwith "abr_patterns");
  [T "cm"; T "3"],				(function [a;b] -> 	[ct [a] "centymetr" "subst:_:$c:m3"; ct [b] "sześcienny" "adj:_:$c:m3:pos"] | _ -> failwith "abr_patterns");
  [T "dcn"; Sym "."],				(function [a;b] -> 	std a b [1,"daleki","adj:sg:nom:m3:com";1,"ciąg","subst:sg:nom:m3";1,"nastąpić","fin:sg:ter:perf"] | _ -> failwith "abr_patterns");
  [T "dm"; T "3"],				(function [a;b] -> 	[ct [a] "decymetr" "subst:_:$c:m3"; ct [b] "sześcienny" "adj:_:$c:m3:pos"] | _ -> failwith "abr_patterns");
  [T "ds"; Sym "."],				(function [a;b] -> 	std a b [1,"do","prep:gen";1,"sprawa","subst:pl:gen:f"] | _ -> failwith "abr_patterns");
  [T "d"; T "/"; T "s"],			(function [a;b;c] -> 	[ct [a;b] "do" "prep:gen"; ct [c] "sprawa" "subst:pl:gen:f"] | _ -> failwith "abr_patterns");
  [T "itd"; Sym "."],				(function [a;b] -> 	std a b [1,"i","conj";1,"tak","adv:pos";1,"daleko","adv:com"] | _ -> failwith "abr_patterns");
  [T "itede"; Sym "."],				(function [a;b] -> 	std a b [1,"i","conj";2,"tak","adv:pos";2,"daleko","adv:com"] | _ -> failwith "abr_patterns");
  [T "itp"; Sym "."],				(function [a;b] -> 	std a b [1,"i","conj";1,"tym","adv";1,"podobny","adj:pl:nom:_:pos"] | _ -> failwith "abr_patterns");
  [T "j"; Sym "."; T "m"; Sym "."],		(function [a;b;c;d] -> 	[ct [a;b] "jednostka" "subst:_:_:f"; ct [c;d] "miary" "subst:sg:gen:f"] | _ -> failwith "abr_patterns");
  [T "j"; Sym "."; T "m"; Sym "."],		(function [a;b;c;d] -> 	[ct [a;b] "jednostka" "subst:_:_:f"; ct [c;d] "międzynarodowy" "adj:_:_:f:pos"] | _ -> failwith "abr_patterns");
  [T "jw"; Sym "."],				(function [a;b] -> 	std a b [1,"jak","adv:pos";1,"wysoko","adv:com"] | _ -> failwith "abr_patterns");
  [T "JWP"],					(function [a] -> 	st a [1,"jaśnie","adv:pos";1,"wielmożny","adj:_:$c:m1:pos";1,"pan","subst:_:$c:m1"] | _ -> failwith "abr_patterns");
  [T "JWP"],					(function [a] -> 	st a [1,"jaśnie","adv:pos";1,"wielmożny","adj:_:$c:f:pos";1,"pani","subst:_:$c:f"] | _ -> failwith "abr_patterns");
  [T "km"; Sym "."; T "2"],			(function [a;b;c] -> 	[ct [a;b] "kilometr" "subst:_:$c:m3"; ct [c] "kwadratowy" "adj:_:$c:m3:pos"] | _ -> failwith "abr_patterns");
  [T "km"; T "2"],				(function [a;b] -> 	[ct [a] "kilometr" "subst:_:$c:m3"; ct [b] "kwadratowy" "adj:_:$c:m3:pos"] | _ -> failwith "abr_patterns");
  [T "km"; T "²"],				(function [a;b] -> 	[ct [a] "kilometr" "subst:_:$c:m3"; ct [b] "kwadratowy" "adj:_:$c:m3:pos"] | _ -> failwith "abr_patterns");
  [T "lm"; Sym "."],				(function [a;b] -> 	std a b [1,"liczba","subst:sg:$c:f";1,"mnogi","adj:sg:$c:f:pos"] | _ -> failwith "abr_patterns");
  [T "lp"; Sym "."],				(function [a;b] -> 	std a b [1,"liczba","subst:sg:$c:f";1,"pojedynczy","adj:sg:$c:f:pos"] | _ -> failwith "abr_patterns");
  [T "m"; Sym "."; T "in"; Sym "."],		(function [a;b;c;d] -> 	[ct [a;b] "między" "prep:inst"; ct [c;d] "inny" "adj:pl:inst:_:pos"] | _ -> failwith "abr_patterns");
  [T "m"; Sym "."; T "in"],			(function [a;b;c] -> 	[ct [a;b] "między" "prep:inst"; ct [c] "inny" "adj:pl:inst:_:pos"] | _ -> failwith "abr_patterns");
  [T "m"; Sym "."; T "inn"; Sym "."],		(function [a;b;c;d] -> 	[ct [a;b] "między" "prep:inst"; ct [c;d] "inny" "adj:pl:inst:_:pos"] | _ -> failwith "abr_patterns");
  [T "m"; Sym "."; T "st"; Sym "."],		(function [a;b;c;d] -> 	[ct [a;b] "miasto" "subst:_:$c:n:ncol"; ct [c;d] "stołeczny" "adj:_:$c:n:pos"] | _ -> failwith "abr_patterns");
  [T "m"; T "^"; T "2"],			(function [a;b;c] -> 	[ct [a] "metr" "subst:_:$c:m3"; ct [b;c] "kwadratowy" "adj:_:$c:m3:pos"] | _ -> failwith "abr_patterns");
  [T "m"; T "2"],				(function [a;b] -> 	[ct [a] "metr" "subst:_:$c:m3"; ct [b] "kwadratowy" "adj:_:$c:m3:pos"] | _ -> failwith "abr_patterns");
  [T "m"; T "3"],				(function [a;b] -> 	[ct [a] "metr" "subst:_:$c:m3"; ct [b] "sześcienny" "adj:_:$c:m3:pos"] | _ -> failwith "abr_patterns");
  (* [T "min"; Sym "."],				(function [a;b] -> 	std a b [1,"między","prep:inst";2,"inny","adj:pl:inst:_:pos"] | _ -> failwith "abr_patterns"); *)
  [T "mc"; Sym "."],				(function [a;b] -> 	std a b [1,"masa","subst:sg:$c:f";1,"ciało","subst:sg:gen:n:ncol"] | _ -> failwith "abr_patterns");
  [T "mkw"; Sym "."],				(function [a;b] -> 	std a b [1,"metr","subst:_:$c:m3";2,"kwadratowy","adj:_:$c:m3:pos"] | _ -> failwith "abr_patterns");
  [T "n"; Sym "."; T "e"; Sym "."],			(function [a;b;c;d] -> 	[ct [a;b] "nasz" "adj:sg:gen:f:pos"; ct [c;d] "era" "subst:sg:gen:f"] | _ -> failwith "abr_patterns");
  [T "n"; Sym "."; T "p"; Sym "."; T "m"; Sym "."],	(function [a;b;c;d;e;f] -> [ct [a;b] "nad" "prep:inst"; ct [c;d] "poziom" "subst:sg:inst:m3"; ct [e;f] "morze" "subst:sg:gen:n:ncol"] | _ -> failwith "abr_patterns");
  [T "np"; Sym "."],				(function [a;b] -> 	std a b [1,"na","prep:acc";1,"przykład","subst:sg:acc:m3"] | _ -> failwith "abr_patterns");
  [T "nt"; Sym "."],				(function [a;b] -> 	std a b [1,"na","prep:acc";1,"temat","subst:sg:acc:m3"] | _ -> failwith "abr_patterns");
  [T "NTG"],					(function [a] -> 	st a [1,"nie","qub";1,"ta","adj:sg:nom:f:pos";1,"grupa","subst:sg:nom:f"] | _ -> failwith "abr_patterns");
  [T "o"; Sym "."; T "o"; Sym "."],			(function [a;b;c;d] -> 	[ct [a;b] "ograniczony" "adj:sg:$c:f:pos"; ct [c;d] "odpowiedzialność" "subst:sg:$c:f"] | _ -> failwith "abr_patterns");
  [T "p"; Sym "."; T "n"; Sym "."; T "e"; Sym "."],	(function [a;b;c;d;e;f] -> [ct [a;b] "przed" "prep:inst"; ct [c;d] "nasz" "adj:sg:inst:f:pos"; ct [e;f] "era" "subst:sg:inst:f"] | _ -> failwith "abr_patterns");
  [T "p"; Sym "."; T "o"; Sym "."],			(function [a;b;c;d] -> 	[ct [a;b] "pełniący" "pact:_:_:m1.m2.m3:imperf:aff"; ct [c;d] "obowiązek" "subst:pl:acc:m3"] | _ -> failwith "abr_patterns");
  [T "p"; Sym "."; T "p"; Sym "."; T "m"; Sym "."],	(function [a;b;c;d;e;f] -> [ct [a;b] "pod" "prep:inst"; ct [c;d] "poziom" "subst:sg:inst:m3"; ct [e;f] "morze" "subst:sg:gen:n:ncol"] | _ -> failwith "abr_patterns");
  [T "p"; Sym "."; T "t"; Sym "."],			(function [a;b;c;d] -> 	[ct [a;b] "pod" "prep:inst:nwokc"; ct [c;d] "tytuł" "subst:sg:inst:m3"] | _ -> failwith "abr_patterns");
  [T "pn"; Sym "."],				(function [a;b] -> 	std a b [1,"pod","prep:inst";1,"nazwa","subst:sg:inst:f"] | _ -> failwith "abr_patterns");
  [T "pne"; Sym "."],				(function [a;b] -> 	std a b [1,"przed","prep:inst";1,"nasz","adj:sg:inst:f:pos";1,"era","subst:sg:inst:f"] | _ -> failwith "abr_patterns");
  [T "pt"; Sym "."],				(function [a;b] -> 	std a b [1,"pod","prep:inst";1,"tytuł","subst:sg:inst:m3"] | _ -> failwith "abr_patterns");
  [T "PW"],					(function [a] -> 	st a [1,"prywatny","adj:_:$c:f:pos";1,"wiadomość","subst:_:$c:f"] | _ -> failwith "abr_patterns");
  [T "pw"; Sym "."],				(function [a;b] -> 	std a b [1,"pod","prep:inst";1,"wezwanie","subst:sg:inst:n:ncol"] | _ -> failwith "abr_patterns");
(*  [T "S"; Sym "."; T "A"; Sym "."],			(function [a;b;c;d] -> 	[ct [a;b] "spółka" "subst:sg:$c:f"; ct [c;d] "akcyjny" "adj:sg:$c:f:pos"] | _ -> failwith "abr_patterns");
  [T "s"; Sym "."; T "c"; Sym "."],			(function [a;b;c;d] -> 	[ct [a;b] "spółka" "subst:sg:$c:f"; ct [c;d] "cywilny" "adj:sg:$c:f:pos"] | _ -> failwith "abr_patterns");*)
(*   [T "SA"],					(function [a] -> 	st a [1,"spółka","subst:sg:$c:f";1,"akcyjny","adj:sg:$c:f:pos"] | _ -> failwith "abr_patterns"); *)
  [T "ś"; Sym "."; T "p"; Sym "."],			(function [a;b;c;d] -> 	[ct [a;b] "święty" "adj:sg:gen:f:pos"; ct [c;d] "pamięć" "subst:sg:gen:f"] | _ -> failwith "abr_patterns");
  [T "śp"; Sym "."],				(function [a;b] -> 	std a b [1,"święty","adj:sg:gen:f:pos";1,"pamięć","subst:sg:gen:f"] | _ -> failwith "abr_patterns");
  [T "tgz"; Sym "."],				(function [a;b] -> 	std a b [2,"tak","adv";1,"zwać","ppas:_:_:_:_:aff"] | _ -> failwith "abr_patterns");
  [T "tj"; Sym "."],				(function [a;b] -> 	std a b [1,"to","subst:sg:nom:n:ncol";1,"być","fin:sg:ter:imperf"] | _ -> failwith "abr_patterns");
  [T "tzn"; Sym "."],				(function [a;b] -> 	std a b [1,"to","subst:sg:nom:n:ncol";2,"znaczyć","fin:sg:ter:imperf"] | _ -> failwith "abr_patterns");
  [T "tzw"; Sym "."],				(function [a;b] -> 	std a b [1,"tak","adv:pos";2,"zwać","ppas:_:_:_:imperf:aff"] | _ -> failwith "abr_patterns");
  [T "ub"; Sym "."; T "r"; Sym "."],		(function [a;b;c;d] -> 	[ct [a;b] "ubiegły" "adj:sg:$c:m3:pos"; ct [c;d] "rok" "subst:sg:$c:m3"] | _ -> failwith "abr_patterns");
  [T "w"; Sym "."; T "w"; Sym "."],			(function [a;b;c;d] -> 	[ct [a;b] "wysoko" "adv:com"; ct [c;d] "wymienić" "ppas:_:_:_:perf:aff"] | _ -> failwith "abr_patterns");
  [T "w"; T "/"; T "m"],			(function [a;b;c] -> 	[ct [a;b] "w" "prep:loc"; ct [c] "miejsce" "subst:_:loc:m3"] | _ -> failwith "abr_patterns");
  [T "w"; T "/"; T "w"],			(function [a;b;c] -> 	[ct [a;b] "wysoko" "adv:com"; ct [c] "wymienić" "ppas:_:_:_:perf:aff"] | _ -> failwith "abr_patterns");
  [T "ws"; Sym "."],				(function [a;b] -> 	std a b [1,"w","prep:loc:nwok";1,"sprawa","subst:sg:loc:f"] | _ -> failwith "abr_patterns");
  [T "ww"; Sym "."],				(function [a;b] -> 	std a b [1,"wysoko","adv:com";1,"wymieniony","ppas:_:_:_:perf:aff"] | _ -> failwith "abr_patterns");
  ]

(* let query_patterns = [
  [I "<query>"; S "."; O "u"; S "."],			(function [a;b;c;d] -> 	[ct [a;b] "bez" "prep:gen:nwok"; ct [c;d] "uwaga" "subst:pl:gen:f"] | _ -> failwith "abr_patterns"); *)

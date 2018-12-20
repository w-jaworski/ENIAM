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

(*let load_mte filename lines = lines @ File.load_lines filename

let load_mte_patterns () =
  let lines = File.catch_no_file (load_mte mte_filename) [] in
  let lines = File.catch_no_file (load_mte mte_filename2) lines in
  let l = List.rev (Xlist.rev_map lines (fun line ->
    match Str.split (Str.regexp "\t") line with
      [orths; lemma; interp] -> Str.split (Str.regexp " ") orths, lemma, interp
    | _ -> failwith ("load_mte_patterns: " ^ line))) in
  List.rev (Xlist.rev_map l (fun (orths,lemma,interp) ->
    Xlist.map orths (function
        "." -> S "."
      | "-" -> I "-"
      | "’" -> I "’"
      | "..." -> I "…"
      | orth -> T orth), (fun (_:token_env list) -> Tokens.make_lemma (lemma,interp))))*)

let mte_patterns = (ref [] : (pat list * (token_env list -> token)) list ref)

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

let acronym_patterns = [
(*  [L; I "-"; T "owscy"], (function [x;_;_] -> compose_lemma x "-owski" "adj:pl:nom.voc:m1:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owska"], (function [x;_;_] -> compose_lemma x "-owski" "adj:sg:nom.voc:f:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owski"], (function [x;_;_] -> compose_lemma x "-owski" "adj:sg:acc:m3:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owski"], (function [x;_;_] -> compose_lemma x "-owski" "adj:sg:nom.voc:m1.m2.m3:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owskich"], (function [x;_;_] -> compose_lemma x "-owski" "adj:pl:acc:m1:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owskich"], (function [x;_;_] -> compose_lemma x "-owski" "adj:pl:gen:m1.m2.m3.f.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owskich"], (function [x;_;_] -> compose_lemma x "-owski" "adj:pl:loc:m1.m2.m3.f.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owskie"], (function [x;_;_] -> compose_lemma x "-owski" "adj:pl:acc:m2.m3.f.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owskie"], (function [x;_;_] -> compose_lemma x "-owski" "adj:pl:nom.voc:m2.m3.f.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owskie"], (function [x;_;_] -> compose_lemma x "-owski" "adj:sg:acc:n:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owskie"], (function [x;_;_] -> compose_lemma x "-owski" "adj:sg:nom.voc:n:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owskiego"], (function [x;_;_] -> compose_lemma x "-owski" "adj:sg:acc:m1.m2:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owskiego"], (function [x;_;_] -> compose_lemma x "-owski" "adj:sg:gen:m1.m2.m3.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owskiej"], (function [x;_;_] -> compose_lemma x "-owski" "adj:sg:dat:f:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owskiej"], (function [x;_;_] -> compose_lemma x "-owski" "adj:sg:gen:f:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owskiej"], (function [x;_;_] -> compose_lemma x "-owski" "adj:sg:loc:f:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owskiemu"], (function [x;_;_] -> compose_lemma x "-owski" "adj:sg:dat:m1.m2.m3.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owskim"], (function [x;_;_] -> compose_lemma x "-owski" "adj:pl:dat:m1.m2.m3.f.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owskim"], (function [x;_;_] -> compose_lemma x "-owski" "adj:sg:inst:m1.m2.m3.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owskim"], (function [x;_;_] -> compose_lemma x "-owski" "adj:sg:loc:m1.m2.m3.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owskimi"], (function [x;_;_] -> compose_lemma x "-owski" "adj:pl:inst:m1.m2.m3.f.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owsko"], (function [x;_;_] -> compose_lemma x "-owski" "adja" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owsko"], (function [x;_;_] -> compose_lemma x "-owsko" "adv:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owsku"], (function [x;_;_] -> compose_lemma x "-owski" "adjp" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owską"], (function [x;_;_] -> compose_lemma x "-owski" "adj:sg:acc:f:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owską"], (function [x;_;_] -> compose_lemma x "-owski" "adj:sg:inst:f:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "wscy"], (function [x;_;_] -> compose_lemma x "-wski" "adj:pl:nom.voc:m1:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "wska"], (function [x;_;_] -> compose_lemma x "-wski" "adj:sg:nom.voc:f:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "wski"], (function [x;_;_] -> compose_lemma x "-wski" "adj:sg:acc:m3:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "wski"], (function [x;_;_] -> compose_lemma x "-wski" "adj:sg:nom.voc:m1.m2.m3:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "wskich"], (function [x;_;_] -> compose_lemma x "-wski" "adj:pl:acc:m1:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "wskich"], (function [x;_;_] -> compose_lemma x "-wski" "adj:pl:gen:m1.m2.m3.f.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "wskich"], (function [x;_;_] -> compose_lemma x "-wski" "adj:pl:loc:m1.m2.m3.f.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "wskie"], (function [x;_;_] -> compose_lemma x "-wski" "adj:pl:acc:m2.m3.f.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "wskie"], (function [x;_;_] -> compose_lemma x "-wski" "adj:pl:nom.voc:m2.m3.f.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "wskie"], (function [x;_;_] -> compose_lemma x "-wski" "adj:sg:acc:n:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "wskie"], (function [x;_;_] -> compose_lemma x "-wski" "adj:sg:nom.voc:n:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "wskiego"], (function [x;_;_] -> compose_lemma x "-wski" "adj:sg:acc:m1.m2:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "wskiego"], (function [x;_;_] -> compose_lemma x "-wski" "adj:sg:gen:m1.m2.m3.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "wskiej"], (function [x;_;_] -> compose_lemma x "-wski" "adj:sg:dat:f:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "wskiej"], (function [x;_;_] -> compose_lemma x "-wski" "adj:sg:gen:f:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "wskiej"], (function [x;_;_] -> compose_lemma x "-wski" "adj:sg:loc:f:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "wskiemu"], (function [x;_;_] -> compose_lemma x "-wski" "adj:sg:dat:m1.m2.m3.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "wskim"], (function [x;_;_] -> compose_lemma x "-wski" "adj:pl:dat:m1.m2.m3.f.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "wskim"], (function [x;_;_] -> compose_lemma x "-wski" "adj:sg:inst:m1.m2.m3.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "wskim"], (function [x;_;_] -> compose_lemma x "-wski" "adj:sg:loc:m1.m2.m3.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "wskimi"], (function [x;_;_] -> compose_lemma x "-wski" "adj:pl:inst:m1.m2.m3.f.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "wsko"], (function [x;_;_] -> compose_lemma x "-wski" "adja" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "wsko"], (function [x;_;_] -> compose_lemma x "-wsko" "adv:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "wsku"], (function [x;_;_] -> compose_lemma x "-wski" "adjp" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "wską"], (function [x;_;_] -> compose_lemma x "-wski" "adj:sg:acc:f:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "wską"], (function [x;_;_] -> compose_lemma x "-wski" "adj:sg:inst:f:pos" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owa"], (function [x;_;_] -> compose_lemma x "’owy" "adj:sg:nom.voc:f:pos" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owe"], (function [x;_;_] -> compose_lemma x "’owy" "adj:pl:acc:m2.m3.f.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owe"], (function [x;_;_] -> compose_lemma x "’owy" "adj:pl:nom.voc:m2.m3.f.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owe"], (function [x;_;_] -> compose_lemma x "’owy" "adj:sg:acc:n:pos" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owe"], (function [x;_;_] -> compose_lemma x "’owy" "adj:sg:nom.voc:n:pos" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owego"], (function [x;_;_] -> compose_lemma x "’owy" "adj:sg:acc:m1.m2:pos" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owego"], (function [x;_;_] -> compose_lemma x "’owy" "adj:sg:gen:m1.m2.m3.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owej"], (function [x;_;_] -> compose_lemma x "’owy" "adj:sg:dat:f:pos" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owej"], (function [x;_;_] -> compose_lemma x "’owy" "adj:sg:gen:f:pos" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owej"], (function [x;_;_] -> compose_lemma x "’owy" "adj:sg:loc:f:pos" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owemu"], (function [x;_;_] -> compose_lemma x "’owy" "adj:sg:dat:m1.m2.m3.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owi"], (function [x;_;_] -> compose_lemma x "’owy" "adj:pl:nom.voc:m1:pos" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owo"], (function [x;_;_] -> compose_lemma x "’owo" "adv:pos" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owo"], (function [x;_;_] -> compose_lemma x "’owy" "adja" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owy"], (function [x;_;_] -> compose_lemma x "’owy" "adj:sg:acc:m3:pos" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owy"], (function [x;_;_] -> compose_lemma x "’owy" "adj:sg:nom.voc:m1.m2.m3:pos" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owych"], (function [x;_;_] -> compose_lemma x "’owy" "adj:pl:acc:m1:pos" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owych"], (function [x;_;_] -> compose_lemma x "’owy" "adj:pl:gen:m1.m2.m3.f.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owych"], (function [x;_;_] -> compose_lemma x "’owy" "adj:pl:loc:m1.m2.m3.f.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owym"], (function [x;_;_] -> compose_lemma x "’owy" "adj:pl:dat:m1.m2.m3.f.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owym"], (function [x;_;_] -> compose_lemma x "’owy" "adj:sg:inst:m1.m2.m3.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owym"], (function [x;_;_] -> compose_lemma x "’owy" "adj:sg:loc:m1.m2.m3.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owymi"], (function [x;_;_] -> compose_lemma x "’owy" "adj:pl:inst:m1.m2.m3.f.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "ową"], (function [x;_;_] -> compose_lemma x "’owy" "adj:sg:acc:f:pos" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "ową"], (function [x;_;_] -> compose_lemma x "’owy" "adj:sg:inst:f:pos" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owscy"], (function [x;_;_] -> compose_lemma x "’owski" "adj:pl:nom.voc:m1:pos" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owska"], (function [x;_;_] -> compose_lemma x "’owski" "adj:sg:nom.voc:f:pos" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owski"], (function [x;_;_] -> compose_lemma x "’owski" "adj:sg:acc:m3:pos" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owski"], (function [x;_;_] -> compose_lemma x "’owski" "adj:sg:nom.voc:m1.m2.m3:pos" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owskich"], (function [x;_;_] -> compose_lemma x "’owski" "adj:pl:acc:m1:pos" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owskich"], (function [x;_;_] -> compose_lemma x "’owski" "adj:pl:gen:m1.m2.m3.f.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owskich"], (function [x;_;_] -> compose_lemma x "’owski" "adj:pl:loc:m1.m2.m3.f.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owskie"], (function [x;_;_] -> compose_lemma x "’owski" "adj:pl:acc:m2.m3.f.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owskie"], (function [x;_;_] -> compose_lemma x "’owski" "adj:pl:nom.voc:m2.m3.f.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owskie"], (function [x;_;_] -> compose_lemma x "’owski" "adj:sg:acc:n:pos" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owskie"], (function [x;_;_] -> compose_lemma x "’owski" "adj:sg:nom.voc:n:pos" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owskiego"], (function [x;_;_] -> compose_lemma x "’owski" "adj:sg:acc:m1.m2:pos" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owskiego"], (function [x;_;_] -> compose_lemma x "’owski" "adj:sg:gen:m1.m2.m3.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owskiej"], (function [x;_;_] -> compose_lemma x "’owski" "adj:sg:dat:f:pos" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owskiej"], (function [x;_;_] -> compose_lemma x "’owski" "adj:sg:gen:f:pos" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owskiej"], (function [x;_;_] -> compose_lemma x "’owski" "adj:sg:loc:f:pos" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owskiemu"], (function [x;_;_] -> compose_lemma x "’owski" "adj:sg:dat:m1.m2.m3.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owskim"], (function [x;_;_] -> compose_lemma x "’owski" "adj:pl:dat:m1.m2.m3.f.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owskim"], (function [x;_;_] -> compose_lemma x "’owski" "adj:sg:inst:m1.m2.m3.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owskim"], (function [x;_;_] -> compose_lemma x "’owski" "adj:sg:loc:m1.m2.m3.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owskimi"], (function [x;_;_] -> compose_lemma x "’owski" "adj:pl:inst:m1.m2.m3.f.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owsko"], (function [x;_;_] -> compose_lemma x "’owski" "adja" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owsko"], (function [x;_;_] -> compose_lemma x "’owsko" "adv:pos" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owsku"], (function [x;_;_] -> compose_lemma x "’owski" "adjp" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owską"], (function [x;_;_] -> compose_lemma x "’owski" "adj:sg:acc:f:pos" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owską"], (function [x;_;_] -> compose_lemma x "’owski" "adj:sg:inst:f:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "a"], (function [x;_;_] -> compose_lemma x "" "subst:sg:gen:m1.m2.m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "u"], (function [x;_;_] -> compose_lemma x "" "subst:sg:gen:m2.m3.n:ncol" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owi"], (function [x;_;_] -> compose_lemma x "" "subst:sg:dat:m1.m2.m3.n:ncol" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "a"], (function [x;_;_] -> compose_lemma x "" "subst:sg:acc:m1.m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "u"], (function [x;_;_] -> compose_lemma x "" "subst:sg:acc:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "iem"], (function [x;_;_] -> compose_lemma x "" "subst:sg:inst:m2.m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "em"], (function [x;_;_] -> compose_lemma x "" "subst:sg:inst:m1.m2.m3.n:ncol" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "ie"], (function [x;_;_] -> compose_lemma x "" "subst:sg:loc.voc:m1.m2.m3" | _ -> failwith "acronym_patterns");
  [CL; I "-"; T "cie"], (function [x;_;_] -> compose_lemma x "T" "subst:sg:loc.voc:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "ze"], (function [x;_;_] -> compose_lemma x "" "subst:sg:loc.voc:m2.m3.n:ncol" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "zie"], (function [x;_;_] -> compose_lemma x "" "subst:sg:loc.voc:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "u"], (function [x;_;_] -> compose_lemma x "" "subst:sg:loc.voc:m1.m2.m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "e"], (function [x;_;_] -> compose_lemma x "" "depr:pl:nom.voc:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "y"], (function [x;_;_] -> compose_lemma x "" "depr:pl:nom.voc:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owie"], (function [x;_;_] -> compose_lemma x "" "subst:pl:nom.voc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "y"], (function [x;_;_] -> compose_lemma x "" "subst:pl:nom.acc.voc:m2.m3.n:ncol" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "e"], (function [x;_;_] -> compose_lemma x "" "subst:pl:nom.voc:m1.m2.m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "i"], (function [x;_;_] -> compose_lemma x "" "subst:pl:nom.voc:m2.m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "i"], (function [x;_;_] -> compose_lemma x "" "subst:pl:gen:m1.m2.m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "ów"], (function [x;_;_] -> compose_lemma x "" "subst:pl:gen:m1.m2.m3.n:ncol" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "om"], (function [x;_;_] -> compose_lemma x "" "subst:pl:dat:m1.m2.m3.n:ncol" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "ów"], (function [x;_;_] -> compose_lemma x "" "subst:pl:acc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "i"], (function [x;_;_] -> compose_lemma x "" "subst:pl:acc:m1.m2.m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "e"], (function [x;_;_] -> compose_lemma x "" "subst:pl:acc:m2.m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "ami"], (function [x;_;_] -> compose_lemma x "" "subst:pl:inst:m1.m2.m3.n:ncol" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "ach"], (function [x;_;_] -> compose_lemma x "" "subst:pl:loc:m1.m2.m3.n:ncol" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "etu"], (function [x;_;_] -> compose_lemma x "" "subst:sg:gen:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "etowi"], (function [x;_;_] -> compose_lemma x "" "subst:sg:dat:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "etem"], (function [x;_;_] -> compose_lemma x "" "subst:sg:inst:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "ecie"], (function [x;_;_] -> compose_lemma x "" "subst:sg:loc.voc:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "ety"], (function [x;_;_] -> compose_lemma x "" "subst:pl:nom.acc.voc:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "etów"], (function [x;_;_] -> compose_lemma x "" "subst:pl:gen:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "etom"], (function [x;_;_] -> compose_lemma x "" "subst:pl:dat:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "etami"], (function [x;_;_] -> compose_lemma x "" "subst:pl:inst:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "etach"], (function [x;_;_] -> compose_lemma x "" "subst:pl:loc:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "ocie"], (function [x;_;_] -> compose_lemma x "" "subst:sg:loc.voc:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "otach"], (function [x;_;_] -> compose_lemma x "" "subst:pl:loc:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "otami"], (function [x;_;_] -> compose_lemma x "" "subst:pl:inst:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "otem"], (function [x;_;_] -> compose_lemma x "" "subst:sg:inst:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "otom"], (function [x;_;_] -> compose_lemma x "" "subst:pl:dat:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "otowi"], (function [x;_;_] -> compose_lemma x "" "subst:sg:dat:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "otu"], (function [x;_;_] -> compose_lemma x "" "subst:sg:gen:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "oty"], (function [x;_;_] -> compose_lemma x "" "subst:pl:nom.acc.voc:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "otów"], (function [x;_;_] -> compose_lemma x "" "subst:pl:gen:m3" | _ -> failwith "acronym_patterns");
  [CL; I "-"; T "i"], (function [x;_;_] -> compose_lemma x "A" "subst:sg:gen.dat.loc:f" | _ -> failwith "acronym_patterns");
  [CL; I "-"; T "y"], (function [x;_;_] -> compose_lemma x "A" "subst:sg:gen:f" | _ -> failwith "acronym_patterns");
  [CL; I "-"; T "ie"], (function [x;_;_] -> compose_lemma x "A" "subst:sg:dat.loc:f" | _ -> failwith "acronym_patterns");
  [CL; I "-"; T "cie"], (function [x;_;_] -> compose_lemma x "TA" "subst:sg:dat.loc:f" | _ -> failwith "acronym_patterns");
  [CL; I "-"; T "ę"], (function [x;_;_] -> compose_lemma x "A" "subst:sg:acc:f" | _ -> failwith "acronym_patterns");
  [CL; I "-"; T "ą"], (function [x;_;_] -> compose_lemma x "A" "subst:sg:inst:f" | _ -> failwith "acronym_patterns");
  [CL; I "-"; T "o"], (function [x;_;_] -> compose_lemma x "A" "subst:sg:voc:f" | _ -> failwith "acronym_patterns");
  [CL; I "-"; T "y"], (function [x;_;_] -> compose_lemma x "A" "subst:pl:nom.acc.voc:f" | _ -> failwith "acronym_patterns");
  [CL; I "-"; T "e"], (function [x;_;_] -> compose_lemma x "A" "subst:pl:nom.acc.voc:f" | _ -> failwith "acronym_patterns");
  [CL; I "-"; T "i"], (function [x;_;_] -> compose_lemma x "A" "subst:pl:gen:f" | _ -> failwith "acronym_patterns");
  [CL; I "-"; T "om"], (function [x;_;_] -> compose_lemma x "A" "subst:pl:dat:f" | _ -> failwith "acronym_patterns");
  [CL; I "-"; T "ami"], (function [x;_;_] -> compose_lemma x "A" "subst:pl:inst:f" | _ -> failwith "acronym_patterns");
  [CL; I "-"; T "ach"], (function [x;_;_] -> compose_lemma x "A" "subst:pl:loc:f" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "a"], (function [x;_;_] -> compose_lemma x "" "subst:sg:acc:m1.m2" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "a"], (function [x;_;_] -> compose_lemma x "" "subst:sg:gen:m1.m2.m3" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "ach"], (function [x;_;_] -> compose_lemma x "" "subst:pl:loc:m1.m2.m3" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "ach"], (function [x;_;_] -> compose_lemma x "s" "subst:pl:loc:n:pt" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "ami"], (function [x;_;_] -> compose_lemma x "" "subst:pl:inst:m1.m2.m3" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "ami"], (function [x;_;_] -> compose_lemma x "s" "subst:pl:inst:n:pt" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "e"], (function [x;_;_] -> compose_lemma x "" "depr:pl:nom.voc:m2" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "e"], (function [x;_;_] -> compose_lemma x "" "subst:pl:nom.acc.voc:m2.m3" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "ego"], (function [x;_;_] -> compose_lemma x "" "subst:sg:acc:m1" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "ego"], (function [x;_;_] -> compose_lemma x "" "subst:sg:gen:m1" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "em"], (function [x;_;_] -> compose_lemma x "" "subst:sg:inst:m1.m2.m3" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "emu"], (function [x;_;_] -> compose_lemma x "" "subst:sg:dat:m1" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "go"], (function [x;_;_] -> compose_lemma x "" "subst:sg:acc:m1" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "go"], (function [x;_;_] -> compose_lemma x "" "subst:sg:gen:m1" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "i"], (function [x;_;_] -> compose_lemma x "" "subst:pl:gen:m2.m3" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "m"], (function [x;_;_] -> compose_lemma x "" "subst:sg:inst.loc:m1" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "mu"], (function [x;_;_] -> compose_lemma x "" "subst:sg:dat:m1" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "om"], (function [x;_;_] -> compose_lemma x "" "subst:pl:dat:m1.m2.m3" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "om"], (function [x;_;_] -> compose_lemma x "s" "subst:pl:dat:n:pt" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owi"], (function [x;_;_] -> compose_lemma x "" "subst:sg:dat:m1.m2.m3" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owie"], (function [x;_;_] -> compose_lemma x "" "subst:pl:nom.voc:m1" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "u"], (function [x;_;_] -> compose_lemma x "" "subst:sg:gen:m3" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "u"], (function [x;_;_] -> compose_lemma x "" "subst:sg:loc.voc:m1.m2.m3" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "y"], (function [x;_;_] -> compose_lemma x "" "depr:pl:nom.voc:m2" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "y"], (function [x;_;_] -> compose_lemma x "" "subst:pl:acc.nom.voc:m2.m3" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "y"], (function [x;_;_] -> compose_lemma x "" "subst:pl:gen:m3" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "ów"], (function [x;_;_] -> compose_lemma x "" "subst:pl:acc:m1" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "ów"], (function [x;_;_] -> compose_lemma x "" "subst:pl:gen:m1.m2.m3" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "ów"], (function [x;_;_] -> compose_lemma x "s" "subst:pl:gen:n:pt" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "ista"], (function [x;_;_] -> compose_lemma x "-ista" "subst:sg:nom:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "istach"], (function [x;_;_] -> compose_lemma x "-ista" "subst:pl:loc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "istami"], (function [x;_;_] -> compose_lemma x "-ista" "subst:pl:inst:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "isto"], (function [x;_;_] -> compose_lemma x "-ista" "subst:sg:voc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "istom"], (function [x;_;_] -> compose_lemma x "-ista" "subst:pl:dat:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "isty"], (function [x;_;_] -> compose_lemma x "-ista" "depr:pl:nom:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "isty"], (function [x;_;_] -> compose_lemma x "-ista" "depr:pl:voc:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "isty"], (function [x;_;_] -> compose_lemma x "-ista" "subst:sg:gen:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "istów"], (function [x;_;_] -> compose_lemma x "-ista" "subst:pl:acc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "istów"], (function [x;_;_] -> compose_lemma x "-ista" "subst:pl:gen:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "istą"], (function [x;_;_] -> compose_lemma x "-ista" "subst:sg:inst:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "istę"], (function [x;_;_] -> compose_lemma x "-ista" "subst:sg:acc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "iści"], (function [x;_;_] -> compose_lemma x "-ista" "subst:pl:nom:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "iści"], (function [x;_;_] -> compose_lemma x "-ista" "subst:pl:voc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "iście"], (function [x;_;_] -> compose_lemma x "-ista" "subst:sg:dat:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "iście"], (function [x;_;_] -> compose_lemma x "-ista" "subst:sg:loc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owca"], (function [x;_;_] -> compose_lemma x "-owiec" "subst:sg:acc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owca"], (function [x;_;_] -> compose_lemma x "-owiec" "subst:sg:gen:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owcach"], (function [x;_;_] -> compose_lemma x "-owiec" "subst:pl:loc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owcami"], (function [x;_;_] -> compose_lemma x "-owiec" "subst:pl:inst:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owce"], (function [x;_;_] -> compose_lemma x "-owiec" "depr:pl:nom:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owce"], (function [x;_;_] -> compose_lemma x "-owiec" "depr:pl:voc:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owcem"], (function [x;_;_] -> compose_lemma x "-owiec" "subst:sg:inst:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owcom"], (function [x;_;_] -> compose_lemma x "-owiec" "subst:pl:dat:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owcowi"], (function [x;_;_] -> compose_lemma x "-owiec" "subst:sg:dat:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owcu"], (function [x;_;_] -> compose_lemma x "-owiec" "subst:sg:loc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owcu"], (function [x;_;_] -> compose_lemma x "-owiec" "subst:sg:voc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owcy"], (function [x;_;_] -> compose_lemma x "-owiec" "subst:pl:nom:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owcy"], (function [x;_;_] -> compose_lemma x "-owiec" "subst:pl:voc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owcze"], (function [x;_;_] -> compose_lemma x "-owiec" "subst:sg:voc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owców"], (function [x;_;_] -> compose_lemma x "-owiec" "subst:pl:acc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owców"], (function [x;_;_] -> compose_lemma x "-owiec" "subst:pl:gen:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owiec"], (function [x;_;_] -> compose_lemma x "-owiec" "subst:sg:nom:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owskości"], (function [x;_;_] -> compose_lemma x "-owskość" "subst:pl:acc:f" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owskości"], (function [x;_;_] -> compose_lemma x "-owskość" "subst:pl:gen:f" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owskości"], (function [x;_;_] -> compose_lemma x "-owskość" "subst:pl:nom:f" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owskości"], (function [x;_;_] -> compose_lemma x "-owskość" "subst:pl:voc:f" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owskości"], (function [x;_;_] -> compose_lemma x "-owskość" "subst:sg:dat:f" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owskości"], (function [x;_;_] -> compose_lemma x "-owskość" "subst:sg:gen:f" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owskości"], (function [x;_;_] -> compose_lemma x "-owskość" "subst:sg:loc:f" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owskości"], (function [x;_;_] -> compose_lemma x "-owskość" "subst:sg:voc:f" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owskościach"], (function [x;_;_] -> compose_lemma x "-owskość" "subst:pl:loc:f" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owskościami"], (function [x;_;_] -> compose_lemma x "-owskość" "subst:pl:inst:f" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owskościom"], (function [x;_;_] -> compose_lemma x "-owskość" "subst:pl:dat:f" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owskością"], (function [x;_;_] -> compose_lemma x "-owskość" "subst:sg:inst:f" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owskość"], (function [x;_;_] -> compose_lemma x "-owskość" "subst:sg:acc:f" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "owskość"], (function [x;_;_] -> compose_lemma x "-owskość" "subst:sg:nom:f" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "wca"], (function [x;_;_] -> compose_lemma x "-wiec" "subst:sg:acc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "wca"], (function [x;_;_] -> compose_lemma x "-wiec" "subst:sg:gen:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "wcach"], (function [x;_;_] -> compose_lemma x "-wiec" "subst:pl:loc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "wcami"], (function [x;_;_] -> compose_lemma x "-wiec" "subst:pl:inst:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "wce"], (function [x;_;_] -> compose_lemma x "-wiec" "depr:pl:nom:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "wce"], (function [x;_;_] -> compose_lemma x "-wiec" "depr:pl:voc:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "wcem"], (function [x;_;_] -> compose_lemma x "-wiec" "subst:sg:inst:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "wcom"], (function [x;_;_] -> compose_lemma x "-wiec" "subst:pl:dat:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "wcowi"], (function [x;_;_] -> compose_lemma x "-wiec" "subst:sg:dat:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "wcu"], (function [x;_;_] -> compose_lemma x "-wiec" "subst:sg:loc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "wcu"], (function [x;_;_] -> compose_lemma x "-wiec" "subst:sg:voc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "wcy"], (function [x;_;_] -> compose_lemma x "-wiec" "subst:pl:nom:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "wcy"], (function [x;_;_] -> compose_lemma x "-wiec" "subst:pl:voc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "wców"], (function [x;_;_] -> compose_lemma x "-wiec" "subst:pl:acc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "wców"], (function [x;_;_] -> compose_lemma x "-wiec" "subst:pl:gen:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; T "wiec"], (function [x;_;_] -> compose_lemma x "-wiec" "subst:sg:nom:m1" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owości"], (function [x;_;_] -> compose_lemma x "’owość" "subst:pl:acc:f" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owości"], (function [x;_;_] -> compose_lemma x "’owość" "subst:pl:gen:f" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owości"], (function [x;_;_] -> compose_lemma x "’owość" "subst:pl:nom:f" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owości"], (function [x;_;_] -> compose_lemma x "’owość" "subst:pl:voc:f" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owości"], (function [x;_;_] -> compose_lemma x "’owość" "subst:sg:dat:f" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owości"], (function [x;_;_] -> compose_lemma x "’owość" "subst:sg:gen:f" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owości"], (function [x;_;_] -> compose_lemma x "’owość" "subst:sg:loc:f" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owości"], (function [x;_;_] -> compose_lemma x "’owość" "subst:sg:voc:f" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owościach"], (function [x;_;_] -> compose_lemma x "’owość" "subst:pl:loc:f" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owościami"], (function [x;_;_] -> compose_lemma x "’owość" "subst:pl:inst:f" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owościom"], (function [x;_;_] -> compose_lemma x "’owość" "subst:pl:dat:f" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owością"], (function [x;_;_] -> compose_lemma x "’owość" "subst:sg:inst:f" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owość"], (function [x;_;_] -> compose_lemma x "’owość" "subst:sg:acc:f" | _ -> failwith "acronym_patterns");
  [L; I "’"; T "owość"], (function [x;_;_] -> compose_lemma x "’owość" "subst:sg:nom:f" | _ -> failwith "acronym_patterns");

  [L; I "-"; L; I "-"; T "owscy"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owski" "adj:pl:nom.voc:m1:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owska"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owski" "adj:sg:nom.voc:f:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owski"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owski" "adj:sg:acc:m3:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owski"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owski" "adj:sg:nom.voc:m1.m2.m3:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owskich"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owski" "adj:pl:acc:m1:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owskich"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owski" "adj:pl:gen:m1.m2.m3.f.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owskich"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owski" "adj:pl:loc:m1.m2.m3.f.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owskie"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owski" "adj:pl:acc:m2.m3.f.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owskie"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owski" "adj:pl:nom.voc:m2.m3.f.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owskie"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owski" "adj:sg:acc:n:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owskie"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owski" "adj:sg:nom.voc:n:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owskiego"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owski" "adj:sg:acc:m1.m2:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owskiego"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owski" "adj:sg:gen:m1.m2.m3.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owskiej"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owski" "adj:sg:dat:f:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owskiej"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owski" "adj:sg:gen:f:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owskiej"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owski" "adj:sg:loc:f:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owskiemu"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owski" "adj:sg:dat:m1.m2.m3.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owskim"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owski" "adj:pl:dat:m1.m2.m3.f.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owskim"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owski" "adj:sg:inst:m1.m2.m3.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owskim"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owski" "adj:sg:loc:m1.m2.m3.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owskimi"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owski" "adj:pl:inst:m1.m2.m3.f.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owsko"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owski" "adja" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owsko"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owsko" "adv:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owsku"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owski" "adjp" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owską"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owski" "adj:sg:acc:f:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owską"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owski" "adj:sg:inst:f:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "wscy"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-wski" "adj:pl:nom.voc:m1:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "wska"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-wski" "adj:sg:nom.voc:f:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "wski"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-wski" "adj:sg:acc:m3:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "wski"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-wski" "adj:sg:nom.voc:m1.m2.m3:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "wskich"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-wski" "adj:pl:acc:m1:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "wskich"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-wski" "adj:pl:gen:m1.m2.m3.f.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "wskich"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-wski" "adj:pl:loc:m1.m2.m3.f.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "wskie"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-wski" "adj:pl:acc:m2.m3.f.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "wskie"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-wski" "adj:pl:nom.voc:m2.m3.f.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "wskie"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-wski" "adj:sg:acc:n:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "wskie"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-wski" "adj:sg:nom.voc:n:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "wskiego"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-wski" "adj:sg:acc:m1.m2:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "wskiego"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-wski" "adj:sg:gen:m1.m2.m3.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "wskiej"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-wski" "adj:sg:dat:f:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "wskiej"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-wski" "adj:sg:gen:f:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "wskiej"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-wski" "adj:sg:loc:f:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "wskiemu"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-wski" "adj:sg:dat:m1.m2.m3.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "wskim"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-wski" "adj:pl:dat:m1.m2.m3.f.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "wskim"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-wski" "adj:sg:inst:m1.m2.m3.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "wskim"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-wski" "adj:sg:loc:m1.m2.m3.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "wskimi"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-wski" "adj:pl:inst:m1.m2.m3.f.n:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "wsko"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-wski" "adja" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "wsko"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-wsko" "adv:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "wsku"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-wski" "adjp" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "wską"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-wski" "adj:sg:acc:f:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "wską"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-wski" "adj:sg:inst:f:pos" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "a"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:acc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "a"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:acc:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "a"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:gen:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "a"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:gen:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "a"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:gen:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "ach"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:loc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "ach"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:loc:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "ach"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:loc:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "ach"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:loc:n:ncol" | _ -> failwith "acronym_patterns");
  [CL; I "-"; CL; I "-"; T "ach"], (function [x;y;z;_;_] -> compose_lemma3 x y z "A" "subst:pl:loc:f" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "ami"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:inst:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "ami"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:inst:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "ami"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:inst:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "ami"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:inst:n:ncol" | _ -> failwith "acronym_patterns");
  [CL; I "-"; CL; I "-"; T "ami"], (function [x;y;z;_;_] -> compose_lemma3 x y z "A" "subst:pl:inst:f" | _ -> failwith "acronym_patterns");
  [CL; I "-"; CL; I "-"; T "cie"], (function [x;y;z;_;_] -> compose_lemma3 x y z "T" "subst:sg:loc:m3" | _ -> failwith "acronym_patterns");
  [CL; I "-"; CL; I "-"; T "cie"], (function [x;y;z;_;_] -> compose_lemma3 x y z "T" "subst:sg:voc:m3" | _ -> failwith "acronym_patterns");
  [CL; I "-"; CL; I "-"; T "cie"], (function [x;y;z;_;_] -> compose_lemma3 x y z "TA" "subst:sg:dat:f" | _ -> failwith "acronym_patterns");
  [CL; I "-"; CL; I "-"; T "cie"], (function [x;y;z;_;_] -> compose_lemma3 x y z "TA" "subst:sg:loc:f" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "e"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "depr:pl:nom:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "e"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "depr:pl:voc:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "e"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:acc:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "e"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:acc:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "e"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:nom:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "e"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:nom:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "e"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:nom:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "e"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:voc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "e"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:voc:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "e"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:voc:m3" | _ -> failwith "acronym_patterns");
  [CL; I "-"; CL; I "-"; T "e"], (function [x;y;z;_;_] -> compose_lemma3 x y z "A" "subst:pl:acc:f" | _ -> failwith "acronym_patterns");
  [CL; I "-"; CL; I "-"; T "e"], (function [x;y;z;_;_] -> compose_lemma3 x y z "A" "subst:pl:nom:f" | _ -> failwith "acronym_patterns");
  [CL; I "-"; CL; I "-"; T "e"], (function [x;y;z;_;_] -> compose_lemma3 x y z "A" "subst:pl:voc:f" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "ecie"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:loc:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "ecie"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:voc:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "em"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:inst:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "em"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:inst:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "em"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:inst:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "em"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:inst:n:ncol" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "etach"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:loc:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "etami"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:inst:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "etem"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:inst:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "etom"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:dat:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "etowi"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:dat:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "etu"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:gen:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "ety"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:acc:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "ety"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:nom:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "ety"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:voc:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "etów"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:gen:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "i"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:acc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "i"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:acc:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "i"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:acc:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "i"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:gen:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "i"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:gen:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "i"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:nom:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "i"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:nom:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "i"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:voc:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "i"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:voc:m3" | _ -> failwith "acronym_patterns");
  [CL; I "-"; CL; I "-"; T "i"], (function [x;y;z;_;_] -> compose_lemma3 x y z "A" "subst:pl:gen:f" | _ -> failwith "acronym_patterns");
  [CL; I "-"; CL; I "-"; T "i"], (function [x;y;z;_;_] -> compose_lemma3 x y z "A" "subst:sg:dat:f" | _ -> failwith "acronym_patterns");
  [CL; I "-"; CL; I "-"; T "i"], (function [x;y;z;_;_] -> compose_lemma3 x y z "A" "subst:sg:gen:f" | _ -> failwith "acronym_patterns");
  [CL; I "-"; CL; I "-"; T "i"], (function [x;y;z;_;_] -> compose_lemma3 x y z "A" "subst:sg:loc:f" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "ie"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:loc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "ie"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:loc:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "ie"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:loc:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "ie"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:voc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "ie"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:voc:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "ie"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:voc:m3" | _ -> failwith "acronym_patterns");
  [CL; I "-"; CL; I "-"; T "ie"], (function [x;y;z;_;_] -> compose_lemma3 x y z "A" "subst:sg:dat:f" | _ -> failwith "acronym_patterns");
  [CL; I "-"; CL; I "-"; T "ie"], (function [x;y;z;_;_] -> compose_lemma3 x y z "A" "subst:sg:loc:f" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "iem"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:inst:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "iem"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:inst:m3" | _ -> failwith "acronym_patterns");
  [CL; I "-"; CL; I "-"; T "o"], (function [x;y;z;_;_] -> compose_lemma3 x y z "A" "subst:sg:voc:f" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "ocie"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:loc:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "ocie"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:voc:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "om"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:dat:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "om"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:dat:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "om"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:dat:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "om"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:dat:n:ncol" | _ -> failwith "acronym_patterns");
  [CL; I "-"; CL; I "-"; T "om"], (function [x;y;z;_;_] -> compose_lemma3 x y z "A" "subst:pl:dat:f" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "otach"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:loc:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "otami"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:inst:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "otem"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:inst:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "otom"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:dat:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "otowi"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:dat:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "otu"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:gen:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "oty"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:acc:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "oty"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:nom:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "oty"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:voc:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "otów"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:gen:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owi"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:dat:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owi"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:dat:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owi"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:dat:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owi"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:dat:n:ncol" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owie"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:nom:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owie"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:voc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "u"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:acc:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "u"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:gen:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "u"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:gen:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "u"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:gen:n:ncol" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "u"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:loc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "u"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:loc:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "u"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:loc:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "u"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:voc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "u"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:voc:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "u"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:voc:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "y"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "depr:pl:nom:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "y"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "depr:pl:voc:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "y"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:acc:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "y"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:acc:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "y"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:acc:n:ncol" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "y"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:nom:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "y"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:nom:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "y"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:nom:n:ncol" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "y"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:voc:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "y"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:voc:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "y"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:voc:n:ncol" | _ -> failwith "acronym_patterns");
  [CL; I "-"; CL; I "-"; T "y"], (function [x;y;z;_;_] -> compose_lemma3 x y z "A" "subst:pl:acc:f" | _ -> failwith "acronym_patterns");
  [CL; I "-"; CL; I "-"; T "y"], (function [x;y;z;_;_] -> compose_lemma3 x y z "A" "subst:pl:nom:f" | _ -> failwith "acronym_patterns");
  [CL; I "-"; CL; I "-"; T "y"], (function [x;y;z;_;_] -> compose_lemma3 x y z "A" "subst:pl:voc:f" | _ -> failwith "acronym_patterns");
  [CL; I "-"; CL; I "-"; T "y"], (function [x;y;z;_;_] -> compose_lemma3 x y z "A" "subst:sg:gen:f" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "ze"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:loc:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "ze"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:loc:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "ze"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:loc:n:ncol" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "ze"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:voc:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "ze"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:voc:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "ze"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:voc:n:ncol" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "zie"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:loc:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "zie"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:voc:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "ów"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:acc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "ów"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:gen:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "ów"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:gen:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "ów"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:gen:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "ów"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:gen:n:ncol" | _ -> failwith "acronym_patterns");
  [CL; I "-"; CL; I "-"; T "ą"], (function [x;y;z;_;_] -> compose_lemma3 x y z "A" "subst:sg:inst:f" | _ -> failwith "acronym_patterns");
  [CL; I "-"; CL; I "-"; T "ę"], (function [x;y;z;_;_] -> compose_lemma3 x y z "A" "subst:sg:acc:f" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "ista"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-ista" "subst:sg:nom:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "istach"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-ista" "subst:pl:loc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "istami"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-ista" "subst:pl:inst:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "isto"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-ista" "subst:sg:voc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "istom"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-ista" "subst:pl:dat:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "isty"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-ista" "depr:pl:nom:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "isty"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-ista" "depr:pl:voc:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "isty"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-ista" "subst:sg:gen:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "istów"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-ista" "subst:pl:acc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "istów"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-ista" "subst:pl:gen:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "istą"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-ista" "subst:sg:inst:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "istę"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-ista" "subst:sg:acc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "iści"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-ista" "subst:pl:nom:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "iści"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-ista" "subst:pl:voc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "iście"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-ista" "subst:sg:dat:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "iście"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-ista" "subst:sg:loc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owca"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owiec" "subst:sg:acc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owca"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owiec" "subst:sg:gen:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owcach"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owiec" "subst:pl:loc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owcami"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owiec" "subst:pl:inst:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owce"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owiec" "depr:pl:nom:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owce"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owiec" "depr:pl:voc:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owcem"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owiec" "subst:sg:inst:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owcom"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owiec" "subst:pl:dat:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owcowi"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owiec" "subst:sg:dat:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owcu"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owiec" "subst:sg:loc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owcu"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owiec" "subst:sg:voc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owcy"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owiec" "subst:pl:nom:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owcy"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owiec" "subst:pl:voc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owcze"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owiec" "subst:sg:voc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owców"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owiec" "subst:pl:acc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owców"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owiec" "subst:pl:gen:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owiec"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owiec" "subst:sg:nom:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owskości"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owskość" "subst:pl:acc:f" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owskości"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owskość" "subst:pl:gen:f" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owskości"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owskość" "subst:pl:nom:f" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owskości"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owskość" "subst:pl:voc:f" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owskości"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owskość" "subst:sg:dat:f" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owskości"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owskość" "subst:sg:gen:f" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owskości"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owskość" "subst:sg:loc:f" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owskości"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owskość" "subst:sg:voc:f" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owskościach"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owskość" "subst:pl:loc:f" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owskościami"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owskość" "subst:pl:inst:f" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owskościom"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owskość" "subst:pl:dat:f" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owskością"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owskość" "subst:sg:inst:f" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owskość"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owskość" "subst:sg:acc:f" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "owskość"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-owskość" "subst:sg:nom:f" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "wca"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-wiec" "subst:sg:acc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "wca"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-wiec" "subst:sg:gen:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "wcach"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-wiec" "subst:pl:loc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "wcami"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-wiec" "subst:pl:inst:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "wce"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-wiec" "depr:pl:nom:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "wce"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-wiec" "depr:pl:voc:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "wcem"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-wiec" "subst:sg:inst:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "wcom"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-wiec" "subst:pl:dat:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "wcowi"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-wiec" "subst:sg:dat:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "wcu"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-wiec" "subst:sg:loc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "wcu"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-wiec" "subst:sg:voc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "wcy"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-wiec" "subst:pl:nom:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "wcy"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-wiec" "subst:pl:voc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "wców"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-wiec" "subst:pl:acc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "wców"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-wiec" "subst:pl:gen:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "-"; T "wiec"], (function [x;y;z;_;_] -> compose_lemma3 x y z "-wiec" "subst:sg:nom:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "a"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:acc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "a"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:acc:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "a"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:gen:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "a"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:gen:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "a"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:gen:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "ach"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:loc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "ach"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:loc:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "ach"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:loc:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "ach"], (function [x;y;z;_;_] -> compose_lemma3 x y z "s" "subst:pl:loc:n:pt" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "ami"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:inst:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "ami"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:inst:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "ami"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:inst:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "ami"], (function [x;y;z;_;_] -> compose_lemma3 x y z "s" "subst:pl:inst:n:pt" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "e"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "depr:pl:nom:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "e"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "depr:pl:voc:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "e"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:acc:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "e"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:acc:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "e"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:nom:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "e"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:nom:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "e"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:voc:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "e"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:voc:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "ego"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:acc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "ego"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:gen:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "em"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:inst:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "em"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:inst:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "em"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:inst:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "emu"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:dat:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "go"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:acc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "go"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:gen:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "i"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:gen:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "i"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:gen:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "m"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:inst:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "m"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:loc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "mu"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:dat:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "om"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:dat:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "om"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:dat:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "om"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:dat:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "om"], (function [x;y;z;_;_] -> compose_lemma3 x y z "s" "subst:pl:dat:n:pt" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "owi"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:dat:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "owi"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:dat:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "owi"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:dat:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "owie"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:nom:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "owie"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:voc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "u"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:gen:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "u"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:loc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "u"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:loc:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "u"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:loc:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "u"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:voc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "u"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:voc:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "u"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:sg:voc:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "y"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "depr:pl:nom:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "y"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "depr:pl:voc:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "y"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:acc:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "y"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:acc:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "y"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:gen:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "y"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:nom:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "y"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:nom:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "y"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:voc:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "y"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:voc:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "ów"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:acc:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "ów"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:gen:m1" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "ów"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:gen:m2" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "ów"], (function [x;y;z;_;_] -> compose_lemma3 x y z "" "subst:pl:gen:m3" | _ -> failwith "acronym_patterns");
  [L; I "-"; L; I "’"; T "ów"], (function [x;y;z;_;_] -> compose_lemma3 x y z "s" "subst:pl:gen:n:pt" | _ -> failwith "acronym_patterns");*)
  ]

let name_patterns = [
(*  [T "O"; I "’"; L], (function [x;y;z] -> compose_lemma3 x y z "" "subst:_:_:_" | _ -> failwith "name_patterns");
  [T "d"; I "’"; L], (function [x;y;z] -> compose_lemma3 x y z "" "subst:_:_:_" | _ -> failwith "name_patterns");
  [T "l"; I "’"; L], (function [x;y;z] -> compose_lemma3 x y z "" "subst:_:_:_" | _ -> failwith "name_patterns");
  [L; I "’"; T "s"], (function [x;y;z] -> compose_lemma3 x y z "" "subst:_:_:_" | _ -> failwith "name_patterns");
  [L; I "’"; T "sa"], (function [x;_;_] -> compose_lemma x "’s" "subst:sg:gen.acc:_" | _ -> failwith "name_patterns");*)
  ]

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

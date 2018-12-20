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

open Xstd
open Printf
open TokenizerTypes

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
  String.concat "" (Xlist.map l (fun t -> Tokens.get_orth t.token))

let concat_intnum = function
    [{token=Ideogram(v4,_)};_;{token=Ideogram(v3,_)};_;{token=Ideogram(v2,_)};_;{token=Ideogram(v1,_)}] -> v4^v3^v2^v1
  | [{token=Ideogram(v3,_)};_;{token=Ideogram(v2,_)};_;{token=Ideogram(v1,_)}] -> v3^v2^v1
  | [{token=Ideogram(v2,_)};_;{token=Ideogram(v1,_)}] -> v2^v1
  | [{token=Ideogram(v1,_)}] -> v1
  | _ -> failwith "concat_intnum"

let dig_value t =
  match t.token with
    Ideogram(v,_) -> v
  | _ -> failwith "dig_value"
  
let digit_patterns1 = [ (* FIXME: problem z nadmiarowymi interpretacjami - trzeba uwzględnić w preprocesingu brak spacji - albo w dezambiguacji *)
  [I "dig"; Sym "."; I "dig"; Sym "."; I "dig"; Sym "."; I "dig"; Sym "."; I "dig"], (fun tokens -> Ideogram(concat_orths tokens,"obj-id"),[]);
  [I "dig"; Sym "."; I "dig"; Sym "."; I "dig"; Sym "."; I "dig"], (fun tokens -> Ideogram(concat_orths tokens,"obj-id"),[]);
  [I "dig"; Sym "."; I "dig"; Sym "."; I "dig"], (fun tokens -> Ideogram(concat_orths tokens,"obj-id"),[]);
  [I "dig"; Sym "."; I "dig"], (fun tokens -> Ideogram(concat_orths tokens,"obj-id"),[]);
  [I "pref3dig"; Sym "."; I "3dig"; Sym "."; I "3dig"; Sym "."; I "3dig"], (fun tokens -> Ideogram(concat_intnum tokens,"intnum"),[]);
  [I "pref3dig"; Sym "."; I "3dig"; Sym "."; I "3dig"], (fun tokens -> Ideogram(concat_intnum tokens,"intnum"),[]);
  [I "pref3dig"; Sym "."; I "3dig"], (fun tokens -> Ideogram(concat_intnum tokens,"intnum"),[]);
  [I "pref3dig"; Sym " "; I "3dig"; Sym " "; I "3dig"; Sym " "; I "3dig"], (fun tokens -> Ideogram(concat_intnum tokens,"intnum"),[]);
  [I "pref3dig"; Sym " "; I "3dig"; Sym " "; I "3dig"], (fun tokens -> Ideogram(concat_intnum tokens,"intnum"),[]);
  [I "pref3dig"; Sym " "; I "3dig"], (fun tokens -> Ideogram(concat_intnum tokens,"intnum"),[]);
  (* [I "intnum"; Sym "."], (function [token;_] -> Ideogram(concat_intnum [token],"ordnum") | _ -> failwith "digit_patterns1"); *) (* to zagłusza inne wzorce *)
  [I "day"; Sym "."; I "month"; Sym "."; I "year"], (function ([day;_;month;_;year] as tokens) -> Ideogram(concat_orths tokens,"date"),[day;month;year] | _ -> failwith "digit_patterns2");
  [I "day"; Sym "."; I "roman-month"; Sym "."; I "year"], (function ([day;_;month;_;year] as tokens) -> Ideogram(concat_orths tokens,"date"),[day;month;year] | _ -> failwith "digit_patterns3");
  [I "day"; Sym " "; I "roman-month"; Sym " "; I "year"], (function ([day;_;month;_;year] as tokens) -> Ideogram(concat_orths tokens,"date"),[day;month;year] | _ -> failwith "digit_patterns3");
  [I "day"; Sym "."; I "month"; Sym "."; I "2dig"], (function ([day;_;month;_;year] as tokens) -> Ideogram(concat_orths tokens,"date"),[day;month;year] | _ -> failwith "digit_patterns2");
  [I "day"; Sym "."; I "roman-month"; Sym "."; I "2dig"], (function ([day;_;month;_;year] as tokens) -> Ideogram(concat_orths tokens,"date"),[day;month;year] | _ -> failwith "digit_patterns3");
  (* [I "day"; Sym "."; I "month"; Sym "."], (function [day;_;month;_] as tokens) -> Ideogram(concat_orths tokens,"day-month"),[day;month] | _ -> failwith "digit_patterns4"); *) (* to zagłusza inne wzorce *)
  [I "day"; Sym "."; I "month"], (function ([day;_;month] as tokens) -> Ideogram(concat_orths tokens,"day-month"),[day;month] | _ -> failwith "digit_patterns4");
  [I "hour"; Sym "."; I "minute"], (function ([hour;_;minute] as tokens) -> Ideogram(concat_orths tokens,"hour-minute"),[hour;minute] | _ -> failwith "digit_patterns5");
  [I "hour"; N ":"; I "minute"], (function ([hour;_;minute] as tokens) -> Ideogram(concat_orths tokens,"hour-minute"),[hour;minute] | _ -> failwith "digit_patterns6");
  [I "intnum"; N ":"; I "intnum"], (function ([x;_;y] as tokens) -> Ideogram(concat_orths tokens,"match-result"),[x;y] | _ -> failwith "digit_patterns7");
  [I "3dig"; N "-"; I "3dig"; N "-"; I "3dig"], (fun tokens -> Ideogram(concat_orths tokens,"phone-number"),[]);
  [I "3dig"; Sym " "; I "3dig"; Sym " "; I "3dig"], (fun tokens -> Ideogram(concat_orths tokens,"phone-number"),[]);
  [I "3dig"; N "-"; I "2dig"; N "-"; I "2dig"], (fun tokens -> Ideogram(concat_orths tokens,"phone-number"),[]);
  [I "3dig"; Sym " "; I "2dig"; Sym " "; I "2dig"], (fun tokens -> Ideogram(concat_orths tokens,"phone-number"),[]);
  [I "2dig"; N "-"; I "2dig"; N "-"; I "2dig"], (fun tokens -> Ideogram(concat_orths tokens,"phone-number"),[]);
  [I "2dig"; Sym " "; I "2dig"; Sym " "; I "2dig"], (fun tokens -> Ideogram(concat_orths tokens,"phone-number"),[]);
  [I "3dig"; Sym " "; I "3dig"], (fun tokens -> Ideogram(concat_orths tokens,"phone-number"),[]);
  [O "0"; N "-"; I "2dig"; O " "; I "3dig"; N "-"; I "2dig"; N "-"; I "2dig"], (fun tokens -> Ideogram(concat_orths tokens,"phone-number"),[]);
  [O "0"; N "-"; I "2dig"; O " "; I "2dig"; N "-"; I "2dig"; N "-"; I "2dig"], (fun tokens -> Ideogram(concat_orths tokens,"phone-number"),[]);
  [N "("; O "0"; N "-"; I "2dig"; N ")"; O " "; I "3dig"; N "-"; I "2dig"; N "-"; I "2dig"], (fun tokens -> Ideogram(concat_orths tokens,"phone-number"),[]);
  [N "("; O "0"; N "-"; I "2dig"; N ")"; O " "; I "2dig"; N "-"; I "2dig"; N "-"; I "2dig"], (fun tokens -> Ideogram(concat_orths tokens,"phone-number"),[]);
  [N "("; I "3dig"; N ")"; O " "; I "3dig"; N "-"; I "2dig"; N "-"; I "2dig"], (fun tokens -> Ideogram(concat_orths tokens,"phone-number"),[]);
  [N "("; I "3dig"; N ")"; O " "; I "2dig"; N "-"; I "2dig"; N "-"; I "2dig"], (fun tokens -> Ideogram(concat_orths tokens,"phone-number"),[]);
  [N "("; I "3dig"; N ")"; O " "; I "3dig"; Sym " "; I "2dig"; Sym " "; I "2dig"], (fun tokens -> Ideogram(concat_orths tokens,"phone-number"),[]);
  [N "("; I "3dig"; N ")"; O " "; I "2dig"; Sym " "; I "2dig"; Sym " "; I "2dig"], (fun tokens -> Ideogram(concat_orths tokens,"phone-number"),[]);
  [O "0"; N "-"; I "2dig"; N "-"; I "2dig"; N "-"; I "2dig"], (fun tokens -> Ideogram(concat_orths tokens,"phone-number"),[]);
  [O "0"; N "-"; I "3dig"; N "-"; I "2dig"; N "-"; I "3dig"], (fun tokens -> Ideogram(concat_orths tokens,"phone-number"),[]);
  [I "3dig"; Sym " "; I "3dig"; Sym " "; I "2dig"; Sym " "; I "2dig"], (fun tokens -> Ideogram(concat_orths tokens,"phone-number"),[]);
  [I "3dig"; Sym " "; I "3dig"; Sym " "; I "4dig"], (fun tokens -> Ideogram(concat_orths tokens,"phone-number"),[]);
(* [D "year"; SL], (fun tokens -> Lemma(*Proper*)(concat_orths tokens,"building-number",[[]],["building-number"])); (* year - bo jest to dodatnia liczba całkowita *)
  [D "year"; Sym " "; SL2], (fun tokens -> Lemma(*Proper*)(concat_orths tokens,"building-number",[[]],["building-number"])); (* year - bo jest to dodatnia liczba całkowita *)
  [D "year"; SL; N "/"; D "year"], (fun tokens -> Lemma(*Proper*)(concat_orths tokens,"building-number",[[]],["building-number"])); (* year - bo jest to dodatnia liczba całkowita *)
  [D "year"; N "/"; D "year"], (fun tokens -> Lemma(*Proper*)(concat_orths tokens,"building-number",[[]],["building-number"])); (* year - bo jest to dodatnia liczba całkowita *)
  [D "year"; N "/"; D "year"; SL], (fun tokens -> Lemma(*Proper*)(concat_orths tokens,"building-number",[[]],["building-number"])); (* year - bo jest to dodatnia liczba całkowita *)
  [D "year"; SL; N "/"; D "year"; SL], (fun tokens -> Lemma(*Proper*)(concat_orths tokens,"building-number",[[]],["building-number"])); (* year - bo jest to dodatnia liczba całkowita *)
  [D "year"; SL; N "/"; D "year"; N "/"; D "year"], (fun tokens -> Lemma(*Proper*)(concat_orths tokens,"building-number",[[]],["building-number"])); (* year - bo jest to dodatnia liczba całkowita *)
  [D "year"; N "/"; D "year"; N "/"; D "year"], (fun tokens -> Lemma(*Proper*)(concat_orths tokens,"building-number",[[]],["building-number"])); (* year - bo jest to dodatnia liczba całkowita *)
  [D "year"; N "/"; D "year"; SL; N "/"; D "year"], (fun tokens -> Lemma(*Proper*)(concat_orths tokens,"building-number",[[]],["building-number"])); (* year - bo jest to dodatnia liczba całkowita *)
  [D "year"; SL; N "/"; D "year"; SL; N "/"; D "year"], (fun tokens -> Lemma(*Proper*)(concat_orths tokens,"building-number",[[]],["building-number"])); (* year - bo jest to dodatnia liczba całkowita *)*)
  (* [SL; N ")"], (fun tokens -> Ideogram(concat_orths tokens,"list-item")); *)
  [I "intnum"; Sym "."; I "dig"], (function [x;_;y] -> Ideogram(dig_value x ^ "," ^ dig_value y,"realnum"),[] | _ -> failwith "digit_patterns8");
  ] (* bez 1 i *2 *3 *4 mamy rec *) (* w morfeuszu zawsze num:pl?*)

let digit_patterns2 = [
  [I "intnum"; N ","; I "dig"], (function [x;_;y] -> Ideogram(dig_value x ^ "," ^ dig_value y,"realnum"),[] | _ -> failwith "digit_patterns8");
(*  [N "-"; D "intnum"; N ","; D "dig"], (function [_;x;_;y] ->  Ideogram("-" ^ dig_value x ^ "," ^ dig_value y,"realnum") | _ -> failwith "digit_patterns9");
  [N "-"; D "intnum"], (function [_;x] ->  Ideogram("-" ^ dig_value x,"realnum") | _ -> failwith "digit_patterns10");*)
  [N "’"; I "2dig"], (function [_;x] -> Ideogram("’" ^ dig_value x,"year"),[] | _ -> failwith "digit_patterns12");
(*   [D "intnum"], "realnum"; *)
  ]

let compose_latek_lemma t interp =
  Tokens.make_lemma (Tokens.get_orth t.token ^ "-latek", interp)

let compose_latka_lemma t interp =
  Tokens.make_lemma (Tokens.get_orth t.token ^ "-latka", interp)

let compose_tka_lemma t interp =
  Tokens.make_lemma (Tokens.get_orth t.token ^ "-tka", interp)

let compose_latek_int_lemma t t2 interp =
  Tokens.make_lemma (Tokens.get_orth t.token ^ "-" ^ Tokens.get_orth t2.token ^ "-latek", interp)

let compose_latka_int_lemma t t2 interp =
  Tokens.make_lemma (Tokens.get_orth t.token ^ "-" ^ Tokens.get_orth t2.token ^ "-latka", interp)

let compose_lecie_lemma t interp =
  Tokens.make_lemma (Tokens.get_orth t.token ^ "-lecie", interp)

let compose_ordnum_lemma t interp =
  Tokens.make_lemma (Tokens.get_orth t.token ^ ".", interp)

let digit_patterns3 = [
  [N "-"; I "intnum"], (function [_;x] ->  Ideogram("-" ^ dig_value x,"intnum"),[] | _ -> failwith "digit_patterns10");
  [N "-"; I "realnum"], (function [_;x] ->  Ideogram("-" ^ dig_value x,"realnum"),[] | _ -> failwith "digit_patterns10");
  [I "2dig"; N "-"; I "3dig"], (fun tokens -> Ideogram(concat_orths tokens,"postal-code"),[]);
  [I "3dig"; N "-"; I "3dig"], (fun tokens -> Ideogram(concat_orths tokens,"phone-number"),[]);
  [I "intnum"; N "-"; I "intnum"], (function ([x;_;y] as tokens) -> Ideogram(concat_orths tokens,"intnum-interval"),[x;y] | _ -> failwith "digit_patterns11");
  [I "realnum"; N "-"; I "realnum"], (function ([x;_;y] as tokens) -> Ideogram(concat_orths tokens,"realnum-interval"),[x;y] | _ -> failwith "digit_patterns12"); (* FIXME: konflikt z liczbami ujemnymi *)
  [I "intnum"; N "-"; I "realnum"], (function ([x;_;y] as tokens) -> Ideogram(concat_orths tokens,"realnum-interval"),[x;y] | _ -> failwith "digit_patterns12"); (* FIXME: konflikt z liczbami ujemnymi *)
  [I "realnum"; N "-"; I "intnum"], (function ([x;_;y] as tokens) -> Ideogram(concat_orths tokens,"realnum-interval"),[x;y] | _ -> failwith "digit_patterns12"); (* FIXME: konflikt z liczbami ujemnymi *)
  [I "date"; N "-"; I "date"], (function ([x;_;y] as tokens) -> Ideogram(concat_orths tokens,"date-interval"),[x;y] | _ -> failwith "digit_patterns13");
  [I "day-month"; N "-"; I "day-month"], (function ([x;_;y] as tokens) -> Ideogram(concat_orths tokens,"day-month-interval"),[x;y] | _ -> failwith "digit_patterns14");
  [I "day"; N "-"; I "day"], (function ([x;_;y] as tokens) -> Ideogram(concat_orths tokens,"day-interval"),[x;y] | _ -> failwith "digit_patterns15");
  [I "month"; N "-"; I "month"], (function ([x;_;y] as tokens) -> Ideogram(concat_orths tokens,"month-interval"),[x;y] | _ -> failwith "digit_patterns16");
  [I "roman-month"; N "-"; I "roman-month"], (function ([x;_;y] as tokens) -> Ideogram(concat_orths tokens,"month-interval"),[x;y] | _ -> failwith "digit_patterns17");
  [I "year"; N "-"; I "year"], (function ([x;_;y] as tokens) -> Ideogram(concat_orths tokens,"year-interval"),[x;y] | _ -> failwith "digit_patterns16");
  [I "year"; N "-"; I "2dig"], (function ([x;_;y] as tokens) -> Ideogram(concat_orths tokens,"year-interval"),[x;y] | _ -> failwith "digit_patterns16");
  [I "hour-minute"; N "-"; I "hour-minute"], (function ([x;_;y] as tokens) -> Ideogram(concat_orths tokens,"hour-minute-interval"),[x;y] | _ -> failwith "digit_patterns18");
  [I "hour"; N "-"; I "hour"], (function ([x;_;y] as tokens) -> Ideogram(concat_orths tokens,"hour-interval"),[x;y] | _ -> failwith "digit_patterns19");
  [I "minute"; N "-"; I "minute"], (function ([x;_;y] as tokens) -> Ideogram(concat_orths tokens,"minute-interval"),[x;y] | _ -> failwith "digit_patterns20");
  [I "roman"; N "-"; I "roman"], (function ([x;_;y] as tokens) -> Ideogram(concat_orths tokens,"roman-interval"),[x;y] | _ -> failwith "digit_patterns21");
  [I "intnum"; Sym " "; N "-"; Sym " "; I "intnum"], (function ([x;_;_;_;y] as tokens) -> Ideogram(concat_orths tokens,"intnum-interval"),[x;y] | _ -> failwith "digit_patterns11");
  [I "realnum"; Sym " "; N "-"; Sym " "; I "realnum"], (function ([x;_;_;_;y] as tokens) -> Ideogram(concat_orths tokens,"realnum-interval"),[x;y] | _ -> failwith "digit_patterns12"); (* FIXME: konflikt z liczbami ujemnymi *)
  [I "intnum"; Sym " "; N "-"; Sym " "; I "realnum"], (function ([x;_;_;_;y] as tokens) -> Ideogram(concat_orths tokens,"realnum-interval"),[x;y] | _ -> failwith "digit_patterns12"); (* FIXME: konflikt z liczbami ujemnymi *)
  [I "realnum"; Sym " "; N "-"; Sym " "; I "intnum"], (function ([x;_;_;_;y] as tokens) -> Ideogram(concat_orths tokens,"realnum-interval"),[x;y] | _ -> failwith "digit_patterns12"); (* FIXME: konflikt z liczbami ujemnymi *)
  [I "date"; Sym " "; N "-"; Sym " "; I "date"], (function ([x;_;_;_;y] as tokens) -> Ideogram(concat_orths tokens,"date-interval"),[x;y] | _ -> failwith "digit_patterns13");
  [I "day-month"; Sym " "; N "-"; Sym " "; I "day-month"], (function ([x;_;_;_;y] as tokens) -> Ideogram(concat_orths tokens,"day-month-interval"),[x;y] | _ -> failwith "digit_patterns14");
  [I "day"; Sym " "; N "-"; Sym " "; I "day"], (function ([x;_;_;_;y] as tokens) -> Ideogram(concat_orths tokens,"day-interval"),[x;y] | _ -> failwith "digit_patterns15");
  [I "month"; Sym " "; N "-"; Sym " "; I "month"], (function ([x;_;_;_;y] as tokens) -> Ideogram(concat_orths tokens,"month-interval"),[x;y] | _ -> failwith "digit_patterns16");
  [I "roman-month"; Sym " "; N "-"; Sym " "; I "roman-month"], (function ([x;_;_;_;y] as tokens) -> Ideogram(concat_orths tokens,"month-interval"),[x;y] | _ -> failwith "digit_patterns17");
  [I "year"; Sym " "; N "-"; Sym " "; I "year"], (function ([x;_;_;_;y] as tokens) -> Ideogram(concat_orths tokens,"year-interval"),[x;y] | _ -> failwith "digit_patterns16");
  [I "year"; Sym " "; N "-"; Sym " "; I "2dig"], (function ([x;_;_;_;y] as tokens) -> Ideogram(concat_orths tokens,"year-interval"),[x;y] | _ -> failwith "digit_patterns16");
  [I "hour-minute"; Sym " "; N "-"; Sym " "; I "hour-minute"], (function ([x;_;_;_;y] as tokens) -> Ideogram(concat_orths tokens,"hour-minute-interval"),[x;y] | _ -> failwith "digit_patterns18");
  [I "hour"; Sym " "; N "-"; Sym " "; I "hour"], (function ([x;_;_;_;y] as tokens) -> Ideogram(concat_orths tokens,"hour-interval"),[x;y] | _ -> failwith "digit_patterns19");
  [I "minute"; Sym " "; N "-"; Sym " "; I "minute"], (function ([x;_;_;_;y] as tokens) -> Ideogram(concat_orths tokens,"minute-interval"),[x;y] | _ -> failwith "digit_patterns20");
  [I "roman"; Sym " "; N "-"; Sym " "; I "roman"], (function ([x;_;_;_;y] as tokens) -> Ideogram(concat_orths tokens,"roman-interval"),[x;y] | _ -> failwith "digit_patterns21");
(*  [D "intnum"; I "-"; T "latek"], (function [x;_;_] -> compose_latek_lemma x "subst:sg:nom:m1" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "latka"], (function [x;_;_] -> compose_latek_lemma x "subst:sg:gen.acc:m1" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "latkowi"], (function [x;_;_] -> compose_latek_lemma x "subst:sg:dat:m1" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "latkiem"], (function [x;_;_] -> compose_latek_lemma x "subst:sg:inst:m1" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "latku"], (function [x;_;_] -> compose_latek_lemma x "subst:sg:loc.voc:m1" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "latkowie"], (function [x;_;_] -> compose_latek_lemma x "subst:pl:nom.voc:m1" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "latków"], (function [x;_;_] -> compose_latek_lemma x "subst:pl:gen.acc:m1" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "latkom"], (function [x;_;_] -> compose_latek_lemma x "subst:pl:dat:m1.f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "latkami"], (function [x;_;_] -> compose_latek_lemma x "subst:pl:inst:m1.f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "latkach"], (function [x;_;_] -> compose_latek_lemma x "subst:pl:loc:m1.f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "latka"], (function [x;_;_] -> compose_latka_lemma x "subst:sg:nom:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "latki"], (function [x;_;_] -> compose_latka_lemma x "subst:sg:gen:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "latce"], (function [x;_;_] -> compose_latka_lemma x "subst:sg:dat.loc:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "latkę"], (function [x;_;_] -> compose_latka_lemma x "subst:sg:acc:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "latką"], (function [x;_;_] -> compose_latka_lemma x "subst:sg:inst:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "latko"], (function [x;_;_] -> compose_latka_lemma x "subst:sg:voc:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "latki"], (function [x;_;_] -> compose_latka_lemma x "subst:pl:nom.acc.voc:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "latek"], (function [x;_;_] -> compose_latka_lemma x "subst:pl:gen:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; D "intnum"; I "-"; T "latek"], (function [x;_;y;_;_] -> compose_latek_int_lemma x y "subst:sg:nom:m1" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; D "intnum"; I "-"; T "latka"], (function [x;_;y;_;_] -> compose_latek_int_lemma x y "subst:sg:gen.acc:m1" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; D "intnum"; I "-"; T "latkowi"], (function [x;_;y;_;_] -> compose_latek_int_lemma x y "subst:sg:dat:m1" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; D "intnum"; I "-"; T "latkiem"], (function [x;_;y;_;_] -> compose_latek_int_lemma x y "subst:sg:inst:m1" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; D "intnum"; I "-"; T "latku"], (function [x;_;y;_;_] -> compose_latek_int_lemma x y "subst:sg:loc.voc:m1" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; D "intnum"; I "-"; T "latkowie"], (function [x;_;y;_;_] -> compose_latek_int_lemma x y "subst:pl:nom.voc:m1" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; D "intnum"; I "-"; T "latków"], (function [x;_;y;_;_] -> compose_latek_int_lemma x y "subst:pl:gen.acc:m1" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; D "intnum"; I "-"; T "latkom"], (function [x;_;y;_;_] -> compose_latek_int_lemma x y "subst:pl:dat:m1.f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; D "intnum"; I "-"; T "latkami"], (function [x;_;y;_;_] -> compose_latek_int_lemma x y "subst:pl:inst:m1.f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; D "intnum"; I "-"; T "latkach"], (function [x;_;y;_;_] -> compose_latek_int_lemma x y "subst:pl:loc:m1.f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; D "intnum"; I "-"; T "latka"], (function [x;_;y;_;_] -> compose_latka_int_lemma x y "subst:sg:nom:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; D "intnum"; I "-"; T "latki"], (function [x;_;y;_;_] -> compose_latka_int_lemma x y "subst:sg:gen:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; D "intnum"; I "-"; T "latce"], (function [x;_;y;_;_] -> compose_latka_int_lemma x y "subst:sg:dat.loc:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; D "intnum"; I "-"; T "latkę"], (function [x;_;y;_;_] -> compose_latka_int_lemma x y "subst:sg:acc:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; D "intnum"; I "-"; T "latką"], (function [x;_;y;_;_] -> compose_latka_int_lemma x y "subst:sg:inst:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; D "intnum"; I "-"; T "latko"], (function [x;_;y;_;_] -> compose_latka_int_lemma x y "subst:sg:voc:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; D "intnum"; I "-"; T "latki"], (function [x;_;y;_;_] -> compose_latka_int_lemma x y "subst:pl:nom.acc.voc:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; D "intnum"; I "-"; T "latek"], (function [x;_;y;_;_] -> compose_latka_int_lemma x y "subst:pl:gen:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "lecie"], (function [x;_;_] -> compose_lecie_lemma x "subst:sg:nom.acc.voc:m1" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "lecia"], (function [x;_;_] -> compose_lecie_lemma x "subst:sg:gen:m1" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "leciu"], (function [x;_;_] -> compose_lecie_lemma x "subst:sg:dat.loc:m1" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "leciem"], (function [x;_;_] -> compose_lecie_lemma x "subst:sg:inst:m1" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "tka"], (function [x;_;_] -> compose_tka_lemma x "subst:sg:nom:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "tki"], (function [x;_;_] -> compose_tka_lemma x "subst:sg:gen:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "tce"], (function [x;_;_] -> compose_tka_lemma x "subst:sg:dat.loc:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "tkę"], (function [x;_;_] -> compose_tka_lemma x "subst:sg.acc:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "tką"], (function [x;_;_] -> compose_tka_lemma x "subst:sg.inst:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "tko"], (function [x;_;_] -> compose_tka_lemma x "subst:sg:voc:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "tki"], (function [x;_;_] -> compose_tka_lemma x "subst:pl:nom.acc.voc:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "tek"], (function [x;_;_] -> compose_tka_lemma x "subst:pl:gen:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "tkom"], (function [x;_;_] -> compose_tka_lemma x "subst:pl:dat:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "tkami"], (function [x;_;_] -> compose_tka_lemma x "subst:pl:inst:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "tkach"], (function [x;_;_] -> compose_tka_lemma x "subst:pl:loc:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "szy"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:nom.voc:m1.m2.m3:pos|adj:sg:acc:m3:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "sze"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:nom.acc.voc:n:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "sza"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:nom.voc:f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "szego"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:gen:m1.m2.m3.n:pos|adj:sg:acc:m1.m2:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "szemu"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:dat:m1.m2.m3.n:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "szej"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:gen.dat.loc:f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "szym"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:inst.loc:m1.m2.m3.n:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "szą"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:acc.inst:f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "si"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:nom.voc:m1:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "sze"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:nom.acc.voc:m2.m3.n.f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "szych"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:gen.loc:_:pos|adj:pl:acc:m1:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "szym"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:dat:_:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "szymi"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:inst:_:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "szo"], (function [x;_;_] -> compose_ordnum_lemma x "adja" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "gi"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:nom.voc:m1.m2.m3:pos|adj:sg:acc:m3:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "gie"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:nom.acc.voc:n:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "ga"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:nom.voc:f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "giego"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:gen:m1.m2.m3.n:pos|adj:sg:acc:m1.m2:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "giemu"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:dat:m1.m2.m3.n:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "giej"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:gen.dat.loc:f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "gim"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:inst.loc:m1.m2.m3.n:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "gą"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:acc.inst:f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "dzy"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:nom.voc:m1:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "gie"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:nom.acc.voc:m2.m3.n.f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "gich"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:gen.loc:_:pos|adj:pl:acc:m1:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "gim"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:dat:_:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "gimi"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:inst:_:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "go"], (function [x;_;_] -> compose_ordnum_lemma x "adja" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "ci"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:nom.voc:m1.m2.m3:pos|adj:sg:acc:m3:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "cie"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:nom.acc.voc:n:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "cia"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:nom.voc:f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "ciego"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:gen:m1.m2.m3.n:pos|adj:sg:acc:m1.m2:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "ciemu"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:dat:m1.m2.m3.n:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "ciej"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:gen.dat.loc:f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "cim"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:inst.loc:m1.m2.m3.n:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "cią"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:acc.inst:f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "ci"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:nom.voc:m1:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "cie"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:nom.acc.voc:m2.m3.n.f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "cich"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:gen.loc:_:pos|adj:pl:acc:m1:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "cim"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:dat:_:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "cimi"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:inst:_:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "cio"], (function [x;_;_] -> compose_ordnum_lemma x "adja" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "ty"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:nom.voc:m1.m2.m3:pos|adj:sg:acc:m3:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "te"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:nom.acc.voc:n:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "ta"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:nom.voc:f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "tego"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:gen:m1.m2.m3.n:pos|adj:sg:acc:m1.m2:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "temu"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:dat:m1.m2.m3.n:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "tej"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:gen.dat.loc:f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "tym"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:inst.loc:m1.m2.m3.n:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "tą"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:acc.inst:f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "ci"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:nom.voc:m1:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "te"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:nom.acc.voc:m2.m3.n.f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "tych"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:gen.loc:_:pos|adj:pl:acc:m1:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "tym"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:dat:_:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "tymi"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:inst:_:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "to"], (function [x;_;_] -> compose_ordnum_lemma x "adja" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "sty"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:nom.voc:m1.m2.m3:pos|adj:sg:acc:m3:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "ste"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:nom.acc.voc:n:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "sta"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:nom.voc:f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "stego"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:gen:m1.m2.m3.n:pos|adj:sg:acc:m1.m2:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "stemu"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:dat:m1.m2.m3.n:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "stej"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:gen.dat.loc:f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "stym"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:inst.loc:m1.m2.m3.n:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "stą"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:acc.inst:f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "ści"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:nom.voc:m1:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "ste"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:nom.acc.voc:m2.m3.n.f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "stych"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:gen.loc:_:pos|adj:pl:acc:m1:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "stym"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:dat:_:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "stymi"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:inst:_:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "sto"], (function [x;_;_] -> compose_ordnum_lemma x "adja" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "my"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:nom.voc:m1.m2.m3:pos|adj:sg:acc:m3:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "me"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:nom.acc.voc:n:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "ma"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:nom.voc:f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "mego"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:gen:m1.m2.m3.n:pos|adj:sg:acc:m1.m2:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "memu"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:dat:m1.m2.m3.n:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "mej"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:gen.dat.loc:f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "mym"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:inst.loc:m1.m2.m3.n:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "mą"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:acc.inst:f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "mi"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:nom.voc:m1:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "me"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:nom.acc.voc:m2.m3.n.f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "mych"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:gen.loc:_:pos|adj:pl:acc:m1:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "mym"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:dat:_:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "mymi"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:inst:_:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "mo"], (function [x;_;_] -> compose_ordnum_lemma x "adja" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "y"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:nom.voc:m1.m2.m3:pos|adj:sg:acc:m3:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "i"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:nom.voc:m1.m2.m3:pos|adj:sg:acc:m3:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "e"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:nom.acc.voc:n:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "a"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:nom.voc:f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "ego"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:gen:m1.m2.m3.n:pos|adj:sg:acc:m1.m2:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "go"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:gen:m1.m2.m3.n:pos|adj:sg:acc:m1.m2:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "emu"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:dat:m1.m2.m3.n:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "mu"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:dat:m1.m2.m3.n:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "ej"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:gen.dat.loc:f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "ym"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:inst.loc:m1.m2.m3.n:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "im"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:inst.loc:m1.m2.m3.n:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "ą"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:acc.inst:f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "i"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:nom.voc:m1:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "y"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:nom.voc:m1:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "e"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:nom.acc.voc:m2.m3.n.f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "ych"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:gen.loc:_:pos|adj:pl:acc:m1:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "ich"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:gen.loc:_:pos|adj:pl:acc:m1:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "ym"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:dat:_:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "im"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:dat:_:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "ymi"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:inst:_:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "imi"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:inst:_:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "o"], (function [x;_;_] -> compose_ordnum_lemma x "adja" | _ -> failwith "digit_patterns22");*)
  ]

let rec make_tys n t = 
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
  | _ -> failwith "make_tys"

let digit_patterns4 = [
  [I "intnum"; N "-"; T "tys"; Sym "."], (function [x;_;_;_] -> make_tys 3 x | _ -> failwith "digit_patterns8");
  [I "intnum"; Sym " "; T "tys"; Sym "."], (function [x;_;_;_] -> make_tys 3 x | _ -> failwith "digit_patterns8");
  [I "intnum"; T "tys"; Sym "."], (function [x;_;_] -> make_tys 3 x | _ -> failwith "digit_patterns8");
  [I "realnum"; N "-"; T "tys"; Sym "."], (function [x;_;_;_] -> make_tys 3 x | _ -> failwith "digit_patterns8");
  [I "realnum"; Sym " "; T "tys"; Sym "."], (function [x;_;_;_] -> make_tys 3 x | _ -> failwith "digit_patterns8");
  [I "realnum"; T "tys"; Sym "."], (function [x;_;_] -> make_tys 3 x | _ -> failwith "digit_patterns8");
  [I "intnum-interval"; N "-"; T "tys"; Sym "."], (function [x;_;_;_] -> make_tys 3 x | _ -> failwith "digit_patterns8");
  [I "intnum-interval"; Sym " "; T "tys"; Sym "."], (function [x;_;_;_] -> make_tys 3 x | _ -> failwith "digit_patterns8");
  [I "intnum-interval"; T "tys"; Sym "."], (function [x;_;_] -> make_tys 3 x | _ -> failwith "digit_patterns8");
  [I "realnum-interval"; N "-"; T "tys"; Sym "."], (function [x;_;_;_] -> make_tys 3 x | _ -> failwith "digit_patterns8");
  [I "realnum-interval"; Sym " "; T "tys"; Sym "."], (function [x;_;_;_] -> make_tys 3 x | _ -> failwith "digit_patterns8");
  [I "realnum-interval"; T "tys"; Sym "."], (function [x;_;_] -> make_tys 3 x | _ -> failwith "digit_patterns8");
  [I "intnum"; N "-"; T "mln"; Sym "."], (function [x;_;_;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [I "intnum"; Sym " "; T "mln"; Sym "."], (function [x;_;_;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [I "intnum"; T "mln"; Sym "."], (function [x;_;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [I "realnum"; N "-"; T "mln"; Sym "."], (function [x;_;_;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [I "realnum"; Sym " "; T "mln"; Sym "."], (function [x;_;_;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [I "realnum"; T "mln"; Sym "."], (function [x;_;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [I "intnum-interval"; N "-"; T "mln"; Sym "."], (function [x;_;_;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [I "intnum-interval"; Sym " "; T "mln"; Sym "."], (function [x;_;_;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [I "intnum-interval"; T "mln"; Sym "."], (function [x;_;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [I "realnum-interval"; N "-"; T "mln"; Sym "."], (function [x;_;_;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [I "realnum-interval"; Sym " "; T "mln"; Sym "."], (function [x;_;_;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [I "realnum-interval"; T "mln"; Sym "."], (function [x;_;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [I "intnum"; N "-"; T "mld"; Sym "."], (function [x;_;_;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");
  [I "intnum"; Sym " "; T "mld"; Sym "."], (function [x;_;_;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");
  [I "intnum"; T "mld"; Sym "."], (function [x;_;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");
  [I "realnum"; N "-"; T "mld"; Sym "."], (function [x;_;_;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");
  [I "realnum"; Sym " "; T "mld"; Sym "."], (function [x;_;_;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");
  [I "realnum"; T "mld"; Sym "."], (function [x;_;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");
  [I "intnum-interval"; N "-"; T "mld"; Sym "."], (function [x;_;_;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");
  [I "intnum-interval"; Sym " "; T "mld"; Sym "."], (function [x;_;_;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");
  [I "intnum-interval"; T "mld"; Sym "."], (function [x;_;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");
  [I "realnum-interval"; N "-"; T "mld"; Sym "."], (function [x;_;_;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");
  [I "realnum-interval"; Sym " "; T "mld"; Sym "."], (function [x;_;_;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");
  [I "realnum-interval"; T "mld"; Sym "."], (function [x;_;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");
  [I "intnum"; N "-"; T "tys"], (function [x;_;_] -> make_tys 3 x | _ -> failwith "digit_patterns8");
  [I "intnum"; Sym " "; T "tys"], (function [x;_;_] -> make_tys 3 x | _ -> failwith "digit_patterns8");
  [I "intnum"; T "tys"], (function [x;_] -> make_tys 3 x | _ -> failwith "digit_patterns8");
  [I "realnum"; N "-"; T "tys"], (function [x;_;_] -> make_tys 3 x | _ -> failwith "digit_patterns8");
  [I "realnum"; Sym " "; T "tys"], (function [x;_;_] -> make_tys 3 x | _ -> failwith "digit_patterns8");
  [I "realnum"; T "tys"], (function [x;_] -> make_tys 3 x | _ -> failwith "digit_patterns8");
  [I "intnum-interval"; N "-"; T "tys"], (function [x;_;_] -> make_tys 3 x | _ -> failwith "digit_patterns8");
  [I "intnum-interval"; Sym " "; T "tys"], (function [x;_;_] -> make_tys 3 x | _ -> failwith "digit_patterns8");
  [I "intnum-interval"; T "tys"], (function [x;_] -> make_tys 3 x | _ -> failwith "digit_patterns8");
  [I "realnum-interval"; N "-"; T "tys"], (function [x;_;_] -> make_tys 3 x | _ -> failwith "digit_patterns8");
  [I "realnum-interval"; Sym " "; T "tys"], (function [x;_;_] -> make_tys 3 x | _ -> failwith "digit_patterns8");
  [I "realnum-interval"; T "tys"], (function [x;_] -> make_tys 3 x | _ -> failwith "digit_patterns8");
  [I "intnum"; N "-"; T "mln"], (function [x;_;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [I "intnum"; Sym " "; T "mln"], (function [x;_;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [I "intnum"; T "mln"], (function [x;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [I "realnum"; N "-"; T "mln"], (function [x;_;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [I "realnum"; Sym " "; T "mln"], (function [x;_;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [I "realnum"; T "mln"], (function [x;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [I "intnum-interval"; N "-"; T "mln"], (function [x;_;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [I "intnum-interval"; Sym " "; T "mln"], (function [x;_;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [I "intnum-interval"; T "mln"], (function [x;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [I "realnum-interval"; N "-"; T "mln"], (function [x;_;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [I "realnum-interval"; Sym " "; T "mln"], (function [x;_;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [I "realnum-interval"; T "mln"], (function [x;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [I "intnum"; N "-"; T "mld"], (function [x;_;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");
  [I "intnum"; Sym " "; T "mld"], (function [x;_;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");
  [I "intnum"; T "mld"], (function [x;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");
  [I "realnum"; N "-"; T "mld"], (function [x;_;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");
  [I "realnum"; Sym " "; T "mld"], (function [x;_;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");
  [I "realnum"; T "mld"], (function [x;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");
  [I "intnum-interval"; N "-"; T "mld"], (function [x;_;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");
  [I "intnum-interval"; Sym " "; T "mld"], (function [x;_;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");
  [I "intnum-interval"; T "mld"], (function [x;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");
  [I "realnum-interval"; N "-"; T "mld"], (function [x;_;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");
  [I "realnum-interval"; Sym " "; T "mld"], (function [x;_;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");
  [I "realnum-interval"; T "mld"], (function [x;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");
(*  [D "intnum"; Sym " "; T "miliony"; Sym "."], (function [x;_;_;_] -> make_tys 6 x | _ -> failwith "digit_patterns8"); FIXME: trzeba uwzględnić przypadek w lemacie
  [D "intnum"; Sym " "; T "milionów"; Sym "."], (function [x;_;_;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [D "intnum"; Sym " "; T "milionach"; Sym "."], (function [x;_;_;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [D "intnum"; Sym " "; T "miliardów"; Sym "."], (function [x;_;_;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");*)
  ]

(*let url_patterns1 = [
  [L; S "."; O "pl"], (function l -> Dig(concat_orths2 l,"url"));
  [L; D "dig"; S "."; O "pl"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; O "pl"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; D "dig"; S "."; O "pl"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; L; S "."; O "pl"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; L; S "."; L; S "."; O "pl"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; L; S "."; L; S "."; L; S "."; O "pl"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; I "-"; L; S "."; O "pl"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; O "uk"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; O "uk"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; L; S "."; O "uk"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; L; S "."; L; S "."; O "uk"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; L; S "."; L; S "."; L; S "."; O "uk"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; I "-"; L; S "."; O "uk"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; O "cz"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; O "cz"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; L; S "."; O "cz"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; L; S "."; L; S "."; O "cz"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; L; S "."; L; S "."; L; S "."; O "cz"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; I "-"; L; S "."; O "cz"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; O "eu"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; O "eu"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; L; S "."; O "eu"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; L; S "."; L; S "."; O "eu"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; L; S "."; L; S "."; L; S "."; O "eu"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; I "-"; L; S "."; O "eu"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; O "org"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; O "org"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; L; S "."; O "org"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; L; S "."; L; S "."; O "org"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; L; S "."; L; S "."; L; S "."; O "org"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; I "-"; L; S "."; O "org"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; O "com"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; O "com"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; L; S "."; O "com"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; L; S "."; L; S "."; O "com"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; L; S "."; L; S "."; L; S "."; O "com"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; I "-"; L; S "."; O "com"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; O "net"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; O "net"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; L; S "."; O "net"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; L; S "."; L; S "."; O "net"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; L; S "."; L; S "."; L; S "."; O "net"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; I "-"; L; S "."; O "net"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; O "gov"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; O "gov"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; L; S "."; O "gov"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; L; S "."; L; S "."; O "gov"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; L; S "."; L; S "."; L; S "."; O "gov"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; I "-"; L; S "."; O "gov"], (function l -> Dig(concat_orths2 l,"url"));
  ]

let url_patterns2 = [
  [L; S "@"; D "url"], (function l -> Dig(concat_orths2 l,"email"));
  [L; S "."; L; S "@"; D "url"], (function l -> Dig(concat_orths2 l,"email"));
  [L; S "_"; L; S "@"; D "url"], (function l -> Dig(concat_orths2 l,"email"));
  [L; S "."; L; S "."; L; S "@"; D "url"], (function l -> Dig(concat_orths2 l,"email"));
  [L; S "."; D "dig"; S "."; L; S "@"; D "url"], (function l -> Dig(concat_orths2 l,"email"));
  [L; D "intnum"; S "@"; D "url"], (function l -> Dig(concat_orths2 l,"email"));
  [L; S "."; L; D "dig"; S "@"; D "url"], (function l -> Dig(concat_orths2 l,"email"));
  [L; D "dig"; S "@"; D "url"], (function l -> Dig(concat_orths2 l,"email"));
  [O "http"; I ":"; I "/"; I "/"; D "url"], (function l -> Dig(concat_orths2 l,"url"));
  [O "https"; I ":"; I "/"; I "/"; D "url"], (function l -> Dig(concat_orths2 l,"url"));
  ]

let url_patterns3 = [
  [D "url"; I "/"], (function l -> Dig(concat_orths2 l,"url"));
  [D "url"; I "/"; L], (function l -> Dig(concat_orths2 l,"url"));
  [D "url"; I "/"; L; S "."; L], (function l -> Dig(concat_orths2 l,"url"));
]*)

let html_patterns = [
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
    attrs=Tokens.merge_attrs l}] @ matching.suffix)

let execute_abr_command matching =
  let l = List.rev matching.matched in
  Seq((List.rev matching.prefix) @ (matching.command_abr l) @ matching.suffix)

let rec check_interp sels = function
    [],[] -> true
  | s :: interp, ["_"] :: interp2 -> check_interp sels (interp,interp2)
  | V s :: interp, l2 :: interp2 -> if Xlist.mem l2 s then check_interp sels (interp,interp2) else false
  | S s :: interp, l2 :: interp2 ->
      (try
        let l = Xlist.assoc sels s in
        let b = Xlist.fold l false (fun b s -> Xlist.mem l2 s || b) in
        if b then check_interp sels (interp,interp2) else false
      with Not_found -> check_interp sels (interp,interp2))
  | G :: interp, l2 :: interp2 -> check_interp sels (interp,interp2)
(*  | [],[["ncol"]] -> true  (* FIXME ncol *)
  | [],[["col"]] -> true  (* FIXME ncol *)
  | [],[["pt"]] -> true  (* FIXME ncol *)*)
  | [],l -> failwith ("check_interp 1: " ^ Tagset.render [l])
  | _,l -> failwith ("check_interp 2: " ^ Tagset.render [l])

let rec get_sels sels = function
    [],[] -> sels
  | s :: interp, ["_"] :: interp2 -> get_sels sels (interp,interp2)
  | V s :: interp, l2 :: interp2 -> get_sels sels (interp,interp2)
  | S s :: interp, l2 :: interp2 ->
      (try
        let l = Xlist.assoc sels s in
        let sels = List.remove_assoc s sels in
        let l = Xlist.fold l [] (fun l s -> if Xlist.mem l2 s then s :: l else l) in
        get_sels ((s,l) :: sels) (interp,interp2)
      with Not_found -> get_sels ((s,l2) :: sels) (interp,interp2))
  | G :: interp, l2 :: interp2 -> get_sels sels (interp,interp2)
(*  | [],[["ncol"]] ->  sels (* FIXME ncol *)
  | [],[["col"]] ->  sels (* FIXME ncol *)
  | [],[["pt"]] ->  sels (* FIXME ncol *)*)
  | _ -> failwith "get_sels"

let match_token sels = function
    I m, Ideogram(_,m2) -> if m = m2 then [sels] else raise Not_found
  | C c, Lemma(_,_,_,c2) -> if c = c2 then [sels] else raise Not_found
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
      (* print_endline ("find_abr_pattern_tail 1: " ^ Tokens.string_of_tokens 0 token); *)
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
      (* print_endline ("find_abr_pattern 1: " ^ Tokens.string_of_tokens 0 token); *)
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
  (* Xlist.iter tokens (fun token -> print_endline ("A " ^ Tokens.string_of_tokens 0 token)); *)
  let tokens = find_abr_pattern (Xlist.map patterns (fun (pattern,command) ->
    {prefix=[]; matched=[]; suffix=[]; pattern=pattern; command=(fun _ -> Symbol "",[]); command_abr=command})) [] tokens in
  (* Xlist.iter tokens (fun token -> print_endline ("B " ^ Tokens.string_of_tokens 0 token)); *)
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
    [q;t] -> let next=t.next in [Token q;Token{t with len=t.len-2;next=next-2};Tokens.s_beg (next-2);Tokens.c_beg (next-1)]
  | [q] -> let next=q.next in [Token{q with len=q.len-2;next=next-2};Tokens.s_beg (next-2);Tokens.c_beg (next-1)]
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


let find_replacement_patterns tokens =
  let tokens = find_patterns digit_patterns1 tokens in
  let tokens = normalize_tokens [] tokens in
  let tokens = find_patterns digit_patterns2 tokens in
  let tokens = normalize_tokens [] tokens in
  let tokens = find_patterns digit_patterns3 tokens in
  let tokens = normalize_tokens [] tokens in
  let tokens = find_patterns digit_patterns4 tokens in
  let tokens = normalize_tokens [] tokens in
  let tokens = find_patterns Acronyms.acronym_patterns tokens in
  let tokens = normalize_tokens [] tokens in
(*  let tokens = find_patterns !Acronyms.mte_patterns tokens in
  let tokens = normalize_tokens [] tokens in*)
(*   Xlist.iter tokens (fun t -> print_endline (Tokens.string_of_tokens 0 t)); *)
(*   let tokens = find_patterns Acronyms.name_patterns tokens in *) (* Wyłączone ze względu na to, że są w konflikcie z MWE *)
(*   Xlist.iter tokens (fun t -> print_endline (Tokens.string_of_tokens 0 t)); *)
  let tokens = normalize_tokens [] tokens in
(*  let tokens = find_patterns url_patterns1 tokens in
  let tokens = normalize_tokens [] tokens in
  let tokens = find_patterns url_patterns2 tokens in
  let tokens = normalize_tokens [] tokens in
  let tokens = find_patterns url_patterns3 tokens in
  let tokens = normalize_tokens [] tokens in*)
  let tokens = find_patterns html_patterns tokens in
  let tokens = normalize_tokens [] tokens in
  (*   Xlist.iter tokens (fun t -> print_endline (Tokens.string_of_tokens 0 t)); *)
  tokens

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
  

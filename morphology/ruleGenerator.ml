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

(* let alternation_map = MorphologyRules.alternation_map *)

let rule_types = Xlist.fold [
(*  Xlist.map (StringMap.find alternation_map "obce_ch") (fun (_,s,t) -> sprintf "%sch\t%s" s t), "{x}ych\t{x}";
  Xlist.map (StringMap.find alternation_map "obce_ch") (fun (_,s,t) -> sprintf "%smi\t%s" s t), "{x}ymi\t{x}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_miekkie_iy") (fun (_,s,t) -> sprintf "%s\t%s" s t), "{'}y\t{'}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_miekkie_iy") (fun (_,s,t) -> sprintf "%sch\t%s" s t), "{'}ych\t{'}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_miekkie_iy") (fun (_,s,t) -> sprintf "%sm\t%s" s t), "{'}ym\t{'}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_miekkie_iy") (fun (_,s,t) -> sprintf "%smi\t%s" s t), "{'}ymi\t{'}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_miekkie_ae") (fun (_,s,t) -> sprintf "%se\t%s" s t), "{'}e\t{'}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_miekkie_ae") (fun (_,s,t) -> sprintf "%sego\t%s" s t), "{'}ego\t{'}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_miekkie_ae") (fun (_,s,t) -> sprintf "%sej\t%s" s t), "{'}ej\t{'}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_miekkie_ae") (fun (_,s,t) -> sprintf "%semu\t%s" s t), "{'}emu\t{'}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_miekkie_ae") (fun (_,s,t) -> sprintf "%sa\t%s" s t), "{'}a\t{'}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_miekkie_ae") (fun (_,s,t) -> sprintf "%są\t%s" s t), "{'}ą\t{'}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_miekkie_ae") (fun (_,s,t) -> sprintf "%so\t%s" s t), "{'}o\t{'}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_miekkie_ae") (fun (_,s,t) -> sprintf "%sę\t%s" s t), "{'}ę\t{'}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_miekkie_ae") (fun (_,s,t) -> sprintf "%su\t%s" s t), "{'}u\t{'}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_miekkie_ae") (fun (_,s,t) -> sprintf "%sów\t%s" s t), "{'}ów\t{'}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_miekkie_ae") (fun (_,s,t) -> sprintf "%som\t%s" s t), "{'}om\t{'}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_miekkie_ae") (fun (_,s,t) -> sprintf "%sami\t%s" s t), "{'}ami\t{'}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_miekkie_ae") (fun (_,s,t) -> sprintf "%sach\t%s" s t), "{'}ach\t{'}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_miekkie_ae") (fun (_,s,t) -> sprintf "%sowi\t%s" s t), "{'}owi\t{'}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_miekkie_ae") (fun (_,s,t) -> sprintf "%sowie\t%s" s t), "{'}owie\t{'}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_miekkie_ae") (fun (_,s,t) -> sprintf "%sum\t%s" s t), "{'}um\t{'}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_miekkie_ae") (fun (_,s,t) -> sprintf "%sem\t%s" s t), "{'}em\t{'}";
(*  Xlist.map (StringMap.find alternation_map "funkcjonalnie_miekkie_ii") (fun (_,s,t) -> sprintf "%s\t%s" s t), "{'}ii\t{'}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_miekkie_yj") (fun (_,s,t) -> sprintf "%s\t%s" s t), "{'}yj\t{'}";*)
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_miekkie_wyglos") (fun (_,s,t) -> sprintf "%s\t%s" s t), "{'}ε\t{'}";
(*   Xlist.map (StringMap.find alternation_map "funkcjonalnie_miekkie_nowe") (fun (_,s,t) -> sprintf "%s\t%s" s t), "{v'}y\t{v'}"; *)
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_miekkie_nowe") (fun (_,s,t) -> sprintf "%sch\t%s" s t), "{v'}ych\t{v'}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_miekkie_nowe") (fun (_,s,t) -> sprintf "%sm\t%s" s t), "{v'}ym\t{v'}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_miekkie_nowe") (fun (_,s,t) -> sprintf "%smi\t%s" s t), "{v'}ymi\t{v'}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_miekkie_nowe") (fun (_,s,t) -> sprintf "%se\t%s" s t), "{v'}e\t{v'}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_miekkie_nowe") (fun (_,s,t) -> sprintf "%sego\t%s" s t), "{v'}ego\t{v'}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_miekkie_nowe") (fun (_,s,t) -> sprintf "%sej\t%s" s t), "{v'}ej\t{v'}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_miekkie_nowe") (fun (_,s,t) -> sprintf "%semu\t%s" s t), "{v'}emu\t{v'}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_miekkie_nowe") (fun (_,s,t) -> sprintf "%sa\t%s" s t), "{v'}a\t{v'}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_miekkie_nowe") (fun (_,s,t) -> sprintf "%są\t%s" s t), "{v'}ą\t{v'}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_miekkie_nowe") (fun (_,s,t) -> sprintf "%so\t%s" s t), "{v'}o\t{v'}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_miekkie_nowe") (fun (_,s,t) -> sprintf "%sę\t%s" s t), "{v'}ę\t{v'}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_miekkie_nowe") (fun (_,s,t) -> sprintf "%su\t%s" s t), "{v'}u\t{v'}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_miekkie_nowe") (fun (_,s,t) -> sprintf "%sów\t%s" s t), "{v'}ów\t{v'}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_miekkie_nowe") (fun (_,s,t) -> sprintf "%som\t%s" s t), "{v'}om\t{v'}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_miekkie_nowe") (fun (_,s,t) -> sprintf "%sami\t%s" s t), "{v'}ami\t{v'}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_miekkie_nowe") (fun (_,s,t) -> sprintf "%sach\t%s" s t), "{v'}ach\t{v'}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_miekkie_nowe") (fun (_,s,t) -> sprintf "%sowi\t%s" s t), "{v'}owi\t{v'}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_miekkie_nowe") (fun (_,s,t) -> sprintf "%sowie\t%s" s t), "{v'}owie\t{v'}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_miekkie_nowe") (fun (_,s,t) -> sprintf "%sum\t%s" s t), "{v'}um\t{v'}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_miekkie_nowe") (fun (_,s,t) -> sprintf "%sem\t%s" s t), "{v'}em\t{v'}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_miekkie_nowe_wyglos") (fun (_,s,t) -> sprintf "%s\t%s" s t), "{v'}ε\t{v'}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_twarde_y") (fun (_,s,t) -> sprintf "%s\t%s" s t), "{}y\t{}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_twarde_y") (fun (_,s,t) -> sprintf "%sch\t%s" s t), "{}ych\t{}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_twarde_y") (fun (_,s,t) -> sprintf "%sm\t%s" s t), "{}ym\t{}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_twarde_y") (fun (_,s,t) -> sprintf "%smi\t%s" s t), "{}ymi\t{}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_twarde_e") (fun (_,s,t) -> sprintf "%se\t%s" s t), "{}e\t{}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_twarde_e") (fun (_,s,t) -> sprintf "%sego\t%s" s t), "{}ego\t{}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_twarde_e") (fun (_,s,t) -> sprintf "%sej\t%s" s t), "{}ej\t{}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_twarde_e") (fun (_,s,t) -> sprintf "%semu\t%s" s t), "{}emu\t{}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_twarde_a") (fun (_,s,t) -> sprintf "%sa\t%s" s t), "{}a\t{}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_twarde_a") (fun (_,s,t) -> sprintf "%są\t%s" s t), "{}ą\t{}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_twarde_a") (fun (_,s,t) -> sprintf "%so\t%s" s t), "{}o\t{}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_twarde_a") (fun (_,s,t) -> sprintf "%sę\t%s" s t), "{}ę\t{}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_twarde_a") (fun (_,s,t) -> sprintf "%su\t%s" s t), "{}u\t{}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_twarde_a") (fun (_,s,t) -> sprintf "%sów\t%s" s t), "{}ów\t{}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_twarde_a") (fun (_,s,t) -> sprintf "%som\t%s" s t), "{}om\t{}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_twarde_a") (fun (_,s,t) -> sprintf "%sami\t%s" s t), "{}ami\t{}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_twarde_a") (fun (_,s,t) -> sprintf "%sach\t%s" s t), "{}ach\t{}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_twarde_a") (fun (_,s,t) -> sprintf "%sowi\t%s" s t), "{}owi\t{}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_twarde_a") (fun (_,s,t) -> sprintf "%sowie\t%s" s t), "{}owie\t{}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_twarde_a") (fun (_,s,t) -> sprintf "%sum\t%s" s t), "{}um\t{}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_twarde_e") (fun (_,s,t) -> sprintf "%sem\t%s" s t), "{}em\t{}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_twarde_i") (fun (_,s,t) -> sprintf "%s\t%s" s t), "{}'i\t{}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_twarde_ie") (fun (_,s,t) -> sprintf "%s\t%s" s t), "{}'ie\t{}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_twarde_wyglos") (fun (_,s,t) -> sprintf "%s\t%s" s t), "{}ε\t{}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_twarde_nowe_y") (fun (_,s,t) -> sprintf "%s\t%s" s t), "{v}y\t{v}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_twarde_nowe_y") (fun (_,s,t) -> sprintf "%sch\t%s" s t), "{v}ych\t{v}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_twarde_nowe_y") (fun (_,s,t) -> sprintf "%sm\t%s" s t), "{v}ym\t{v}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_twarde_nowe_y") (fun (_,s,t) -> sprintf "%smi\t%s" s t), "{v}ymi\t{v}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_twarde_nowe") (fun (_,s,t) -> sprintf "%se\t%s" s t), "{v}e\t{v}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_twarde_nowe") (fun (_,s,t) -> sprintf "%sego\t%s" s t), "{v}ego\t{v}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_twarde_nowe") (fun (_,s,t) -> sprintf "%sej\t%s" s t), "{v}ej\t{v}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_twarde_nowe") (fun (_,s,t) -> sprintf "%semu\t%s" s t), "{v}emu\t{v}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_twarde_nowe") (fun (_,s,t) -> sprintf "%sa\t%s" s t), "{v}a\t{v}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_twarde_nowe") (fun (_,s,t) -> sprintf "%są\t%s" s t), "{v}ą\t{v}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_twarde_nowe") (fun (_,s,t) -> sprintf "%so\t%s" s t), "{v}o\t{v}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_twarde_nowe") (fun (_,s,t) -> sprintf "%sę\t%s" s t), "{v}ę\t{v}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_twarde_nowe") (fun (_,s,t) -> sprintf "%su\t%s" s t), "{v}u\t{v}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_twarde_nowe") (fun (_,s,t) -> sprintf "%sów\t%s" s t), "{v}ów\t{v}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_twarde_nowe") (fun (_,s,t) -> sprintf "%som\t%s" s t), "{v}om\t{v}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_twarde_nowe") (fun (_,s,t) -> sprintf "%sami\t%s" s t), "{v}ami\t{v}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_twarde_nowe") (fun (_,s,t) -> sprintf "%sach\t%s" s t), "{v}ach\t{v}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_twarde_nowe") (fun (_,s,t) -> sprintf "%sowi\t%s" s t), "{v}owi\t{v}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_twarde_nowe") (fun (_,s,t) -> sprintf "%sowie\t%s" s t), "{v}owie\t{v}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_twarde_nowe") (fun (_,s,t) -> sprintf "%sum\t%s" s t), "{v}um\t{v}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_twarde_nowe_ie") (fun (_,s,t) -> sprintf "%s\t%s" s t), "{v}'ie\t{v}";
  Xlist.map (StringMap.find alternation_map "funkcjonalnie_twarde_nowe_wyglos") (fun (_,s,t) -> sprintf "%s\t%s" s t), "{v}ε\t{v}";
  Xlist.map (StringMap.find alternation_map "kapitaliki_y") (fun (_,s,t) -> sprintf "%s\t%s" s t), "{-}y\t{-}";
  Xlist.map (StringMap.find alternation_map "kapitaliki_y") (fun (_,s,t) -> sprintf "%sch\t%s" s t), "{-}ych\t{-}";
  Xlist.map (StringMap.find alternation_map "kapitaliki_y") (fun (_,s,t) -> sprintf "%sm\t%s" s t), "{-}ym\t{-}";
  Xlist.map (StringMap.find alternation_map "kapitaliki_y") (fun (_,s,t) -> sprintf "%smi\t%s" s t), "{-}ymi\t{-}";
  Xlist.map (StringMap.find alternation_map "kapitaliki_a") (fun (_,s,t) -> sprintf "%se\t%s" s t), "{-}e\t{-}";
  Xlist.map (StringMap.find alternation_map "kapitaliki_a") (fun (_,s,t) -> sprintf "%sego\t%s" s t), "{-}ego\t{-}";
  Xlist.map (StringMap.find alternation_map "kapitaliki_a") (fun (_,s,t) -> sprintf "%sej\t%s" s t), "{-}ej\t{-}";
  Xlist.map (StringMap.find alternation_map "kapitaliki_a") (fun (_,s,t) -> sprintf "%semu\t%s" s t), "{-}emu\t{-}";
  Xlist.map (StringMap.find alternation_map "kapitaliki_a") (fun (_,s,t) -> sprintf "%sa\t%s" s t), "{-}a\t{-}";
  Xlist.map (StringMap.find alternation_map "kapitaliki_a") (fun (_,s,t) -> sprintf "%są\t%s" s t), "{-}ą\t{-}";
  Xlist.map (StringMap.find alternation_map "kapitaliki_a") (fun (_,s,t) -> sprintf "%so\t%s" s t), "{-}o\t{-}";
  Xlist.map (StringMap.find alternation_map "kapitaliki_a") (fun (_,s,t) -> sprintf "%sę\t%s" s t), "{-}ę\t{-}";
  Xlist.map (StringMap.find alternation_map "kapitaliki_a") (fun (_,s,t) -> sprintf "%su\t%s" s t), "{-}u\t{-}";
  Xlist.map (StringMap.find alternation_map "kapitaliki_a") (fun (_,s,t) -> sprintf "%sów\t%s" s t), "{-}ów\t{-}";
  Xlist.map (StringMap.find alternation_map "kapitaliki_a") (fun (_,s,t) -> sprintf "%som\t%s" s t), "{-}om\t{-}";
  Xlist.map (StringMap.find alternation_map "kapitaliki_a") (fun (_,s,t) -> sprintf "%sem\t%s" s t), "{-}em\t{-}";
  Xlist.map (StringMap.find alternation_map "kapitaliki_a") (fun (_,s,t) -> sprintf "%sami\t%s" s t), "{-}ami\t{-}";
  Xlist.map (StringMap.find alternation_map "kapitaliki_a") (fun (_,s,t) -> sprintf "%sach\t%s" s t), "{-}ach\t{-}";
  Xlist.map (StringMap.find alternation_map "kapitaliki_a") (fun (_,s,t) -> sprintf "%sowi\t%s" s t), "{-}owi\t{-}";
  Xlist.map (StringMap.find alternation_map "kapitaliki_a") (fun (_,s,t) -> sprintf "%sowie\t%s" s t), "{-}owie\t{-}";
  Xlist.map (StringMap.find alternation_map "kapitaliki_a") (fun (_,s,t) -> sprintf "%sum\t%s" s t), "{-}um\t{-}";
  Xlist.map (StringMap.find alternation_map "kapitaliki_ie") (fun (_,s,t) -> sprintf "%s\t%s" s t), "{-}'ie\t{-}";
  Xlist.map (StringMap.find alternation_map "kapitaliki_wyglos") (fun (_,s,t) -> sprintf "%s\t%s" s t), "{-}ε\t{-}";*)
  ] StringMap.empty (fun map (l,code) ->
      Xlist.fold l map (fun map rule -> StringMap.add_inc map rule code (fun code2 ->
        print_endline ("rule_types: " ^ rule ^ " " ^ code ^ " " ^ code2); code2)))

let rec cut_prefix_list c ll =
  Xlist.map ll (function
    [] -> raise Not_found
  | x :: l -> if x = c then l else raise Not_found)

let rec find_common_prefix_length_rec n = function
    [] :: _ -> n
  | (c :: l) :: ll ->
      (try
        let ll = cut_prefix_list c ll in
        find_common_prefix_length_rec (n + String.length c) (l :: ll)
      with Not_found -> n)
 | [] -> failwith "find_common_prefix_length_rec"

let find_common_prefix_length l =
  let ll = Xlist.map l Xunicode.utf8_chars_of_utf8_string(*Stem.text_to_chars*) in
  find_common_prefix_length_rec 0 ll

let cut_prefixn i s =
  let n = String.length s in
  if i >= n then "" else
  try String.sub s i (n-i) with _ -> failwith ("cut_prefixn: " ^ s ^ " " ^ string_of_int i)

let rule_code (a,b) =
  let s = sprintf "%s\t%s" a b in
  try StringMap.find rule_types s, true with Not_found ->
  if Xstring.check_prefix b a then
    let suf = Xstring.cut_prefix b a in
    suf ^ "_" ^ (String.concat "_" (List.rev (Xunicode.utf8_chars_of_utf8_string(*Stem.text_to_chars*) b))), false
  else "???", false

let generate_rule stem stem_pref orth =
  let n = find_common_prefix_length [stem_pref;orth] in
  let a = cut_prefixn n orth in
  let b = cut_prefixn n stem in
  let c,f = rule_code (a,b) in
  if f then "\t" ^ c else sprintf "%s\t%s\t%s" c a b

let rec classify_entry entry = function
    (class_interp,suf,cl) :: class_sel ->
       let l = Xlist.fold entry.forms [] (fun l form ->
         if form.interp = class_interp then form.orth :: l else l) in
       let b = Xlist.fold l false (fun b orth ->
         if Xstring.check_sufix suf orth then true else b) in
       if b then cl else classify_entry entry class_sel
(*       let l = StringSet.to_list (Xlist.fold l StringSet.empty (fun set orth ->
         if check_prefix stem orth then
           StringSet.add set (cut_prefix stem orth)
         else set)) in
       if Xlist.mem l suf then cl else classify_noun lemma stem interps class_sel
       let l = StringSet.to_list (Xlist.fold l StringSet.empty (fun set orth ->
         if check_prefix stem orth then
           StringSet.add set (cut_prefix stem orth)
         else set)) in
       if Xlist.mem l suf then cl else classify_noun lemma stem interps class_sel*)
(*       (match l with
         [] -> classify_noun lemma stem interps class_sel
       | [s] -> if s = suf then cl else classify_noun lemma stem interps class_sel
       | _ -> print_endline ("classify_noun multiple class: " ^ lemma ^ " " ^ String.concat " " l);
              classify_noun lemma stem interps class_sel)*)
  | [] -> (*print_endline ("classify_noun unknown class: " ^ lemma);*) "X"

let entry_classes =
  List.flatten (Xlist.map ["m1";"m2";"m3";"n1";"n2";"f";"p1";"p2";"p3"] (fun gender ->
    Xlist.map ["ii";"ji";"yj"] (fun sufix ->
      "subst:pl:gen:" ^ gender, sufix,"II"))) @
  List.flatten (Xlist.map ["m1";"m2";"m3";"n1";"n2";"f"] (fun gender ->
    Xlist.map ["a"] (fun sufix ->
      "subst:sg:nom:" ^ gender, sufix,"A"))) @
  List.flatten (Xlist.map ["m1";"m2";"m3";"n1";"n2";"f"] (fun gender ->
    Xlist.map ["ę"] (fun sufix ->
      "subst:sg:acc:" ^ gender, sufix,"Ę"))) @
  List.flatten (Xlist.map ["m1";"m2";"m3";"n1";"n2";"f"] (fun gender ->
    Xlist.map ["ą"] (fun sufix ->
      "subst:sg:inst:" ^ gender, sufix,"Ą"))) @
(*  List.flatten (Xlist.map ["m1";"m2";"m3";"n1";"n2";"f";"p1";"p2";"p3"] (fun gender ->
    Xlist.map ["ym";"im";"m"] (fun sufix ->
      "subst:pl:dat:" ^ gender, sufix,"ADJ"))) @
  List.flatten (Xlist.map ["m1";"m2";"m3";"n1";"n2";"f"] (fun gender ->
    Xlist.map ["a","A";"o","O";"e","E"] (fun (sufix,s) ->
      "subst:sg:nom:" ^ gender, sufix,s))) @*)
[
   "subst:sg:nom:n2","um","UM";
  ]

let generate_rules_entry entry =
  let stem_pref = Stem.cut_stem_sufix entry.stem in
  let cl = classify_entry entry entry_classes in
  Xlist.map entry.forms (fun form ->
    form.interp,cl ^ "\t" ^ generate_rule entry.stem stem_pref form.orth)

let phon_generate_rules_entry entry =
  Xlist.fold entry.phon_stem [] (fun found stem ->
    let stem_pref = Stem.cut_stem_sufix stem in
    let cl = classify_entry entry entry_classes in
    Xlist.fold entry.forms found (fun found form ->
      Xlist.fold form.phon_orth found (fun found orth ->
        (* printf "lemma=%s stem=%s phon_stem=%s stem_pref=%s phon_orth=%s interp=%s\n%!" entry.lemma entry.stem stem stem_pref orth form.interp; *)
        (form.interp,cl ^ "\t" ^ generate_rule stem stem_pref orth.phon) :: found)))

let generate_rules_lu_entry entry =
  let stem_pref = Stem.cut_stem_sufix entry.lu_stem in
  let v1 = if entry.validated1 then "V" else "" in
  let v2 = if entry.validated2 then "V" else "" in
  [v1 ^ "A" ^ string_of_int entry.rel_id,generate_rule entry.lu_stem stem_pref entry.lemma1,entry.lemma1 ^ "->" ^ entry.lemma2;
   v2 ^ "B" ^ string_of_int entry.rel_id,generate_rule entry.lu_stem stem_pref entry.lemma2,entry.lemma2 ^ "->" ^ entry.lemma1]

let generate_interp_rules rules con_flag group_flag lemma_flag simple_lemma form =
  let candidates = MorphologyRules.CharTrees.find rules form.orth in
      (* printf "S %d\n" (Xlist.size forms); *)
  let candidates = Xlist.fold candidates [] (fun candidates (stem,rule) ->
        (* printf "R %s\t%s\n" stem (MorphologyRules.string_of_rule rule); *)
    if stem ^ rule.set = simple_lemma then rule :: candidates else candidates) in
  Xlist.rev_map candidates (fun rule ->
    let tags = rule.tags in
    let tags = if con_flag then snd (MorphologyRules.extract_tag "con" [] tags) else tags in
    let tags = if group_flag then snd (MorphologyRules.extract_tag "group" [] tags) else tags in
    let tags = if lemma_flag then snd (MorphologyRules.extract_tag "lemma" [] tags) else tags in
    let tags = Xlist.sort tags MorphologyRules.compare_tag in
    String.concat " " (Xlist.map tags (fun (k,v) -> k ^ "=" ^ v)) ^ "\t" ^ form.interp)

(* let check_palat tags =
  try
    let palat = Xlist.assoc tags "palat" in
    let lpalat = Xlist.assoc tags "lpalat" in
    palat = lpalat
  with Not_found -> true *)

let calculate_number_value = function
    "sg" -> 2
  | "pl" -> 1
  | "sg.pl" -> 0
  | _ -> failwith "calculate_number_value2"

let calculate_case_value2 = function
    "nom" -> 7
  | "gen" -> 6
  | "dat" -> 5
  | "acc" -> 4
  | "inst" -> 3
  | "loc" -> 2
  | "voc" -> 1
  | "" -> 0
  | _ -> failwith "calculate_case_value2"

let calculate_case_value s =
  let c1,c2,c3 = match Xstring.split "\\." s with
      [c1] -> c1,"",""
    | [c1;c2] -> c1,c2,""
    | c1 :: c2 :: c3 :: _ -> c1,c2,c3
    | _ -> failwith "calculate_case_value" in
  100 * calculate_case_value2 c1 + 10 * calculate_case_value2 c2 + calculate_case_value2 c3

let calculate_gender_value = function
  | "m1" -> 8
  | "m2" -> 7
  | "m3" -> 6
  | "f" -> 5
  | "n:ncol" -> 4
  | "n:col" -> 3
  | "n:pt" -> 2
  | "m1:pt" -> 1
  | "depr:pl:nom.acc.voc:m2" -> 8
  (* | s -> print_endline ("calculate_gender_value: " ^ s); 0 *)
  | s -> failwith ("calculate_gender_value: " ^ s)

let calculate_grad_value = function
  | "pos" -> 3
  | "com" -> 2
  | "sub" -> 1
  | s -> failwith ("calculate_grad_value: " ^ s)

let calculate_person_value = function
  | "pri" -> 3
  | "sec" -> 2
  | "ter" -> 1
  | s -> failwith ("calculate_person_value: " ^ s)

let calculate_rule_value tags interp =
  if interp = "" then failwith "calculate_rule_value: empty interp" else
  let cat = MorphologyRules.get_tag tags "cat" in
  let lemma = MorphologyRules.get_tag tags "lemma" in
  let group = MorphologyRules.get_tag tags "group" in
  if cat = "noun" || cat = "adj" then
    let lemma_val = match lemma with
        "a" -> 20
      | "ε" -> 19
      | "y" -> 18
      | "e" -> 17
      | "o" -> 16
      | "um" -> 15
      | "us" -> 14
      | "owie" -> 13
      | "i" -> 12
      | "o(n)" -> 11
      | "ę" -> 10
      | "anin" -> 9
      | "mię" -> 8
      | "stwo" -> 7
      | _ -> 0 in
    let interp_val =
      match Xstring.split ":" (List.hd (Xstring.split "|" interp)) with
        "subst" :: n :: c :: g -> 10000 * calculate_gender_value (String.concat ":" g) + 1000 * calculate_number_value n + calculate_case_value c
      | "depr" :: _ -> 10000 * calculate_gender_value interp
      | "adj" :: n :: c :: g :: d :: [] -> 10000 * calculate_grad_value d + 1000 * calculate_number_value n + calculate_case_value c
      | ["adjc"] -> 1
      | ["adja"] -> 2
      | ["adjp"] -> 3
      | _ -> failwith ("calculate_rule_value: " ^ interp) in
    100000 * lemma_val + interp_val
  else if cat = "verb" then
    let lemma_val = match lemma with
        "ać" -> 20
      | "ować" -> 19
      | "ywać" -> 18
      | "iwać" -> 17
      | "awać" -> 16
      | "owywać" -> 15
      | "uć" -> 14
      | "yć" -> 13
      | "nąć" -> 12
      | "ąć" -> 11
      | "eć" -> 10
      | "ć" -> 9
      | "c" -> 8
      | "palat-ć" -> 7
      | "palat-eć" -> 6
      | "ać2" -> 5
      | _ -> 0 in
    let group_val = match group with
        "a" -> 32
      | "aje" -> 31
      | "aj" -> 30
      | "e" -> 29
      | "eje" -> 28
      | "ej" -> 27
      | "u" -> 26
      | "uje" -> 25
      | "uj" -> 24
      | "y" -> 23
      | "yje" -> 22
      | "yj" -> 21
      | "ε" -> 20
      | "J" -> 19
      | "j" -> 18
      | "Je" -> 17
      | "Ja" -> 16
      | "Jo" -> 15
      | "ną" -> 14
      | "ą" -> 13
      | "nie" -> 12
      | "nię" -> 11
      | "ę" -> 10
      | "nę" -> 9
      | "ie" -> 8
      | "ń" -> 7
      | "nij" -> 6
      | "mij" -> 5
      | "n" -> 4
      | "o" -> 3
      | "io" -> 2
      | _ -> 0 in
    let interp_val =
      match Xstring.split ":" (List.hd (Xstring.split "|" interp)) with
        ["fin";n;p;_] -> 4200000 + 4 * calculate_number_value n + calculate_person_value p
      | ["impt";n;p;_] -> 4100000 + 4 * calculate_number_value n + calculate_person_value p
      | ["pcon";_] -> 3000100
      | ["pacta"] -> 3000000
      | ["pact";n;c;g;_;_] -> 2000000 + 1000 * calculate_number_value n + calculate_case_value c
      | ["inf";_] -> 1500000
      | ["praet";n;g] -> 1001000 + 1000 * calculate_number_value n
      | ["pant";_] -> 1000200
      | ["imps";_] -> 1000100
      | ["ger";n;c;g;_] -> 1000000 + 1000 * calculate_number_value n + calculate_case_value c
      | ["ppas";n;c;g;_;_] -> 1000 * calculate_number_value n + calculate_case_value c
      | _ -> 0 in
    10000000 * lemma_val + 10000 * group_val + interp_val
  else 0

let phon_generate_interp_rules selected_tags form =
  Xlist.rev_map form.candidates (fun (_,rule,_,_) ->
      let tags = Xlist.fold rule.tags [] (fun tags (k,v) ->
        if StringSet.mem selected_tags k then (k,v) :: tags else tags) in
      let tags = Xlist.sort tags MorphologyRules.compare_tag in
      calculate_rule_value rule.tags form.interp,
      String.concat " " (Xlist.map tags (fun (k,v) -> k ^ "=" ^ v)) ^ "\t" ^ form.interp)

(* let phon_generate_interp_rules rules selected_tags simple_lemma form =
  Xlist.fold form.phon_orth [] (fun found orth ->
    let candidates = MorphologyRules.CharTrees.find rules orth.phon in
      (* printf "S %d\n" (Xlist.size forms); *)
    let candidates = Xlist.fold candidates [] (fun candidates (stem,rule) ->
      let candidate_lemma = Fonetics.rev_translate true Fonetics.rev_rules (stem ^ rule.set) orth.mapping in
      (* printf "R %s\t%s\n" stem (MorphologyRules.string_of_rule rule); *)
      if candidate_lemma = simple_lemma (*&& check_palat rule.tags*) then rule :: candidates else candidates) in
    Xlist.rev_map candidates (fun rule ->
      let tags = Xlist.fold rule.tags [] (fun tags (k,v) ->
        if StringSet.mem selected_tags k then (k,v) :: tags else tags) in
      let tags = Xlist.sort tags MorphologyRules.compare_tag in
      calculate_rule_value rule.tags form.interp,
      String.concat " " (Xlist.map tags (fun (k,v) -> k ^ "=" ^ v)) ^ "\t" ^ form.interp) @ found) *)

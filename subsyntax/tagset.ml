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

let render interps =
  String.concat "|" (Xlist.map interps (fun interp ->
    (String.concat ":" (Xlist.map interp (fun interp2 ->
      (String.concat "." interp2))))))

let render_full pos interps =
  String.concat "|" (Xlist.map interps (fun interp ->
    (String.concat ":" (Xlist.map ([pos] :: interp) (fun interp2 ->
      (String.concat "." interp2))))))

let split_genders_n l =
  Xlist.fold l ([],[]) (fun (n,others) -> function
      "n" -> "n" :: n, others
    | g -> n, g :: others)

let split_genders_nm1 l =
  Xlist.fold l ([],[]) (fun (n,others) -> function
      "n" -> "n" :: n, others
    | "m1" -> "m1" :: n, others
    | g -> n, g :: others)

let parse s =
  let l = Xlist.map (Xstring.split "|" s) (fun s ->
    match Xlist.map (Xstring.split ":" s) (fun t -> Xstring.split "\\." t) with
      [pos] :: tags -> pos, tags
    | _ -> failwith ("parse: " ^ s)) in
  Xlist.fold l [] (fun l -> function
    "subst",[n;c;g;["pt"]] ->
       let x,y = split_genders_nm1 g in
       let x = Xlist.map x (fun s -> s ^ ":pt") in
       ("subst",[n;c;y @ x]) :: l
(*       let l = if x = [] then l else ("subst",[n;c;Xlist.map x (fun s -> s :: ":pt")]) :: l in
       if y = [] then l else ("subst",[n;c;y]) :: l*)
  | "subst",[n;c;g;col] ->
       let x,y = split_genders_n g in
       let x = Xlist.rev_map (Xlist.multiply_list [x;col]) (String.concat ":") in
       ("subst",[n;c;y @ x]) :: l
(*       let l = if x = [] then l else ("subst",[n;c;x;col]) :: l in
       if y = [] then l else ("subst",[n;c;y]) :: l*)
  | "num",[n;c;g;a;col] ->
       let x,y = split_genders_n g in
       let x = Xlist.rev_map (Xlist.multiply_list [x;col]) (String.concat ":") in
       ("num",[n;c;y @ x;a]) :: l
(*       let l = if x = [] then l else ("num",[n;c;x;a;col]) :: l in
       if y = [] then l else ("num",[n;c;y;a]) :: l*)
  | "adv",[] -> ("adv",[["pos"]]) :: l
  | pos,interp -> (pos,interp) :: l)

let patterns = Xlist.fold [
  "subst",["numbers";"cases";"genders"];
(*   "subst",["numbers";"cases";"genders";"cols"]; *)
  "depr",["numbers";"cases";"genders"];
  "ppron12",["numbers";"cases";"genders";"persons"];
  "ppron12",["numbers";"cases";"genders";"persons";"akcs"];
  "ppron3",["numbers";"cases";"genders";"persons"];
  "ppron3",["numbers";"cases";"genders";"persons";"akcs"];
  "ppron3",["numbers";"cases";"genders";"persons";"akcs";"praeps"];
  "siebie",["cases"];
  "prep",["cases";"woks"];
  "prep",["cases"];
  "x",[];
  "num",["numbers";"cases";"genders";"acms"];
(*   "num",["numbers";"cases";"genders";"acms";"cols"]; *)
  "numcomp",[];
(*  "intnum",[];
  "realnum",[];
  "intnum-interval",[];
  "realnum-interval",[];*)
  "symbol",["modes"];
  "ordnum",["numbers";"cases";"genders"];
(*  "ordnum",[];
  "date",[];
  "date-interval",[];
  "hour-minute",[];
  "hour",[];
  "hour-minute-interval",[];
  "hour-interval",[];
  "year",[];
  "year-interval",[];
  "day",[];
  "day-interval",[];
  "day-month",[];
  "day-month-interval",[];
  "month-interval",[];*)
(*  "initial",[];
  "roman",[];
  "roman-interval",[];
  "match-result",[];
  "url",[];
  "email",[];
  "phone-number",[];
  "postal-code",[];
  "obj-id",[];
  "building-number",[];*)
  "fixed",[];
  "adj",["numbers";"cases";"genders";"grads"];
  "adjc",[];
  "adjp",[];
  "adja",[];
  "adv",["grads"];
  "ger",["numbers";"cases";"genders";"aspects";"negations"];
  "pact",["numbers";"cases";"genders";"aspects";"negations"];
  "ppas",["numbers";"cases";"genders";"aspects";"negations"];
  "fin",["numbers";"persons";"aspects"];
  "bedzie",["numbers";"persons";"aspects"];
  "praet",["numbers";"genders";"aspects";"agls"];
  "praet",["numbers";"genders";"aspects"];
  "winien",["numbers";"genders";"aspects"];
  "impt",["numbers";"persons";"aspects"];
  "imps",["aspects"];
  "pred",[];
  "aglt",["numbers";"persons";"aspects";"woks"];
  "inf",["aspects"];
  "pcon",["aspects"];
  "pant",["aspects"];
  "pacta",[];
  "qub",[];
  "comp",[];
  "conj",[];
  "interj",[];
  "sinterj",[];
  "burk",[];
  "interp",[];
  "unk",[];
  "xxx",[];
(*  "html-tag",[];
  "list-item",[];*)
  "coord1",[];
  "coord1list",[];
  "coord2",[];
  "coord2",["cases"];
  "coord2",["numbers";"cases";"genders"];
  "coord2list",[];
  "coord",[];
  "coord",["count"];
  ] StringMap.empty (fun map (k,v) ->
    let map2 = try StringMap.find map k with Not_found -> IntMap.empty in
    let map2 = IntMap.add map2 (Xlist.size v) v in
    StringMap.add map k map2)

let tags = Xlist.fold [
  "numbers",["sg";"pl"];
  "cases",["nom";"gen";"dat";"acc";"inst";"loc";"voc"];
  "genders",["m1";"m2";"m3";"f";"n";"m1:pt";"n:pt";"n:ncol";"n:col"];
(*   "cols",["col";"ncol";"pt"]; *)
  "persons",["pri";"sec";"ter"];
  "akcs",["akc";"nakc"];
  "praeps",["praep";"npraep"];
  "woks",["wok";"nwok"];
  "acms",["congr";"rec"];
  "grads",["pos";"com";"sup"];
  "aspects",["perf";"imperf"];
  "negations",["aff";"neg"];
  "agls",["agl";"nagl"];
  ] StringMap.empty (fun map (k,l) -> StringMap.add map k l)

let validate lemma pos interps =
  let patterns = try StringMap.find patterns pos with Not_found -> failwith ("validate: unknown pos " ^ pos ^ " in lemma " ^ lemma) in
  Xlist.map interps (fun interp ->
    let pattern =
      try IntMap.find patterns (Xlist.size interp)
      with Not_found -> failwith ("validate: unknown pattern for " ^ render_full pos [interp] ^ " in lemma " ^ lemma) in
    Xlist.map2 pattern interp (fun p interp2 ->
      if p = "modes" || p = "count" then interp2 else
      let tags = try StringMap.find tags p with Not_found -> failwith ("validate: unknown p " ^ p) in
      if interp2 = ["_"] || (p = "cases" && interp2 = ["$c"]) then tags else (
      Xlist.iter interp2 (fun s -> if Xlist.mem tags s then () else failwith ("validate: " ^ s ^ " is not a proper value for " ^ p ^ " in lemma " ^ lemma));
      interp2)))

let parse_and_validate lemma s =
  Xlist.rev_map (parse s) (fun (pos,interps) -> pos, validate lemma pos [interps])

let expand interps =
  Xlist.fold interps [] (fun l interp ->
    Xlist.multiply_list interp @ l)

let select_tag tag_pat tag pos interp =
(*   Printf.printf "select_tag 1: %s %s %s %s\n%!" tag_pat tag pos (render [interp]); *)
  let patterns = try StringMap.find patterns pos with Not_found -> failwith ("validate: unknown pos " ^ pos) in
  let pattern =
    try IntMap.find patterns (Xlist.size interp)
    with Not_found -> failwith ("select_tag: unknown pattern for " ^ render_full pos [interp]) in
  Xlist.map2 pattern interp (fun p interp2 ->
    if p = "genders" && tag_pat = "genders" then
      match tag with 
(*        "m1" -> 
	       let l = Xlist.fold interp2 [] (fun l s -> if Xlist.mem ["m1";"m1:pt"] s then s :: l else l) in
	       if l = [] then raise Not_found else l*)
	  | "n" -> 
(* 	       print_endline ("select_tag 2: [" ^ String.concat "." interp2 ^ "]"); *)
	       let l = Xlist.fold interp2 [] (fun l s -> if Xlist.mem ["n";"n:col";"n:ncol"(*;"n:pt"*)] s then s :: l else l) in
(* 	       print_endline ("select_tag 3: [" ^ String.concat "." l ^ "]"); *)
	       if l = [] then raise Not_found else l
(*	  | "m1:pt" -> if Xlist.mem interp2 tag || Xlist.mem interp2 "m1" then [tag] else raise Not_found
	  | "n:pt" -> if Xlist.mem interp2 tag || Xlist.mem interp2 "n" then [tag] else raise Not_found*)
	  | "n:ncol" -> if Xlist.mem interp2 tag || Xlist.mem interp2 "n" then [tag] else raise Not_found
	  | "n:col" -> if Xlist.mem interp2 tag || Xlist.mem interp2 "n" then [tag] else raise Not_found
	  | _ -> if Xlist.mem interp2 tag then [tag] else raise Not_found
    else if p = tag_pat then if Xlist.mem interp2 tag then [tag] else raise Not_found
    else interp2)

let simplify_pos = function
    "subst" -> "noun"
  | "depr" -> "noun"
  | "symbol" -> "symbol"
  | "unk" -> "noun"
  | "xxx" -> "noun"
  | "other" -> "other"
  | "adj" -> "adj"
  | "adjc" -> "adj"
  | "adjp" -> "adj"
  | "adja" -> "adj"
  | "ordnum" -> "ordnum"
  | "ger" -> "verb"
  | "pact" -> "verb"
  | "ppas" -> "verb"
  | "fin" -> "verb"
  | "bedzie" -> "verb"
  | "praet" -> "verb"
  | "winien" -> "verb"
  | "impt" -> "verb"
  | "imps" -> "verb"
  | "inf" -> "verb"
  | "pcon" -> "verb"
  | "pant" -> "verb"
  | "pacta" -> "verb"
  | "pred" -> "verb"
  | "ppron12" -> "pron"
  | "ppron3" -> "pron"
  | "siebie" -> "pron"
  | "fixed" -> "fixed"
  | "num" -> "num"
  | "numcomp" -> "numcomp"
  | "interp" -> "interp"
  | "x" -> "prep"
  | "pro" -> "pro"
  | s -> s


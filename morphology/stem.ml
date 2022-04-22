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

(* Zakładam, że zbiór form należy do jednego leksemu *)

let adj_stem_sel = [
  1,"adj:sg:nom.voc:f:pos", "a","";
  1,"adj:sg:nom:f:pos", "a","";
  ]

let noun_stem_sel =
  List.flatten (Xlist.map ["m1";"m2";"m3";"n1";"n2";"f";"p1";"p2";"p3";"m1:pt";"n:col";"n:ncol";"n:pt";"";""] (fun gender -> [
    1,"subst:pl:loc:" ^ gender, "’ach","";
    1,"subst:pl:loc:" ^ gender, "-ach","";
    1,"subst:pl:loc:" ^ gender, "-etach","";
    1,"subst:pl:loc:" ^ gender, "-otach","";
    2,"subst:pl:dat:" ^ gender, "om","";
    2,"subst:pl:loc:" ^ gender, "ach","";
    2,"subst:pl:loc:" ^ gender, "ych","";
    2,"subst:pl:loc:" ^ gender, "bich","bi";
    2,"subst:pl:loc:" ^ gender, "cich","ci";
    2,"subst:pl:loc:" ^ gender, "dzich","dzi";
    2,"subst:pl:loc:" ^ gender, "fich","fi";
    2,"subst:pl:loc:" ^ gender, "mich","mi";
    2,"subst:pl:loc:" ^ gender, "nich","ni";
    2,"subst:pl:loc:" ^ gender, "pich","pi";
    2,"subst:pl:loc:" ^ gender, "sich","si";
    2,"subst:pl:loc:" ^ gender, "wich","wi";
    2,"subst:pl:loc:" ^ gender, "zich","zi";
    2,"subst:pl:loc:" ^ gender, "kich","k";
    2,"subst:pl:loc:" ^ gender, "gich","g";
    2,"subst:pl:loc:" ^ gender, "lich","l";
    2,"subst:pl:loc:" ^ gender, "żich","żi";
    3,"subst:sg:gen:" ^ gender, "kiego","k";
    3,"subst:sg:gen:" ^ gender, "ojego","oj";
    3,"subst:sg:gen:" ^ gender, "nego","n";
    3,"subst:sg:gen:" ^ gender, "tego","t";
    3,"subst:sg:gen:" ^ gender, "wego","w";
    3,"subst:sg:gen:" ^ gender, "siego","si";
    3,"subst:sg:gen:" ^ gender, "ojej","oj";
    ])) @
  [3,"subst:pl:inst:p1", "wem","w";
   3,"subst:pl:inst:m1:pt", "wem","w";
   3,"subst:pl:nom:m1", "owie","";
   4,"subst:pl:gen:p1", "oich","oj";
   ]

let verb_stem_sel2 =
  List.flatten (Xlist.map ["imperf";"perf";"imperf.perf"] (fun aspect -> [
    4,"praet:sg:f:" ^ aspect, "kła","k";
    4,"praet:sg:f:" ^ aspect, "gła","g";
    4,"praet:sg:f:" ^ aspect, "zła","z";
    4,"praet:sg:f:" ^ aspect, "sła","s";
    4,"praet:sg:f:" ^ aspect, "zła","z";
    4,"praet:sg:f:" ^ aspect, "dła","d";
    4,"praet:sg:f:" ^ aspect, "tła","t";
    4,"praet:sg:f:" ^ aspect, "bła","b";
    4,"praet:sg:f:" ^ aspect, "łła","ł";
    4,"praet:sg:f:" ^ aspect, "rła","r";
    5,"inf:" ^ aspect, "ieć","";
    6,"inf:" ^ aspect, "eć","";
(*     3,"ppas:sg:nom.voc:m1.m2.m3:" ^ aspect ^ ":aff", "ty",""; *)
(*       3,"praaaet:sg:f:" ^ aspect, "zła","z";   *)
    ]))

let prepare_stem_sel map stem_sel =
  Xlist.fold stem_sel map (fun map (priority,tags,a,b) ->
    StringMap.add_inc map tags [a,b,priority] (fun l -> (a,b,priority) :: l))

let stem_sel =
  let map = prepare_stem_sel StringMap.empty adj_stem_sel in
  let map = prepare_stem_sel map noun_stem_sel in
  let map = prepare_stem_sel map verb_stem_sel2 in
  map


let adv_stem_sel = [
  "o","",1;
  "wie","w",1;
  "nie","n",1;
  "dze","g",1;
  "le","ł",1;
  "cie","t",1;
  "dzie","d",1;
  "mie","m",1;
  "rze","r",1;
  "ce","k",1;
  ]

let verb_stem_sel = [
   "ować","",1;
   "owywać","",1;
   "iwać","",1;
   "ywać","",2;
   "awać","",1;
   "ać","",3;
   "nąć","",1;
   "ąć","",2;
(*     "eć","e",1;     *)
   "ić","",1;
   "yć","",1;
    "uć","u",1;
(*   "ć","",2; *)
  ]

let lemma_stem_sel =
  let map = StringMap.add StringMap.empty "adv" adv_stem_sel in
  let map = StringMap.add map "verb" verb_stem_sel in
  map

let is_applicable_sel (pat,_,_) s = Xstring.check_sufix pat s

let apply_sel (pat,set,_) s =
  (Xstring.cut_sufix pat s) ^ set

let get_priority (_,_,p) = p

let simplify_lemma s =
  match Xstring.split ":" s with
    [s] -> s
  | [s;_] -> s
  | _ -> failwith ("simplify_lemma: " ^ s)

let simplify_lemma_full s =
  match Xstring.split ":" s with
    [s] -> s,""
  | [s;t] -> s,t
  | _ -> failwith ("simplify_lemma_full: " ^ s)

let generate_stem entry =
  let orth = simplify_lemma entry.lemma in
  let lemma_stem_sel = try StringMap.find lemma_stem_sel entry.cat with Not_found -> [] in
  let stems = Xlist.fold lemma_stem_sel StringMap.empty (fun stems sel ->
      if is_applicable_sel sel orth then
        StringMap.add_inc stems (apply_sel sel orth) (get_priority sel) (fun priority -> min priority (get_priority sel))
      else stems) in
  let stems2 = Xlist.fold entry.forms StringMap.empty (fun stems form ->
    let sels = try StringMap.find stem_sel form.interp with Not_found -> [] in
    Xlist.fold sels stems (fun stems sel ->
      if is_applicable_sel sel form.orth then
        StringMap.add_inc stems (apply_sel sel form.orth) (get_priority sel) (fun priority -> min priority (get_priority sel))
      else stems)) in
  let stems = if StringMap.is_empty stems then stems2 else stems in
  let stems,_ = StringMap.fold stems ([],max_int) (fun (stems,priority) stem p ->
    if p < priority then [stem],p else
    if p > priority then stems,priority else
    stem :: stems, priority) in
  match stems with
     [] -> (*print_endline ("stem not found for " ^ entry.lemma);
          Xlist.iter entry.forms (fun form -> printf "  %s\t%s\n" form.orth form.interp); *)
          ""
  | [s] -> if s = "" then print_endline ("empty stem found for " ^ entry.lemma);
           s
  | l -> print_endline ("many stems found for " ^ entry.lemma ^ ": " ^ String.concat " " l); ""
         (*printf "\"%s\"; " entry.lemma; ""*)

let cut_stem_sufix s =
  let l = Xunicode.utf8_chars_of_utf8_string s in
  let l = match List.rev l with
      "i" :: _ :: l -> l
    | "j" :: _ :: l -> l
    | _ :: l -> l
    | _ -> [] in
  String.concat "" (List.rev l)

let rec longest_common_prefix rev = function
    x1 :: l1, x2 :: l2 -> if x1 = x2 then longest_common_prefix (x1 :: rev) (l1,l2) else List.rev rev
  | _ -> List.rev rev

let generate_stem_lu lemma orth =
  let l = longest_common_prefix [] (Xunicode.utf8_chars_of_utf8_string lemma,Xunicode.utf8_chars_of_utf8_string orth) in
  let stem = String.concat "" l in
  (* Printf.printf "%s %s %s\n%!" lemma orth stem; *)
  stem

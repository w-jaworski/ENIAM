(*
 *  LCGlexicon is a library that provides LCG lexicon form Polish
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

open LCGlexiconTypes
open Xstd

let all_numbers = ["sg";"pl"]
let all_cases = ["nom";"gen";"dat";"acc";"inst";"loc";"voc"]
(* let all_genders = ["m1";"m2";"m3";"f";"n1";"n2";"p1";"p2";"p3"] *)
let all_genders = ["m1";"m2";"m3";"f";"n"]
let all_persons = ["pri";"sec";"ter"]
(* FIXME: zamiast wszystkich możliwych wartości można używać Zero gdy nie ma uzgodnienia *)

let all_phrases = [
  "np";"adjp";"advp";"infp";"ip";"nump";
  "prepnp";"cp";"ncp";"prepncp";"padvp";"colonp";"mp";"intp";"admod";
  "adja";"prepadjp";"comparp";"xp";"xpnom";"xpgen";"symbol";"fixed";
  "prepfixed";"rp";"other";(*"";"";"";"";"";"";"";"";*)]

let selector_values = Xlist.fold [
    Lemma, [];
    IncludeLemmata, [];
    ProLemma, [];
    Pos, ["subst";"depr";"ppron12";"ppron3";"siebie";"prep";"fixed";(*"initial";*)"num";"numcomp";"symbol";"ordnum";
          "adj";"adjc";"adjp";"adja";"adv";"ger";"pact";"ppas";"fin";"bedzie";"praet";"winien";"impt";
          "imps";"pred";"aglt";"inf";"pcon";"pant";"pacta";"qub";"part";"comp";"conj";"interj";
          "sinterj";"burk";"interp";"xxx";"unk";(*"html-tag";*)(*"apron";*)"compar";"x"(*;"other"*);"pro"];
    Pos2, [];
    Cat, [];
    Coerced, [];
    Role, [];
    Irole, [];
    Prole, [];
    Nrole, [];
(*    SNode, ["concept";"sit";"dot";"relations"];
    Inode, ["concept";"sit";"dot";"relations"];
    Pnode, ["concept";"sit";"dot";"relations"];
    Nnode, ["concept";"sit";"dot";"relations"];*)
    Phrase, all_phrases;
    Number, all_numbers;
    Case, "postp" :: "pred" :: all_cases;
    Gender, all_genders;
    Person, all_persons;
    Grad, ["pos";"com";"sup"];
    Praep, ["praep";"npraep";"praep-npraep"];
    Acm, ["congr";"rec"];
    Ctype, ["int";"rel";"sub";"coord"];
    Mode, [(*"abl";"adl";"locat";"perl";"dur";"temp";"mod"*)];
    Aspect, ["perf";"imperf"];
    Negation, ["neg";"aff"];
    Mood, ["indicative";"imperative";"conditional";"modal"];
    Tense, ["past";"pres";"fut"];
    Nsyn, ["proper";"pronoun";"common"];
    Nsem, ["count";"mass";"unique"];
    (* Psem, ["sem";"nosem"]; *)
    Pt, ["pt";"npt"];
    Col, ["col";"ncol"];
    Ucase, all_cases;
] SelectorMap.empty (fun map (selector,vals) -> SelectorMap.add map selector vals)


let expand_numbers numbers =
  if Xlist.mem numbers "_" then all_numbers else numbers

let expand_genders genders  =
  if Xlist.mem genders "_" then all_genders else genders

let expand_cases cases  =
  if Xlist.mem cases "_" || Xlist.mem cases "$C" then all_cases else cases

let expand_akcs akcs  =
  if Xlist.mem akcs "_" then ["akc";"nakc"] else akcs

let expand_col = function
    ["pt"] -> "pt",["col";"ncol"]
  | ["col"] -> "npt",["col"]
  | ["ncol"] -> "npt",["ncol"]
  | _ -> failwith "expand_col"

let split_voc cases =
  Xlist.fold cases ([],[]) (fun (cases,voc) -> function
        "voc" -> cases, "voc" :: voc
      | s -> s :: cases, voc)

let load_subst_data filename _ =
  StringSet.of_list (File.load_lines filename)

let subst_uncountable_lexemes = ref StringSet.empty
let subst_uncountable_lexemes2 = ref StringSet.empty
(* let subst_container_lexemes = ref StringSet.empty *)
let subst_numeral_lexemes = ref StringSet.empty
(* let subst_time_lexemes = ref StringSet.empty *)

let subst_pronoun_lexemes = StringSet.of_list ["co"; "kto"; "cokolwiek"; "ktokolwiek"; "nic"; "nikt"; "coś"; "ktoś"; "to"]
let adj_pronoun_lexemes = StringSet.of_list ["czyj"; "jaki"; "który"; "jakiś"; "ten"; "taki"]
let compar_lexemes = StringSet.of_list ["jak"; "jako"; "niż"; "niczym"; "niby"; "co"; "zamiast"]

(* let adj_quant_lexemes = StringSet.of_list ["każdy"; "wszelki"; "wszystek"; "żaden"; "jakiś"; "pewien"; "niektóry"; "jedyny"; "sam"] *)

let load_adv_modes filename adv_modes =
  File.fold_tab filename adv_modes (fun adv_modes -> function
      [adv;mode] -> StringMap.add_inc adv_modes adv [mode] (fun l -> mode :: l)
    | _ -> failwith "load_adv_modes")

(*let load_num_nsems filename num_nsems =
  File.fold_tab filename num_nsems (fun num_nsems -> function
      lemma :: _ :: nsems :: _ ->
        Xlist.fold (Xstring.split "," nsems) num_nsems (fun num_nsems nsem ->
          StringMap.add_inc num_nsems lemma [nsem] (fun l -> nsem :: l))
    | _ -> failwith "load_num_nsems")*)

let adv_modes = ref (StringMap.empty : string list StringMap.t)
(* let num_nsems = ref (StringMap.empty : string list StringMap.t) *)

let initialize () =
  subst_uncountable_lexemes := File.catch_no_file (load_subst_data subst_uncountable_lexemes_filename) StringSet.empty;
  subst_uncountable_lexemes2 := File.catch_no_file (load_subst_data subst_uncountable_lexemes_filename2) StringSet.empty;
  (* subst_container_lexemes := File.catch_no_file (load_subst_data subst_container_lexemes_filename) StringSet.empty; *)
  subst_numeral_lexemes := File.catch_no_file (load_subst_data subst_numeral_lexemes_filename) StringSet.empty;
  (* subst_time_lexemes := File.catch_no_file (load_subst_data subst_time_lexemes_filename) StringSet.empty; *)
  adv_modes := File.catch_no_file (load_adv_modes adv_modes_filename) StringMap.empty;
(*  num_nsems := File.catch_no_file (load_num_nsems num_nsems_filename) StringMap.empty;*)
  ()

let noun_type proper lemma pos =
  let nsyn =
    if proper then "proper" else
    if pos = "ppron12" || pos = "ppron3" || pos = "siebie" then "pronoun" else
    if pos = "symbol" (*|| pos = "date" || pos = "date-interval" || pos = "hour" || pos = "hour-minute" || pos = "hour-interval" || pos = "hour-minute-interval" ||
       pos = "year" || pos = "year-interval" || pos = "day" || pos = "day-interval" || pos = "day-month" || pos = "day-month-interval" ||
       pos = "match-result" || pos = "month-interval" || pos = "initial" || pos = "roman" || pos = "roman-interval" || pos = "url" || pos = "email" || pos = "phone-number" || pos = "postal-code" || pos = "obj-id" || pos = "building-number" || pos = "date"*) then "proper" else
    if StringSet.mem subst_pronoun_lexemes lemma then "pronoun" else
    "common" in
  let nsem = ["count"; "mass"; "unique"] in
(*    if pos = "ppron12" || pos = "ppron3" || pos = "siebie" then ["count"] else
    (* if StringSet.mem !subst_time_lexemes lemma then ["time"] else *)
    let l = ["count"] in
    let l = if StringSet.mem !subst_uncountable_lexemes lemma || StringSet.mem !subst_uncountable_lexemes2 lemma then "mass" :: l else l in
    (*if StringSet.mem !subst_container_lexemes lemma then "measure" :: l else*) l in*)
  [nsyn],nsem

let adv_mode lemma =
  try
    StringMap.find !adv_modes lemma
  with Not_found -> ["mod"]

(*let num_nsem lemma =
  try
    StringMap.find !num_nsems lemma
  with Not_found -> (*try
    StringMap.find !num_nsems (String.lowercase lemma)
  with Not_found ->*) failwith ("num_nsem: " ^ lemma)*)


let part_set = StringSet.of_list ["się"; "nie"; "by"; "niech"; "niechaj"; "niechże"; "niechajże"; "czy"; "gdyby"]

(* let snode = SelectorMap.find selector_values SNode *)

let check_genders genders =
  let genders,pt,col = Xlist.fold genders ([],"npt",[]) (fun (genders,pt,col) -> function
      "n:col" -> "n" :: genders,pt,"col" :: col
    | "n:ncol" -> "n" :: genders,pt,"ncol" :: col
    | "n:pt" -> "n" :: genders,"pt",col
    | "m1:pt" -> "m1" :: genders,"pt",col
    | s -> s :: genders,pt,col) in
    genders,pt,(*if col = [] then ["col";"ncol"] else*) col


let clarify_categories proper cat coerced (lemma,pos,interp) =
  let cats = {empty_cats with lemma=lemma; pos=pos; pos2=Tagset.simplify_pos pos; cat=cat; coerced=coerced; (*snode=snode;*) phrase=all_phrases} in
  match lemma,pos,interp with
    lemma,"subst",[numbers;cases;genders] ->
      let numbers = expand_numbers numbers in
      let cases = expand_cases cases in
      let genders = expand_genders genders in
      let genders,pt,col = check_genders genders in
      let cases,voc = split_voc cases in
      let nsyn,nsem = noun_type proper lemma "subst" in
      (if cases = [] then [] else
         [{cats with numbers=numbers; cases=cases; genders=genders; persons=["ter"]; nsyn=nsyn; nsem=nsem; pt=pt; col=col}]) @
      (if voc = [] then [] else
         [{cats with numbers=numbers; cases=voc; genders=genders; persons=["sec"]; nsyn=nsyn; nsem=nsem; pt=pt; col=col}])
  | lemma,"subst",[numbers;cases;genders;col] ->
      let numbers = expand_numbers numbers in
      let cases = expand_cases cases in
      let genders = expand_genders genders in
      let genders,_,_ = check_genders genders in
      let cases,voc = split_voc cases in
      let nsyn,nsem = noun_type proper lemma "subst" in
      let pt,col = expand_col col in
      (if cases = [] then [] else
         [{cats with numbers=numbers; cases=cases; genders=genders; persons=["ter"]; nsyn=nsyn; nsem=nsem; pt=pt; col=col}]) @
      (if voc = [] then [] else
         [{cats with numbers=numbers; cases=voc; genders=genders; persons=["sec"]; nsyn=nsyn; nsem=nsem; pt=pt; col=col}])
  | lemma,"depr",[numbers;cases;genders] ->
      let numbers = expand_numbers numbers in
      let cases = expand_cases cases in
      let genders = expand_genders genders in
      let cases,voc = split_voc cases in
      let nsyn,nsem = noun_type proper lemma "depr" in
      (if cases = [] then [] else
         [{cats with pos="depr"; numbers=numbers; cases=cases; genders=genders; persons=["ter"]; nsyn=nsyn; nsem=nsem; pt="npt"; col=["col";"ncol"]}]) @
      (if voc = [] then [] else
         [{cats with pos="depr"; numbers=numbers; cases=voc; genders=genders; persons=["sec"]; nsyn=nsyn; nsem=nsem; pt="npt"; col=["col";"ncol"]}])
  | lemma,"ppron12",[numbers;cases;genders;persons] ->
      let numbers = expand_numbers numbers in
      let cases = expand_cases cases in
      let genders = expand_genders genders in
      [{cats with numbers=numbers; cases=cases; genders=genders; persons=persons}]
  | lemma,"ppron12",[numbers;cases;genders;persons;akcs] ->
      let numbers = expand_numbers numbers in
      let cases = expand_cases cases in
      let genders = expand_genders genders in
      [{cats with numbers=numbers; cases=cases; genders=genders; persons=persons}]
  | lemma,"ppron3",[numbers;cases;genders;persons] ->
      let numbers = expand_numbers numbers in
      let cases = expand_cases cases in
      let genders = expand_genders genders in
      [{cats with numbers=numbers; cases=cases; genders=genders; persons=persons; praeps=["praep-npraep"]}]
  | lemma,"ppron3",[numbers;cases;genders;persons;akcs] ->
      let numbers = expand_numbers numbers in
      let cases = expand_cases cases in
      let genders = expand_genders genders in
      [{cats with numbers=numbers; cases=cases; genders=genders; persons=persons; praeps=["praep-npraep"]}]
  | lemma,"ppron3",[numbers;cases;genders;persons;akcs;praep] ->
      let numbers = expand_numbers numbers in
      let cases = expand_cases cases in
      let genders = expand_genders genders in
      let praep = match praep with
        ["praep";"npraep"] -> ["praep-npraep"]
      | ["npraep";"praep"] -> ["praep-npraep"]
      | _ -> praep in
      [{cats with numbers=numbers; cases=cases; genders=genders; persons=persons; praeps=praep}]
  | lemma,"siebie",[cases] -> (* FIXME: czy tu określać numbers genders persons? *)
      let cases = expand_cases cases in
      [{cats with numbers=all_numbers; cases=cases; genders=all_genders; persons=["ter"]}]
  | lemma,"prep",[cases;_] | lemma,"prep",[cases] ->
      if StringSet.mem compar_lexemes lemma then
        [{cats with pos="x"; cases=["nom";"gen";"dat";"acc";"inst"]};
         {cats with pos="compar"; cases=["nom";"gen";"dat";"acc";"inst"]}] else
      let cases = expand_cases cases in
      [{cats with pos="x"; cases=cases;};{cats with cases=cases}]
  | lemma,"x",[] -> [{cats with pos="x"}]
  | lemma,"num",[numbers;cases;genders;acms] ->
      let numbers = expand_numbers numbers in
      let cases = expand_cases cases in
      let genders = expand_genders genders in
      let genders,pt,col = check_genders genders in
(*       let nsem = num_nsem lemma in *)
      [{cats with numbers=numbers; cases=cases; genders=genders; persons=["ter"]; acms=acms; (*nsem=nsem;*) pt=pt; col=col}]
  | lemma,"num",[numbers;cases;genders;acms;_] ->
      let numbers = expand_numbers numbers in
      let cases = expand_cases cases in
      let genders = expand_genders genders in
      let genders,pt,col = check_genders genders in
(*       let nsem = num_nsem lemma in *)
      [{cats with numbers=numbers; cases=cases; genders=genders; persons=["ter"]; acms=acms; (*nsem=nsem;*) pt=pt; col=col}]
  | lemma,"numcomp",[] -> [cats]
  | lemma,"symbol",[["intnum"]] ->
      let numbers,acms =
        if lemma = "1" || lemma = "-1" then ["sg"],["congr"] else
        let s = String.get lemma (String.length lemma - 1) in
        ["pl"],if s = '2' || s = '3' || s = '4' then ["rec";"congr"] else ["rec"] in
      [{cats with modes=["intnum"]};
       {cats with pos="num"; pos2=Tagset.simplify_pos "num"; numbers=numbers; cases=all_cases; genders=all_genders; persons=["ter"]; acms=acms; (*nsem=["count"]*)}]
  | lemma,"symbol",[[mode]] ->
      [{cats with modes=[mode]}]
(*  | lemma,"realnum",[] ->
      [{cats with numbers=["sg"]; cases=all_cases; genders=all_genders; persons=["ter"]; acms=["rec"]; nsem=["count"]}]
  | lemma,"intnum-interval",[] ->
      [{cats with numbers=["pl"]; cases=all_cases; genders=all_genders; persons=["ter"]; acms=["rec";"congr"]; nsem=["count"]}]
  | lemma,"realnum-interval",[] ->
      [{cats with numbers=["sg"]; cases=all_cases; genders=all_genders; persons=["ter"]; acms=["rec"]; nsem=["count"]}]
  | lemma,"symbol",[] ->
      [{cats with numbers=["sg"]; cases=all_cases; genders=all_genders; persons=["ter"]}]
  | lemma,"ordnum",[] ->
      [{cats with numbers=all_numbers; cases=all_cases; genders=all_genders; grads=["pos"]}] (* FIXME: czy dać możliwość więcej niż jednego stopnia *)
  | lemma,"date",[] ->
      let nsyn,nsem = noun_type proper lemma "date" in
      [{cats with nsyn=nsyn; nsem=nsem}]
  | lemma,"date-interval",[] ->
      let nsyn,nsem = noun_type proper lemma "date-interval" in
      [{cats with nsyn=nsyn; nsem=nsem}]
  | lemma,"hour-minute",[] ->
      let nsyn,nsem = noun_type proper lemma "hour-minute" in
      [{cats with nsyn=nsyn; nsem=nsem}]
  | lemma,"hour",[] ->
      let nsyn,nsem = noun_type proper lemma "hour" in
      [{cats with nsyn=nsyn; nsem=nsem}]
  | lemma,"hour-minute-interval",[] ->
      let nsyn,nsem = noun_type proper lemma "hour-minute-interval" in
      [{cats with nsyn=nsyn; nsem=nsem}]
  | lemma,"hour-interval",[] ->
      let nsyn,nsem = noun_type proper lemma "hour-interval" in
      [{cats with nsyn=nsyn; nsem=nsem}]
  | lemma,"year",[] ->
      let nsyn,nsem = noun_type proper lemma "year" in
      [{cats with nsyn=nsyn; nsem=nsem}]
  | lemma,"year-interval",[] ->
      let nsyn,nsem = noun_type proper lemma "year-interval" in
      [{cats with nsyn=nsyn; nsem=nsem}]
  | lemma,"day",[] ->
      let nsyn,nsem = noun_type proper lemma "day" in
      [{cats with nsyn=nsyn; nsem=nsem}]
  | lemma,"day-interval",[] ->
      let nsyn,nsem = noun_type proper lemma "day-interval" in
      [{cats with nsyn=nsyn; nsem=nsem}]
  | lemma,"day-month",[] ->
      let nsyn,nsem = noun_type proper lemma "day-month" in
      [{cats with nsyn=nsyn; nsem=nsem}]
  | lemma,"day-month-interval",[] ->
      let nsyn,nsem = noun_type proper lemma "day-month-interval" in
      [{cats with nsyn=nsyn; nsem=nsem}]
  | lemma,"month-interval",[] ->
      let nsyn,nsem = noun_type proper lemma "month-interval" in
      [{cats with nsyn=nsyn; nsem=nsem}]
  | lemma,"initial",[] ->
      let nsyn,nsem = noun_type proper lemma "initial" in
      [{cats with nsyn=nsyn; nsem=nsem}]
  | lemma,"roman",[] ->
      let nsyn,nsem = noun_type proper lemma "roman" in
      [{cats with pos="roman-ordnum"; numbers=all_numbers; cases=all_cases; genders=all_genders; grads=["pos"]};
       {cats with nsyn=nsyn; nsem=nsem}]
  | lemma,"roman-interval",[] ->
      let nsyn,nsem = noun_type proper lemma "roman-interval" in
      [{cats with nsyn=nsyn; nsem=nsem}]
  | lemma,"match-result",[] ->
      let nsyn,nsem = noun_type proper lemma "match-result" in
      [{cats with nsyn=nsyn; nsem=nsem}]
  | lemma,"url",[] ->
      let nsyn,nsem = noun_type proper lemma "url" in
      [{cats with nsyn=nsyn; nsem=nsem}]
  | lemma,"email",[] ->
      let nsyn,nsem = noun_type proper lemma "email" in
      [{cats with nsyn=nsyn; nsem=nsem}]
  | lemma,"phone-number",[] ->
      let nsyn,nsem = noun_type proper lemma "phone-number" in
      [{cats with nsyn=nsyn; nsem=nsem}]
  | lemma,"postal-code",[] ->
      let nsyn,nsem = noun_type proper lemma "postal-code" in
      [{cats with nsyn=nsyn; nsem=nsem}]
  | lemma,"obj-id",[] ->
      let nsyn,nsem = noun_type proper lemma "obj-id" in
      [{cats with nsyn=nsyn; nsem=nsem}]
  | lemma,"building-number",[] ->
      let nsyn,nsem = noun_type proper lemma "building-number" in
      [{cats with nsyn=nsyn; nsem=nsem}]*)
  | lemma,"fixed",[] -> [cats]
  | lemma,"adj",[numbers;cases;genders;grads] -> (* FIXME: adjsyn *)
      let numbers = expand_numbers numbers in
      let cases = expand_cases cases in
      let cases = if Xlist.mem cases "nom" then "pred" :: cases else cases in
      let genders = expand_genders genders in
      let pos,pos2 = (*if StringSet.mem adj_pronoun_lexemes lemma then "apron","adj" else*) "adj","adj" in
      [{cats with pos=pos; pos2=pos2; numbers=numbers; cases=cases; genders=genders; grads=grads}] (* FIXME: czy dać możliwość więcej niż jednego stopnia *)
  | lemma,"adjc",[] ->
      [{cats with numbers=["sg"]; cases=["pred"]; genders=["m1";"m2";"m3"]; grads=["pos"]}]
  | lemma,"adjp",[] ->
      [{cats with numbers=all_numbers; cases=["postp"]; genders=all_genders; grads=["pos"]}]
  | lemma,"adja",[] -> [cats]
  | lemma,"ordnum",[numbers;cases;genders] ->
      let numbers = expand_numbers numbers in
      let cases = expand_cases cases in
      let cases = if Xlist.mem cases "nom" then "pred" :: cases else cases in
      let genders = expand_genders genders in
      [{cats with numbers=numbers; cases=cases; genders=genders}]
  | lemma,"adv",[grads] -> [{cats with grads=grads; modes=adv_mode lemma}]
  | lemma,"adv",[] -> [{cats with grads=["pos"]; modes=adv_mode lemma}]
  | lemma,"ger",[numbers;cases;genders;aspects;negations] ->
      let numbers = expand_numbers numbers in
      let cases = expand_cases cases in
      let genders = expand_genders genders in
      [{cats with numbers=numbers; cases=cases; genders=genders; persons=["ter"]; aspects=aspects; negations=negations}] (* FIXME: kwestia osoby przy voc *)
  | lemma,"pact",[numbers;cases;genders;aspects;negations] ->
      let numbers = expand_numbers numbers in
      let cases = expand_cases cases in
      let cases = if Xlist.mem cases "nom" then "pred" :: cases else cases in
      let genders = expand_genders genders in
      [{cats with numbers=numbers; cases=cases; genders=genders; aspects=aspects; negations=negations}]
  | lemma,"ppas",[numbers;cases;genders;aspects;negations] ->
      let numbers = expand_numbers numbers in
      let cases = expand_cases cases in
      let cases = if Xlist.mem cases "nom" then "pred" :: cases else cases in
      let genders = expand_genders genders in
      [{cats with numbers=numbers; cases=cases; genders=genders; aspects=aspects; negations=negations}]
  | lemma,"fin",[numbers;persons;aspects] ->  (* FIXME: genders bez przymnogich *)
      let numbers = expand_numbers numbers in
      let persons2 = Xlist.fold persons [] (fun l -> function "sec" -> l | s -> s :: l) in
      let cats2 = {cats with numbers=numbers; genders=all_genders; persons=persons; negations=["aff"; "neg"]; moods=["indicative"]} in
      (Xlist.map aspects (function
            "imperf" -> {cats2 with aspects=["imperf"]; tenses=["pres"]}
          | "perf" -> {cats2 with aspects=["perf"]; tenses=["fut"]}
          | _ -> failwith "clarify_categories")) @
      (if persons2 = [] then [] else
        [{cats with numbers=numbers; genders=all_genders; persons=persons; aspects=aspects; negations=["aff"; "neg"]; moods=["imperative"]; tenses=["fut"]}])
  | lemma,"bedzie",[numbers;persons;aspects] ->
      let numbers = expand_numbers numbers in
      let persons2 = Xlist.fold persons [] (fun l -> function "sec" -> l | s -> s :: l) in
      [{cats with numbers=numbers; genders=all_genders; persons=persons; aspects=aspects; negations=["aff"; "neg"]; moods=["indicative"]; tenses=["fut"]}] @
      (if persons2 = [] then [] else
        [{cats with numbers=numbers; genders=all_genders; persons=persons; aspects=aspects; negations=["aff"; "neg"]; moods=["imperative"]; tenses=["fut"]}])
  | lemma,"praet",[numbers;genders;aspects;nagl] ->
      let numbers = expand_numbers numbers in
      let genders = expand_genders genders in
      [{cats with numbers=numbers; genders=genders; persons=all_persons; aspects=aspects; negations=["aff"; "neg"]; moods=["indicative";"conditional"]; tenses=["past"]}] @
      (if Xlist.mem aspects "imperf" then
        [{cats with numbers=numbers; genders=genders; persons=all_persons; aspects=["imperf"]; negations=["aff"; "neg"]; moods=["indicative"]; tenses=["fut"]}]
       else [])
  | lemma,"praet",[numbers;genders;aspects] ->
      let numbers = expand_numbers numbers in
      let genders = expand_genders genders in
      [{cats with numbers=numbers; genders=genders; persons=all_persons; aspects=aspects; negations=["aff"; "neg"]; moods=["indicative";"conditional"]; tenses=["past"]}] @
      (if Xlist.mem aspects "imperf" then
        [{cats with numbers=numbers; genders=genders; persons=all_persons; aspects=["imperf"]; negations=["aff"; "neg"]; moods=["indicative"]; tenses=["fut"]}]
       else [])
  | lemma,"winien",[numbers;genders;aspects] ->
      let numbers = expand_numbers numbers in
      let genders = expand_genders genders in
      [{cats with numbers=numbers; genders=genders; persons=all_persons; aspects=aspects; negations=["aff"; "neg"]; moods=["indicative";"conditional"]; tenses=["pres"]};
       {cats with numbers=numbers; genders=genders; persons=all_persons; aspects=aspects; negations=["aff"; "neg"]; moods=["indicative"]; tenses=["past"]}] @
      (if Xlist.mem aspects "imperf" then
        [{cats with numbers=numbers; genders=genders; persons=all_persons; aspects=["imperf"]; negations=["aff"; "neg"]; moods=["indicative"]; tenses=["fut"]}]
       else [])
  | lemma,"impt",[numbers;persons;aspects] ->
      let numbers = expand_numbers numbers in
      [{cats with numbers=numbers; genders=all_genders; persons=persons; aspects=aspects; negations=["aff"; "neg"]; moods=["imperative"]; tenses=["fut"]}]
  | lemma,"imps",[aspects] ->
      [{cats with numbers=all_numbers; genders=all_genders; persons=all_persons; aspects=aspects; negations=["aff"; "neg"]; moods=["indicative"]; tenses=["past"]}]
  | lemma,"pred",[] -> (* FIXME: czy predykatyw zawsze jest niedokonany? *)
      [{cats with numbers=["sg"]; genders=[(*"n2"*)"n"]; persons=["ter"]; aspects=["imperf"]; negations=["aff"; "neg"]; moods=["indicative"]; tenses=["pres";"past";"fut"]}]
  | lemma,"aglt",[numbers;persons;aspects;wok] ->
      let numbers = expand_numbers numbers in
      [{cats with numbers=numbers; persons=persons; aspects=aspects}]
  | lemma,"inf",[aspects] -> [{cats with aspects=aspects; negations=["aff"; "neg"]}]
  | lemma,"pcon",[aspects] -> [{cats with aspects=aspects; negations=["aff"; "neg"]}]
  | lemma,"pant",[aspects] -> [{cats with aspects=aspects; negations=["aff"; "neg"]}]
  | lemma,"pacta",[] -> [cats]
  | lemma,"qub",[] ->
      if StringSet.mem part_set lemma then [{cats with pos="part"}]
      else [cats]
  | lemma,"comp",[] -> [cats]
  | lemma,"conj",[] -> (*print_endline ("clarify_categories: " ^ lemma);*) [cats]
  | lemma,"interj",[] -> [cats]
  | lemma,"sinterj",[] -> [cats]
  | lemma,"burk",[] -> [cats]
(*   | ",","interp",[] -> [{cats with pos="conj"}] *)
  | lemma,"interp",[] -> [cats]
  | lemma,"unk",[] ->
      [{cats with numbers=all_numbers; cases=all_cases; genders=all_genders; persons=["ter"]}]
  | lemma,"xxx",[] ->
      [{cats with numbers=all_numbers; cases=all_cases; genders=all_genders; persons=["ter"]}]
(*  | lemma,"html-tag",[] -> [cats]
  | lemma,"list-item",[] -> [cats]*)
  | lemma,c,l -> failwith ("clarify_categories: " ^ lemma ^ " + " ^ c ^ " " ^ (String.concat " + " (Xlist.map l (String.concat "."))))

(* FIXME: przenieść gdzieś indziej *)
(* let assign token =
  match token.SubsyntaxTypes.token with
    SubsyntaxTypes.Lemma(lemma,pos,interp) -> List.flatten (Xlist.map interp (fun interp -> clarify_categories false (lemma,pos,interp)))
  | SubsyntaxTypes.Proper(lemma,pos,interp,_) -> List.flatten (Xlist.map interp (fun interp -> clarify_categories true (lemma,pos,interp)))
  | SubsyntaxTypes.Interp lemma -> clarify_categories false (lemma,"interp",[])
  | _ -> [] *)

let selector_names = StringSet.of_list [
    "lemma";"pos";"pos2";"cat";"coerced";"role";"irole";"prole";"nrole";"node";"inode";"pnode";"nnode";"phrase";"number";"case";"gender";"person";"grad";
    "praep";"acm";"aspect";"negation";"mood";"tense";"nsyn";"nsem";"ctype";"mode";(*"psem";*)"pt";"col";
    "icat";"inumber";"igender";"iperson";"nperson";"ncat";"plemma";"pcat";
    "unumber";"ucase";"ugender";"uperson";"amode"]


let string_of_selector = function
    Lemma -> "lemma"
  | IncludeLemmata -> "include-lemmata"
  | ProLemma -> "pro-lemma"
  (* | NewLemma -> "newlemma" *)
  | Pos -> "pos"
  | Pos2 -> "pos2"
  | Cat -> "cat"
  | Coerced -> "coerced"
  | Role -> "role"
  | Irole -> "irole"
  | Prole -> "prole"
  | Nrole -> "nrole"
(*  | SNode -> "node"
  | Inode -> "inode"
  | Pnode -> "pnode"
  | Nnode -> "nnode"*)
  | Phrase -> "phrase"
  | Number -> "number"
  | Case -> "case"
  | Gender -> "gender"
  | Person -> "person"
  | Grad -> "grad"
  | Praep -> "praep"
  | Acm -> "acm"
  | Aspect -> "aspect"
  | Negation -> "negation"
  | Mood -> "mood"
  | Tense -> "tense"
  | Nsyn -> "nsyn"
  | Nsem -> "nsem"
  | Pt -> "pt"
  | Col -> "col"
  | Ctype -> "ctype"
  | Mode -> "mode"
  (* | Psem -> "psem" *)
  | Icat -> "icat"
  | Inumber -> "inumber"
  | Igender -> "igender"
  | Iperson -> "iperson"
  | Nperson -> "nperson"
  | Ncat -> "ncat"
  | Plemma -> "plemma"
  | Pcat -> "pcat"
  | Unumber -> "unumber"
  | Ucase -> "ucase"
  | Ugender -> "ugender"
  | Uperson -> "uperson"
  | Amode -> "amode"

let string_of_selectors selectors =
  String.concat ", " (Xlist.map selectors (fun c ->
      let rel = if c.rel = Eq then "=" else "!=" in
      string_of_selector c.sel ^ rel ^ (String.concat "|" c.values)))

let selector_of_string = function
    "lemma" -> Lemma
  | "include-lemmata" -> IncludeLemmata
  | "pro-lemma" -> ProLemma
  (* | NewLemma -> "newlemma" *)
  | "pos" -> Pos
  | "pos2" -> Pos2
  | "cat" -> Cat
  | "coerced" -> Coerced
  | "role" -> Role
  | "irole" -> Irole
  | "prole" -> Prole
  | "nrole" -> Nrole
(*  | "node" -> SNode
  | "inode" -> Inode
  | "pnode" -> Pnode
  | "nnode" -> Nnode*)
  | "phrase" -> Phrase
  | "number" -> Number
  | "case" -> Case
  | "gender" -> Gender
  | "person" -> Person
  | "grad" -> Grad
  | "praep" -> Praep
  | "acm" -> Acm
  | "aspect" -> Aspect
  | "negation" -> Negation
  | "mood" -> Mood
  | "tense" -> Tense
  | "nsyn" -> Nsyn
  | "nsem" -> Nsem
  | "pt" -> Pt
  | "col" -> Col
  | "ctype" -> Ctype
  | "mode" -> Mode
  (* | "psem" -> Psem *)
  | "icat" -> Icat
  | "inumber" -> Inumber
  | "igender" -> Igender
  | "iperson" -> Iperson
  | "nperson" -> Nperson
  | "ncat" -> Ncat
  | "plemma" -> Plemma
  | "pcat" -> Pcat
  | "unumber" -> Unumber
  | "ucase" -> Ucase
  | "ugender" -> Ugender
  | "uperson" -> Uperson
  | "amode" -> Amode
  | s -> failwith ("selector_of_string: " ^ s)

let match_selector cats = function
    Lemma -> [cats.lemma]
(* | NewLemma -> [] *)
  | Pos -> [cats.pos]
  | Cat -> [cats.cat]
  | Coerced -> cats.coerced
  | Role -> cats.roles
(*   | SNode -> cats.snode *)
  | Phrase -> cats.phrase
  | Number -> cats.numbers
  | Case -> cats.cases
  | Gender -> cats.genders
  | Person -> cats.persons
  | Grad -> cats.grads
  | Praep -> cats.praeps
  | Acm -> cats.acms
  | Aspect -> cats.aspects
  | Negation -> cats.negations
  | Mood -> cats.moods
  | Tense -> cats.tenses
  | Nsyn -> cats.nsyn
  | Nsem -> cats.nsem
  | Mode -> cats.modes
  (* | Psem -> cats.psem *)
  | Pt -> [cats.pt]
  | Col -> cats.col
  | c -> failwith ("match_selector: " ^ string_of_selector c)

let set_selector cats vals = function
    Number -> {cats with numbers=vals}
  | Case -> {cats with cases=vals}
  | Gender -> {cats with genders=vals}
  | Person -> {cats with persons=vals}
  | Grad -> {cats with grads=vals}
  | Praep -> {cats with praeps=vals}
  | Acm -> {cats with acms=vals}
  | Aspect -> {cats with aspects=vals}
  | Negation -> {cats with negations=vals}
  | Mood -> {cats with moods=vals}
  | Tense -> {cats with tenses=vals}
  | Nsyn -> {cats with nsyn=vals}
  | Nsem -> {cats with nsem=vals}
  | Mode -> {cats with modes=vals}
  (* | Psem -> {cats with psem=vals} *)
  | Pt -> (match vals with [v] -> {cats with pt=v} | _ -> failwith "set_selector: Pt")
  | Col -> {cats with col=vals}
  | Lemma -> (match vals with [v] -> {cats with lemma=v} | _ -> failwith "set_selector: Lemma")
  | Pos -> (match vals with [v] -> {cats with pos=v} | _ -> failwith "set_selector: Pos")
  | Cat -> (match vals with [v] -> {cats with cat=v} | _ -> failwith "set_selector: Cat")
  | Coerced -> {cats with coerced=vals}
  | Role -> {cats with roles=vals}
(*   | SNode -> {cats with snode=vals} *)
  | Phrase -> {cats with phrase=vals}
  | c -> failwith ("set_selector: " ^ string_of_selector c)

let rec apply_selectors cats = function
    [] -> cats
  | {sel=sel;rel=Eq;values=vals} :: l ->
(*     print_endline "apply_selectors Eq"; *)
    if match_selector cats sel = [] then apply_selectors (set_selector cats vals sel) l else
    let vals = StringSet.intersection (StringSet.of_list (match_selector cats sel)) (StringSet.of_list vals) in
    if StringSet.is_empty vals then ((*print_endline (string_of_selector sel);*) raise Not_found) else
      apply_selectors (set_selector cats (StringSet.to_list vals) sel) l
  | {sel=sel;rel=Neq;values=vals} :: l ->
(*     print_endline "apply_selectors Neq"; *)
    let vals = StringSet.difference (StringSet.of_list (match_selector cats sel)) (StringSet.of_list vals) in
    if StringSet.is_empty vals then ((*print_endline (string_of_selector sel);*) raise Not_found) else
      apply_selectors (set_selector cats (StringSet.to_list vals) sel) l

let pos_categories = Xlist.fold [
    "subst",            [Lemma;Cat;Coerced;Role;(*SNode;*)Phrase;Number;Case;Gender;Person;Nsyn;Nsem;Pt;Col;];
    "depr",             [Lemma;Cat;Coerced;Role;(*SNode;*)Phrase;Number;Case;Gender;Person;Nsyn;Nsem;Pt;Col;];
    "ppron12",          [Lemma;Cat;Role;(*SNode;*)Phrase;Number;Case;Gender;Person;];
    "ppron3",           [Lemma;Cat;Role;(*SNode;*)Phrase;Number;Case;Gender;Person;Praep;];
    "siebie",           [Lemma;Cat;Role;(*SNode;*)Phrase;Number;Case;Gender;Person;];
    "prep",             [Lemma;Cat;Coerced;Role;(*SNode;*)Phrase;Case;];
    "compar",           [Lemma;Cat;Coerced;Role;(*SNode;*)Phrase;Case;];
    "x",                [Lemma;Cat;Coerced;Role;(*SNode;*)Phrase;Case;];
    "num",              [Lemma;Cat;Coerced;Role;(*SNode;*)Phrase;Number;Case;Gender;Person;Acm;(*Nsem;*)];
    "numcomp",          [Lemma;Cat;Role;(*SNode;*)Phrase];
(*    "intnum",           [Lemma;Cat;Role;(*SNode;*)Phrase;Number;Case;Gender;Person;Acm;Nsem;];
    "realnum",          [Lemma;Cat;Role;(*SNode;*)Phrase;Number;Case;Gender;Person;Acm;Nsem;];
    "intnum-interval",  [Lemma;Cat;Role;(*SNode;*)Phrase;Number;Case;Gender;Person;Acm;Nsem;];
    "realnum-interval", [Lemma;Cat;Role;(*SNode;*)Phrase;Number;Case;Gender;Person;Acm;Nsem;];*)
    "symbol",           [Lemma;Cat;Coerced;Role;(*SNode;*)Phrase;Mode;];
(*    "ordnum",           [Lemma;Cat;Coerced;Role;(*SNode;*)Phrase;Number;Case;Gender;Grad;];
    "date",             [Lemma;Cat;Coerced;Role;(*SNode;*)Phrase;Nsyn;Nsem;];
    "date-interval",    [Lemma;Cat;Coerced;Role;(*SNode;*)Phrase;Nsyn;Nsem;];
    "hour-minute",      [Lemma;Cat;Coerced;Role;(*SNode;*)Phrase;Nsyn;Nsem;];
    "hour",             [Lemma;Cat;Coerced;Role;(*SNode;*)Phrase;Nsyn;Nsem;];
    "hour-minute-interval",[Lemma;Cat;Coerced;Role;(*SNode;*)Phrase;Nsyn;Nsem;];
    "hour-interval",    [Lemma;Cat;Coerced;Role;(*SNode;*)Phrase;Nsyn;Nsem;];
    "year",             [Lemma;Cat;Coerced;Role;(*SNode;*)Phrase;Nsyn;Nsem;];
    "year-interval",    [Lemma;Cat;Coerced;Role;(*SNode;*)Phrase;Nsyn;Nsem;];
    "day",              [Lemma;Cat;Coerced;Role;(*SNode;*)Phrase;Nsyn;Nsem;];
    "day-interval",     [Lemma;Cat;Coerced;Role;(*SNode;*)Phrase;Nsyn;Nsem;];
    "day-month",        [Lemma;Cat;Coerced;Role;(*SNode;*)Phrase;Nsyn;Nsem;];
    "day-month-interval",[Lemma;Cat;Coerced;Role;(*SNode;*)Phrase;Nsyn;Nsem;];
    "month-interval",   [Lemma;Cat;Coerced;Role;(*SNode;*)Phrase;Nsyn;Nsem;];
    "initial",             [Lemma;Cat;Coerced;Role;(*SNode;*)Phrase;Nsyn;Nsem;];
    "roman-ordnum",     [Lemma;Cat;Role;(*SNode;*)Phrase;Number;Case;Gender;Grad;];
    "roman",            [Lemma;Cat;Role;(*SNode;*)Phrase;Nsyn;Nsem;];
    "roman-interval",   [Lemma;Cat;Role;(*SNode;*)Phrase;Nsyn;Nsem;];
    "match-result",     [Lemma;Cat;Coerced;Role;(*SNode;*)Phrase;Nsyn;Nsem;];
    "url",              [Lemma;Cat;Coerced;Role;(*SNode;*)Phrase;Nsyn;Nsem;];
    "email",            [Lemma;Cat;Coerced;Role;(*SNode;*)Phrase;Nsyn;Nsem;];
    "phone-number",     [Lemma;Cat;Coerced;Role;(*SNode;*)Phrase;Nsyn;Nsem;];
    "postal-code",      [Lemma;Cat;Coerced;Role;(*SNode;*)Phrase;Nsyn;Nsem;];
    "obj-id",           [Lemma;Cat;Coerced;Role;(*SNode;*)Phrase;Nsyn;Nsem;];
    "building-number",  [Lemma;Cat;Coerced;Role;(*SNode;*)Phrase;Nsyn;Nsem;];*)
    "fixed",  [Lemma;Cat;Coerced;Role;(*SNode;*)Phrase;];
    "adj",    [Lemma;Cat;Coerced;Role;(*SNode;*)Phrase;Number;Case;Gender;Grad;];
    "adjc",   [Lemma;Cat;Coerced;Role;(*SNode;*)Phrase;Number;Case;Gender;Grad;];
    "adjp",   [Lemma;Cat;Coerced;Role;(*SNode;*)Phrase;Number;Case;Gender;Grad;];
    "ordnum", [Lemma;Cat;Coerced;Role;(*SNode;*)Phrase;Number;Case;Gender;];
(*     "apron",  [Lemma;Cat;Role;(*SNode;*)Phrase;Number;Case;Gender;Grad;]; *)
    "adja",   [Lemma;Cat;Coerced;Role;(*SNode;*)Phrase;];
    "adv",    [Lemma;Cat;Coerced;Role;(*SNode;*)Phrase;Grad;Mode];(* ctype *)
    "ger",    [Lemma;(*NewLemma;*)Cat;Coerced;Role;(*SNode;*)Phrase;Number;Case;Gender;Person;Aspect;Negation;];
    "pact",   [Lemma;(*NewLemma;*)Cat;Coerced;Role;(*SNode;*)Phrase;Number;Case;Gender;Aspect;Negation;];
    "ppas",   [Lemma;(*NewLemma;*)Cat;Coerced;Role;(*SNode;*)Phrase;Number;Case;Gender;Aspect;Negation;];
    "fin",    [Lemma;(*NewLemma;*)Cat;Coerced;Role;(*SNode;*)Phrase;Number;Gender;Person;Aspect;Negation;Mood;Tense;];
    "bedzie", [Lemma;(*NewLemma;*)Cat;Coerced;Role;(*SNode;*)Phrase;Number;Gender;Person;Aspect;Negation;Mood;Tense;];
    "praet",  [Lemma;(*NewLemma;*)Cat;Coerced;Role;(*SNode;*)Phrase;Number;Gender;Person;Aspect;Negation;Mood;Tense;];
    "winien", [Lemma;(*NewLemma;*)Cat;Coerced;Role;(*SNode;*)Phrase;Number;Gender;Person;Aspect;Negation;Mood;Tense;];
    "impt",   [Lemma;(*NewLemma;*)Cat;Coerced;Role;(*SNode;*)Phrase;Number;Gender;Person;Aspect;Negation;Mood;Tense;];
    "imps",   [Lemma;(*NewLemma;*)Cat;Coerced;Role;(*SNode;*)Phrase;Number;Gender;Person;Aspect;Negation;Mood;Tense;];
    "pred",   [Lemma;(*NewLemma;*)Cat;Coerced;Role;(*SNode;*)Phrase;Number;Gender;Person;Aspect;Negation;Mood;Tense;];
    "inf",    [Lemma;(*NewLemma;*)Cat;Coerced;Role;(*SNode;*)Phrase;Aspect;Negation;];
    "pcon",   [Lemma;(*NewLemma;*)Cat;Coerced;Role;(*SNode;*)Phrase;Aspect;Negation;];
    "pant",   [Lemma;(*NewLemma;*)Cat;Coerced;Role;(*SNode;*)Phrase;Aspect;Negation;];
    "pacta",  [Lemma;(*NewLemma;*)Cat;Coerced;Role;(*SNode;*)Phrase;];
    "aglt",   [Lemma;(*SNode;*)Phrase;Number;Person;Aspect;];
    "qub",    [Lemma;Cat;Role;(*SNode;*)Phrase;];
    "part",   [Lemma;Cat;Role;(*SNode;*)Phrase];
    "comp",   [Lemma;Cat;Role;(*SNode;*)Phrase;];(* ctype *)
    "conj",   [Lemma;(*SNode;*)Phrase;];(* ctype *)
    "interj", [Lemma;Cat;Coerced;Role;(*SNode;*)Phrase;];
    "sinterj",[Lemma;Cat;Coerced;Role;(*SNode;*)Phrase;];
    "burk",   [Lemma;(*SNode;*)Phrase;];
    "interp", [Lemma;Cat;(*SNode;*)Phrase;];
    "unk",    [Lemma;Cat;Role;(*SNode;*)Phrase;Number;Case;Gender;Person;];
    "xxx",    [Lemma;Cat;Role;(*SNode;*)Phrase;Number;Case;Gender;Person;];
    "pro",    [Lemma;Cat;Coerced;Role;(*SNode;*)Phrase;];
(*    "html-tag",[Lemma;(*SNode;*)Phrase;];
    "list-item",[Lemma;(*SNode;*)Phrase;];*)
  ] StringMap.empty (fun map (k,l) -> StringMap.add map k l)

let string_of_cats cats =
  String.concat ", " (SelectorMap.fold selector_values [] (fun l sel _ ->
    try
      let s = String.concat "|" (match_selector cats sel) in
      if s = "" then l else
      (string_of_selector sel ^ "=" ^ s) :: l
    with _ -> l))

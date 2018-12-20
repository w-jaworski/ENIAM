(*
 *  ENIAMsubsyntax: MWE, abbreviation and sentence detecion for Polish
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

open SubsyntaxTypes
open TokenizerTypes
open Xstd

let load_lemma_frequencies filename map =
  let l = Str.split_delim (Str.regexp "\n") (File.load_file filename) in
  Xlist.fold l map (fun map line ->
    if String.length line = 0 then map else
    if String.get line 0 = '#' then map else
    match Str.split_delim (Str.regexp "\t") line with
      [count; lemma; cat] -> StringMap.add map (lemma ^ "\t" ^ cat) (log10 (float_of_string count +. 1.))
    | _ -> failwith ("load_lemma_frequencies: " ^ line))

let lemma_frequencies = ref (StringMap.empty : float StringMap.t)

let modify_weights paths =
  List.rev (Xlist.fold paths [] (fun paths t ->
    let w = Xlist.fold t.attrs t.weight (fun w -> function
        TokNotFound -> w -. 25.
      | LemmNotVal -> w -. 20.
      | NotValProper -> w -. 1.
      | LemmLowercase -> w -. 0.1
      | _ -> w) in
    let freq = match t.token with
        Lemma(lemma,cat,_,_) -> (try StringMap.find !lemma_frequencies (lemma ^ "\t" ^ cat) with Not_found -> w)
(*       | Proper(lemma,cat,_,_) -> (try StringMap.find !lemma_frequencies (lemma ^ "\t" ^ cat) with Not_found -> w) *)
      | _ -> w in
    {t with weight = w +. freq; lemma_frequency=freq} :: paths))

let translate_digs paths = (* FIXME: brakuje initial, postal-code i być może innych *)
  Xlist.map paths (fun t ->
(*     if not !recognize_proper_names then *)
    match t.token with (* FIXME !!! *)
(*       Ideogram(lemma,mode) -> {t with token=Lemma(lemma,"symbol",[[mode]])} *)
(*      Ideogram(lemma,"dig") -> t
    | Ideogram(lemma,"intnum") -> {t with token=Lemma(lemma,"intnum",[[]]); cat="Number"}
    | Ideogram(lemma,"realnum") -> {t with token=Lemma(lemma,"realnum",[[]]); cat="Number"}
    | Ideogram(lemma,"year") -> {t with token=Lemma(lemma,"year",[[]]); cat="YearNumber"}
    | Ideogram(lemma,"month") -> t (*{t with token=Lemma(lemma,"month",[[]])}*)
    | Ideogram(lemma,"hour") -> {t with token=Lemma(lemma,"hour",[[]]); cat="HourNumber"}
    | Ideogram(lemma,"day") -> {t with token=Lemma(lemma,"day",[[]]); cat="DayNumber"}
    | Ideogram(lemma,"minute") -> t (*{t with token=Lemma(lemma,"minute",[[])}*)
    | Ideogram(lemma,"2dig") -> t
    | Ideogram(lemma,"3dig") -> t
    | Ideogram(lemma,"4dig") -> t
    | Ideogram(lemma,"pref3dig") -> t
    | Ideogram(lemma,"roman") -> {t with token=Lemma(lemma,"roman",[[]]); cat="OrdNumber"}
    | Ideogram(lemma,"roman-month") -> t (*{t with token=Lemma(lemma,"symbol",[[]]); attrs="roman" :: t.attrs}*)
    | Ideogram(lemma,"ordnum") -> {t with token=Lemma(lemma,"ordnum",[[]])}
    | Compound("date",[Dig(d,"day");Dig(m,"month");Dig(y,"year")]) -> {t with token=Lemma(d ^ "." ^ m ^ "." ^ y,"date",[[]]); cat="DayNumber"}
    | Compound("date",[Dig(d,"day");RomanDig(m,"month");Dig(y,"year")]) -> {t with token=Lemma(d ^ "." ^ m ^ "." ^ y,"date",[[]]); cat="DayNumber"}
    | Compound("date",[Dig(d,"day");Dig(m,"month");Dig(y,"2dig")]) -> {t with token=Lemma(d ^ "." ^ m ^ "." ^ y,"date",[[]]); cat="DayNumber"}
    | Compound("date",[Dig(d,"day");RomanDig(m,"month");Dig(y,"2dig")]) -> {t with token=Lemma(d ^ "." ^ m ^ "." ^ y,"date",[[]]); cat="DayNumber"}
    | Compound("day-month",[Dig(d,"day");Dig(m,"month")]) -> {t with token=Lemma(d ^ "." ^ m,"day-month",[[]]); cat="DayNumber"}
    | Compound("hour-minute",[Dig(h,"hour");Dig(m,"minute")]) -> {t with token=Lemma(h ^ ":" ^ m,"hour-minute",[[]]); cat="HourNumber"}
    | Compound("match-result",[Dig(x,"intnum");Dig(y,"intnum")]) -> {t with token=Lemma(x ^ ":" ^ y,"match-result",[[]]); cat="X"}
    | Compound("intnum-interval",[Dig(x,"intnum");Dig(y,"intnum")]) -> {t with token=Lemma(x ^ "-" ^ y,"intnum-interval",[[]]); cat="Number"}
    | Compound("roman-interval",[RomanDig(x,"roman");RomanDig(y,"roman")]) -> {t with token=Lemma(x ^ "-" ^ y,"roman-interval",[[]]); cat="OrdNumber"}
    | Compound("realnum-interval",[Dig(x,"realnum");Dig(y,"realnum")]) -> {t with token=Lemma(x ^ "-" ^ y,"realnum-interval",[[]]); cat="Number"}
    | Compound("realnum-interval",[Dig(x,"intnum");Dig(y,"realnum")]) -> {t with token=Lemma(x ^ "-" ^ y,"realnum-interval",[[]]); cat="Number"}
    | Compound("realnum-interval",[Dig(x,"realnum");Dig(y,"intnum")]) -> {t with token=Lemma(x ^ "-" ^ y,"realnum-interval",[[]]); cat="Number"}
    | Compound("date-interval",[Compound("date",[Dig(d1,"day");Dig(m1,"month");Dig(y1,"year")]);
        Compound("date",[Dig(d2,"day");Dig(m2,"month");Dig(y2,"year")])]) -> {t with token=Lemma(d1 ^ "." ^ m1 ^ "." ^ y1 ^ "-" ^ d2 ^ "." ^ m2 ^ "." ^ y2,"date-interval",[[]]); cat="DayNumber"}
    | Compound("date-interval",[Compound("date",[Dig(d1,"day");Dig(m1,"month");Dig(y1,"year")]);
        Compound("date",[Dig(d2,"day");Dig(m2,"month");Dig(y2,"2dig")])]) -> {t with token=Lemma(d1 ^ "." ^ m1 ^ "." ^ y1 ^ "-" ^ d2 ^ "." ^ m2 ^ "." ^ y2,"date-interval",[[]]); cat="DayNumber"}
    | Compound("date-interval",[Compound("date",[Dig(d1,"day");Dig(m1,"month");Dig(y1,"2dig")]);
        Compound("date",[Dig(d2,"day");Dig(m2,"month");Dig(y2,"year")])]) -> {t with token=Lemma(d1 ^ "." ^ m1 ^ "." ^ y1 ^ "-" ^ d2 ^ "." ^ m2 ^ "." ^ y2,"date-interval",[[]]); cat="DayNumber"}
    | Compound("date-interval",[Compound("date",[Dig(d1,"day");Dig(m1,"month");Dig(y1,"2dig")]);
        Compound("date",[Dig(d2,"day");Dig(m2,"month");Dig(y2,"2dig")])]) -> {t with token=Lemma(d1 ^ "." ^ m1 ^ "." ^ y1 ^ "-" ^ d2 ^ "." ^ m2 ^ "." ^ y2,"date-interval",[[]]); cat="DayNumber"}
    | Compound("day-month-interval",[Compound("day-month",[Dig(d1,"day");Dig(m1,"month")]);
        Compound("day-month",[Dig(d2,"day");Dig(m2,"month")])]) -> {t with token=Lemma(d1 ^ "." ^ m1 ^ "-" ^ d2 ^ "." ^ m2,"day-month-interval",[[]]); cat="DayNumber"}
    | Compound("day-interval",[Dig(d1,"day");Dig(d2,"day")]) -> {t with token=Lemma(d1 ^ "-" ^ d2,"day-interval",[[]]); cat="DayNumber"}
    | Compound("month-interval",[Dig(m1,"month");Dig(m2,"month")]) -> {t with token=Lemma(m1 ^ "-" ^ m2,"month-interval",[[]]); cat="Month"}
    | Compound("month-interval",[RomanDig(m1,"month");RomanDig(m2,"month")]) -> {t with token=Lemma(m1 ^ "-" ^ m2,"month-interval",[[]]); attrs=Roman :: t.attrs; cat="Month"}
    | Compound("year-interval",[Dig(y1,"year");Dig(y2,"year")]) -> {t with token=Lemma(y1 ^ "-" ^ y2,"year-interval",[[]]); cat="YearNumber"}
    | Compound("year-interval",[Dig(y1,"year");Dig(y2,"2dig")]) -> {t with token=Lemma(y1 ^ "-" ^ y2,"year-interval",[[]]); cat="YearNumber"}
    | Compound("hour-minute-interval",[Compound("hour-minute",[Dig(h1,"hour");Dig(m1,"minute")]);Compound("hour-minute",[Dig(h2,"hour");Dig(m2,"minute")])]) ->
       {t with token=Lemma(h1 ^ ":" ^ m1 ^ "-" ^ h2 ^ ":" ^ m2,"hour-minute-interval",[[]]); cat="HourNumber"}
    | Compound("hour-interval",[Dig(h1,"hour");Dig(h2,"hour")]) -> {t with token=Lemma(h1 ^ "-" ^ h2,"hour-interval",[[]]); cat="HourNumber"}
    | Compound("minute-interval",[Dig(m1,"minute");Dig(m2,"minute")]) -> t (*{t with token=Lemma(m1 ^ "-" ^ m2,"minute-interval",[[]])}*)
    | Ideogram(lemma,"url") -> {t with token=Lemma(lemma,"url",[[]]); cat="X"}
    | Ideogram(lemma,"email") -> {t with token=Lemma(lemma,"email",[[]]); cat="X"}
    | Ideogram(lemma,"html-tag") -> {t with token=Lemma(lemma,"html-tag",[[]]); cat="X"}
    | Ideogram(lemma,"list-item") -> {t with token=Lemma(lemma,"list-item",[[]]); cat="X"}
    | Ideogram(lemma,cat) -> failwith ("translate_digs: Dig " ^ cat)
    | Interp "." -> {t with cat="Interp"}
    | Interp "," -> {t with token=Lemma(",","conj",[[]]); cat="Conj"}
    | Interp "</query>" -> {t with cat="Interp"}
    | Interp "<query>" -> {t with cat="Interp"}
(*    | Interp "</sentence>" -> {t with cat="Interp"}
    | Interp "<sentence>" -> {t with cat="Interp"}
    | Interp "</clause>" -> {t with cat="Interp"}
    | Interp "<clause>" -> {t with cat="Interp"}
    | Interp s -> Printf.printf "translate_digs: „%s”\n" s; t*)
    | Interp "(" -> {t with cat="Interp"}
    | Interp ")" -> {t with cat="Interp"}
    | Interp "-" -> {t with cat="Interp"}
    | Interp "?" -> {t with cat="Interp"}
    | Interp "!" -> {t with cat="Interp"}
(*     | Interp "α" -> {t with cat="Interp"} *)
(*    | RomanDig(lemma,cat) -> failwith ("translate_digs: Romandig " ^ cat)
    | Compound(cat,_) as t -> failwith ("translate_digs: " ^ Tokens.string_of_token t)*)*)
    | _ -> t
(*    else
    match t.token with
      Dig(lemma,"dig") -> t
    | Dig(lemma,"intnum") -> {t with token=Lemma(lemma,"intnum",[[]])}
    | Dig(lemma,"realnum") -> {t with token=Lemma(lemma,"realnum",[[]])}
    | Dig(lemma,"year") -> {t with token=Proper(lemma,"year",[[]],["rok"])}
    | Dig(lemma,"month") -> t (*{t with token=Proper(lemma,"month",[[]],["miesiąc"])}*)
    | Dig(lemma,"hour") -> {t with token=Proper(lemma,"hour",[[]],["godzina"])}
    | Dig(lemma,"day") -> {t with token=Proper(lemma,"day",[[]],["dzień"])}
    | Dig(lemma,"minute") -> t (*{t with token=Proper(lemma,"minute",[[]],["minuta"])}*)
    | Dig(lemma,"2dig") -> t
    | Dig(lemma,"3dig") -> t
    | Dig(lemma,"4dig") -> t
    | Dig(lemma,"pref3dig") -> t
    | RomanDig(lemma,"roman") -> {t with token=Lemma(lemma,"roman",[[]]); attrs=t.attrs}
    | RomanDig(lemma,"month") -> t (*{t with token=Proper(lemma,"symbol",[[]],["month"]); attrs="roman" :: t.attrs}*)
    | Dig(lemma,"ordnum") -> {t with token=Lemma(lemma,"ordnum",[[]])}
    | Compound("date",[Dig(d,"day");Dig(m,"month");Dig(y,"year")]) -> {t with token=Proper(d ^ "." ^ m ^ "." ^ y,"date",[[]],["data"])}
    | Compound("date",[Dig(d,"day");RomanDig(m,"month");Dig(y,"year")]) -> {t with token=Proper(d ^ "." ^ m ^ "." ^ y,"date",[[]],["data"])}
    | Compound("date",[Dig(d,"day");Dig(m,"month");Dig(y,"2dig")]) -> {t with token=Proper(d ^ "." ^ m ^ "." ^ y,"date",[[]],["data"])}
    | Compound("date",[Dig(d,"day");RomanDig(m,"month");Dig(y,"2dig")]) -> {t with token=Proper(d ^ "." ^ m ^ "." ^ y,"date",[[]],["data"])}
    | Compound("day-month",[Dig(d,"day");Dig(m,"month")]) -> {t with token=Proper(d ^ "." ^ m,"day-month",[[]],["data"])}
    | Compound("hour-minute",[Dig(h,"hour");Dig(m,"minute")]) -> {t with token=Proper(h ^ ":" ^ m,"hour-minute",[[]],["godzina"])}
    | Compound("match-result",[Dig(x,"intnum");Dig(y,"intnum")]) -> {t with token=Proper(x ^ ":" ^ y,"match-result",[[]],["rezultat"])}
    | Compound("intnum-interval",[Dig(x,"intnum");Dig(y,"intnum")]) -> {t with token=Lemma(x ^ "-" ^ y,"intnum-interval",[[]])}
    | Compound("roman-interval",[RomanDig(x,"roman");RomanDig(y,"roman")]) -> {t with token=Lemma(x ^ "-" ^ y,"roman-interval",[[]]); attrs=t.attrs}
    | Compound("realnum-interval",[Dig(x,"realnum");Dig(y,"realnum")]) -> {t with token=Lemma(x ^ "-" ^ y,"realnum-interval",[[]])}
    | Compound("realnum-interval",[Dig(x,"intnum");Dig(y,"realnum")]) -> {t with token=Lemma(x ^ "-" ^ y,"realnum-interval",[[]])}
    | Compound("realnum-interval",[Dig(x,"realnum");Dig(y,"intnum")]) -> {t with token=Lemma(x ^ "-" ^ y,"realnum-interval",[[]])}
    | Compound("date-interval",[Compound("date",[Dig(d1,"day");Dig(m1,"month");Dig(y1,"year")]);
        Compound("date",[Dig(d2,"day");Dig(m2,"month");Dig(y2,"year")])]) -> {t with token=Proper(d1 ^ "." ^ m1 ^ "." ^ y1 ^ "-" ^ d2 ^ "." ^ m2 ^ "." ^ y2,"date-interval",[[]],["interwał"])}
    | Compound("day-month-interval",[Compound("day-month",[Dig(d1,"day");Dig(m1,"month")]);
        Compound("day-month",[Dig(d2,"day");Dig(m2,"month")])]) -> {t with token=Proper(d1 ^ "." ^ m1 ^ "-" ^ d2 ^ "." ^ m2,"day-month-interval",[[]],["interwał"])}
    | Compound("day-interval",[Dig(d1,"day");Dig(d2,"day")]) -> {t with token=Proper(d1 ^ "-" ^ d2,"day-interval",[[]],["interwał"])}
    | Compound("month-interval",[Dig(m1,"month");Dig(m2,"month")]) -> {t with token=Proper(m1 ^ "-" ^ m2,"month-interval",[[]],["interwał"])}
    | Compound("month-interval",[RomanDig(m1,"month");RomanDig(m2,"month")]) -> {t with token=Proper(m1 ^ "-" ^ m2,"month-interval",[[]],["interwał"]); attrs=Roman :: t.attrs}
    | Compound("year-interval",[Dig(y1,"year");Dig(y2,"year")]) -> {t with token=Proper(y1 ^ "-" ^ y2,"year-interval",[[]],["interwał"])}
    | Compound("year-interval",[Dig(y1,"year");Dig(y2,"2dig")]) -> {t with token=Proper(y1 ^ "-" ^ y2,"year-interval",[[]],["interwał"])}
    | Compound("hour-minute-interval",[Compound("hour-minute",[Dig(h1,"hour");Dig(m1,"minute")]);Compound("hour-minute",[Dig(h2,"hour");Dig(m2,"minute")])]) ->
       {t with token=Proper(h1 ^ ":" ^ m1 ^ "-" ^ h2 ^ ":" ^ m2,"hour-minute-interval",[[]],["interwał"])}
    | Compound("hour-interval",[Dig(h1,"hour");Dig(h2,"hour")]) -> {t with token=Proper(h1 ^ "-" ^ h2,"hour-interval",[[]],["interwał"])}
    | Compound("minute-interval",[Dig(m1,"minute");Dig(m2,"minute")]) -> t (*{t with token=Proper(m1 ^ "-" ^ m2,"minute-interval",[[]],["interwał"])}*)
    | Dig(lemma,"url") -> {t with token=Proper(lemma,"url",[[]],["url"])}
    | Dig(lemma,"email") -> {t with token=Proper(lemma,"email",[[]],["email"])}
    | Dig(lemma,"html-tag") -> {t with token=Lemma(lemma,"html-tag",[[]])}
    | Dig(lemma,"list-item") -> {t with token=Lemma(lemma,"list-item",[[]])}
    | Dig(lemma,cat) -> failwith ("translate_digs: Dig " ^ cat)
    | RomanDig(lemma,cat) -> failwith ("translate_digs: Romandig " ^ cat)
    | Compound(cat,_) as t -> failwith ("translate_digs: " ^ Tokens.string_of_token t)
    | _ -> t)*))

(**********************************************************************************)

module OrderedStringList = struct

  type t = string list

  let compare x y = compare (Xlist.sort x compare) (Xlist.sort y compare)

end

module OrderedStringListList = struct

  type t = string list list

  let compare x y = compare (Xlist.sort x compare) (Xlist.sort y compare)

end

module StringListMap = Xmap.Make(OrderedStringList)
module StringListListMap = Xmap.Make(OrderedStringListList)
module StringListListSet = Xset.Make(OrderedStringListList)

type tree = T of tree StringListMap.t | S of StringSet.t

let single_tags = function
    [_] :: _ -> true
  | _ -> false

let rec make_tree interp =
  if single_tags interp then S (StringSet.of_list (List.flatten (List.flatten interp))) else
  let map = Xlist.fold interp StringListMap.empty (fun map tags ->
    StringListMap.add_inc map (List.hd tags) [List.tl tags] (fun l -> (List.tl tags) :: l)) in
  T(StringListMap.map map make_tree)

let is_s_tree map =
  StringListListMap.fold map false (fun b _ -> function
      S _ -> true
    | T _ -> b)

let rec fold_tree_rec rev s f = function
    S set -> f s (List.rev rev) set
  | T map -> StringListMap.fold map s (fun s tag tree ->
       fold_tree_rec (tag :: rev) s f tree)

let fold_tree tree s f = fold_tree_rec [] s f tree

let rec combine_interps_rec map =
  if is_s_tree map then
    StringListListMap.fold map [] (fun interp tail_tags -> function
        S tag -> ((Xlist.sort (StringSet.to_list tag) compare) :: tail_tags) :: interp
      | _ -> failwith "combine_interps_rec")
  else
    let map = StringListListMap.fold map StringListListMap.empty (fun map tail_tags tree ->
      fold_tree tree map (fun map head_tags tag ->
        StringListListMap.add_inc map ((Xlist.sort (StringSet.to_list tag) compare) :: tail_tags) [head_tags] (fun l -> head_tags :: l))) in
    combine_interps_rec (StringListListMap.map map make_tree)

let combine_interp interp =
  try
    let map = StringListListMap.add StringListListMap.empty [] (make_tree interp) in
    combine_interps_rec map
  with e -> failwith ("combine_interp: " ^ Printexc.to_string e)

let combine_pos = StringSet.of_list ["subst"; "depr"; "ppron12"; "ppron3"; "siebie"; "adj"; "num"; "ger"; "praet"; "fin"; "impt"; "imps"; "pcon"; "ppas"; "pact";
  "inf"; "bedzie"; "aglt"; "winien"; "pant"; "prep"]

let combine_subst_tags = function
    [n;c;g] -> Xlist.map (Xlist.multiply_list [n;c;g]) (fun l -> Xlist.map l (fun x -> [x]))
  | [n;c;[g];[col]] -> Xlist.map (Xlist.multiply_list [n;c;[g ^ ":" ^ col]]) (fun l -> Xlist.map l (fun x -> [x]))
  | _ -> failwith "combine_subst_tags"

let combine_interps paths =
  List.rev (Xlist.rev_map paths (fun t ->
    match t.token with
      Lemma(lemma,pos,interp,cat) ->
        (* Printf.printf "%s %s %s\n" lemma pos (Tagset.render interp); *)
        if StringSet.mem combine_pos pos && interp = [[]] then failwith ("combine_interps: interp=[[]] for " ^ lemma ^ ":" ^ pos) else
        let interp =
          if pos = "subst" then List.flatten (Xlist.map interp combine_subst_tags) else
          if pos = "ppron12" then Xlist.map interp (fun tags -> if Xlist.size tags = 4 then tags @ [["_"]] else tags)
          else interp in
        let interp =
          if StringSet.mem combine_pos pos then combine_interp interp else
          StringListListSet.to_list (StringListListSet.of_list interp) in
        {t with token=Lemma(lemma,pos,interp,cat)}
(*    | Proper(lemma,pos,interp,cat) ->
              (* Printf.printf "%s %s %s\n" lemma pos (Tagset.render interp); *)
      if StringSet.mem combine_pos pos && interp = [[]] then failwith ("combine_interps: interp=[[]] for " ^ lemma ^ ":" ^ pos) else
      let interp =
        if pos = "subst" then List.flatten (Xlist.map interp combine_subst_tags) else
        if pos = "ppron12" then Xlist.map interp (fun tags -> if Xlist.size tags = 4 then tags @ [["_"]] else tags)
        else interp in
      let interp =
        if StringSet.mem combine_pos pos then combine_interp interp else
          StringListListSet.to_list (StringListListSet.of_list interp) in
      {t with token=Proper(lemma,pos,interp,cat)}*)
    | _ -> t))

(**********************************************************************************)

let select_tokens paths =
  List.rev (Xlist.fold paths [] (fun paths t ->
    match t.token with
(*      RomanDig(v,cat) -> {t with token=Lemma(v,cat,[[]])} :: paths
    | Interp orth -> {t with token=Lemma(orth,"interp",[[]])} :: paths
    | Dig(value,cat) -> {t with token=Lemma(value,cat,[[]])} :: paths
    | Other2 orth -> {t with token=Lemma(orth,"unk",[[]])} :: paths
    | Lemma(lemma,cat,interp) -> t :: paths
    | Proper _ -> failwith "select_tokens"
    | Compound _ -> t :: paths*)
(*       RomanDig(v,cat) -> t :: paths *)
    | Interp orth -> t :: paths
	| Ideogram(value,mode) -> t :: paths
    | Other orth -> t :: paths
    | Lemma(lemma,pos,interp,_) -> if pos = "brev" then paths else t :: paths
(*     | Proper(lemma,pos,interp,cat) -> if pos = "brev" then paths else t :: paths *)
(*     | Compound _ -> t :: paths *)
    | _ -> paths))

let add_token paths (q,t,n) =
  let map = try IntMap.find paths t.beg with Not_found -> IntMap.empty in
  let map = IntMap.add_inc map t.next [q,t,n] (fun l -> (q,t,n) :: l) in
  IntMap.add paths t.beg map

let rec select_tokens2_rec last paths nodes map =
  let node = IntSet.min_elt nodes in
  if node = last then try snd (IntMap.find map node) with Not_found -> failwith "select_tokens2_rec: token graph is not connected" else
  let nodes = IntSet.remove nodes node in
  if not (IntMap.mem map node) then select_tokens2_rec last paths nodes map else
  let qselected,selected = IntMap.find map node in
  let map2 = try IntMap.find paths node with Not_found -> IntMap.empty in
  let map = IntMap.fold map2 map (fun map next l ->
    Xlist.fold l map (fun map (q,t,n) ->
      let selected = IntSet.add selected n in
      let qselected = qselected+q in
      IntMap.add_inc map t.next (qselected,selected) (fun (qselected2,selected2) ->
        if qselected2 > qselected then qselected2,selected2 else
        if qselected2 < qselected then qselected,selected else
        qselected,IntSet.union selected selected2))) in
  select_tokens2_rec last paths nodes map

let rec calculate_quality q = function
    FC :: l -> calculate_quality (q-2) l
  | CS :: l -> calculate_quality (q-2) l
  | MaybeCS :: l -> calculate_quality q l
  | HasAglSuffix :: l -> calculate_quality q l
  | AglSuffix :: l -> calculate_quality q l
  | MWE :: l -> calculate_quality (q+6) l
  | Capitalics :: l -> calculate_quality (q+1) l
  | LemmNotVal :: l -> calculate_quality (q-5) l
  | TokNotFound :: l -> calculate_quality (q-10) l
  | NotValProper :: l -> calculate_quality (q-1) l
  | LemmLowercase :: l -> calculate_quality q l
  | Roman :: l -> calculate_quality q l
  | SentBeg :: l -> calculate_quality q l
  | SentBegEnd :: l -> calculate_quality q l
  | SentEnd :: l -> calculate_quality q l
  | BrevLemma _ :: l -> calculate_quality q l
  | Disamb _ :: l -> calculate_quality q l
  | [] -> q

let added_quality t =
  match Tokens.get_pos t with
    "prep" -> 11
  | "conj" -> 11
  | "qub" -> 11
  | "interj" -> 11
  | "adv" -> 11
  | "adja" -> 11
  | "comp" -> 11
  | "ppron12" -> 11
  | "ppron3" -> 11
  | _ -> 0

let select_tokens2 paths =
  (* print_endline "select_tokens2" ; *)
  let beg,last = Xlist.fold paths (max_int,-1) (fun (beg,last) t ->
    min beg t.beg, max last t.next) in
  let nodes = Xlist.fold paths IntSet.empty (fun nodes t ->
    IntSet.add (IntSet.add nodes t.beg) t.next) in
  let paths2,_ = Xlist.fold paths ([],1) (fun (paths2,n) t ->
    (* Printf.printf "%3d %3d %s\n%!" (added_quality t.token) (calculate_quality 0 t.attrs) (Tokens.string_of_token_env t); *)
    (added_quality t.token + calculate_quality 0 t.attrs, t, n) :: paths2, n+1) in
  let paths2 = Xlist.fold paths2 IntMap.empty add_token in
  let selected = select_tokens2_rec last paths2 nodes (IntMap.add IntMap.empty beg (0,IntSet.empty)) in
  (* print_endline (String.concat " " (StringSet.to_list selected)); *)
  IntMap.fold paths2 [] (fun paths _ map ->
    IntMap.fold map paths (fun paths _ l ->
      Xlist.fold l paths (fun paths (q,t,n) ->
        if IntSet.mem selected n then t :: paths else paths)))


(*let load_proper_name proper = function
    [lemma; types] ->
    let types = Str.split (Str.regexp "|") types in
    StringMap.add_inc proper lemma types (fun types2 -> types @ types2)
  | l -> failwith ("proper_names: " ^ String.concat " " l)

let load_proper_names filename proper =
  File.fold_tab filename proper load_proper_name

let load_proper_names () =
  let proper = File.catch_no_file (load_proper_names proper_names_filename) StringMap.empty in
  let proper = File.catch_no_file (load_proper_names proper_names_filename2) proper in
  let proper = File.catch_no_file (load_proper_names proper_names_filename3) proper in
  let proper = File.catch_no_file (load_proper_names proper_names_filename4) proper in
  let proper = File.catch_no_file (load_proper_names simc_filename) proper in
  let proper = File.catch_no_file (load_proper_names terc_filename) proper in
  proper

let proper_names = ref (StringMap.empty : string list StringMap.t)*)

let remove l s =
  Xlist.fold l [] (fun l t ->
      if s = t then l else t :: l)

(*let find_proper_names t =
  match t.token with
    Lemma(lemma,pos,interp) ->
    if (pos="subst" || pos="depr" || pos="fin" || pos="inf") && StringMap.mem !proper_names lemma then
      {t with token=Proper(lemma,pos,interp,StringMap.find !proper_names lemma);
              attrs=remove (remove t.attrs NotValProper) LemmNotVal} else
    if Xlist.mem t.attrs NotValProper then
      {t with token=Proper(lemma,pos,interp,[])}
    else t
  | _ -> t*)

let get_sock_addr host_name port =
  let he = Unix.gethostbyname host_name in
  let addr = he.Unix.h_addr_list in
  Unix.ADDR_INET(addr.(0),port)

let initialize () =
  Tokenizer.initialize ();
  let mwe_dict,mwe_dict2 = MWE.load_mwe_dicts () in
  MWE.mwe_dict := mwe_dict;
  MWE.mwe_dict2 := mwe_dict2;
  lemma_frequencies := File.catch_no_file (load_lemma_frequencies lemma_frequencies_filename) StringMap.empty;
  if !coord_enabled then
  let c_in,c_out = 
    if !coord_enabled then 
      Unix.open_connection (get_sock_addr !coord_host_name !coord_port)
	else stdin,stdout in
  coord_in := c_in;
  coord_out := c_out;  
(*   proper_names := load_proper_names (); *)
  ()

let disambiguate_coordination paths =
  Marshal.to_channel !coord_out paths []; 
  flush !coord_out;
  let paths,msg = (Marshal.from_channel !coord_in : token_env list * string) in
  if msg <> "" then failwith ("disambiguate_coordination: " ^ msg) else
  paths

let parse query =
  let l = Tokenizer.parse query in
  (* print_endline "a6"; *)
  let paths = Paths.translate_into_paths l in
  print_endline "XXXXXXXXXXXXXXXXXXXXXXXXX a7"; 
  print_endline (SubsyntaxStringOf.token_list (fst paths)); 
  print_endline "XXXXXXXXXXXXXXXXXXXXXXXXX a8"; 
(*   print_endline (SubsyntaxStringOf.token_list (fst paths)); *)
  (* print_endline "XXXXXXXXXXXXXXXXXXXXXXXXX a9"; *)
  let paths,last = MWE.process paths in
(*   print_endline "XXXXXXXXXXXXXXXXXXXXXXXXX a12"; *)
   print_endline (SubsyntaxStringOf.token_list paths); 
(*   let paths =  if !recognize_proper_names then List.rev (Xlist.rev_map paths find_proper_names) else paths in *)
  print_endline "XXXXXXXXXXXXXXXXXXXXXXXXX a13"; 
  (* print_endline (SubsyntaxStringOf.token_list paths); *)
  let paths = modify_weights paths in
  let paths = translate_digs paths in
  (* print_endline "a14"; *)
  let paths = combine_interps paths in
(*   print_endline "XXXXXXXXXXXXXXXXXXXXXXXXX a16"; *)
(*   print_endline (SubsyntaxStringOf.token_list paths); *)
  let paths = select_tokens paths in
  let paths = Xlist.sort paths Paths.compare_token_record in
  let paths = Paths.remove_inaccessible_tokens paths 0 last in
  let paths = Xlist.sort paths Paths.compare_token_record in
  let paths = if !concraft_enabled then Concraft.process_paths paths else paths in
  (* print_endline "XXXXXXXXXXXXXXXXXXXXXXXXX a16"; *)
  (* print_endline (SubsyntaxStringOf.token_list paths); *)
  let paths = if !concraft_disambiguate then Concraft.disambiguate paths last else paths in
  (* print_endline "XXXXXXXXXXXXXXXXXXXXXXXXX a17"; *)
  (* print_endline (SubsyntaxStringOf.token_list paths); *)
(*   let paths = if !strong_disambiguate_flag then select_tokens2 paths else paths in (* Ta procedura wycina potrzebne tokeny *) *)
(*   let paths = Paths.process_interpunction paths in *)
  let paths = Xlist.sort paths Paths.compare_token_record in
  (* print_endline "XXXXXXXXXXXXXXXXXXXXXXXXX a18"; *)
  (* print_endline (SubsyntaxStringOf.token_list paths); *)
  let paths = if !coord_enabled then disambiguate_coordination paths else paths in
  let paths = Xlist.sort paths Paths.compare_token_record in
  (* print_endline "XXXXXXXXXXXXXXXXXXXXXXXXX a19"; *)
  paths(*, next_id*)

let parse_text_tokens sentence_split_flag par_names_flag tokens query =
(*   print_endline ("parse_text_tokens 1: " ^ query); *)
  let paragraphs = Xstring.split "\n\\|\r" query in
  let paragraphs = List.rev (Xlist.fold paragraphs [] (fun l -> function "" -> l | s -> s :: l)) in
(*   print_endline ("parse_text_tokens 2: " ^ query); *)
  let paragraphs = List.rev (Xlist.rev_map paragraphs (fun paragraph ->
    if par_names_flag then
      match Xstring.split_delim "\t" paragraph with
        [name; paragraph] -> 
          let paragraph = if paragraph = "" || paragraph = " " then "¶" else paragraph in (* FIXME: to koniecznie trzeba poprawić tak by napisy zawierające jedynie białe znaki nie wywracały parsera *)
          (match Xstring.split " | " name with 
            [name; id] -> name, id, paragraph
          | _ -> name,"", paragraph)
      | _ -> failwith ("parse_text_tokens: " ^ paragraph)
    else "", "", paragraph)) in
(*   print_endline ("parse_text_tokens 3: " ^ query); *)
  let n = if Xlist.size paragraphs = 1 then 0 else 1 in (* FIXME: powyższe do przeniesienia do osobnej procedury *)
  let paragraphs,_ = Xlist.fold paragraphs ([],n) (fun (paragraphs,n) (name,id,paragraph) ->
    try
      (* print_endline paragraph; *)
      let paths = parse paragraph in
      (* print_endline "parse_text 1"; *)
      let pid = if n = 0 then "" else string_of_int n ^ "_" in
      let sentences =
        if sentence_split_flag then Sentences.split_into_sentences pid paragraph tokens paths
        else Sentences.no_split_into_sentences pid paragraph tokens paths in
      (AltParagraph ((if par_names_flag then [Name,RawParagraph name] else []) @ (if id = "" then [] else [Identifier,RawParagraph id]) @
        [Raw,RawParagraph paragraph; Struct,StructParagraph sentences])) :: paragraphs, n+1
    with e ->
      (AltParagraph ((if par_names_flag then [Name,RawParagraph name] else []) @ (if id = "" then [] else [Identifier,RawParagraph id]) @
        [Raw,RawParagraph paragraph; Error,ErrorParagraph (Printexc.to_string e)])) :: paragraphs, n+1) in
  AltText[Raw,RawText query; Struct,StructText(List.rev paragraphs)], tokens

let parse_text sentence_split_flag par_names_flag query =
  (* print_endline ("parse_text: " ^ query); *)
  let tokens = ExtArray.make 100 empty_token_env in
  let _ = ExtArray.add tokens empty_token_env in (* id=0 jest zarezerwowane dla pro; FIXME: czy to jest jeszcze aktualne? *)
  parse_text_tokens sentence_split_flag par_names_flag tokens query

let catch_parse text =
  try
    let tokens = parse text in tokens,""
  with 
    BrokenPaths(beg,last,n,paths) -> [], Printf.sprintf "BrokenPaths beg=%d last=%d n=%d\n%s\n" beg last n (SubsyntaxStringOf.token_list paths)
  | e -> [], Printexc.to_string e

let catch_parse_text sentence_split_flag par_names_flag text =
  try
    parse_text sentence_split_flag par_names_flag text
  with e ->
    AltText[Raw,RawText text; Error,ErrorText (Printexc.to_string e)],
    ExtArray.make 0 empty_token_env

    
    
let is_parsed tokens paths last =
(*   Printf.printf "is_parsed: last=%d\n" last; *)
  let set = Xlist.fold paths (IntSet.singleton 0) (fun set (id,lnode,rnode) ->
    let t = ExtArray.get tokens id in    
(*     Printf.printf "is_parsed: lnode=%d rnode=%d orth=%s token=%s cat=%s\n" lnode rnode t.orth (Tokens.string_of_token t.token) t.cat; *)
    if IntSet.mem set lnode && Tokens.get_cat t.token <> "X" && Tokens.get_cat t.token <> "MWEcomponent" then IntSet.add set rnode else set) in
(*   if IntSet.mem set last then Printf.printf "is_parsed: true\n" else Printf.printf "is_parsed: false\n"; *)
  IntSet.mem set last
    
let rec select_not_parsed_sentence mode tokens = function
  | QuotedSentences sentences ->
      let sentences = Xlist.rev_map sentences (fun p ->
        let sentence = select_not_parsed_sentence mode tokens p.sentence in
        {p with sentence=List.hd sentence}) in
      [QuotedSentences(List.rev sentences)]
  | AltSentence l ->
      let l = List.flatten (Xlist.rev_map l (fun (mode,sentence) ->
        Xlist.rev_map (select_not_parsed_sentence mode tokens sentence) (fun s -> mode, s))) in
      [AltSentence(List.rev l)]
  | RawSentence s -> (*Printf.printf "select_not_parsed_sentence: %s\n" s;*) [RawSentence s]
  | StructSentence(paths,last) -> if is_parsed tokens paths last then [] else [StructSentence(paths,last)]
  | DepSentence _ -> failwith "select_not_parsed_sentence"
  | ErrorSentence s -> (*Printf.printf "select_not_parsed_sentence: %s\n" s;*) [ErrorSentence s]

let rec select_not_parsed_paragraph mode tokens = function
    RawParagraph s -> RawParagraph s
  | StructParagraph sentences ->
      let sentences = Xlist.rev_map sentences (fun p ->
        let sentence = select_not_parsed_sentence mode tokens p.sentence in
        {p with sentence=List.hd sentence}) in
      StructParagraph(List.rev sentences)
  | AltParagraph l ->
      let l = Xlist.rev_map l (fun (mode,paragraph) ->
        mode, select_not_parsed_paragraph mode tokens paragraph) in
      AltParagraph(List.rev l)
  | ErrorParagraph s -> ErrorParagraph s

let rec select_not_parsed_text mode tokens = function
    RawText s -> RawText s
  | StructText paragraphs ->
      let paragraphs = Xlist.rev_map paragraphs (fun paragraph ->
        select_not_parsed_paragraph mode tokens paragraph) in
      StructText(List.rev paragraphs)
  | AltText l -> AltText(Xlist.map l (fun (mode,text) ->
       mode, select_not_parsed_text mode tokens text))
  | ErrorText s -> ErrorText s

let select_not_parsed tokens text =
  select_not_parsed_text Struct tokens text

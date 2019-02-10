(*
 *  ENIAMsemantics implements semantic processing for ENIAM
 *  Copyright (C) 2018 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
 *  Copyright (C) 2018 Institute of Informatics, University of Warsaw
 *  Copyright (C) 2018 SELIDOR - T. Puza, Ł. Wasilewski Sp.J.
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

open LCGtypes
open Xstd

let rec count_args_rec tree cost = function
    Node t -> 
      (if t.symbol = Val "<arg>" then 0. else 1.) +. ((count_args_rec tree cost t.args) /. 10.)
  | Tuple l -> Xlist.fold l 0. (fun n t -> n +. count_args_rec tree cost t)
  | Variant(e2,l) -> Xlist.fold l max_float (fun n (_,t) -> min n (count_args_rec tree cost t))
  | Dot -> 0.
  | Ref i ->
      if cost.(i) > 0. then cost.(i) else
      let n = count_args_rec tree cost tree.(i) in
      cost.(i) <- n;
      n
  | t -> failwith ("count_args: " ^ LCGstringOf.linear_term 0 t)

let count_args tree =
  let cost = Array.make (Array.length tree) (-1.) in
  let n = count_args_rec tree cost tree.(0) in
  cost.(0) <- n;
  cost

let rec count_args_local cost = function
    Dot -> 0.
  | Tuple l -> Xlist.fold l 0. (fun n t -> n +. count_args_local cost t)
  | Variant(_,l) ->  Xlist.fold l max_float (fun n (_,t) -> min n (count_args_local cost t))
  | Ref i -> cost.(i)
  | t -> failwith ("count_args: " ^ LCGstringOf.linear_term 0 t)

let rec disambiguate_args cost = function
    Dot -> Dot
  | Tuple l -> Tuple(List.rev (Xlist.rev_map l (disambiguate_args cost)))
  | Variant(e,l) ->
       let l = Xlist.fold l [] (fun l (i,t) -> (i, disambiguate_args cost t) :: l) in
       let _,l = Xlist.fold l (max_float,[]) (fun (min_n,l) (i,t) ->
         let n = count_args_local cost t in
         if n < min_n then n,[i,t] else
         if n > min_n then min_n,l else
         min_n, (i,t) :: l) in
      (match l with
          [] -> failwith "disambiguate_args"
        | [_,t] -> t
        | _ -> Variant(e,l)) (* FIXME: trzeba by zmienić numerację *)
  | Ref i -> Ref i
  | t -> failwith ("disambiguate_args: " ^ LCGstringOf.linear_term 0 t)


let disambiguate_tree cost = function
    Node t -> Node {t with args=disambiguate_args cost t.args}
  | t -> failwith ("disambiguate_tree: " ^ LCGstringOf.linear_term 0 t)

let eniam_disambiguate tree =
  let tree = Array.copy tree in
  let cost = count_args tree in
  Int.iter 0 (Array.length tree - 1) (fun i ->
    tree.(i) <- disambiguate_tree cost tree.(i));
  tree

let rec count_argument_position tree tokens = function
    Node t -> (ExtArray.get tokens t.id).SubsyntaxTypes.beg
  | Tuple l -> Xlist.fold l max_int (fun n t -> min n (count_argument_position tree tokens t))
  | Variant(e2,l) -> Xlist.fold l max_int (fun n (_,t) -> min n (count_argument_position tree tokens t))
  | Dot -> max_int
  | Ref i -> count_argument_position tree tokens tree.(i)
  | t -> failwith ("count_argument_position: " ^ LCGstringOf.linear_term 0 t)

let rec sort_arguments_args tree tokens = function
    Dot -> Dot
  | Tuple l ->
      let l = Xlist.rev_map l (sort_arguments_args tree tokens) in
      let l = Xlist.rev_map l (fun t ->
        count_argument_position tree tokens t, t) in
      Tuple(List.rev (Xlist.rev_map (Xlist.sort l compare) snd))
  | Variant(e,l) -> Variant(e,List.rev (Xlist.rev_map l (fun(i,t) -> i, sort_arguments_args tree tokens t) ))
  | Ref i -> Ref i
  | t -> failwith ("sort_arguments_args: " ^ LCGstringOf.linear_term 0 t)

let sort_arguments_tree tree tokens = function
    Node t -> Node {t with args=sort_arguments_args tree tokens t.args}
  | t -> failwith ("sort_arguments_tree: " ^ LCGstringOf.linear_term 0 t)

let eniam_sort_arguments tokens tree =
  let tree = Array.copy tree in
  Int.iter 0 (Array.length tree - 1) (fun i ->
    tree.(i) <- sort_arguments_tree tree tokens tree.(i));
  tree

let make_variant = function
    [] -> failwith "make_variant"
  | [t] -> t
  | l ->
    let e = LCGreductions.get_variant_label () in
    let l,_ = Xlist.fold l ([],1) (fun (l,i) -> function
          t -> (string_of_int i,t) :: l, i+1) in
    Variant(e,l)

let rec extract_attr attr rev = function
   (a, t) :: l ->
     if a = attr then t, List.rev rev @ l else
     extract_attr attr ((a,t) :: rev) l
 | [] -> raise Not_found

let rec split_variants tree = function
    Node t ->
      (* print_endline "split_variants Node"; *)
      (try
        let role, attrs = extract_attr "ROLE" [] t.attrs in
        let a,b = split_variants tree role in
        (if a = [] then [] else [Node {t with attrs=attrs}]),
        if b = [] then [] else [Node {t with attrs=("ROLE", make_variant b) :: attrs}]
      with Not_found -> [],[Node t])
  | Tuple l ->
      (* print_endline "split_variants Tuple"; *)
      let l = Xlist.rev_map l (split_variants tree) in
      let l = Xlist.rev_map l (fun (a,b) -> b @ a) in
      let lemmas,others = Xlist.fold (Xlist.multiply_list l) ([],[]) (fun (lemmas,others) l ->
        let b = Xlist.fold l false (fun b -> function
            Node _ -> true | _ -> b) in
        if b then Tuple l :: lemmas, others else lemmas, Tuple l :: others) in
      lemmas, if others = [] then [] else [make_variant others]
  | Variant(e,l) ->
      (* print_endline "split_variants Variant"; *)
      let lemmas,others = Xlist.fold l ([],[]) (fun (lemmas,others) (_,t) ->
        let a,b = split_variants tree t in
        a @ lemmas, b @ others) in
      lemmas, if others = [] then [] else [make_variant others]
  | Val "Lemma" -> [Val "Lemma"],[]
  | Val s -> [], [Val s]
  | Dot -> [],[Dot]
  | Ref i ->
      (* print_endline "split_variants Ref 1"; *)
      let a,b = split_variants tree tree.(i) in
      (* print_endline "split_variants Ref 2"; *)
      if a = [] then [], [Ref i] else a,b
  | t -> failwith ("split_variants: " ^ LCGstringOf.linear_term 0 t)

let rec split_args = function
    Tuple l ->
      let lemmas,others = Xlist.fold l ([],[]) (fun (lemmas,others) t ->
        let a,b = split_args t in
        a @ lemmas, b :: others) in
      lemmas, Tuple(List.rev others)
  | Node t -> [t],Dot
  | Ref i -> [], Ref i
  | Variant(e,l) -> [], Variant(e,l)
  | Dot -> [], Dot
  | t -> failwith ("split_args: " ^ LCGstringOf.linear_term 0 t)

let convert_adj_form lemma suf =
  if lemma = "ten" then match suf with
    "a" -> "ta"
  | "e" -> "te"
  | "i" -> "ci"
  | "o" -> "to"
  | _ -> failwith "convert_adj_form" else
  if lemma = "jeden" then match suf with
    "a" -> "jedna"
  | "e" -> "jedne"
  | "i" -> "jedni"
  | "o" -> "jedno"
  | _ -> failwith "convert_adj_form" else
  if lemma = "żaden" then match suf with
    "a" -> "żadna"
  | "e" -> "żadne"
  | "i" -> "żadni"
  | "o" -> "żadno"
  | _ -> failwith "convert_adj_form" else
  if lemma = "sam" then match suf with
    "a" -> "sama"
  | "e" -> "same"
  | "i" -> "sami"
  | "o" -> "samo"
  | _ -> failwith "convert_adj_form" else
  if lemma = "wszystek" then match suf with
    "a" -> "wszystka"
  | "e" -> "wszystkie"
  | "i" -> "wszyscy"
  | "o" -> "wszystko"
  | _ -> failwith "convert_adj_form" else
  let l = match suf, List.rev (Xunicode.utf8_chars_of_utf8_string lemma) with
    "a", "y" :: l               -> "a" :: l
  | "a", "i" :: "g" :: l        -> "a" :: "g" :: l
  | "a", "i" :: "k" :: l        -> "a" :: "k" :: l
  | "a", "i" :: "l" :: l        -> "a" :: "l" :: l
  | "a", "i" :: "o" :: l        -> "a" :: "j" :: "o" :: l
  | "a", "i" :: "y" :: l        -> "a" :: "j" :: "y" :: l
  | "a", "i" :: "e" :: l        -> "a" :: "j" :: "e" :: l
  | "a", "i" :: "u" :: l        -> "a" :: "j" :: "u" :: l
  | "a", "i" :: "a" :: l        -> "a" :: "j" :: "a" :: l
  | "a", "i" :: l               -> "a" :: "i" :: l
  | "e", "y" :: l               -> "e" :: l
  (* | "e", "i" :: "g" :: l -> "e" :: "i" :: "g" :: l
  | "e", "i" :: "k" :: l -> "e" :: "i" :: "k" :: l *)
  | "e", "i" :: "l" :: l        -> "e" :: "l" :: l
  | "e", "i" :: "o" :: l        -> "e" :: "j" :: "o" :: l
  | "e", "i" :: "y" :: l        -> "e" :: "j" :: "y" :: l
  | "e", "i" :: "e" :: l        -> "e" :: "j" :: "e" :: l
  | "e", "i" :: "u" :: l        -> "e" :: "j" :: "u" :: l
  | "e", "i" :: "a" :: l        -> "e" :: "j" :: "a" :: l
  | "e", "i" :: l               -> "e" :: "i" :: l
  | "i", "y" :: "b" :: l        -> "i" :: "b" :: l
  | "i", "y" :: "h" :: "c" :: l -> "i" :: "s" :: l
  | "i", "y" :: "d" :: l        -> "i" :: "z" :: "d" :: l
  | "i", "y" :: "f" :: l        -> "i" :: "f" :: l
  | "i", "y" :: "h" :: l        -> "i" :: "z" :: l
  | "i", "y" :: "ł" :: l        -> "i" :: "l" :: l
  | "i", "y" :: "m" :: l        -> "i" :: "m" :: l
  | "i", "y" :: "n" :: l        -> "i" :: "n" :: l
  | "i", "y" :: "p" :: l        -> "i" :: "p" :: l
  | "i", "y" :: "r" :: l        -> "y" :: "z" :: "r" :: l
  | "i", "y" :: "s" :: l        -> "i" :: "s" :: l
  | "i", "y" :: "t" :: l        -> "i" :: "c" :: l
  | "i", "y" :: "v" :: l        -> "i" :: "v" :: l
  | "i", "y" :: "w" :: l        -> "i" :: "w" :: l
  | "i", "y" :: "z" :: "c" :: l -> "y" :: "z" :: "c" :: l
  | "i", "y" :: "z" :: "d" :: l -> "y" :: "z" :: "d" :: l
  | "i", "y" :: "z" :: "r" :: l -> "y" :: "z" :: "r" :: l
  | "i", "y" :: "z" :: "s" :: l -> "y" :: "z" :: "s" :: l
  | "i", "y" :: "z" :: l        -> "i" :: "z" :: l
  | "i", "y" :: "c" :: l        -> "y" :: "c" :: l
  | "i", "y" :: "ż" :: l        -> "y" :: "ż" :: l
  | "i", "i" :: "g" :: l        -> "y" :: "z" :: "d" :: l
  | "i", "i" :: "k" :: l        -> "y" :: "c" :: l
  | "i", "i" :: l               -> "i" :: l
  | "o", "y" :: l               -> "o" :: l
  | "o", "i" :: "g" :: l        -> "o" :: "g" :: l
  | "o", "i" :: "k" :: l        -> "o" :: "k" :: l
  | "o", "i" :: "l" :: l        -> "o" :: "l" :: l
  | "o", "i" :: "o" :: l        -> "o" :: "j" :: "o" :: l
  | "o", "i" :: "y" :: l        -> "o" :: "j" :: "y" :: l
  | "o", "i" :: "e" :: l        -> "o" :: "j" :: "e" :: l
  | "o", "i" :: "u" :: l        -> "o" :: "j" :: "u" :: l
  | "o", "i" :: "a" :: l        -> "o" :: "j" :: "a" :: l
  | "o", "i" :: l               -> "o" :: "i" :: l
  | _ -> failwith ("convert_adj_form: " ^ lemma) in
  String.concat "" (List.rev l)

let convert_adj_form_gender lemma = function
            "m" -> lemma
          | "f" -> convert_adj_form lemma "a"
          | "n" | "nmo" -> convert_adj_form lemma "e"
          | "mo" -> convert_adj_form lemma "i"
          | _ -> failwith "convert_adj_form_gender"

let process_arg head_pos head_beg head_gend beg t =
  let prec = if beg > head_beg then 1 else -1 in
  match head_pos with
    "subst" ->
      let lemmata =
        if t.pos = "adj" then
          Xlist.rev_map head_gend (function
            "m" -> t.lemma
          | "f" -> convert_adj_form t.lemma "a"
          | "n" | "nmo" -> convert_adj_form t.lemma "e"
          | "mo" -> convert_adj_form t.lemma "i"
          | _ -> failwith "process_arg")
        else [t.lemma] in
      prec,beg,lemmata
  | "fin" | "praet" | "inf" | "impt" | "imps" | "winien" ->
      let prec = if t.lemma = "się" then 1 else prec in
      prec,beg,[t.lemma]
  | "symbol" ->
      let prec = if t.lemma = "rok" then 1 else failwith ("process_arg symbol: " ^ t.lemma) in
      prec,beg,["roku"]
  | "interp" ->
      let lemmata =
        if t.pos = "adja" then [convert_adj_form t.lemma "o"]
        else [t.lemma] in
      prec,beg,lemmata
  | "adj" -> prec,beg,[t.lemma]
  | _ -> failwith ("process_arg: " ^ head_pos)

let rec convert_gender_pl set = function
    Variant(e,l) -> Xlist.fold l set (fun set (_,t) -> convert_gender_pl set t)
  | Val "m1" -> StringSet.add set "mo"
  | Val "m2" -> StringSet.add set "nmo"
  | Val "m3" -> StringSet.add set "nmo"
  | Val "n" -> StringSet.add set "nmo"
  | Val "f" -> StringSet.add set "nmo"
  | _ -> failwith "convert_gender_pl"

let rec convert_gender_sg set = function
    Variant(e,l) -> Xlist.fold l set (fun set (_,t) -> convert_gender_sg set t)
  | Val "m1" -> StringSet.add set "m"
  | Val "m2" -> StringSet.add set "m"
  | Val "m3" -> StringSet.add set "m"
  | Val "n" -> StringSet.add set "n"
  | Val "f" -> StringSet.add set "f"
  | Val s -> failwith ("convert_gender_sg: " ^ s)
  | _ -> failwith "convert_gender_sg"

let extract_genders t =
  if t.pos <> "subst" && t.pos <> "depr" then [] else
  let gend = fst (extract_attr "GEND" [] t.attrs) in
  match fst (extract_attr "PT" [] t.attrs) with
    Val "pt" -> StringSet.to_list (convert_gender_pl StringSet.empty gend)
  | Val "npt" -> StringSet.to_list (convert_gender_sg StringSet.empty gend)
  | _ -> failwith "extract_genders"

let rec insert_spaces = function
   a :: b :: l ->
    if Xstring.check_sufix "-" a || Xstring.check_prefix "-" b then
      a :: (insert_spaces (b :: l))
    else a :: " " :: (insert_spaces (b :: l))
  | [a] -> [a]
  | [] -> []

let create_mwe_lemma tokens t args =
  (* print_endline "create_mwe_lemma"; *)
  let lemmas,args = split_args args in
  let args = Tuple(args :: Xlist.rev_map lemmas (fun t -> t.args)) in
  let head_pos = t.pos in
  let head_gend = extract_genders t in
  let head_beg = (ExtArray.get tokens t.id).SubsyntaxTypes.beg in
  let lemmas = Xlist.rev_map lemmas (fun t ->
    process_arg head_pos head_beg head_gend (ExtArray.get tokens t.id).SubsyntaxTypes.beg t) in
  let lemmas = Xlist.sort ((0,head_beg,[t.lemma]) :: lemmas) (fun (prec1,beg1,_) (prec2,beg2,_) ->
    if prec1 <> prec2 then compare prec1 prec2 else compare beg1 beg2) in
  let ll = Xlist.multiply_list (List.rev (Xlist.rev_map lemmas (fun (_,_,l) -> l))) in
  Xlist.rev_map ll (fun l -> {t with lemma=String.concat "" (insert_spaces l); args=args})

let rec shift_lemma_variants_rec tokens tree visited = function
    Node t ->
      (* print_endline ("shift_lemma_variants_rec: " ^ LCGstringOf.linear_term 0 (Node t)); *)
      let args = shift_lemma_variants_rec tokens tree visited t.args in
      (* Printf.printf "shift_lemma_variants_rec: args=%s\n%!" (LCGstringOf.linear_term 0 args); *)
      let lemmas,others = split_variants tree args in
      (* Printf.printf "shift_lemma_variants_rec: %s |lemmas|=%d |others|=%d\n%!" t.lemma (Xlist.size lemmas) (Xlist.size others); *)
      let l = Xlist.rev_map others (fun args -> Node{t with args=args}) in
      let l = Xlist.fold lemmas l (fun l args ->
        Xlist.fold (create_mwe_lemma tokens t args) l (fun l t -> Node t :: l)) in
      if l = [] then failwith "shift_lemma_variants_rec: empty" else
      make_variant l
  | Tuple l ->
      (* print_endline "shift_lemma_variants_rec Tuple"; *)
      Tuple(List.rev (Xlist.rev_map l (shift_lemma_variants_rec tokens tree visited)))
  | Variant(e,l) ->
      (* print_endline "shift_lemma_variants_rec Variant"; *)
      Variant(e,List.rev (Xlist.rev_map l (fun (i,t) ->
      i, shift_lemma_variants_rec tokens tree visited t)))
  | Dot -> Dot
  | Ref i ->
      (* print_endline "shift_lemma_variants_rec Ref"; *)
      if visited.(i) then Ref i else
      let t = shift_lemma_variants_rec tokens tree visited tree.(i) in
      tree.(i) <- t;
      Ref i
  | t -> failwith ("shift_lemma_variants_rec: " ^ LCGstringOf.linear_term 0 t)

let shift_lemma_variants tokens tree =
  (* print_endline "shift_lemma_variants"; *)
  let visited = Array.make (Array.length tree) false in
  let _ = shift_lemma_variants_rec tokens tree visited tree.(0) in
  LCGreductions.reshape_dependency_tree (ExtArray.of_array tree Dot)


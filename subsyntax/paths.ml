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

open Xstd
open TokenizerTypes

(* let to_string_indexed (paths,last) =
  String.concat "\n" (Xlist.map paths (fun (i,j,t) ->
    Printf.sprintf "%2d %2d %s" i j (Tokens.string_of_tokens 0 (Token t))))
  ^ Printf.sprintf "\nlast=%d" last *)

(*let indexed_token_record_to_xml i j t =
  let lemma,pos,tags =
    match t.token with
      Lemma(lemma,pos,tags) -> lemma,pos,tags
    | _ -> failwith "indexed_token_record_to_xml" in
  Xml.Element("token",["i",string_of_int i;"j",string_of_int j;
                       "beg",string_of_int t.beg;"len",string_of_int t.len;"weight",string_of_float t.weight],[
    Xml.Element("orth",[],[Xml.PCData t.orth]);
    Xml.Element("lemma",[],[Xml.PCData lemma]);
    Xml.Element("pos",[],[Xml.PCData pos]);
    Xml.Element("tags",[],Xlist.map tags (fun l ->
      Xml.Element("variant",[],[Xml.PCData (String.concat ":" (Xlist.map l (fun l2 -> String.concat "." l2)))])));
    Xml.Element("valence",[],Xlist.map t.valence WalXmlOf.num_frame);
    Xml.Element("senses",[],
      Xlist.map t.senses (fun (sense,hipero,weight) ->
        Xml.Element("sense",["name",sense;"weight",string_of_float weight],
          Xlist.map hipero (fun s -> Xml.Element("hipero",[],[Xml.PCData s])))))])

let to_xml (paths,last) =
  Xml.Element("paths",["last",string_of_int last],
    Xlist.map paths (fun (i,j,t) -> indexed_token_record_to_xml i j t))  *)

let compare_token_record p r =
  let v = compare p.beg r.beg in
  if v <> 0 then v else
  let v = compare p.next r.next in
  if v <> 0 then v else
  compare p r

let sort (paths,last) =
  Xlist.sort paths compare_token_record, last

let rec uniq_rec rev = function
    [] -> List.rev rev
  | [p] -> List.rev (p :: rev)
  | p :: r :: l -> 
(*      Printf.printf "uniq_rec 1: %s\n" (Tokens.string_of_token_env p);
      Printf.printf "uniq_rec 2: %s\n" (Tokens.string_of_token_env r);
      if p = r then Printf.printf "uniq_rec eq %d\n" (compare p r) else Printf.printf "uniq_rec neq %d\n" (compare p r);*)
      if p = r then uniq_rec rev (r :: l) else uniq_rec (p :: rev) (r :: l)

let uniq (paths,last) =
  uniq_rec [] paths, last

let rec translate_into_paths_rec paths = function
    Token t -> t :: paths
  | Seq l -> Xlist.fold l paths translate_into_paths_rec
  | Variant l -> Xlist.fold l paths translate_into_paths_rec

let translate_into_paths tokens =
  let paths = Xlist.fold tokens [] (fun paths token ->
    translate_into_paths_rec paths token) in
  let last = if paths = [] then 0 else (List.hd paths).next in
  let paths = sort (paths,last) in
  let paths = uniq paths in
  paths

let remove_inaccessible_tokens paths beg last =
  let set = Xlist.fold paths (IntSet.singleton beg) (fun set t ->
    if IntSet.mem set t.beg then IntSet.add set t.next else set) in
  if not (IntSet.mem set last) then raise (SubsyntaxTypes.BrokenPaths(beg,last,IntSet.max_elt set,paths)) else
  Xlist.fold paths [] (fun paths t ->
    if IntSet.mem set t.beg then t :: paths else paths)

let remove_category cat paths =
  List.rev (Xlist.fold paths [] (fun paths t ->
    if Tokens.get_cat t.token = cat then paths else t :: paths))
  

(**********************************************************************************)

let create_sentence_end_beg i len next orth =
  [{empty_token_env with beg=i;len=20;next=i+20;token=Interp "</clause>"(*;cat="Interp"*)};
   {empty_token_env with orth=orth;beg=i+20;len=20;next=i+40;token=Interp "</sentence>"(*;cat="Interp"*)};
   {empty_token_env with beg=i+40;len=20;next=i+60;token=Interp "<sentence>"(*;cat="Interp"*)};
   {empty_token_env with beg=i+60;len=len-60;next=next;token=Interp "<clause>"(*;cat="Interp"*)}]

let create_clause_end_beg i len next orth =
  [{empty_token_env with beg=i;len=60;next=i+60;token=Interp "</clause>"(*;cat="Interp"*)};
   {empty_token_env with beg=i+60;len=len-60;next=next;token=Interp "<clause>"(*;cat="Interp"*)}]

let process_interpunction_token beg next t = 
  if t.beg = beg then 
    if t.next = next then [
      {empty_token_env with beg=t.beg;len=20;next=t.beg+20;token=Interp "<sentence>"(*;cat="Interp"*)};
      {empty_token_env with beg=t.beg+20;len=20;next=t.beg+40;token=Interp "<clause>"(*;cat="Interp"*)};
      {t with beg=t.beg+40;len=t.len-80;next=t.beg+t.len-40};
      {empty_token_env with beg=t.beg+t.len-40;len=20;next=t.beg+t.len-20;token=Interp "</clause>"(*;cat="Interp"*)};
      {empty_token_env with beg=t.beg+t.len-20;len=20;next=t.next;token=Interp "</sentence>"(*;cat="Interp"*)}]
    else [
      {empty_token_env with beg=t.beg;len=20;next=t.beg+20;token=Interp "<sentence>"(*;cat="Interp"*)};
      {empty_token_env with beg=t.beg+20;len=20;next=t.beg+40;token=Interp "<clause>"(*;cat="Interp"*)};
      {t with beg=t.beg+40;len=60;next=t.next}]
  else 
    if t.next = next then match t.token with
        Interp "." -> [
            {empty_token_env with beg=t.beg;len=20;next=t.beg+20;token=Interp "</clause>"(*;cat="Interp"*)};
            {t with beg=t.beg+20;len=t.len-20;token=Interp "</sentence>"(*;cat="Interp"*)}]
      | _ -> [
            {t with len=t.len-40;next=t.beg+t.len-40};
            {empty_token_env with beg=t.beg+t.len-40;len=20;next=t.beg+t.len-20;token=Interp "</clause>"(*;cat="Interp"*)};
            {empty_token_env with beg=t.beg+t.len-20;len=20;next=t.next;token=Interp "</sentence>"(*;cat="Interp"*)}]
    else match t.token with 
        Interp "." -> t :: (create_sentence_end_beg t.beg t.len t.next t.orth)
      | Lemma(",","conj",[[]],_) -> t :: (create_clause_end_beg t.beg t.len t.next t.orth)
      | Interp ":" -> t :: (create_clause_end_beg t.beg t.len t.next t.orth) @ (create_sentence_end_beg t.beg t.len t.next t.orth)
      | Interp ";" -> t :: (create_sentence_end_beg t.beg t.len t.next t.orth)
      | Interp "¶" -> t :: (create_clause_end_beg t.beg t.len t.next t.orth) @ (create_sentence_end_beg t.beg t.len t.next t.orth)
      | _ -> [t]

let rec process_interpunction beg next paths = 
  List.flatten (List.rev (Xlist.rev_map paths (fun t -> 
    process_interpunction_token beg next t)))

(**********************************************************************************)

(* Korzystamy z tego, że istnieje wierzchołem najmniejszy i największy *)
let rec biconnected_compontents_rec next found rev = function
    [] -> if rev = [] then found else ((*List.rev*) rev) :: found
  | t :: paths -> 
      if t.beg > next then failwith "biconnected_compontents_rec" else
      if t.beg = next then  
        biconnected_compontents_rec t.next (if rev = [] then found else ((*List.rev*) rev) :: found) [t] paths else
      biconnected_compontents_rec (max next t.next) found (t :: rev) paths

let biconnected_compontents paths =
  List.rev (biconnected_compontents_rec 0 [] [] paths)

(**********************************************************************************)


(* FIXME: dodać 'co do' prep:gen *)


(* Dane do przekazania:
- lematy i interpretacje: generowanie typów i termów
- orths
- początki i długości: decydują o wyświetlaniu struktury składnikowej (zwłaszcza niejednoznacznej)
- struktura grafu: wyróżniki przy niejednoznaczności
- sensy wraz z hiperonimami
- <indent> *)

(*
Ala zjadła kota.
Ala subst:sg:nom:f imię -> istota
kot subst:sg:nom:m2 pospolita - kot 2 - istota 1 istota żywa 1 zwierzę 1 strunowiec 1 czaszkowiec 1 kręgowiec 1 tetrapod 1 owodniowiec 1 ssak 1 ssak żyworodny 1 łożyskowiec 1 ssak drapieżny 1 kot 1 kot 2
zjeść - zjeść 1 - CZASOWNIK 1 CZASOWNIK należący do określonego pola leksykalnego 1 CZASOWNIK oznaczający sytuację związaną z reakcją organizmu lub czynnością fizjologiczną 1 CZASOWNIK - AKT oznaczający reakcję organizmu lub czynność fizjologiczne 1 zjeść 1

Ala -> common("imię")
Ala -> proper("istota")
kot -> common("kot 2")

czas 3 doba 1=dzień 2
czas 3 miesiąc 1
czas 3 rok 1 rok 2

czas 3 termin 1 dzień 3
data 1=termin 1

czas 3 pora roku 1 lato 1

punkt lub odcinek czasu w obrębie doby, określany na podstawie wskazań zegara
"o godzinie 15:20."
czas 3 godzina 4

do opisu czasu trwania:
jednostka czasu 1: godzina 3, sekunda 2, (minuta 4 - nie podłączona) dzień 2, miesiąc 1, rok 1/2

*)

(**

(* empty *)

let empty = IntMap.empty, 0, 0

let dict_empty = {lemmas=StringMap.empty; dbeg=0-1; dlen=0-1}

let poss_record_empty = {interp=[]; attrs=[]; proper=[]; senses=[]}

(* add *)

let dict_add dict lemma postags attrs beg len =
  if postags = [] then dict else
  let interps = try StringMap.find dict.lemmas lemma with Not_found -> StringMap.empty in
  let interps = Xlist.fold postags interps (fun interps (pos,tags) ->
    StringMap.add_inc interps pos {poss_record_empty with interp=[tags]; attrs=attrs} (fun l ->
      {l with interp=tags :: l.interp; attrs=StringSet.to_list (StringSet.union (StringSet.of_list l.attrs) (StringSet.of_list attrs))})) in
  if dict.dbeg <> beg && dict.dbeg <> -1 then failwith "dict_add" else
  if dict.dlen <> len && dict.dlen <> -1 then failwith "dict_add" else
  {lemmas=StringMap.add dict.lemmas lemma interps; dbeg=beg; dlen=len}

let add_simple map i j orth lemma postags attrs beg len =
  let map2 = try IntMap.find map i with Not_found -> IntMap.empty in
  let orths = try IntMap.find map2 j with Not_found -> StringMap.empty in
  let dict = try StringMap.find orths orth with Not_found -> dict_empty in
  let dict = dict_add dict lemma postags attrs beg len in
  let orths = StringMap.add orths orth dict in
  let map2 = IntMap.add map2 j orths in
  IntMap.add map i map2

let add_edge (map,last,n) i j orth lemma postags attrs beg len =
  add_simple map i j orth lemma postags attrs beg len, max j last, max j n

let rec add_path (map,last,n) i j = function
    [] -> failwith "add_path"
  | [orth,lemma,postags,beg,len] ->
     add_simple map i j orth lemma postags [] beg len, last, n
  | (orth,lemma,postags,beg,len) :: l ->
     add_path (add_simple map i (n+1) orth lemma postags [] beg len, last, n+1) (n+1) j l
(*
let insert (map,last,n) i j orth dict =
  let map2 = try IntMap.find map i with Not_found -> IntMap.empty in
  let orths = try IntMap.find map2 j with Not_found -> StringMap.empty in
  let orths = StringMap.add orths orth dict in
  let map2 = IntMap.add map2 j orths in
  IntMap.add map i map2, last, n

let rec insert_path (map,last,n) i j = function
    [] -> failwith "add_path"
  | [orth,dict] ->
     insert (map,last,n) i j orth dict
  | (orth,dict) :: l ->
     insert_path (insert (map,last,n+1) i (n+1) orth dict) (n+1) j l

let set_sentence_begin (map,last,n) i j orth =
  try
    let map2 = IntMap.find map i in
    let orths = IntMap.find map2 j in
    let dict = StringMap.find orths orth in
    let orths = StringMap.add orths orth {dict with sentence_begin=true} in
    let map2 = IntMap.add map2 j orths in
    IntMap.add map i map2, last, n
  with Not_found -> failwith "set_sentence_begin"

let set_sentence_end (map,last,n) i j orth =
  try
    let map2 = IntMap.find map i in
    let orths = IntMap.find map2 j in
    let dict = StringMap.find orths orth in
    let orths = StringMap.add orths orth {dict with sentence_end=true} in
    let map2 = IntMap.add map2 j orths in
    IntMap.add map i map2, last, n
  with Not_found -> failwith "set_sentence_end"

let is_sentence_end (map,last,n) i j orth =
  try
    let map2 = IntMap.find map i in
    let orths = IntMap.find map2 j in
    let dict = StringMap.find orths orth in
    dict.sentence_end
  with Not_found -> failwith "is_sentence_end"

let manage_sentence_end (map,last,n) =
  IntMap.map map (fun map2 ->
    IntMap.map map2 (fun orths ->
      StringMap.fold orths StringMap.empty (fun orths orth dict ->
        if orth = ".last_node" then StringMap.add orths "." {dict with sentence_end=true}
        else StringMap.add orths orth dict))),last,n

(* other *)

let remove (map,last,n) i j orth =
  try
    let map2 = IntMap.find map i in
    let orths = IntMap.find map2 j in
    let orths = StringMap.remove orths orth in
    let map2 = if StringMap.is_empty orths then IntMap.remove map2 j else IntMap.add map2 j orths in
    (if IntMap.is_empty map2 then IntMap.remove map i else IntMap.add map i map2), last, n
  with Not_found -> map,last,n


let rec find_paths_bound (map,last,n) k i =
  if i = last || k = 0 then [[]] else
  if not (IntMap.mem map i) then failwith "find_paths_bound" else
  IntMap.fold (IntMap.find map i) [] (fun paths j set ->
    let tails = find_paths_bound (map,last,n) (k-1) j in
    StringMap.fold set paths (fun paths s _ ->
      Xlist.fold tails paths (fun paths tail -> (s :: tail) :: paths)))

let rec find_paths_rec (map,last,n) i =
  if i = last then [[]] else
  if not (IntMap.mem map i) then failwith "find_paths_rec" else
  IntMap.fold (IntMap.find map i) [] (fun paths j set ->
    let tails = find_paths_rec (map,last,n) j in
    StringMap.fold set paths (fun paths s _ ->
      Xlist.fold tails paths (fun paths tail -> (s :: tail) :: paths)))

let find_paths (map,last,n) =
  find_paths_rec (map,last,n) 0
*)
let has_lemma orths =
  StringMap.fold orths false (fun b _ dict ->
    if StringMap.is_empty dict.lemmas then b else true)

let rec no_possible_path_rec map last i =
  if last = i then false else
  let map2 = try IntMap.find map i with Not_found -> IntMap.empty in
  IntMap.fold map2 true (fun b j orths ->
    if has_lemma orths then
      b && no_possible_path_rec map last j
    else b)

let no_possible_path (map,last,n) =
  no_possible_path_rec map last 0
(*
let rec match_path_rec map found i rev = function
    [] -> (i :: rev) :: found
  | s :: l ->
     let map2 = try IntMap.find map i with Not_found -> IntMap.empty in
     let found2 = IntMap.fold map2 [] (fun found2 j set ->
       if StringMap.mem set s then j :: found2 else found2) in
     Xlist.fold found2 found (fun found j -> match_path_rec map found j (i :: rev) l)

let match_path (map,last,n) = function
    [] -> failwith "match_path"
  | s :: l ->
     let found = IntMap.fold map [] (fun found i map2 ->
       IntMap.fold map2 found (fun found j set ->
         if StringMap.mem set s then (i,j) :: found else found)) in
     Xlist.fold found [] (fun found (i,j) -> match_path_rec map found j [i] l)

let get_matched orths = function
    Orth s -> if StringMap.mem orths s then [s] else []
  | Pos s -> (*print_endline ("a1 " ^ s);*) StringSet.to_list (StringMap.fold orths StringSet.empty (fun set orth dict ->
      StringMap.fold dict.lemmas set (fun set lemma interps ->
        StringMap.fold interps set (fun set pos _ ->
(*           print_endline ("a2 " ^ pos);  *)
          if s = pos then StringSet.add set orth else set))))
(*   | All -> orths *)

let rec match_path_ex_rec map found i rev = function
    [] -> ((i,[]) :: rev) :: found
  | s :: l ->
     let map2 = try IntMap.find map i with Not_found -> IntMap.empty in
     let found2 = IntMap.fold map2 [] (fun found2 j orths ->
       let l = get_matched orths s in
       if l <> [] then (j,l) :: found2 else found2) in
     Xlist.fold found2 found (fun found (j,l2) -> match_path_ex_rec map found j ((i,l2) :: rev) l)

let match_path_ex (map,last,n) = function
    [] -> failwith "match_path_ex"
  | s :: l ->
     let found = IntMap.fold map [] (fun found i map2 ->
       IntMap.fold map2 found (fun found j orths ->
       let l = get_matched orths s in
       if l <> [] then (i,j,l) :: found else found)) in
     Xlist.fold found [] (fun found (i,j,l2) -> (*print_endline ("b1 " );*) match_path_ex_rec map found j [i,l2] l)

let last_node (_,last,_) = last

let set_last_node (map,last,n) new_last = map, new_last, n

let find (map,last,n) i =
  try
    IntMap.fold (IntMap.find map i) [] (fun found j orths ->
      StringMap.fold orths found (fun found orth _ ->
        (i,j,orth) :: found))
  with Not_found -> []

let find_full (map,last,n) i =
  try
    IntMap.fold (IntMap.find map i) [] (fun found j orths ->
      StringMap.fold orths found (fun found orth dict ->
        (i,j,orth,dict) :: found))
  with Not_found -> []
*)
let fold (map,last,n) s f =
  IntMap.fold map s (fun s i map2 ->
    IntMap.fold map2 s (fun s j set ->
      StringMap.fold set s (fun s orth lemmas ->
        f s orth i j lemmas)))
(*
let map (map,last,n) f =
  IntMap.map map (fun map2 ->
    IntMap.map map2 (fun orths ->
      StringMap.map orths (fun lemmas ->
        f lemmas))), last, n

let mapi (map,last,n) f =
  IntMap.mapi map (fun i map2 ->
    IntMap.mapi map2 (fun j orths ->
      StringMap.mapi orths (fun orth lemmas ->
        f orth i j lemmas))), last, n

let get_edges (map,_,_) i j =
  IntMap.find (IntMap.find map i) j

let get_edges_from (map,_,_) i =
  IntMap.find map i
*)
let rec topological_sort_rec map visited l i =
  if IntSet.mem visited i then (l,visited) else
  let l, visited = IntMap.fold (try IntMap.find map i with Not_found -> IntMap.empty) (l,IntSet.add visited i) (fun (l,visited) j _ ->
    topological_sort_rec map visited l j) in
  i :: l, visited

let topological_sort (map,last,n) =
  let l, _ = topological_sort_rec map IntSet.empty [] 0 in
  let translation, k = Xlist.fold l (IntMap.empty,0) (fun (translation,k) i ->
    IntMap.add translation i k, k+1) in
  let map = IntMap.fold map IntMap.empty (fun map i map2 ->
    let map2 = IntMap.fold map2 IntMap.empty (fun map2 j orths ->
      try IntMap.add map2 (IntMap.find translation j) orths with Not_found -> map2) in
    try IntMap.add map (IntMap.find translation i) map2 with Not_found -> map) in
  map, (try IntMap.find translation last with Not_found -> failwith "topological_sort 3"), k-1

(*let interp_to_string interp =
  String.concat " " (Xlist.fold interp.interp [] (fun l tags ->
    (String.concat ":" (Xlist.map tags (String.concat "."))) :: l))

let interps_to_string interps =
  String.concat " " (StringMap.fold interps [] (fun l pos interp ->
    (pos ^ "[" ^ interp_to_string interp ^ "]") :: l))

let lemmas_to_string lemmas =
  String.concat " " (StringMap.fold lemmas [] (fun l lemma interps ->
    (lemma ^ "[" ^ interps_to_string interps ^ "]") :: l))

let to_string (map,last,n) =
  let l = IntMap.fold map [] (fun l i map2 ->
    IntMap.fold map2 l (fun l j orths ->
      (Printf.sprintf "%5d %5d %s" i j (String.concat " " (StringMap.fold orths [] (fun l2 orths dict ->
         (Printf.sprintf "%s %5d %5d [%s]" orths dict.dbeg dict.dlen (lemmas_to_string dict.lemmas)) :: l2)))) :: l)) in
  Printf.sprintf "last=%d n=%d\n  %s" last n (String.concat "\n  " (List.sort compare l))*)
    (*
let make_unique_orths (map,last,n) =
  let names = fold (map,last,n) StringQMap.empty (fun names orth _ _ _ ->
    StringQMap.add names orth) in
  let names = StringQMap.fold names StringSet.empty (fun names name n ->
    if n = 1 (*|| name = "."*) then names else StringSet.add names name) in (* FIXME: trzeba dodać usuwanie wszystkich orth zdefiniowanych w leksykonach POLFIE *)
  let map,_ = IntMap.fold map (IntMap.empty,StringMap.empty) (fun (map,used) i map2 ->
    let map2,used = IntMap.fold map2 (IntMap.empty,used) (fun (map2,used) j orths ->
      let orths,used = StringMap.fold orths (StringMap.empty,used) (fun (orths,used) orth lemmas ->
        let orth,used =
          if StringSet.mem names orth then
            let n =
              try StringMap.find used orth + 1
              with Not_found -> 1 in
            orth ^ "-" ^ string_of_int n, StringMap.add used orth n
          else orth,used in
        StringMap.add orths orth lemmas, used) in
      IntMap.add map2 j orths, used) in
    IntMap.add map i map2, used) in
  map,last,n

*)

**)
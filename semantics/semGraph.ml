(*
 *  ENIAMsemantics implements semantic processing for ENIAM
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

open SemTypes
open Xstd
open Printf

let make_tuple = function
    [] -> Dot
  | [t] -> t
  | l -> Tuple l

let rec make_tree_rec references = function
    Node t -> Node{t with args=make_tree_rec references t.args}
  | Concept c -> Concept{c with relations=make_tree_rec references c.relations; contents=make_tree_rec references c.contents}
(*   | Context c -> Context{c with cx_contents=make_tree_rec references c.cx_contents; cx_relations=make_tree_rec references c.cx_relations} *)
  | Relation(r,a,t) -> Relation(r,a,make_tree_rec references t)
  | RevRelation(r,a,t) -> RevRelation(r,a,make_tree_rec references t)
  | SingleRelation r  -> SingleRelation r
  (* | TripleRelation(r,a,s,t) -> TripleRelation(r,a,make_tree_rec references s,make_tree_rec references t) *)
  | AddRelation(t,r,a,s) -> AddRelation(make_tree_rec references t,r,a,make_tree_rec references s)
  | AddParentRelation(t,s) -> AddParentRelation(make_tree_rec references t,make_tree_rec references s)
  | AddSingleRelation(r,s) -> AddSingleRelation(r,make_tree_rec references s)
  | RemoveRelation(r,a,t) -> RemoveRelation(r,a,make_tree_rec references t)
  | SetContextName(s,t) -> SetContextName(s,make_tree_rec references t)
  | CreateContext(s,t) -> CreateContext(s,make_tree_rec references t)
  (* | MakeTripleRelation(r,a,t) -> MakeTripleRelation(r,a,make_tree_rec references t) *)
  | ManageCoordination(n,t) -> ManageCoordination(n,make_tree_rec references t)
  | Tuple l -> Tuple(Xlist.map l (make_tree_rec references))
  | Variant(e,l) -> Variant(e,Xlist.map l (fun (i,t) -> i, make_tree_rec references t))
  | Dot -> Dot
  | Val s -> Val s
  | Ref i -> make_tree_rec references references.(i)
  (* | t -> failwith ("make_tree_rec: " ^ LCGstringOf.linear_term 0 t) *)

let make_tree references =
  (*RemoveRelation*)(make_tree_rec references references.(0))

let rec validate_translation r = function
    Node t ->
      r := ("validate_translation: " ^ SemStringOf.linear_term 0 (Node{t with args=Dot})) :: !r;
      validate_translation r t.args
  | Concept c -> validate_translation r c.relations; validate_translation r c.contents
(*   | Context c -> validate_translation r c.cx_contents; validate_translation r c.cx_relations *)
  | Relation(_,_,t) -> validate_translation r t
  | RevRelation(_,_,t) -> validate_translation r t
  | SingleRelation _  -> ()
  (* | TripleRelation(_,_,s,t) -> validate_translation r s; validate_translation r t *)
  | AddRelation(t,_,_,s) -> validate_translation r t; validate_translation r s
  | AddParentRelation(t,s) -> validate_translation r t; validate_translation r s
  | AddSingleRelation(_,s) -> validate_translation r s
  | RemoveRelation(_,_,t) -> validate_translation r t
  | SetContextName(s,t) -> validate_translation r t
  | CreateContext(s,t) -> validate_translation r t
  (* | MakeTripleRelation(_,_,t) -> validate_translation r t *)
  | ManageCoordination(_,t) -> validate_translation r t
  | Tuple l -> Xlist.iter l (validate_translation r)
  | Variant(e,l) ->
      if e = "" then r := "validate_translation: empty variant label" :: !r;
      Xlist.iter l (fun (i,t) -> validate_translation r t)
  | Dot -> ()
  | t -> failwith ("validate_translation: " ^ SemStringOf.linear_term 0 t)

(***************************************************************************************)

let rec simplify_tree_add_relation r a s = function
    Concept c -> Concept{c with relations=Tuple[Relation(r,a,s);c.relations]}
(*   | Context c -> Context{c with cx_relations=Tuple[Relation(r,a,s);c.cx_relations]} *)
  | Variant(e,l) -> Variant(e,Xlist.map l (fun (i,t) -> i, simplify_tree_add_relation r a s t))
  | t -> AddRelation(t,r,a,s)

let rec transpose_tuple_variant e ll =
  match List.hd ll with
    _,[] -> []
  | _ ->
     let hd,tl = Xlist.fold ll ([],[]) (fun (hd,tl) (i,l) ->
       (i,List.hd l) :: hd, (i,List.tl l) :: tl) in
     (Variant (e,List.rev hd)) :: (transpose_tuple_variant e (List.rev tl))

(* FIXME TODO:
Bryka chmara wieczorów: problem z wyborem relacji
uzgadnianie preferencji i role tematyczne przy num, measure i prep:nosem
Witold bryka.: dezambiguacja
Niearanżowany szpak bryka.: lematyzacja 'Niearanżowany'

dobre:
Bryka na chmarze strusi.
Pięć strusi bryka.
*)

let rec is_core_variant = function
    Variant(e,l) -> Xlist.fold l true (fun b (_,t) -> is_core_variant t && b)
  | Relation("CORE","",_) -> true
  | Relation _ -> false
  | RevRelation _ -> false
  | SingleRelation _ -> false
  (* | TripleRelation("CORE","",_,_) -> true *)
  | Dot -> false
  | t -> failwith ("is_core_variant: " ^ SemStringOf.linear_term 0 t)

let get_core_tuple = function
    Tuple l ->
      let core,nocore = Xlist.fold l ([],[]) (fun (core,nocore) t ->
        if is_core_variant t then t :: core,nocore else core,t :: nocore) in
      (match core with
        [t] -> t
      | _ -> failwith "get_core_tuple"),
      (match nocore with
        [] -> Dot
      | [t] -> t
      | l -> Tuple l)
  | t -> if is_core_variant t then t,Dot else failwith ("get_core_tuple: " ^ SemStringOf.linear_term 0 t)

(* let get_core c =
  let core,l = get_core_tuple c.c_relations in
  core,{c with c_relations=l} *)

let set_aroles t r a b =
  if t.arole="" then {t with arole=r; arole_attr=a; arev=b} else
  if t.arole=r && t.arole_attr=a && t.arev=b then t else
  failwith ("set_aroles: t.arole=" ^ t.arole ^ " r=" ^ r)

let rec extract_aroles t = function
    Relation(r,a,s) -> set_aroles t r a false, s
  | RevRelation(r,a,s) -> set_aroles t r a true, s
  | Tuple l ->
      let t,l = Xlist.fold l (t,[]) (fun (t,l) s ->
        let t,s = extract_aroles t s in t, s :: l) in
      t,Tuple(List.rev l)
  | Variant(e,l) ->
      let t,l = Xlist.fold l (t,[]) (fun (t,l) (i,s) ->
        let t,s = extract_aroles t s in t, (i,s) :: l) in
      t,Variant(e,List.rev l)
  | Dot -> t,Dot
  | s -> failwith ("extract_aroles: " ^ SemStringOf.linear_term 0 s)

let rec reduce_tree = function
    Concept c -> Concept{c with relations=reduce_tree c.relations; contents=reduce_tree c.contents}
(*   | Context c -> Context{c with cx_contents=reduce_tree c.cx_contents; cx_relations=reduce_tree c.cx_relations} *)
  | Relation(r,a,t) ->
      (match reduce_tree t with
        AddParentRelation(x,Dot) -> x
      | AddParentRelation(x,y) -> Tuple[Relation(r,a,y);x]
      | t -> Relation(r,a,reduce_tree t))
  | RevRelation(r,a,t) -> RevRelation(r,a,reduce_tree t)
  | SingleRelation r  -> SingleRelation r
  (* | TripleRelation(r,a,s,t) -> TripleRelation(r,a,reduce_tree s,reduce_tree t) *)
(*  | AddRelation(Concept c,r,a,s) -> reduce_tree (Concept{c with c_relations=Tuple[Relation(Val r,Val a,s);c.c_relations]})
  | AddRelation(Context c,r,a,s) -> reduce_tree (Context{c with cx_relations=Tuple[Relation(Val r,Val a,s);c.cx_relations]})*)
  | AddSingleRelation(r,t) ->
      (match reduce_tree t with
        Concept t -> Concept{t with relations=Tuple[t.relations;SingleRelation r]}
(*      | Context({cx_sense=Val "czy"} as t) -> Context t
      | Context({cx_sense=Val "jaki"} as t) -> Context t
      | Context({cx_sense=Dot} as t) -> Context{t with cx_sense=Val "czy"}*)
      | Variant(e,l) -> Variant(e,Xlist.map l (fun (i,t) -> i, reduce_tree (AddSingleRelation(r,t))))
      | t -> AddSingleRelation(r,t))
  | AddRelation(t,r,a,s) -> simplify_tree_add_relation r a (reduce_tree s) (reduce_tree t)
(*      let t = reduce_tree t in
      let s = reduce_tree s in
      (match t with
        Concept c -> Concept{c with c_relations=Tuple[Relation(Val r,Val a,s);c.c_relations]}
      | Context c -> Context{c with cx_relations=Tuple[Relation(Val r,Val a,s);c.cx_relations]}
      | _ -> AddRelation(t,r,a,s))*)
  | AddParentRelation(t,s) -> AddParentRelation(reduce_tree t,reduce_tree s)
  | RemoveRelation(r0,a0,t) ->
      (match reduce_tree t with
        Relation("Arg","",t) when r0="Concept" && a0="" -> Concept{empty_concept with cat="Situation"; contents=t}(* FIXME: to jest obejście błędu *)
      | Relation(r,a,t) ->
            if (r = r0 && a = a0) || r0 = "" then t else
            Concept{empty_concept with cat="Situation"; contents=
              Concept{empty_concept with relations=Relation(r,a,t)}; (*cx_variable=string_of_int id,""; cx_pos=c.c_pos*)}
      (* | TripleRelation(r,a,s,t) ->
            Context{empty_context with cx_contents=
              Concept{empty_concept with c_relations=TripleRelation(r,a,s,t)}; (*cx_variable=string_of_int id,""; cx_pos=c.c_pos*)} *)
      | Dot -> Dot
      | Variant(e,l) -> reduce_tree (Variant(e,Xlist.map l (fun (i,t) -> i,RemoveRelation(r0,a0,t))))
      | Tuple l -> reduce_tree (Tuple(Xlist.map l (fun t -> RemoveRelation(r0,a0,t))))
      | Concept c -> Concept c (* FIXME: to jest obejście błędu *)
      (* | Context t -> Context t
      | Concept t -> Concept t *)
      | t -> RemoveRelation(r0,a0,t))
  | SetContextName(s,t) ->
      (match reduce_tree t with
        Concept({sense=""} as t) -> Concept{t with sense=s}
      | Variant(e,l) -> reduce_tree (Variant(e,Xlist.map l (fun (i,t) -> i,SetContextName(s,t))))
      | t ->  SetContextName(s,t))
  | CreateContext(c,t) ->
      (match reduce_tree t with
      | Variant(e,l) -> reduce_tree (Variant(e,Xlist.map l (fun (i,t) -> i, CreateContext(c,t))))
      | t ->
          let core,t = get_core_tuple t in
          Concept{c with relations=t; contents=reduce_tree (RemoveRelation("CORE","",core))})
(*  | MakeTripleRelation(r,a,t) ->
      (match reduce_tree t with
        Concept t ->
          let core,t = get_core t in
          TripleRelation(r,a,Concept t,reduce_tree (RemoveRelation("CORE","",core)))
      | Variant(e,l) -> reduce_tree (Variant(e,Xlist.map l (fun (i,t) -> i, MakeTripleRelation(r,a,t))))
      | t -> MakeTripleRelation(r,a,t))*)
  | ManageCoordination(t,c) ->
      (match reduce_tree c with
        Concept c ->
           let t,args = extract_aroles {t with arole=""} c.contents in
           (*make_relation t (Context {c with cx_contents=args})*) (* FIXME: to trzeba poprawić tak by działało w obu wersjach parserów *)
           Relation(t.role,"",Concept {c with contents=args})
      | Variant(e,l) -> reduce_tree (Variant(e,Xlist.map l (fun (i,c) -> i,ManageCoordination(t,c))))
      | c -> ManageCoordination(t,c))
  | Tuple l -> Tuple(List.rev (Xlist.rev_map l reduce_tree))
  | Variant(e,l) -> Variant(e,Xlist.map l (fun (i,t) -> i, reduce_tree t))
  | Dot -> Dot
  | Val s -> Val s
  | t -> failwith ("reduce_tree: " ^ SemStringOf.linear_term 0 t)

let rec validate_reduction r = function
    Concept c -> validate_reduction r c.relations; validate_reduction r c.contents
(*   | Context c -> validate_reduction r c.cx_contents; validate_reduction r c.cx_relations *)
  | Relation(_,_,t) -> validate_reduction r t
  | RevRelation(_,_,t) -> validate_reduction r t
  | SingleRelation _  -> ()
  (* | TripleRelation(_,_,s,t) -> validate_reduction r s; validate_reduction r t *)
  | Tuple l -> Xlist.iter l (validate_reduction r)
  | Variant(e,l) ->
      if e = "" then r := "validate_reduction: empty variant label" :: !r;
      Xlist.iter l (fun (i,t) -> validate_reduction r t)
  | Dot -> ()
  | t -> r := ("validate_reduction: " ^ SemStringOf.linear_term 0 t) :: !r

(***************************************************************************************)

let rec count_variant_labels map = function
    Concept c -> Xlist.fold (c.relations :: [c.contents]) map count_variant_labels
(*    Concept c -> Xlist.fold [c.c_sense; c.c_name; c.c_quant; c.c_cat; c.c_relations] map count_variant_labels
  | Context c -> Xlist.fold [c.cx_sense; c.cx_contents; c.cx_cat; c.cx_relations] map count_variant_labels*)
  | Relation(_,_,t) -> count_variant_labels map t
  | RevRelation(_,_,t) -> count_variant_labels map t
  | SingleRelation t  -> count_variant_labels map t
  | Tuple l -> Xlist.fold l map count_variant_labels
  | Variant(e,l) ->
      let map = StringQMap.add map e in
      Xlist.fold l map (fun map (i,t) -> count_variant_labels map t)
  | Dot -> map
  | Val s -> map
  | t -> failwith ("count_variant_labels: " ^ SemStringOf.linear_term 0 t)

let rec remove_variant_labels map = function
    Concept c -> Concept{c with
(*      c_sense=remove_variant_labels map c.c_sense;
      c_name=remove_variant_labels map c.c_name;
      c_quant=remove_variant_labels map c.c_quant;
      c_cat=remove_variant_labels map c.c_cat;*)
      contents=remove_variant_labels map c.contents;
      relations=remove_variant_labels map c.relations}
(*  | Context c -> Context{c with
      cx_sense=remove_variant_labels map c.cx_sense;
      cx_contents=remove_variant_labels map c.cx_contents;
      cx_cat=remove_variant_labels map c.cx_cat;
      cx_relations=remove_variant_labels map c.cx_relations}*)
  | Relation(r,a,t) -> Relation(r,a,remove_variant_labels map t)
  | RevRelation(r,a,t) -> RevRelation(r,a,remove_variant_labels map t)
  | SingleRelation r  -> SingleRelation r
  | Tuple l -> Tuple(List.rev (Xlist.rev_map l (remove_variant_labels map)))
  | Variant(e,l) ->
      let e = if StringQMap.find map e = 1 then "" else e in
      let l = Xlist.rev_map l (fun (i,t) -> i, remove_variant_labels map t) in
      Variant(e,Xlist.sort l (fun x y -> compare (fst x) (fst y)))
  | Dot -> Dot
  | Val s -> Val s
  | t -> failwith ("remove_variant_labels: " ^ SemStringOf.linear_term 0 t)

let rec set_variant_labels map = function
    Concept c -> Concept{c with
(*      c_sense=set_variant_labels map c.c_sense;
      c_name=set_variant_labels map c.c_name;
      c_quant=set_variant_labels map c.c_quant;
      c_cat=set_variant_labels map c.c_cat;*)
      contents=set_variant_labels map c.contents;
      relations=set_variant_labels map c.relations}
(*  | Context c -> Context{c with
      cx_sense=set_variant_labels map c.cx_sense;
      cx_contents=set_variant_labels map c.cx_contents;
      cx_cat=set_variant_labels map c.cx_cat;
      cx_relations=set_variant_labels map c.cx_relations}*)
  | Relation(r,a,t) -> Relation(r,a,set_variant_labels map t)
  | RevRelation(r,a,t) -> RevRelation(r,a,set_variant_labels map t)
  | SingleRelation r  -> SingleRelation r
  | Tuple l -> Tuple(List.rev (Xlist.rev_map l (set_variant_labels map)))
  | Variant(e,l) ->
      let e = try StringMap.find map e with Not_found -> LCGreductions.get_variant_label () in
      let l = Xlist.rev_map l (fun (i,t) -> i, set_variant_labels map t) in
      Variant(e,List.rev l)
  | Dot -> Dot
  | Val s -> Val s
  | t -> failwith ("set_variant_labels: " ^ SemStringOf.linear_term 0 t)

let manage_variant_labels t =
  LCGreductions.reset_variant_label ();
  let qmap = count_variant_labels StringQMap.empty t in
  let map = StringQMap.fold qmap StringMap.empty (fun map k _ ->
    if k = "" then map else
    StringMap.add map k (LCGreductions.get_variant_label ())) in
  set_variant_labels map t

let rec simplify_tree = function
    Concept c -> Concept{c with
(*      c_sense=simplify_tree c.c_sense;
      c_name=simplify_tree c.c_name;
      c_quant=simplify_tree c.c_quant;
      c_cat=simplify_tree c.c_cat;*)
      contents=simplify_tree c.contents;
      relations=simplify_tree c.relations}
(*  | Context c -> Context{c with
      cx_sense=simplify_tree c.cx_sense;
      cx_contents=simplify_tree c.cx_contents;
      cx_cat=simplify_tree c.cx_cat;
      cx_relations=simplify_tree c.cx_relations}*)
  | Relation(r,a,t) -> Relation(r,a,simplify_tree t)
  | RevRelation(r,a,t) -> RevRelation(r,a,simplify_tree t)
  | SingleRelation r  -> SingleRelation r
  (* | TripleRelation(r,a,s,t) -> TripleRelation(r,a,simplify_tree s,simplify_tree t) *)
  | Tuple l ->
      let l = Xlist.fold l [] (fun l t ->
        match simplify_tree t with
          Dot -> l
        | Tuple l2 -> l2 @ l
        | t -> t :: l) in
      make_tuple (List.rev l)
  | Variant(_,[_,t]) -> simplify_tree t
  | Variant(e,l) ->
      let l = Xlist.map l (fun (i,t) -> i, simplify_tree t) in
      let set = Xlist.fold l TermSet.empty (fun set (_,t) -> TermSet.add set t) in
      if TermSet.size set = 1 then TermSet.max_elt set else
      let l = List.rev (fst (TermSet.fold set ([],1) (fun (l,i) t -> (string_of_int i,t) :: l, i+1))) in (* UWAGA: tu następuje zmiana kolejności indeksów, jest to niedozwolone przy wielokrotnych etykietach *)
      let _,t = List.hd l in
      let b = Xlist.fold (List.tl l) true (fun b (_,s) -> if s = t then b else false) in
      if b then t else
      (try
        (match t with
           Concept c ->
             let lt = Xlist.fold l [] (fun lt -> function
                 i,Concept c2 ->
                    if c.sense = c2.sense && c.cat = c2.cat && c.label = c2.label &&
                      c.def_label = c2.def_label && c2.contents = Dot then (i,c2.relations) :: lt else raise Not_found
               | _ -> raise Not_found) in
             let e = if e = "" then LCGreductions.get_variant_label () else e in
             Concept{c with
               relations = simplify_tree (Variant(e,lt))}
(*           Concept c ->
             let lt1,lt2,lt3 = Xlist.fold l ([],[],[]) (fun (lt1,lt2,lt3) -> function
                 i,Concept c2 ->
                    if c.c_sense = c2.c_sense && c.c_name = c2.c_name &&
                      c.c_local_quant = c2.c_local_quant && c.c_label = c2.c_label &&
                      c.c_def_label = c2.c_def_label then (i,c2.c_quant) :: lt1, (i,c2.c_relations) :: lt2, (i,c2.c_cat) :: lt3 else raise Not_found
               | _ -> raise Not_found) in
             let e = if e = "" then LCGreductions.get_variant_label () else e in
             Concept{c with
               c_quant = simplify_tree (Variant(e,lt1));
               c_relations = simplify_tree (Variant(e,lt2));
               c_cat = simplify_tree (Variant(e,lt3))}
         | Context c ->
             let lt1,lt2,lt3 = Xlist.fold l ([],[],[]) (fun (lt1,lt2,lt3) -> function
                 i,Context c2 -> if c.cx_sense = c2.cx_sense && c.cx_label = c2.cx_label &&
                      c.cx_def_label = c2.cx_def_label then (i,c2.cx_contents) :: lt1, (i,c2.cx_relations) :: lt2, (i,c2.cx_cat) :: lt3 else raise Not_found
               | _ -> raise Not_found) in
             let e = if e = "" then LCGreductions.get_variant_label () else e in
             Context{c with
               cx_contents= simplify_tree (Variant(e,lt1));
               cx_relations = simplify_tree (Variant(e,lt2));
               cx_cat = simplify_tree (Variant(e,lt3))}*)
        | Relation(r,a,t) ->
             let lt = Xlist.fold l [] (fun lt -> function
                 i,Relation(r2,a2,t2) -> if r = r2 && a = a2 then (i,t2) :: lt else raise Not_found
               | _ -> raise Not_found) in
             simplify_tree (Relation(r,a,Variant(e,lt)))
        (* | TripleRelation(r,a,s,t) ->
             let ls,lt = Xlist.fold l ([],[]) (fun (ls,lt) -> function
                 i,TripleRelation(r2,a2,s2,t2) -> if r = r2 && a = a2 then (i,s2) :: ls, (i,t2) :: lt else raise Not_found
               | _ -> raise Not_found) in
             simplify_tree (TripleRelation(r,a,Variant(e,ls),Variant(e,lt))) *)
(*        | Tuple tl -> (* UWAGA: stwarza wielokrotne etykiety (generuje błąd wskazany powyżej); wielokrotne etykiety nie są kompatybilne z reprezentacją w JSONie *)
(*             print_endline ("V3: " ^ LCGstringOf.linear_term 0 (Variant l));  *)
            let n = Xlist.size tl in
            let lt = Xlist.fold l [] (fun lt -> function
              i,Tuple tl -> if n = Xlist.size tl then (i,tl) :: lt else raise Not_found
            | _ -> raise Not_found) in
            let e = if e = "" then LCGreductions.get_variant_label () else e in
            let t = Tuple(transpose_tuple_variant e lt) in
(*             print_endline ("V4: " ^ LCGstringOf.linear_term 0 t); *)
            simplify_tree t*)
         | Dot -> if Xlist.fold l true (fun b -> function
              _,Dot -> b
            | _ -> false) then Dot else raise Not_found
         | _ -> raise Not_found)
      with Not_found -> Variant(e,l))
(*   Variant(e,Xlist.map l (fun (i,t) -> i, simplify_tree t)) *)
  | Dot -> Dot
  | Val s -> Val s
  | t -> failwith ("simplify_tree: " ^ SemStringOf.linear_term 0 t)

let greater_simplify tree =
  let map = count_variant_labels StringQMap.empty tree in
  let tree = remove_variant_labels map tree in
  let tree = simplify_tree tree in
  let map = count_variant_labels StringQMap.empty tree in
  let tree = remove_variant_labels map tree in
  tree

(*let rec manage_quantification2 (quants,quant) = function
    Tuple l -> Xlist.fold l (quants,quant) manage_quantification2
  | Dot -> quants,quant
  | Val s -> quants,Tuple[Val s;quant]
  | t -> (Relation("Quantifier","",t)) :: quants,quant

let rec manage_quantification = function
    Node t -> Node{t with args=manage_quantification t.args}
  | Concept c ->
       let quants,quant = manage_quantification2 ([],Dot) c.c_quant in
       Concept{c with c_quant=quant; c_relations=manage_quantification (Tuple(c.c_relations :: quants))}
  | Context c -> Context{c with cx_contents=manage_quantification c.cx_contents; cx_relations=manage_quantification c.cx_relations}
  | Relation(r,a,t) -> Relation(r,a,manage_quantification t)
  | RevRelation(r,a,t) -> RevRelation(r,a,manage_quantification t)
  | SingleRelation r  -> SingleRelation r
  | AddRelation(t,r,a,s) -> AddRelation(manage_quantification t,r,a,manage_quantification s)
  (* | RemoveRelation t -> RemoveRelation(manage_quantification t) *)
  | Tuple l -> Tuple(Xlist.map l manage_quantification)
  | Variant(e,l) -> Variant(e,Xlist.map l (fun (i,t) -> i, manage_quantification t))
  | Dot -> Dot
  | Val s -> Val s
  | t -> failwith ("manage_quantification: " ^ SemStringOf.linear_term 0 t)*)

let simplify_gender2 = function
    Variant((*""*)_,l) -> (* FIXME: tu może być błąd, gdy etykieta nie jest unikalna *)
      let l2 = List.sort compare (Xlist.rev_map l (function (_,Val s) -> s | _ -> raise Not_found)) in
      (match l2 with
          ["f"; "m1"; "m2"; "m3"; "n"] -> Dot
        | ["m1"; "m2"; "m3"] -> Val "m"
        | ["f"; "m2"; "m3"; "n"] -> Val "nmo"
        | ["pl"; "sg"] -> Dot
        | _ -> raise Not_found)
  | _ -> raise Not_found

let rec simplify_gender = function
    Concept c -> Concept{c with relations=simplify_gender c.relations; contents=simplify_gender c.contents(*c_quant=simplify_gender c.c_quant*)}
(*   | Context c -> Context{c with cx_contents=simplify_gender c.cx_contents; cx_relations=simplify_gender c.cx_relations} *)
  | Relation(r,a,t) -> Relation(r,a,simplify_gender t)
  | RevRelation(r,a,t) -> RevRelation(r,a,simplify_gender t)
  | SingleRelation r  -> let r = simplify_gender r in if r = Dot then Dot else SingleRelation r
  | Tuple l -> Tuple(Xlist.map l simplify_gender)
  | Variant(e,l) ->
      (try simplify_gender2 (Variant(e,l)) with Not_found ->
        Variant(e,Xlist.map l (fun (i,t) -> i, simplify_gender t)))
  | Dot -> Dot
  | Val s -> Val s
  | t -> failwith ("simplify_gender: " ^ SemStringOf.linear_term 0 t)


(*
 *  LCGparser, a parser for Logical Categorial Grammar formalism
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

open LCGtypes
open Xstd

let variant_label_ref = ref []

let reset_variant_label () =
  variant_label_ref := []

let rec add_variant_label = function
    [] -> ["A"]
  | "Z" :: l -> "A" :: add_variant_label l
  | s :: l -> String.make 1 (Char.chr (Char.code (String.get s 0) + 1)) :: l

let get_variant_label () =
  variant_label_ref := add_variant_label (!variant_label_ref);
  String.concat "" (List.rev (!variant_label_ref))

(* let prepare_references t references next_reference =
   let a = Array.make next_reference Dot in
   Xlist.iter references (fun (i,t) -> a.(i) <- t);
   a.(0) <- t;
   a *)

let prepare_dependency_tree t refs next_ref =
  let a = Array.make next_ref Dot in
  TermMap.iter refs (fun t i -> a.(i) <- t);
  a.(0) <- t;
  a

let rec extract_nth n rev = function
    [] -> failwith "extract_nth"
  | s :: l ->
    if n = 1 then s, (List.rev rev) @ l
    else extract_nth (n-1) (s :: rev) l

let rec is_reduced_rec = function
    Tuple l -> Xlist.fold l true (fun b t -> b && is_reduced_rec t)
  | Variant(_,l) -> Xlist.fold l true (fun b (_,t) -> b && is_reduced_rec t)
  | Dot -> true
  | Val s -> true
  | Node t -> Xlist.fold t.attrs true (fun b (_,t) -> b && is_reduced_rec t) && is_reduced_rec t.symbol && (*is_reduced_rec t.arg_symbol &&*) is_reduced_rec t.args
  (* | Morf m -> true *)
  | Cut t -> is_reduced_rec t
  | Ref i -> true
  | _ -> false

let is_reduced = (*function
                   Triple(_,_,_) as t -> is_reduced_rec t
                   | _ -> false*) is_reduced_rec

let is_reduced_dependency_tree dependency_tree =
  Int.fold 0 (Array.length dependency_tree - 1) true (fun b i ->
      b && is_reduced dependency_tree.(i))

let rec assign_labels_rec = function
    Tuple l -> Tuple(Xlist.map l assign_labels_rec)
  | Variant(_,l) -> Variant(get_variant_label (), fst (Xlist.fold l ([],1) (fun (l,i) (_,t) ->
      (string_of_int i, assign_labels_rec t) :: l,i+1)))
  | Dot -> Dot
  | Val s -> Val s
  | Node t -> Node{t with args=assign_labels_rec t.args}
  (* | Morf m -> Morf m *)
  | Cut t -> Cut(assign_labels_rec t)
  | Ref i -> Ref i
  | _ -> failwith "assign_labels_rec"

let assign_labels dependency_tree =
  Int.iter 0 (Array.length dependency_tree - 1) (fun i ->
      dependency_tree.(i) <- assign_labels_rec dependency_tree.(i))

let rec remove_cuts_rec = function
    Tuple l -> Tuple(Xlist.map l remove_cuts_rec)
  | Variant(e,l) -> Variant(e, Xlist.map l (fun (i,t) -> i, remove_cuts_rec t))
  | Dot -> Dot
  | Val s -> Val s
  | Node t -> Node{t with args=remove_cuts_rec t.args}
  (* | Morf m -> Morf m *)
  | Cut t -> remove_cuts_rec t
  | Ref i -> Ref i
  | _ -> failwith "remove_cuts_rec"

let remove_cuts dependency_tree =
  Int.iter 0 (Array.length dependency_tree - 1) (fun i ->
      dependency_tree.(i) <- remove_cuts_rec dependency_tree.(i))

let linear_term_beta_reduction4 references =
  let reduced = Array.make (ExtArray.size references) Dot in
  let size = ref 0 in
  let refs = ref TermMap.empty in
  let next_ref = ref 1 in

  let rec flatten_variant set = function
      Variant(_,l) -> Xlist.fold l set (fun set (_,t) -> flatten_variant set t)
    | Cut t -> TermSet.add set (Cut t)
    | Dot -> TermSet.add set Dot
    | Tuple l -> TermSet.add set (simplify_args (Tuple l))
    | t -> failwith ("flatten_variant: " ^ LCGstringOf.linear_term 0 t)

  and simplify_args = function
      Tuple l -> Tuple(Xlist.map l simplify_args)
    | Dot -> Dot
    | Variant(e,l) -> (match TermSet.to_list (flatten_variant TermSet.empty (Variant(e,l))) with
          [] -> failwith "simplify_args 1"
        | [t] -> t
        | l -> Variant("",Xlist.map l (fun t -> ("0",t))))
    | Cut t -> Cut t
    | t -> failwith ("simplify_args 2: " ^ LCGstringOf.linear_term 0 t) in

  let rec create_cut_refs = function
      Node t ->
      let t = {t with args=simplify_args t.args} in
(*       Printf.printf "create_cut_refs 1: %s\n" (LCGstringOf.linear_term 0 (Node t)); *)
      (try
         let i = TermMap.find !refs (Node t) in
(*          Printf.printf "create_cut_refs 2: %d\n" i; *)
         Cut(Ref i)
       with Not_found ->
         refs := TermMap.add !refs (Node t) !next_ref;
         let t = Cut(Ref !next_ref) in
         incr next_ref;
(*          Printf.printf "create_cut_refs 3: %d\n" (!next_ref); *)
         t)
    | Tuple l -> Tuple(List.rev (Xlist.rev_map l create_cut_refs))
    | Variant(e,l) -> Variant(e,Xlist.map l (fun (i,t) -> i,create_cut_refs t))
    | Dot -> Dot
    | _ -> failwith "create_cut_refs" in

  let rec linear_term_beta_reduction subst = function
      Var v -> (try StringMap.find subst v with Not_found -> Var v) (* zakladam, ze termy, ktore sa podstawiane na zmienne nie maja zmiennych wolnych *)
    | Tuple l ->
      let l = Xlist.map l (linear_term_beta_reduction subst) in
      (match Xlist.fold l [] (fun l t -> if t = Dot then l else t :: l) with
         [] -> Dot
       | [t] -> t
       | l -> Tuple(List.rev l))
    | Variant(e,l) -> Variant(e,Xlist.map l (fun (i,t) -> i,linear_term_beta_reduction subst t))
    | VariantVar(v,t) -> VariantVar(v, linear_term_beta_reduction subst t)
    | Proj(n,t) ->
      (match linear_term_beta_reduction subst t with
         Variant(e,l) -> if Xlist.size l < n then Proj(n,(Variant(e,l))) else snd (List.nth l (n-1))
       | t2 -> Proj(n,t2))
    | ProjVar(v,t) ->
      (match linear_term_beta_reduction subst t with
         VariantVar(v2,t2) -> if v = v2 then t2 else ProjVar(v,VariantVar(v2,t2))
       | Variant(e,l) -> Variant(e,Xlist.map l (fun (i,s) -> i,linear_term_beta_reduction subst (ProjVar(v,s))))
       | t2 -> ProjVar(v,t2))
    | SubstVar v -> SubstVar v
    | Subst(s,v,t) ->
      (match linear_term_beta_reduction subst s with
       | Tuple l -> Tuple(Xlist.map l (fun s -> linear_term_beta_reduction subst (Subst(s,v,t))))
       | Variant(e,l) -> Variant(e,Xlist.map l (fun (i,s) -> i,linear_term_beta_reduction subst (Subst(s,v,t))))
       | Lambda(v2,s) -> Lambda(v2, linear_term_beta_reduction subst (Subst(s,v,t)))
       | Dot -> Dot
       | SetAttr(e,s1,s2) -> SetAttr(e,linear_term_beta_reduction subst (Subst(s1,v,t)),linear_term_beta_reduction subst (Subst(s2,v,t)))
       | Val s -> Val s
       | Var w -> if w = v then t else Subst(Var w,v,t)
       | SubstVar w -> if w = v then t else SubstVar w
       | Inj(n,s) -> Inj(n,linear_term_beta_reduction subst (Subst(s,v,t)))
       | Node s -> Node{s with attrs=Xlist.map s.attrs (fun (e,s) -> e, linear_term_beta_reduction subst (Subst(s,v,t)));
                               (* gs=linear_term_beta_reduction subst (Subst(s.gs,v,t)); *)
                               symbol=linear_term_beta_reduction subst (Subst(s.symbol,v,t));
                               (* arg_symbol=linear_term_beta_reduction subst (Subst(s.arg_symbol,v,t)); *)
                               args=linear_term_beta_reduction subst (Subst(s.args,v,t))}
       (* | Morf m -> Morf m
       | Gf s -> Gf s *)
       | Cut(Ref i) -> Cut(Ref i)
       | Cut s -> linear_term_beta_reduction subst (Cut(Subst(s,v,t)))
       | s2 -> Subst(s2,v,t))
    | Inj(n,t) -> Inj(n,linear_term_beta_reduction subst t)
    | Case(t,l) ->
      (match linear_term_beta_reduction subst t with
         Inj(n,t) ->
         if Xlist.size l < n then Case(Inj(n,t),l) else
           let v, r = List.nth l (n-1) in
           let subst = StringMap.add subst v t in
           linear_term_beta_reduction subst r
       | Variant(e,l2) -> linear_term_beta_reduction subst (Variant(e,Xlist.map l2 (fun (i,t2) -> i,Case(t2,l))))
       | t2 -> Case(t2,Xlist.map l (fun (v,t) -> v, linear_term_beta_reduction subst t))) (* FIXME alfa-konwersja i przykrywanie *)
    | Lambda(v,t) -> Lambda(v, linear_term_beta_reduction subst t)
    | LambdaSet(l,t) -> LambdaSet(l, linear_term_beta_reduction subst t)
    | LambdaRot(n,t) ->
      (match linear_term_beta_reduction subst t with
         Lambda(v,t) -> if n = 1 then Lambda(v,t) else LambdaRot(n,Lambda(v,t))
       | LambdaSet([v],t) -> if n = 1 then Lambda(v,t) else LambdaRot(n,LambdaSet([v],t))
       | LambdaSet(l,t) ->
         if Xlist.size l < n then LambdaRot(n,LambdaSet(l,t)) else
           let s,l = extract_nth n [] l in
           Lambda(s,LambdaSet(l,t))
       | Variant(e,l) -> Variant(e,Xlist.map l (fun (i,s) -> i,linear_term_beta_reduction subst (LambdaRot(n,s))))
       | t2 -> LambdaRot(n,t2))
    | App(s,t) ->
      let t = linear_term_beta_reduction subst t in
      (match linear_term_beta_reduction subst s, t with
         Lambda(v,s),_ ->
         let subst = StringMap.add subst v t in
         linear_term_beta_reduction subst s
       | LambdaSet([v],s),_ ->  (* FIXME ten przypadek nie powinien miec miejsca, jego wystepowanie wskazuje na brak rotacji przy maczowaniu *)
         let subst = StringMap.add subst v t in
         linear_term_beta_reduction subst s
       | Variant(e,l),_ -> linear_term_beta_reduction subst (Variant(e,Xlist.map l (fun (i,s) -> i,App(s,t))))
       | t2,_ -> App(t2,t))
    | Dot -> Dot
    | Fix(f,t) ->
      (match linear_term_beta_reduction subst f with
         Empty s -> linear_term_beta_reduction subst s
       | Apply s -> linear_term_beta_reduction subst (App(t,s))
       | Insert(s1,s2) -> Tuple[Fix(s1,t);Fix(s2,t)]
       | f -> Fix(f,linear_term_beta_reduction subst t))
    | Empty t -> Empty(linear_term_beta_reduction subst t)
    | Apply t -> Apply(linear_term_beta_reduction subst t)
    | Insert(s,t) -> Insert(linear_term_beta_reduction subst s,linear_term_beta_reduction subst t)
    | Val s -> Val s
    | SetAttr(e,s,t) ->
      (match linear_term_beta_reduction subst t with
         Dot -> Dot
       | Tuple l -> linear_term_beta_reduction subst (Tuple(Xlist.map l (fun t -> SetAttr(e,s,t))))
       | Node t -> (match e,s with
             (* "GF",Gf gf ->  Node{t with agf=gf}
           | "MORF",Morf morf -> Node{t with amorf=morf}
           | "AROLE",Val arole ->  Node{t with arole=arole} *)
           | "ARG_SYMBOL",symbol ->  Node{t with arg_symbol=symbol}
           | "ARG_DIR",Val dir ->  Node{t with arg_dir=dir}
           | _ -> Node{t with attrs=(e,linear_term_beta_reduction subst s) :: t.attrs})
       | Variant(e2,l) -> Variant(e2,Xlist.map l (fun (i,t) -> i,linear_term_beta_reduction subst (SetAttr(e,s,t))))
       | Inj(i,t) -> Inj(i,linear_term_beta_reduction subst (SetAttr(e,s,t)))
       | t -> SetAttr(e,s,t))
    (* | Choice(e,i,t) -> Choice(e,i,linear_term_beta_reduction subst t) *)
    | Node t ->
      if !size > !no_nodes then raise SemTooBig;
      incr size;
      Node{t with attrs=Xlist.map t.attrs (fun (e,t) -> e, linear_term_beta_reduction subst t);
                  (* gs=linear_term_beta_reduction subst t.gs; *)
                  symbol=linear_term_beta_reduction subst t.symbol;
                  (* arg_symbol=linear_term_beta_reduction subst t.arg_symbol; *)
                  args=linear_term_beta_reduction subst t.args}
  | Coord(l,t,s) -> Coord(List.rev (Xlist.rev_map l (linear_term_beta_reduction subst)), linear_term_beta_reduction subst t, linear_term_beta_reduction subst s)
  | AddCoord(s,t) ->
      let s = linear_term_beta_reduction subst s in
      (match linear_term_beta_reduction subst t with
        Coord(l,t,a) -> Coord(s :: l,t,a)
      | Variant(e,l) -> Variant(e,List.rev (Xlist.rev_map l (fun (i,t) ->
          i,linear_term_beta_reduction subst (AddCoord(s,t)))))
      | t -> AddCoord(s,t))
  | MapCoord(s,t) ->
      let t = linear_term_beta_reduction subst t in
      (match linear_term_beta_reduction subst s with
        Coord(l,c,a) -> Coord(List.rev (Xlist.rev_map l (fun s ->
          linear_term_beta_reduction subst (App(t,s)))),c,a)
      | Variant(e,l) -> Variant(e,List.rev (Xlist.rev_map l (fun (i,s) ->
          i,linear_term_beta_reduction subst (MapCoord(s,t)))))
      | s -> MapCoord(s,t))
  | ConcatCoord(s,t) ->
      (match linear_term_beta_reduction subst t with
        Coord(l,c,a) ->
          let l,_ = Xlist.fold l ([],1) (fun (l,n) t -> App(a,SetAttr("COORD_ARG",Val (string_of_int n),t)) :: l, n+1) in
          linear_term_beta_reduction subst (App(App(s,c),Tuple(List.rev l)))
      | Variant(e,l) -> Variant(e,List.rev (Xlist.rev_map l (fun (i,t) ->
          i,linear_term_beta_reduction subst (ConcatCoord(s,t)))))
      | t -> ConcatCoord(s,t))
    (* | Morf m -> Morf m
    | Gf s -> Gf s
    | Choice _ -> failwith "linear_term_beta_reduction"
    | Concept _ -> failwith "linear_term_beta_reduction"
    | Context _ -> failwith "linear_term_beta_reduction"
    | Relation _ -> failwith "linear_term_beta_reduction"
    | RevRelation _ -> failwith "linear_term_beta_reduction"
    | SingleRelation _ -> failwith "linear_term_beta_reduction"
    | AddRelation _ -> failwith "linear_term_beta_reduction"
    | RemoveRelation _ -> failwith "linear_term_beta_reduction"
    | SetContextName _ -> failwith "linear_term_beta_reduction" *)
    | Ref i -> (* nie ma problemu przy wywoływaniu z różnymi podstawieniami, bo termy w poszczególnych referencjach nie mają zmiennych wolnych
                  reduced zawiera termy bez zmiennych *)
      if reduced.(i) = Dot then (
(*         Printf.printf "reduce in Ref %d: %s\n" i (LCGstringOf.linear_term 0 (ExtArray.get references i)); *)
        let t = linear_term_beta_reduction subst (ExtArray.get references i) in
(*         Printf.printf "reduce out Ref %d: %s\n" i (LCGstringOf.linear_term 0 t); *)
        ExtArray.set references i t;
        if is_reduced t then reduced.(i) <- t else ExtArray.set references i t;
        t)
      else (
(*         Printf.printf "reduce done Ref %d: %s\n" i (LCGstringOf.linear_term 0 reduced.(i)); *)
        reduced.(i))
    | Cut(Ref i) -> Cut(Ref i)
    | Cut t ->
      let t = linear_term_beta_reduction subst t in
      (* if t = Dot then Dot else *)
      if is_reduced t then create_cut_refs t else Cut t
  in

(*   Printf.printf "linear_term_beta_reduction4: next_ref=%d\n" !next_ref; *)
  let t = linear_term_beta_reduction StringMap.empty (ExtArray.get references 0) in
(*   Printf.printf "linear_term_beta_reduction4: next_ref=%d\n" !next_ref; *)
  t, !refs, !next_ref

(* dodać usuwanie jednorazowych etykiet i
   zastąpić Cut(Ref i) przez coś innego *)
let reduce t references =
  ExtArray.set references 0 t;
  (* let references = prepare_references t references next_reference in *)
(*   LCGlatexOf.print_references "results/" "references1" "a0" references; *)
  let t,refs,next_ref = linear_term_beta_reduction4 references in
(*   Printf.printf "reduce next_ref=%d t=%s\n" next_ref (LCGstringOf.linear_term 0 t); *)
(*   TermMap.iter refs (fun t i -> Printf.printf "reduce ref=%d t=%s\n" i (LCGstringOf.linear_term 0 t)); *)
  let dependency_tree = prepare_dependency_tree t refs next_ref in
(*   LCGlatexOf.print_references "results/" "references2" "a0" references; *)
  dependency_tree


let rec reshape_dependency_tree_rec dependency_tree a visited = function
    Node t -> Node{t with args = reshape_dependency_tree_rec dependency_tree a visited t.args}
  | Tuple l ->
      let l = Xlist.fold l [] (fun l t ->
        let t = reshape_dependency_tree_rec dependency_tree a visited t in
        match t with
          Dot -> l
        | Tuple l2 -> l2 @ l
        | t -> t :: l) in
      (match l with
        [] -> Dot
      | [t] -> t
      | l -> Tuple l)
  | Variant(e,l) ->
      Variant(e,List.rev (Xlist.rev_map l (fun (i,t) ->
        i, reshape_dependency_tree_rec dependency_tree a visited t)))
  | Dot -> Dot
  | Ref i ->
      if IntMap.mem !visited i then Ref (IntMap.find !visited i) else (
      (* Printf.printf "reshape_dependency_tree_rec 1\n%!"; *)
      let t = reshape_dependency_tree_rec dependency_tree a visited (ExtArray.get dependency_tree i) in
      (* Printf.printf "reshape_dependency_tree_rec 2\n%!"; *)
      let i2 = ExtArray.add a t in
      visited := IntMap.add !visited i i2;
      Ref i2)
  | t -> failwith ("reshape_dependency_tree_rec: " ^ LCGstringOf.linear_term 0 t)

let reshape_dependency_tree dependency_tree =
  let a = ExtArray.make (ExtArray.size dependency_tree) Dot in
  let _ = ExtArray.add a Dot in
  let t = reshape_dependency_tree_rec dependency_tree a (ref IntMap.empty) (ExtArray.get dependency_tree 0) in
  ExtArray.set a 0 t;
  ExtArray.to_array a



let rec find_labels_attrs labels  = function
    Variant(e,l) ->
      let labels = StringMap.add_inc labels e (Xlist.map l fst) (fun l -> l) in
      Xlist.fold l labels (fun labels (i,t) -> find_labels_attrs labels t)
  | Dot -> labels
  | Val s -> labels
  | t -> failwith ("find_labels_attrs: " ^ LCGstringOf.linear_term 0 t)

let rec find_labels_rec dependency_tree visited = function
    Node t ->
      let labels = find_labels_rec dependency_tree visited t.args in
      let attr_labels = Xlist.fold t.attrs StringMap.empty (fun labels (_,t) -> find_labels_attrs labels t) in
      StringMap.fold attr_labels labels (fun labels e l ->
        StringMap.add_inc labels e (1,l) (fun (m,_) -> m + 1, l))
  | Tuple l ->
      Xlist.fold l StringMap.empty (fun comb_labels t ->
        let labels = find_labels_rec dependency_tree visited t in
        StringMap.fold labels comb_labels (fun comb_labels e (n,l) ->
          StringMap.add_inc comb_labels e (n,l) (fun (m,_) -> m + n,l)))
  | Variant(e,l) ->
      Xlist.fold l StringMap.empty (fun comb_labels (_,t) ->
        let labels = find_labels_rec dependency_tree visited t in
        StringMap.fold labels comb_labels (fun comb_labels e (n,l) ->
          StringMap.add_inc comb_labels e (n,l) (fun (m,_) -> max m n,l)))
  | Dot -> StringMap.empty
  | Ref i ->
      if IntMap.mem !visited i then IntMap.find !visited i else
      let labels = find_labels_rec dependency_tree visited dependency_tree.(i) in
      visited := IntMap.add !visited i labels;
      labels
  | t -> failwith ("find_labels_rec: " ^ LCGstringOf.linear_term 0 t)

let find_multiple_labels dependency_tree =
  find_labels_rec dependency_tree (ref IntMap.empty) dependency_tree.(0)

let rec find_label_dependants dependency_tree i e members dependants visited = function
    Node t ->
      let members,dependants,visited = find_label_dependants dependency_tree i e members dependants visited t.args in
      let members,dependants,visited = find_label_dependants dependency_tree i e members dependants visited t.symbol in
      let members,dependants,visited = find_label_dependants dependency_tree i e members dependants visited t.arg_symbol in
      Xlist.fold t.attrs (members,dependants, visited) (fun (members,dependants,visited) (_,t) ->
        find_label_dependants dependency_tree i e members dependants visited t)
  | Tuple l ->
      Xlist.fold l (members,dependants,visited) (fun (members,dependants,visited) t ->
        find_label_dependants dependency_tree i e members dependants visited t)
  | Variant(e2,l) ->
      let members,dependants = if e2 = e then IntSet.add members i, IntSet.add dependants i else members,dependants in
      Xlist.fold l (members,dependants,visited) (fun (members,dependants,visited) (_,t) ->
        find_label_dependants dependency_tree i e members dependants visited t)
  | Dot -> members,dependants,visited
  | Val s -> members,dependants,visited
  | Ref i2 ->
      let members,dependants,visited =
        if IntSet.mem visited i2 then members,dependants,visited else
        find_label_dependants dependency_tree i2 e members dependants visited (ExtArray.get dependency_tree i2) in
      let dependants = if IntSet.mem dependants i2 then IntSet.add dependants i else dependants in
      members, dependants, IntSet.add visited i2
  | t -> failwith ("find_label_dependants: " ^ LCGstringOf.linear_term 0 t)

let rec is_dependant dependants = function
    Tuple l -> Xlist.fold l false (fun b t -> b || is_dependant dependants t)
  | Variant(e,l) -> Xlist.fold l false (fun b (_,t) -> b || is_dependant dependants t)
  | Dot -> false
  | Ref i -> IntSet.mem dependants i
  | t -> failwith ("is_dependant: " ^ LCGstringOf.linear_term 0 t)

let rec select_variant_rec e i = function
    Variant(e2,l) ->
      if e = e2 then
        let t = try Xlist.assoc l i with Not_found -> failwith ("select_variant_rec: '" ^ e ^ "'") in
        select_variant_rec e i t
      else
        Variant(e2,List.rev (Xlist.rev_map l (fun (i2,t) ->
          i2,select_variant_rec e i t)))
  | Tuple l -> Tuple(List.rev (Xlist.rev_map l (select_variant_rec e i)))
  | Dot -> Dot
  | Val s -> Val s
  | t -> failwith ("select_variant_rec: " ^ LCGstringOf.linear_term 0 t)

let select_variant e i = function
    Node t ->
      Node{t with
        attrs = List.rev (Xlist.rev_map t.attrs (fun (s,t) -> s,select_variant_rec e i t));
        symbol = select_variant_rec e i t.symbol}
  | t -> failwith ("select_variant: " ^ LCGstringOf.linear_term 0 t)

let rec set_variant_rec dependency_tree e i members dependants visited = function
    Node t -> Node{t with args = set_variant_rec dependency_tree e i members dependants visited t.args}
  | Tuple l ->
      Tuple(List.rev (Xlist.rev_map l
        (set_variant_rec dependency_tree e i members dependants visited)))
  | Variant(e2,l) ->
      Variant(e2,List.rev (Xlist.rev_map l (fun (i2,t) ->
        i2,set_variant_rec dependency_tree e i members dependants visited t)))
  | Dot -> Dot
  | Ref i2 ->
      if IntMap.mem !visited i2 then Ref(IntMap.find !visited i2) else
      if not (IntSet.mem dependants i2) then Ref i2 else
      let t = ExtArray.get dependency_tree i2 in
      let t = if IntSet.mem members i2 then select_variant e i t else t in
      let t = set_variant_rec dependency_tree e i members dependants visited t in
      let i3 = ExtArray.add dependency_tree t in
      visited := IntMap.add !visited i2 i3;
      Ref i3
  | t -> failwith ("set_variant_rec: " ^ LCGstringOf.linear_term 0 t)

let set_variant dependency_tree e il members dependants t =
  let l = Xlist.map il (fun i ->
    i, set_variant_rec dependency_tree e i members dependants (ref IntMap.empty) t) in
  Variant(e,l)

let rec normalize_variants_rec dependency_tree e il members dependants = function
    Node t -> Node{t with args = normalize_variants_rec dependency_tree e il members dependants t.args}
  | Tuple l ->
      let n = Xlist.fold l 0 (fun n t -> if is_dependant dependants t then n+1 else n) in
      (match n with
        0 -> failwith "normalize_variants_rec"
      | 1 -> Tuple(List.rev (Xlist.fold l [] (fun l t ->
          let t =
            if is_dependant dependants t then
              normalize_variants_rec dependency_tree e il members dependants t
            else t in
          t :: l)))
      | _ -> set_variant dependency_tree e il members dependants (Tuple l))
  | Variant(e2,l) ->
      let n = Xlist.fold l 0 (fun n (_,t) -> if is_dependant dependants t then n+1 else n) in
      (match n with
        0 -> failwith "normalize_variants_rec"
      | 1 -> Variant(e2,List.rev (Xlist.fold l [] (fun l (i2,t) ->
          let t =
            if is_dependant dependants t then
              normalize_variants_rec dependency_tree e il members dependants t
            else t in
          (i2,t) :: l)))
      | _ -> set_variant dependency_tree e il members dependants (Variant(e2,l)))
  | Dot -> Dot
  | Ref i ->
      if IntSet.mem members i then set_variant dependency_tree e il members dependants (Ref i) else
      let t = normalize_variants_rec dependency_tree e il members dependants (ExtArray.get dependency_tree i) in
      ExtArray.set dependency_tree i t;
      Ref i
  | t -> failwith ("normalize_variants_rec: " ^ LCGstringOf.linear_term 0 t)

let normalize_variants dependency_tree =
  (* print_endline "normalize_variants 1"; *)
  let labels = find_multiple_labels dependency_tree in
  (* print_endline "normalize_variants 2"; *)
  let a = ExtArray.make (Array.length dependency_tree) Dot in
  Int.iter 0 (Array.length dependency_tree - 1) (fun i ->
    ExtArray.add a dependency_tree.(i));
  (* print_endline "normalize_variants 3"; *)
  StringMap.iter labels (fun e (count,il) ->
    if count > 1 then
      let members,dependants,_ = find_label_dependants a 0 e IntSet.empty IntSet.empty IntSet.empty (ExtArray.get a 0) in
      if IntSet.mem members 0 then failwith "normalize_variants" else
      let t = normalize_variants_rec a e il members dependants (ExtArray.get a 0) in
      ExtArray.set a 0 t);
  (* print_endline "normalize_variants 4"; *)
  reshape_dependency_tree a (*ExtArray.to_array a*)

let rec validate_dependency_tree_args = function
    Tuple l -> Xlist.iter l validate_dependency_tree_args
  | Variant(e,l) -> Xlist.iter l (fun (i,t) -> validate_dependency_tree_args t)
  | Dot -> ()
  | Ref i -> ()
  | t -> failwith ("validate_dependency_tree_args: " ^ LCGstringOf.linear_term 0 t)

let rec validate_dependency_tree_atrs = function
    Variant(e,l) -> Xlist.iter l (fun (i,t) -> validate_dependency_tree_atrs t)
  | Dot -> ()
  | Val s -> ()
  | t -> failwith ("validate_dependency_tree_atrs: " ^ LCGstringOf.linear_term 0 t)

let validate_dependency_tree2 = function
    Node t ->
       validate_dependency_tree_args t.args;
       Xlist.iter t.attrs (fun (_,s) -> validate_dependency_tree_atrs s)
  (* | Dot -> ()  (* FIXME: to trzebaby usunąć i wprowadzić rekurencję po drzewie *) *)
  | t -> failwith ("validate_dependency_tree2: " ^ LCGstringOf.linear_term 0 t)

(* let rec validate_dependency_tree_labels set  = function
    Variant(e,l) ->
      Xlist.fold l (StringSet.add set e) (fun set (i,t) -> validate_dependency_tree_labels set t)
  | Dot -> set
  | Val s -> set
  | t -> failwith ("validate_dependency_tree_labels: " ^ LCGstringOf.linear_term 0 t) *)

let validate_dependency_tree dependency_tree =
  Int.iter 0 (Array.length dependency_tree - 1) (fun i ->
    validate_dependency_tree2 dependency_tree.(i));
  let labels = find_multiple_labels dependency_tree in
  let labels = StringMap.fold labels [] (fun labels e (n,_) ->
    if n > 1 then e :: labels else labels) in
  if labels <> [] then
    (*failwith ("validate_dependency_tree: multiple labels " ^ String.concat " " labels)*)() else (* FIXME: trzeba zreimplementować obsługę wielokrotnych etykiet *)
  (* let _ = Int.fold 0 (Array.length dependency_tree - 1) StringSet.empty (fun labels i ->
    match dependency_tree.(i) with
      Node t ->
        let set = Xlist.fold t.attrs StringSet.empty (fun set (_,t) -> validate_dependency_tree_labels set t) in
        let intersection = StringSet.intersection labels set in
        if StringSet.is_empty intersection then StringSet.union labels set
        else failwith ("validate_dependency_tree: multiple labels " ^ String.concat " " (StringSet.to_list intersection))
    | _ -> failwith "validate_dependency_tree") in *)
  ()

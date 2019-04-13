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

open Xstd
open LCGtypes

(* let references = ref [0,Ref 0]
   let next_reference = ref 0

   let make_reference sem =
   let r = !next_reference in
   references := (r,sem) :: !references;
   incr next_reference;
   r *)

let new_variable_ref = ref 0

let get_new_variable () =
  incr new_variable_ref;
  "x" ^ (string_of_int (!new_variable_ref))

let rec unify v1 v2 = function
    AVar a,Atom t -> if v2=a then Atom t else failwith ("unify AVar: " ^ v1 ^ "=" ^ (LCGstringOf.internal_grammar_symbol 0 (AVar a)) ^ " " ^ v2 ^ "=" ^ (LCGstringOf.internal_grammar_symbol 0 (Atom t)))
  | Atom s,AVar a -> if v1=a then Atom s else failwith ("unify AVar: " ^ v1 ^ "=" ^ (LCGstringOf.internal_grammar_symbol 0 (Atom s)) ^ " " ^ v2 ^ "=" ^ (LCGstringOf.internal_grammar_symbol 0 (AVar a)))
  | AVar a,With lt -> if v2=a then With lt else failwith ("unify AVar: " ^ v1 ^ "=" ^ (LCGstringOf.internal_grammar_symbol 0 (AVar a)) ^ " " ^ v2 ^ "=" ^ (LCGstringOf.internal_grammar_symbol 0 (With lt)))
  | With ls,AVar a -> if v1=a then With ls else failwith ("unify AVar: " ^ v1 ^ "=" ^ (LCGstringOf.internal_grammar_symbol 0 (With ls)) ^ " " ^ v2 ^ "=" ^ (LCGstringOf.internal_grammar_symbol 0 (AVar a)))
  | AVar a,AVar b when a=b -> AVar a
  | AVar a,t -> failwith ("unify AVar: " ^ v1 ^ "=" ^ (LCGstringOf.internal_grammar_symbol 0 (AVar a)) ^ " " ^ v2 ^ "=" ^ (LCGstringOf.internal_grammar_symbol 0 t))
  | t,AVar a -> failwith ("unify AVar: " ^ v1 ^ "=" ^ (LCGstringOf.internal_grammar_symbol 0 t) ^ " " ^ v2 ^ "=" ^ (LCGstringOf.internal_grammar_symbol 0 (AVar a)))
  | Zero, t -> t
  | t, Zero -> t
  | With ls, With lt ->
    let ls = Xlist.map ls (function Atom s -> s | _ -> failwith "unify: With") in
    let lt = Xlist.map lt (function Atom s -> s | _ -> failwith "unify: With") in
    let set = StringSet.of_list lt in
    let l = Xlist.fold ls [] (fun l s -> if StringSet.mem set s then s :: l else l) in
    (match l with
       [] -> raise Not_found
     | [s] -> Atom s
     | l -> With(Xlist.map l (fun s -> Atom s)))
  | Atom s, t -> unify v1 v2 (With[Atom s],t)
  | s, Atom t -> unify v1 v2 (s,With[Atom t])
  | _,_ -> failwith "unify"

(*let unify_fv afv bfv =
  StringMap.fold afv bfv (fun bfv v g ->
    let g2 = try StringMap.find bfv v with Not_found -> Zero in
    StringMap.add bfv v (unify v v (g,g2))) *)

let find_fv fv v = try StringMap.find fv v with Not_found -> failwith ("find_fv: "^ v)
let add_fv = StringMap.add
let mem_fv = StringMap.mem
let remove_fv = StringMap.remove
let is_empty_fv = StringMap.is_empty
let empty_fv = StringMap.empty
let fold_fv = StringMap.fold

let string_of_fv fv =
  let l = StringMap.fold fv [] (fun l v (t,e) -> (e ^ ": " ^ v ^ ":=" ^ LCGstringOf.internal_grammar_symbol 0 t) :: l) in
  String.concat "," (List.sort compare l)

let rec infer s = function
    Zero -> true
  | Atom t -> t = s
  | With l -> Xlist.fold l false (fun b t -> b || (infer s t))
  | AVar _ -> failwith "infer"
  | Top -> failwith "infer"

let make_variant = function
    [] -> print_endline "make_variant"; failwith "make_variant"
  | [t] -> t
  | l ->
    (*       let e = get_variant_label () in *)
    let l,_ = Xlist.fold l ([],1) (fun (l,i) -> function
          t -> (string_of_int i,t) :: l, i+1) in
    Variant("",List.rev l)

let make_subst e = function
    Zero -> Dot
  | Atom t -> Val t
  | With l ->
    (*       let e = get_variant_label () in *)
    let l,_ = Xlist.fold l ([],1) (fun (l,i) -> function
          Atom t -> (string_of_int i,Val t) :: l, i+1
        | _ -> failwith "make_subst 1") in
    Variant(e,List.rev l)
  | AVar a -> SubstVar a
  | _ -> failwith "make_subst 2"

let internal_deduce_matching afv bfv sem = function (* maczowany term * argument funktora *)
    Atom s, Atom t -> if s = t then [afv,bfv,sem] else []
  | Atom s, Top -> [afv,bfv,sem]
  | Zero, Atom t -> [afv,bfv,sem]
  | Zero, Top -> [afv,bfv,sem]
  | AVar a, Atom t ->
    let g,e = find_fv afv a in
    let l = if infer t g then [add_fv afv a (Atom t,e),bfv,sem] else [] in
    (*       Printf.printf "AVar,Atom: [%s] '%s' [%s] '%s' -> %d\n%!" (string_of_fv afv) (LCGstringOf.internal_grammar_symbol 1 (AVar a)) (string_of_fv bfv) (LCGstringOf.internal_grammar_symbol 1 (Atom t)) (Xlist.size l); *)
    l
  | AVar a, Top -> [afv,bfv,sem]
  | Zero, AVar b -> (*print_endline "idm";*)[afv,bfv,sem]
  | Atom s, AVar b ->
    let g,e = find_fv bfv b in
    if infer s g then [afv, add_fv bfv b (Atom s,e),sem] else []
  | AVar a, AVar b ->
    let ga,ea = find_fv afv a in
    let gb,eb = find_fv bfv b in
    (try let subst = (*print_endline "internal_deduce_matching";*)unify a b (ga,gb) in [add_fv afv a (AVar b,eb), add_fv bfv b (subst,eb),sem] with Not_found -> [])
  | Top, Top -> [afv,bfv,sem]
  | Top, _ -> []
  | s,t -> failwith ("internal_deduce_matching pattern: " ^ LCGstringOf.internal_grammar_symbol 1 s ^ " " ^ LCGstringOf.internal_grammar_symbol 1 t)

let rec imp_selector s dir fv in_sem d = function
    Maybe t ->
    if d = Both || d = dir || dir = Both then
      let x = get_new_variable () in
      let y = get_new_variable () in
      [fv,Imp(s,dir,Maybe t),t,Lambda(x,Lambda(y,App(in_sem,Insert(Apply(Var x),Var y))))] else []
  | t ->  if d = Both || d = dir || dir = Both then [fv,s,t,in_sem] else []

let rec impset_selector s dir fv in_sem rev = function
    [],_ -> []
  | (d,Maybe t) :: l,i ->
    (* print_endline "impset_selector Maybe"; *)
    (if d = Both || d = dir || dir = Both then
       let x = get_new_variable () in
       let y = get_new_variable () in
       let s = if rev = [] && l = [] then s else ImpSet(s,List.rev rev @ l) in
       [fv,Imp(s,dir,Maybe t),t,Lambda(x,Lambda(y,App(LambdaRot(i,in_sem),Insert(Apply(Var x),Var y))))]
     else []) @
    (impset_selector s dir fv in_sem ((d,Maybe t) :: rev) (l,i+1))
  | (d,t) :: l,i ->
    (* print_endline "impset_selector"; *)
    (if d = Both || d = dir || dir = Both then
       let s = if rev = [] && l = [] then s else ImpSet(s,List.rev rev @ l) in
       [fv,s,t,LambdaRot(i,in_sem)]
     else []) @
    (impset_selector s dir fv in_sem ((d,t) :: rev) (l,i+1))

let rec deduce_tensor afv bfv rev_sems = function
    [] -> [afv,bfv,List.rev rev_sems]
  | (s,(t,v)) :: tensor_elems ->
    let l = internal_deduce_matching afv bfv v (s,t) in
    (*         Printf.printf "deduce_tensor: [%s] '%s' [%s] '%s' -> %d\n%!" (string_of_fv afv) (LCGstringOf.internal_grammar_symbol_prime s) (string_of_fv bfv) (LCGstringOf.internal_grammar_symbol_prime t) (Xlist.size l); *)
    Xlist.fold l [] (fun found (afv,bfv,sem) ->
        (deduce_tensor afv bfv (sem :: rev_sems) tensor_elems) @ found)

let rec deduce_matching afv bfv in_sem = function (* maczowany term * argument funktora *)
    Plus[s1;s2], t ->
        (* Printf.printf "\ndeduce_matching\nt=%s\n" (LCGstringOf.grammar_symbol 0 t);
        Printf.printf "s1=%s\n" (LCGstringOf.grammar_symbol 0 s1);
        Printf.printf "s2=%s\n" (LCGstringOf.grammar_symbol 0 s2);
        Printf.printf "afv=%s bfv=%s\n" (string_of_fv afv) (string_of_fv bfv); *)
        let x1 = get_new_variable () in
        let x2 = get_new_variable () in
        Xlist.fold (deduce_matching afv bfv (Var x1) (s1,t)) [] (fun l (afv,bfv,sem1) ->
          (* Printf.printf "1 afv=%s bfv=%s\n" (string_of_fv afv) (string_of_fv bfv); *)
          Xlist.fold (deduce_matching afv bfv (Var x2) (s2,t)) l (fun l (afv,bfv,sem2) ->
            (* Printf.printf "2 afv=%s bfv=%s\n" (string_of_fv afv) (string_of_fv bfv); *)
            (afv,bfv,Case(in_sem,[x1,sem1;x2,sem2])) :: l))
  (*  | Plus l, t -> (* zakładam, że afv jest pusty *)
        let x = get_new_variable () in
        let found = Xlist.multiply_list (Xlist.map l (fun s ->
          Xlist.map (deduce_matching afv bfv (Var x) (s,t)) (fun (afv,bfv,sem) ->
            if not (is_empty_fv afv) then failwith "deduce_matching: is_empty_fv afv" else
            bfv,sem))) in
        Xlist.fold found [] (fun found l ->
          try
            let bfv = Xlist.fold (List.tl l) (fst (List.hd l)) (fun bfv (frame_bfv,_) -> unify_fv bfv frame_bfv) in
            let sem = Case(in_sem,(Xlist.map l (fun (_,sem) -> x,sem))) in
            (empty_fv,bfv,sem) :: found
          with Not_found -> found)*)
  | s, Plus l -> (* istotne jest by prawy plus byl po lewym *)
    fst (Xlist.fold l ([],1) (fun (found,i) t -> Xlist.map (deduce_matching afv bfv in_sem (s,t)) (fun (afv,bfv,sem) -> afv,bfv,Inj(i,sem)) @ found, i+1))
  (*  | Star s, Star t ->
        let x = get_new_variable () in
        Xlist.map (deduce_matching afv bfv (Var x) (s,t)) (fun (afv,bfv,sem) -> afv,bfv,Map(in_sem,Lambda(x,sem)))*)
  | Star _, _ -> []
  | _, Star _ -> []
  | Conj _, _ -> []
  | _, Conj _ -> []
  | Preconj, _ -> []
  | _, Preconj -> []
  | StarWith l, s ->
      fst (Xlist.fold l ([],1) (fun (l,i) t -> (deduce_matching afv bfv (Proj(i,in_sem)) (t,s)) @ l, i+1))
  | WithVar(v,g,e,s),t ->
    Xlist.map (deduce_matching (add_fv afv v (g,e)) bfv (ProjVar(v,in_sem)) (s,t)) (fun (afv,bfv,sem) ->
        let g,e = find_fv afv v in
        remove_fv afv v,bfv,Subst(sem,v,make_subst e g))
  | One, Maybe _ -> [afv,bfv,Empty in_sem]
  | One, One -> [afv,bfv,in_sem]
  | One, _ -> []
  | _, One -> []
  | _, Maybe _ -> []
  | Imp(psi,d,phi), Imp(tau,dir,sigma) ->
    (List.flatten (Xlist.map (deduce_optarg in_sem phi) (fun sem -> deduce_matching afv bfv sem (psi,Imp(tau,dir,sigma))))) @
    let l = imp_selector psi dir afv in_sem d phi in
    List.flatten (Xlist.map l (fun (afv,psi,phi,sem) ->
        let x = get_new_variable () in
        let l = List.flatten (Xlist.map (deduce_matching bfv afv (Var x) (sigma,phi)) (fun (bfv,afv,p) ->
            deduce_matching afv bfv (App(sem,p)) (psi,tau))) in
        Xlist.map l (fun (afv,bfv,sem) -> afv,bfv,Lambda(x,sem))))
  | ImpSet(psi,phi_list), Imp(tau,dir,sigma) ->
    (List.flatten (Xlist.map (deduce_optargs in_sem phi_list) (fun sem -> deduce_matching afv bfv sem (psi,Imp(tau,dir,sigma))))) @
    let l = impset_selector psi dir afv in_sem [] (phi_list,1) in
    List.flatten (Xlist.map l (fun (afv,psi,phi,sem) ->
        let x = get_new_variable () in
        let l = List.flatten (Xlist.map (deduce_matching bfv afv (Var x) (sigma,phi)) (fun (bfv,afv,p) ->
            deduce_matching afv bfv (App(sem,p)) (psi,tau))) in
        Xlist.map l (fun (afv,bfv,sem) -> afv,bfv,Lambda(x,sem))))
  | Imp(s,d,s2), t ->
    List.flatten (Xlist.map (deduce_optarg in_sem s2) (fun sem -> deduce_matching afv bfv sem (s,t)))
  | ImpSet(s,l), t ->
    List.flatten (Xlist.map (deduce_optargs in_sem l) (fun sem -> deduce_matching afv bfv sem (s,t)))
  | _, Imp(s,d,s2) -> []
  | Tensor l1, Tensor l2 ->
    (*       Printf.printf "Tensor: [%s] '%s' [%s] '%s'\n%!" (string_of_fv afv) (LCGstringOf.grammar_symbol 1 (Tensor l1)) (string_of_fv bfv) (LCGstringOf.grammar_symbol 1 (Tensor l2)); *)
    if Xlist.size l1 <> Xlist.size l2 then [] else (
      let dots = Xlist.map (List.tl l1) (fun _ -> Dot) in
      (*       let variables = Xlist.map l2 (fun _ -> get_new_variable ()) in *)
      (*       let variables2 = Xlist.map variables (fun v -> Var v) in *)
      let sem_substs_list = deduce_tensor afv bfv [] (List.combine l1 (List.combine l2 (in_sem :: dots)(*variables2*))) in
      let l = Xlist.map sem_substs_list (fun (afv,bfv,sems) ->
          let sem = List.hd sems(*LetIn(variables,in_sem,Tuple sems)*) in
          afv,bfv,sem) in
      l)
  | Tensor _, _ -> []
  | _, Tensor _ -> []
  | Maybe _, _ -> [] (* zaślepka na potrzeby reguły koordynacji *)
  | s,t -> failwith ("deduce_matching: " ^ LCGstringOf.grammar_symbol 1 s ^ " " ^ LCGstringOf.grammar_symbol 1 t)

and deduce_optarg in_sem t =
  let l = deduce_matching empty_fv empty_fv (Dot(*Triple(Dot,Dot,Dot)*)) (One,t) in
  match l with
    [] -> []
  | [_,_,sem] -> [App(in_sem, sem)]
  | l -> (*print_endline ("deduce_optarg: " ^ LCGstringOf.grammar_symbol 0 t ^ " " ^
           String.concat " " (Xlist.map l (fun (_,_,sem) -> LCGstringOf.linear_term 0 sem)));*) failwith "deduce_optarg"

and deduce_optargs sem l =
  (* print_endline "deduce_optargs"; *)
  let b,sems = Xlist.fold (List.rev l) (true,[]) (fun (b,sems) (_,t) ->
      if not b then b,[] else
        let l = deduce_matching empty_fv empty_fv (Dot(*Triple(Dot,Dot,Dot)*)) (One,t) in
        if l = [] then false,[] else
          b,((List.hd l) :: sems)) in
  if b then
    [Xlist.fold sems sem (fun sem (_,_,s) -> App(LambdaRot(1,sem),s))]
  else []

let make_forward sem l = (* FIXME: po co jest ta procedura? *)
  (* print_endline "make_forward 1"; *)
  let l,sem,_ = Xlist.fold l ([],sem,1) (fun (l,sem,i) -> function
        Forward,t -> (Forward,t) :: l,sem,i+1
      | Both,t -> (Forward,t) :: l,sem,i+1
      | Backward,t ->
        (* print_endline "make_forward 2"; *)
        let res = deduce_matching empty_fv empty_fv Dot (One,t) in
        (* Printf.printf "make_forward 3 |res|=%d\n%!" (Xlist.size res); *)
        if res = [] then raise Not_found else
          let _,_,res = List.hd res in
          l, App(LambdaRot(i,sem),res), i) in
  (* print_endline "make_forward 3"; *)
  List.rev l, sem

let rec deduce_imp dir afv in_sem = function
    Tensor _ -> []
  | Star _ -> []
  | Conj _ -> []
  | Preconj -> []
  | Plus _ -> []
  | WithVar(v,g,e,s) -> (*print_endline "deduce_imp WithVar";*) deduce_imp dir (add_fv afv v (g,e)) (ProjVar(v,in_sem)) s
  | Imp(s,d,t) ->
    (* print_endline "deduce_imp Imp"; *)
    (List.flatten (Xlist.map (deduce_optarg in_sem t) (fun sem -> deduce_imp dir afv sem s))) @
    (imp_selector s dir afv in_sem d t)
  | ImpSet(s,l) ->
    (* print_endline "deduce_imp ImpSet 1"; *)
    let (l2,in_sem2),b =
      if dir = Backward then (l,in_sem),true
      else try make_forward in_sem l,true with Not_found -> ([],Dot),false in
    (* print_endline "deduce_imp ImpSet 2"; *)
    if b then
      (List.flatten (Xlist.map (deduce_optargs in_sem l) (fun sem -> deduce_imp dir afv sem s))) @
      (impset_selector s dir afv in_sem2 [] (l2,1))
    else []
  | StarWith l ->
    fst (Xlist.fold l ([],1) (fun (l,i) t ->
      (deduce_imp dir afv (Proj(i,in_sem)) t) @ l, i+1))
  | Maybe _ -> [] (* zaślepka na potrzeby reguły koordynacji *)
  | s -> failwith ("deduce_imp: " ^ LCGstringOf.grammar_symbol 1 s)

let rec deduce_app references dir (funct,funct_sem) args =
(*   Printf.printf "deduce_app 1: '%s' [%s]\n%!" (LCGstringOf.grammar_symbol 1 funct)
     (String.concat "; " (Xlist.map args (fun (arg,_) -> "'" ^ LCGstringOf.grammar_symbol 1 arg ^ "'")));*)
  let x = List.flatten (Xlist.map (deduce_imp dir empty_fv funct_sem funct) (fun (fv,psi,phi,funct_sem) ->
(*       print_endline "deduce_app 2"; *)
      let l = Xlist.fold args [] (fun l (arg,arg_sem) ->
          let res = deduce_matching empty_fv fv arg_sem (arg,phi) in
(*           Printf.printf "deduce_matching: '%s' '%s' -> %d\n%!" (LCGstringOf.grammar_symbol 1 arg) (LCGstringOf.grammar_symbol 1 phi) (Xlist.size res); *)
          res @ l) in
      let map = Xlist.fold l StringMap.empty (fun map (afv,bfv,sem) ->
          if not (is_empty_fv afv) then failwith "deduce_app" else
            StringMap.add_inc map (string_of_fv bfv) (bfv,[sem]) (fun (fv,sems) -> fv, sem :: sems)) in
      StringMap.fold map [] (fun l _ (bfv,sems) ->
          let sem = App(funct_sem,make_variant sems) in
          let reference = ExtArray.add references sem in
(*          Printf.printf "deduce_app 3: '%s' [%s]\n%!" (LCGstringOf.grammar_symbol 1 funct)
            (String.concat "; " (Xlist.map args (fun (arg,_) -> "'" ^ LCGstringOf.grammar_symbol 1 arg ^ "'")));
          Printf.printf "deduce_app 4: %d %s\n%!" reference (LCGstringOf.linear_term 0 sem);*)
          (fold_fv bfv (psi,Ref reference) (fun (t,sem) v (g,e) -> WithVar(v,g,e,t), VariantVar(v,sem))) :: l))) in
(*   print_endline "deduce_app 5"; *)
  x

let apply_coord references = function
    Star(arg,funct),coord_sem ->
      let x = get_new_variable () in
      let y = get_new_variable () in
      List.flatten (Xlist.map (deduce_imp Forward empty_fv (Var y) funct) (fun (fv,psi,phi,funct_sem) ->
        let l = deduce_matching empty_fv fv (Var x) (arg,phi) in
        let map = Xlist.fold l StringMap.empty (fun map (afv,bfv,sem) ->
          if not (is_empty_fv afv) then failwith "apply_coord" else
          let sem = ConcatCoord(Lambda(y,funct_sem),MapCoord(coord_sem,Lambda(x,sem))) in
          StringMap.add_inc map (string_of_fv bfv) (bfv,[sem]) (fun (fv,sems) -> fv, sem :: sems)) in
        StringMap.fold map [] (fun l _ (bfv,sems) ->
(*           print_endline "apply_coord"; *)
          let reference = ExtArray.add references (make_variant sems) in
          (fold_fv bfv (psi,Ref reference) (fun (t,sem) v (g,e) -> WithVar(v,g,e,t), VariantVar(v,sem))) :: l)))
      (* let x = get_new_variable () in
      let y = get_new_variable () in
      Xlist.rev_map (deduce_app references Forward (coord,Var y) [arg,Var x]) (fun (t,sem) ->
        t, ConcatCoord(MapCoord(coord_sem,Lambda(x,sem)))) *)
  | _ -> failwith "apply_coord"

let rec deduce_contraction references dir (funct,funct_sem) args =
  match funct with
    Star(funct,coord) ->
      let x = get_new_variable () in
      let l = deduce_contraction references dir (funct,Var x) args in
      let l = Xlist.rev_map l (fun (t,sem) -> Star(t,coord), MapCoord(funct_sem,Lambda(x,sem))) in
      Xlist.fold l l (fun l (t,sem) -> (apply_coord references (t,sem)) @ l)
  | Plus[funct1;funct2] ->
      let x1 = get_new_variable () in
      let x2 = get_new_variable () in
      let l1 = deduce_contraction references dir (funct1,Var x1) args in
      let l2 = deduce_contraction references dir (funct2,Var x2) args in
      Xlist.fold l1 [] (fun l (t1,sem1) ->
        Xlist.fold l2 l (fun l (t2,sem2) ->
          (Plus[t1;t2],Case(funct_sem,[x1,Inj(1,sem1);x2,Inj(2,sem2)])) :: l))
  | funct -> deduce_app references dir (funct,funct_sem) args

let rec make_uniq fv n v =
  if n = 1 then
    if mem_fv fv v then make_uniq fv (n+1) v else v
  else
    if mem_fv fv (v ^ string_of_int n) then make_uniq fv (n+1) v else v ^ string_of_int n

let internal_comp_substitute afv bfv arg_sem l = function
    AVar v ->
      let g,e = find_fv afv v in
      (match g with
          Atom _ -> remove_fv afv v, bfv, Subst(arg_sem,v,make_subst e g), g :: l
        | AVar _ -> remove_fv afv v, bfv, Subst(arg_sem,v,make_subst e g), g :: l
        | Top -> failwith "internal_comp_substitute"
        | _ ->
           let w = make_uniq bfv 1 v in
           remove_fv afv v, add_fv bfv w (g,e), Subst(arg_sem,v,make_subst e (AVar w)), AVar w :: l)
  | t -> afv,bfv,arg_sem,t :: l

let rec comp_substitute afv bfv arg_sem = function
    Tensor l ->
      let afv,bfv,arg_sem,l = Xlist.fold l (afv,bfv,arg_sem,[]) (fun (afv,bfv,arg_sem,l) t ->
        internal_comp_substitute afv bfv arg_sem l t) in
      let arg_sem = fold_fv afv arg_sem (fun arg_sem v (g,e) -> Subst(arg_sem,v,make_subst e g)) in
      bfv,Tensor(List.rev l),arg_sem
  (* | Plus l -> bfv,Plus l,arg_sem *)
      (* fst (Xlist.fold l ([],1) (fun (l,i) t ->
        let x = get_new_variable () in
        let arg_sem =
      (comp_substitute afv bfv (Inj(i,arg_sem)) t) @ l, i+1)) *)
  (* | One -> bfv,One,arg_sem *)
  | t -> failwith ("comp_substitute: " ^ LCGstringOf.grammar_symbol 0 t)

let rec split_comp_arg_plus arg_sem = function
    Tensor l -> [Tensor l,arg_sem]
  | Plus l ->
      fst (Xlist.fold l ([],1) (fun (l,i) t ->
        let x = get_new_variable () in
        let arg_sem = Lambda(x,App(arg_sem,Inj(i,Var x))) in
        (split_comp_arg_plus arg_sem t) @ l, i+1))
  | One -> []
  | t -> failwith ("split_comp_arg_plus: " ^ LCGstringOf.grammar_symbol 0 t)

let rec deduce_comp references dir_funct dir_arg (funct,funct_sem) args =
(*   Printf.printf "deduce_comp 1: '%s' [%s]\n%!" (LCGstringOf.grammar_symbol 1 funct) 
     (String.concat "; " (Xlist.map args (fun (arg,_) -> "'" ^ LCGstringOf.grammar_symbol 1 arg ^ "'")));*)
  let x = List.flatten (Xlist.map (deduce_imp dir_funct empty_fv funct_sem funct) (fun (fv,psi,phi,funct_sem) ->
(*       print_endline "deduce_comp 2"; *)
      let l = Xlist.fold args [] (fun l (arg,arg_sem) ->
        let dir_arg2 = if dir_arg = Backward then Both else dir_arg in (* To jest po to, żeby odwrócić make_forward  *)
        Xlist.fold (deduce_imp dir_arg2 empty_fv arg_sem arg) l (fun l (arg_fv,arg_psi,arg_phi,arg_sem) ->
          Xlist.fold (split_comp_arg_plus arg_sem arg_phi) l (fun l (arg_phi,arg_sem) ->
            (* let avars = get_avars arg_phi in *)
            let x = get_new_variable () in
            let res = deduce_matching arg_fv fv (App(arg_sem,Var x)) (arg_psi,phi) in
            Xlist.fold res l (fun l (afv,bfv,arg_sem) ->
              (* Xlist.fold (comp_substitute afv bfv arg_sem arg_phi) l (fun l (bfv,arg_phi,arg_sem) -> *)
              let bfv,arg_phi,arg_sem = comp_substitute afv bfv arg_sem arg_phi in
              (bfv,Imp(psi,dir_arg,arg_phi),Lambda(x,App(funct_sem,arg_sem))) :: l)))) in
        Xlist.fold l [] (fun l (bfv,t,sem) ->
          let reference = ExtArray.add references sem in
(*           Printf.printf "deduce_comp 3: %d %s\n%!" reference (LCGstringOf.linear_term 0 sem); *)
          (fold_fv bfv (t,Ref reference) (fun (t,sem) v (g,e) -> WithVar(v,g,e,t), VariantVar(v,sem))) :: l))) in
(*   print_endline "deduce_comp 4"; *)
  x

let deduce_forward_coord references coord_funct coord_sem args =
  match args with
    [] -> []
  | [arg,arg_sem] -> [Maybe(Star(arg,coord_funct)), App(coord_sem,arg_sem)]
  | _ ->
    let args,args_sem = List.split args in
    [Maybe(Star(StarWith args,coord_funct)), App(coord_sem,make_variant args_sem)]
  (*Xlist.rev_map args (fun (arg,arg_sem) ->
    (* let x = get_new_variable () in *)
    Maybe(Star(arg,coord_funct)), App(coord_sem,arg_sem))
    (* Lambda(x,Coord([Var x; arg_sem],coord_sem))) *)*)

let deduce_forward_precoord references coord_sem args =
  (*let args = Xlist.fold args [] (fun l -> function
      (Star(arg,coord_funct),arg_sem) as t -> t :: l
    | _ -> l) in
  if args = [] then [] else*)
  Xlist.fold args [] (fun l -> function
    (Star(arg,coord_funct),arg_sem) ->
    let x = get_new_variable () in
    let y = get_new_variable () in
    (Maybe(Star(arg,coord_funct)), Lambda(x,AddCoord(Inj(1,Var x),
       MapCoord(arg_sem,Lambda(y,Inj(2,Var y)))))) :: l
  | _ -> l)

let deduce_backward_coord references coord_funct (coord,coord_sem) args =
  let l = match args with
    [] -> []
  | [arg,arg_sem] -> [Star(Plus[arg;coord],coord_funct), App(coord_sem,arg_sem)]
  | _ ->
    let args,args_sem = List.split args in
    [Star(Plus[StarWith args;coord],coord_funct), App(coord_sem,make_variant args_sem)] in
(*  let l = Xlist.rev_map args (function (arg,arg_sem) ->
    Star(Plus[arg;coord],coord_funct), App(coord_sem,arg_sem)) in*)
  Xlist.fold l l (fun l (t,sem) -> (apply_coord references (t,sem)) @ l)

(*let rec forward_application = function
    (Bracket(lf,false,funct),sem), (Bracket(false,rf,arg),arg_sem) -> Xlist.map (deduce_app Forward (funct,sem) (arg,arg_sem)) (fun (t,sem) -> Bracket(lf,rf,t), LCGreductions.linear_term_beta_reduction2 sem)
  | (Bracket(lf,true,funct),sem), (Bracket(true,true,arg),arg_sem) -> Xlist.map (deduce_app Forward (funct,sem) (arg,arg_sem)) (fun (t,sem) -> Bracket(lf,true,t), LCGreductions.linear_term_beta_reduction2 sem)
  | (BracketSet(Forward),_), (Bracket(false,rf,arg),arg_sem) -> [Bracket(true,rf,arg),arg_sem]
  | ((x,_),(y,_)) -> (*Printf.printf "forward_application: '%s' '%s'\n%!" (LCGstringOf.grammar_symbol_prime x) (LCGstringOf.grammar_symbol_prime y);*) []

  let rec backward_application = function
    (Bracket(lf,false,arg),arg_sem), (Bracket(false,rf,funct),sem) -> Xlist.map (deduce_app Backward (funct,sem) (arg,arg_sem)) (fun (t,sem) -> Bracket(lf,rf,t), LCGreductions.linear_term_beta_reduction2 sem)
  | (Bracket(true,true,arg),arg_sem), (Bracket(true,rf,funct),sem) -> Xlist.map (deduce_app Backward (funct,sem) (arg,arg_sem)) (fun (t,sem) -> Bracket(true,rf,t), LCGreductions.linear_term_beta_reduction2 sem)
  | (Bracket(lf,false,arg),arg_sem), (BracketSet(Backward),_) -> [Bracket(lf,true,arg),arg_sem]
  | _ -> []*)

let forward_application references functs args =
(*  Printf.printf "forward_application: [%s] [%s]\n%!"
      (String.concat "; " (Xlist.map functs (fun (arg,_) -> "'" ^ LCGstringOf.grammar_symbol 1 arg ^ "'")))
      (String.concat "; " (Xlist.map args (fun (arg,_) -> "'" ^ LCGstringOf.grammar_symbol 1 arg ^ "'")));*)
  Xlist.fold functs [] (fun l -> function
        Bracket(lf,false,funct),sem ->
        let argst,argsf = Xlist.fold args ([],[]) (fun (argst,argsf) -> function
              Bracket(false,true,arg),arg_sem -> (arg,arg_sem) :: argst, argsf
            | Bracket(false,false,arg),arg_sem -> argst, (arg,arg_sem) :: argsf
            | _ -> argst,argsf) in
        let l = Xlist.fold (deduce_app references Forward (funct,sem) argst) l (fun l (t,sem) ->
            (Bracket(lf,true,t), (*LCGreductions.linear_term_beta_reduction2*) sem) :: l) in
        Xlist.fold (deduce_app references Forward (funct,sem) argsf) l (fun l (t,sem) ->
            (Bracket(lf,false,t), (*LCGreductions.linear_term_beta_reduction2*) sem) :: l)
      | Bracket(lf,true,funct),sem ->
        let args = Xlist.fold args [] (fun args -> function Bracket(true,true,arg),arg_sem -> (arg,arg_sem) :: args | _ -> args) in
        Xlist.fold (deduce_app references Forward (funct,sem) args) l (fun l (t,sem) ->
            (Bracket(lf,true,t), (*LCGreductions.linear_term_beta_reduction2*) sem) :: l)
      | BracketSet(Forward),_ -> Xlist.fold args l (fun l -> function Bracket(false,rf,arg),arg_sem -> (Bracket(true,rf,arg),arg_sem) :: l | _ -> l)
      | _ -> l)

let forward_application_ignore_brackets references functs args =
(*  Printf.printf "forward_application_ignore_brackets: [%s] [%s]\n%!"
      (String.concat "; " (Xlist.map functs (fun (arg,_) -> "'" ^ LCGstringOf.grammar_symbol 1 arg ^ "'")))
      (String.concat "; " (Xlist.map args (fun (arg,_) -> "'" ^ LCGstringOf.grammar_symbol 1 arg ^ "'")));*)
  Xlist.fold functs [] (fun l -> function
        Bracket(lf,false,funct),sem ->
        let argst,argsf = Xlist.fold args ([],[]) (fun (argst,argsf) -> function
              Bracket(_,true,arg),arg_sem -> (arg,arg_sem) :: argst, argsf
            | Bracket(_,false,arg),arg_sem -> argst, (arg,arg_sem) :: argsf
            | _ -> argst,argsf) in
        let l = Xlist.fold (deduce_app references Forward (funct,sem) argst) l (fun l (t,sem) ->
            (Bracket(lf,true,t), (*LCGreductions.linear_term_beta_reduction2*) sem) :: l) in
        Xlist.fold (deduce_app references Forward (funct,sem) argsf) l (fun l (t,sem) ->
            (Bracket(lf,false,t), (*LCGreductions.linear_term_beta_reduction2*) sem) :: l)
      | Bracket(lf,true,funct),sem ->
        let args = Xlist.fold args [] (fun args -> function Bracket(_,_,arg),arg_sem -> (arg,arg_sem) :: args | _ -> args) in
        Xlist.fold (deduce_app references Forward (funct,sem) args) l (fun l (t,sem) ->
            (Bracket(lf,true,t), (*LCGreductions.linear_term_beta_reduction2*) sem) :: l)
      | BracketSet(Forward),_ -> Xlist.fold args l (fun l -> function Bracket(_,rf,arg),arg_sem -> (Bracket(true,rf,arg),arg_sem) :: l | _ -> l)
      | _ -> l)

let forward_application_conll references functs args =
  Xlist.fold functs [] (fun l -> function
        Bracket(_,_,funct),sem ->
        let args = Xlist.fold args [] (fun args -> function Bracket(_,_,arg),arg_sem -> (arg,arg_sem) :: args | _ -> args) in
        Xlist.fold (deduce_app references Forward (funct,sem) args) l (fun l (t,sem) ->
            (Bracket(false,false,t), (*LCGreductions.linear_term_beta_reduction2*) sem) :: l)
      | _ -> l)

let forward_cross_composition references functs args =
  (* print_endline "fcc"; *)
  Xlist.fold functs [] (fun l -> function
        Bracket(lf,false,funct),sem ->
        (* Printf.printf "fcc 1 funct=%s\n%!" (LCGstringOf.grammar_symbol 0 funct); *)
        let argst,argsf = Xlist.fold args ([],[]) (fun (argst,argsf) -> function
              Bracket(false,true,arg),arg_sem -> (arg,arg_sem) :: argst, argsf
            | Bracket(false,false,arg),arg_sem -> argst, (arg,arg_sem) :: argsf
            | _ -> argst,argsf) in
        let l = Xlist.fold (deduce_comp references (*Forward*)Both Backward (funct,sem) argst) l (fun l (t,sem) ->
            (Bracket(lf,true,t), (*LCGreductions.linear_term_beta_reduction2*) sem) :: l) in
        Xlist.fold (deduce_comp references (*Forward*)Both Backward (funct,sem) argsf) l (fun l (t,sem) ->
            (Bracket(lf,false,t), (*LCGreductions.linear_term_beta_reduction2*) sem) :: l)
      | _ -> l)

let backward_application references args functs =
(*  Printf.printf "backward_application: [%s] [%s]\n%!"
      (String.concat "; " (Xlist.map args (fun (arg,_) -> "'" ^ LCGstringOf.grammar_symbol 1 arg ^ "'")))
      (String.concat "; " (Xlist.map functs (fun (arg,_) -> "'" ^ LCGstringOf.grammar_symbol 1 arg ^ "'")));*)
  Xlist.fold functs [] (fun l -> function
        Bracket(false,rf,funct),sem ->
        let argst,argsf = Xlist.fold args ([],[]) (fun (argst,argsf) -> function
              Bracket(true,false,arg),arg_sem -> (arg,arg_sem) :: argst, argsf
            | Bracket(false,false,arg),arg_sem -> argst, (arg,arg_sem) :: argsf
            | _ -> argst,argsf) in
        let l = Xlist.fold (deduce_app references Backward (funct,sem) argst) l (fun l (t,sem) ->
            (Bracket(true,rf,t), (*LCGreductions.linear_term_beta_reduction2*) sem) :: l) in
        Xlist.fold (deduce_app references Backward (funct,sem) argsf) l (fun l (t,sem) ->
            (Bracket(false,rf,t), (*LCGreductions.linear_term_beta_reduction2*) sem) :: l)
      | Bracket(true,rf,funct),sem ->
        let args = Xlist.fold args [] (fun args -> function Bracket(true,true,arg),arg_sem -> (arg,arg_sem) :: args | _ -> args) in
        Xlist.fold (deduce_app references Backward (funct,sem) args) l (fun l (t,sem) ->
            (Bracket(true,rf,t), (*LCGreductions.linear_term_beta_reduction2*) sem) :: l)
      | BracketSet(Backward),_ -> (*print_endline "tt";*) Xlist.fold args l (fun l -> function Bracket(lf,false,arg),arg_sem -> (Bracket(lf,true,arg),arg_sem) :: l | _ -> l)
      | _ -> l)

let backward_application_ignore_brackets references args functs =
(*  Printf.printf "backward_application_ignore_brackets: [%s] [%s]\n%!"
      (String.concat "; " (Xlist.map args (fun (arg,_) -> "'" ^ LCGstringOf.grammar_symbol 1 arg ^ "'")))
      (String.concat "; " (Xlist.map functs (fun (arg,_) -> "'" ^ LCGstringOf.grammar_symbol 1 arg ^ "'")));*)
  Xlist.fold functs [] (fun l -> function
        Bracket(false,rf,funct),sem ->
        let argst,argsf = Xlist.fold args ([],[]) (fun (argst,argsf) -> function
              Bracket(true,_,arg),arg_sem -> (arg,arg_sem) :: argst, argsf
            | Bracket(false,_,arg),arg_sem -> argst, (arg,arg_sem) :: argsf
            | _ -> argst,argsf) in
        let l = Xlist.fold (deduce_app references Backward (funct,sem) argst) l (fun l (t,sem) ->
            (Bracket(true,rf,t), (*LCGreductions.linear_term_beta_reduction2*) sem) :: l) in
        Xlist.fold (deduce_app references Backward (funct,sem) argsf) l (fun l (t,sem) ->
            (Bracket(false,rf,t), (*LCGreductions.linear_term_beta_reduction2*) sem) :: l)
      | Bracket(true,rf,funct),sem ->
        let args = Xlist.fold args [] (fun args -> function Bracket(_,_,arg),arg_sem -> (arg,arg_sem) :: args | _ -> args) in
        Xlist.fold (deduce_app references Backward (funct,sem) args) l (fun l (t,sem) ->
            (Bracket(true,rf,t), (*LCGreductions.linear_term_beta_reduction2*) sem) :: l)
      | BracketSet(Backward),_ -> (*print_endline "tt";*) Xlist.fold args l (fun l -> function Bracket(lf,_,arg),arg_sem -> (Bracket(lf,true,arg),arg_sem) :: l | _ -> l)
      | _ -> l)

let backward_application_conll references args functs =
  (*  Printf.printf "backward_application: [%s] [%s]\n%!"
      (String.concat "; " (Xlist.map args (fun (arg,_) -> "'" ^ LCGstringOf.grammar_symbol 1 arg ^ "'")))
      (String.concat "; " (Xlist.map functs (fun (arg,_) -> "'" ^ LCGstringOf.grammar_symbol 1 arg ^ "'"))); *)
  Xlist.fold functs [] (fun l -> function
        Bracket(_,_,funct),sem ->
        let args = Xlist.fold args [] (fun args -> function Bracket(_,_,arg),arg_sem -> (arg,arg_sem) :: args | _ -> args) in
        Xlist.fold (deduce_app references Backward (funct,sem) args) l (fun l (t,sem) ->
            (Bracket(false,false,t), (*LCGreductions.linear_term_beta_reduction2*) sem) :: l)
      | _ -> l)

let backward_cross_composition references args functs =
  (* Printf.printf "backward_cross_composition: [%s] [%s]\n%!"
      (String.concat ";\n " (Xlist.map args (fun (arg,_) -> "'" ^ LCGstringOf.grammar_symbol 1 arg ^ "'")))
      (String.concat ";\n " (Xlist.map functs (fun (arg,_) -> "'" ^ LCGstringOf.grammar_symbol 1 arg ^ "'"))); *)
  Xlist.fold functs [] (fun l -> function
        Bracket(false,rf,funct),sem ->
        (* Printf.printf "bcc 1 funct=%s\n%!" (LCGstringOf.grammar_symbol 0 funct); *)
        let argst,argsf = Xlist.fold args ([],[]) (fun (argst,argsf) -> function
              Bracket(true,false,arg),arg_sem -> (arg,arg_sem) :: argst, argsf
            | Bracket(false,false,arg),arg_sem -> argst, (arg,arg_sem) :: argsf
            | _ -> argst,argsf) in
        let l = Xlist.fold (deduce_comp references (*Backward*)Both Forward (funct,sem) argst) l (fun l (t,sem) ->
            (Bracket(true,rf,t), (*LCGreductions.linear_term_beta_reduction2*) sem) :: l) in
        Xlist.fold (deduce_comp references (*Backward*)Both Forward (funct,sem) argsf) l (fun l (t,sem) ->
            (Bracket(false,rf,t), (*LCGreductions.linear_term_beta_reduction2*) sem) :: l)
      | _ -> l)

let forward_coordination references coord args =
(*  Printf.printf "forward_application: [%s] [%s]\n%!"
      (String.concat "; " (Xlist.map coord (fun (arg,_) -> "'" ^ LCGstringOf.grammar_symbol 1 arg ^ "'")))
      (String.concat "; " (Xlist.map args (fun (arg,_) -> "'" ^ LCGstringOf.grammar_symbol 1 arg ^ "'")));*)
  Xlist.fold coord [] (fun l -> function
        Bracket(lf,false,Conj funct),sem ->
        let argst,argsf = Xlist.fold args ([],[]) (fun (argst,argsf) -> function
              Bracket(false,true,arg),arg_sem -> (arg,arg_sem) :: argst, argsf
            | Bracket(false,false,arg),arg_sem -> argst, (arg,arg_sem) :: argsf
            | _ -> argst,argsf) in
        let l = Xlist.fold (deduce_forward_coord references funct sem argst) l (fun l (t,sem) ->
            (Bracket(lf,true,t), (*LCGreductions.linear_term_beta_reduction2*) sem) :: l) in
        Xlist.fold (deduce_forward_coord references funct sem argsf) l (fun l (t,sem) ->
            (Bracket(lf,false,t), (*LCGreductions.linear_term_beta_reduction2*) sem) :: l)
      | Bracket(lf,false,Preconj),sem ->
        let argst,argsf = Xlist.fold args ([],[]) (fun (argst,argsf) -> function
              Bracket(false,true,arg),arg_sem -> (arg,arg_sem) :: argst, argsf
            | Bracket(false,false,arg),arg_sem -> argst, (arg,arg_sem) :: argsf
            | _ -> argst,argsf) in
        let l = Xlist.fold (deduce_forward_precoord references sem argst) l (fun l (t,sem) ->
            (Bracket(lf,true,t), (*LCGreductions.linear_term_beta_reduction2*) sem) :: l) in
        Xlist.fold (deduce_forward_precoord references sem argsf) l (fun l (t,sem) ->
            (Bracket(lf,false,t), (*LCGreductions.linear_term_beta_reduction2*) sem) :: l)
      | _ -> l)

let backward_coordination references args coord =
(*  Printf.printf "backward_application: [%s] [%s]\n%!"
      (String.concat "; " (Xlist.map args (fun (arg,_) -> "'" ^ LCGstringOf.grammar_symbol 1 arg ^ "'")))
      (String.concat "; " (Xlist.map coord (fun (arg,_) -> "'" ^ LCGstringOf.grammar_symbol 1 arg ^ "'")));*)
  Xlist.fold coord [] (fun l -> function
        Bracket(false,rf,Maybe(Star(t,coord_funct))),sem ->
        let argst,argsf = Xlist.fold args ([],[]) (fun (argst,argsf) -> function
              Bracket(true,false,arg),arg_sem -> (arg,arg_sem) :: argst, argsf
            | Bracket(false,false,arg),arg_sem -> argst, (arg,arg_sem) :: argsf
            | _ -> argst,argsf) in
        let l = Xlist.fold (deduce_backward_coord references coord_funct (t,sem) argst) l (fun l (t,sem) ->
            (Bracket(true,rf,t), (*LCGreductions.linear_term_beta_reduction2*) sem) :: l) in
        Xlist.fold (deduce_backward_coord references coord_funct (t,sem) argsf) l (fun l (t,sem) ->
            (Bracket(false,rf,t), (*LCGreductions.linear_term_beta_reduction2*) sem) :: l)
      | _ -> l)

let forward_contraction references functs args =
(*  Printf.printf "forward_application: [%s] [%s]\n%!"
      (String.concat "; " (Xlist.map functs (fun (arg,_) -> "'" ^ LCGstringOf.grammar_symbol 1 arg ^ "'")))
      (String.concat "; " (Xlist.map args (fun (arg,_) -> "'" ^ LCGstringOf.grammar_symbol 1 arg ^ "'")));*)
  Xlist.fold functs [] (fun l -> function
        Bracket(lf,false,Star(funct,coord)),sem ->
        let argst,argsf = Xlist.fold args ([],[]) (fun (argst,argsf) -> function
              Bracket(false,true,arg),arg_sem -> (arg,arg_sem) :: argst, argsf
            | Bracket(false,false,arg),arg_sem -> argst, (arg,arg_sem) :: argsf
            | _ -> argst,argsf) in
        let l = Xlist.fold (deduce_contraction references Forward (Star(funct,coord),sem) argst) l (fun l (t,sem) ->
            (Bracket(lf,true,t), (*LCGreductions.linear_term_beta_reduction2*) sem) :: l) in
        Xlist.fold (deduce_contraction references Forward (Star(funct,coord),sem) argsf) l (fun l (t,sem) ->
            (Bracket(lf,false,t), (*LCGreductions.linear_term_beta_reduction2*) sem) :: l)
      | Bracket(lf,true,Star(funct,coord)),sem ->
        let args = Xlist.fold args [] (fun args -> function Bracket(true,true,arg),arg_sem -> (arg,arg_sem) :: args | _ -> args) in
        Xlist.fold (deduce_contraction references Forward (Star(funct,coord),sem) args) l (fun l (t,sem) ->
            (Bracket(lf,true,t), (*LCGreductions.linear_term_beta_reduction2*) sem) :: l)
      | _ -> l)

let backward_contraction references args functs =
(*  Printf.printf "backward_application: [%s] [%s]\n%!"
      (String.concat "; " (Xlist.map args (fun (arg,_) -> "'" ^ LCGstringOf.grammar_symbol 1 arg ^ "'")))
      (String.concat "; " (Xlist.map functs (fun (arg,_) -> "'" ^ LCGstringOf.grammar_symbol 1 arg ^ "'")));*)
  Xlist.fold functs [] (fun l -> function
        Bracket(false,rf,Star(funct,coord)),sem ->
        let argst,argsf = Xlist.fold args ([],[]) (fun (argst,argsf) -> function
              Bracket(true,false,arg),arg_sem -> (arg,arg_sem) :: argst, argsf
            | Bracket(false,false,arg),arg_sem -> argst, (arg,arg_sem) :: argsf
            | _ -> argst,argsf) in
        let l = Xlist.fold (deduce_contraction references Backward (Star(funct,coord),sem) argst) l (fun l (t,sem) ->
            (Bracket(true,rf,t), (*LCGreductions.linear_term_beta_reduction2*) sem) :: l) in
        Xlist.fold (deduce_contraction references Backward (Star(funct,coord),sem) argsf) l (fun l (t,sem) ->
            (Bracket(false,rf,t), (*LCGreductions.linear_term_beta_reduction2*) sem) :: l)
      | Bracket(true,rf,Star(funct,coord)),sem ->
        let args = Xlist.fold args [] (fun args -> function Bracket(true,true,arg),arg_sem -> (arg,arg_sem) :: args | _ -> args) in
        Xlist.fold (deduce_contraction references Backward (Star(funct,coord),sem) args) l (fun l (t,sem) ->
            (Bracket(true,rf,t), (*LCGreductions.linear_term_beta_reduction2*) sem) :: l)
      | _ -> l)


(* FIXME: błąd przy redukcji "Jan chce iść spać" *)

let application_rules = [
  0,backward_application; 0,forward_application;
  0,backward_coordination; 0,forward_coordination;
  0,backward_contraction; 0,forward_contraction]
let application_rules_ignore_brackets = [
  0,backward_application_ignore_brackets; 0,forward_application_ignore_brackets;
  0,backward_coordination; 0,forward_coordination;
  0,backward_contraction; 0,forward_contraction]
let cross_composition_rules = [1,backward_cross_composition;1,forward_cross_composition]

let rec flatten_functor2 l seml = function
    Imp(s,d,t),Lambda(v,sem) -> flatten_functor2 ((d,t) :: l) (v :: seml) (s,sem)
  | ImpSet(s,l2),LambdaSet(vl,sem) -> flatten_functor2 (l2 @ l) (vl @ seml) (s,sem)
  | s,sem -> if l = [] then s,sem else ImpSet(s,l),LambdaSet(seml,sem)

let rec flatten_functor = function
    Bracket(lf,rf,s),t -> let s,t = flatten_functor (s,t) in Bracket(lf,rf,s),t
  | WithVar(v,g,e,s), VariantVar(x,t) -> let s,t = flatten_functor (s,t) in WithVar(v,g,e,s), VariantVar(x,t)
  | t -> flatten_functor2 [] [] t

let rec set_x_type = function
    Bracket(lf,rf,s),t -> let s,t = set_x_type (s,t) in Bracket(lf,rf,s),t
  | WithVar(v,g,e,s), t -> let s,t = set_x_type (s,t) in WithVar(v,g,e,s), t
  | Imp(s,d,t),sem -> let s,sem = set_x_type (s,sem) in Imp(s,d,t),sem
  | ImpSet(s,l2),sem -> let s,sem = set_x_type (s,sem) in ImpSet(s,l2),sem
  | Tensor s,sem -> Tensor[Atom "X"],sem
  | t,_ -> failwith ("set_x_type: " ^ LCGstringOf.grammar_symbol_prime t)

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
open Printf
(* open SubsyntaxTypes
open LexSemanticsTypes *)

let make size max_cost =
  let a = Array.make_matrix (size+1) (size+1) [| |] in
  Int.iter 0 size (fun i ->
    Int.iter 0 size (fun j ->
      a.(i).(j) <- Array.make (max_cost + 1) ([],0)));
  a

let last_node chart = Array.length chart - 1
let max_cost chart = Array.length chart.(0).(0) - 1

let add chart i j cost v layer =
  chart.(i).(j).(cost) <- [v],layer;
  chart

let add_list chart i j cost l layer =
  chart.(i).(j).(cost) <- l,layer;
  chart

let add_inc chart i j cost v layer =
  let l,layer2 = chart.(i).(j).(cost) in
  chart.(i).(j).(cost) <- v :: l, max layer layer2;
  chart

let add_inc_list chart i j cost l layer =
  let l2,layer2 = chart.(i).(j).(cost) in
  chart.(i).(j).(cost) <- l @ l2, max layer layer2;
  chart

let find chart i j cost = fst chart.(i).(j).(cost)
let layer chart i j cost = snd chart.(i).(j).(cost)

let fold chart s f =
  Int.fold 0 (last_node chart) s (fun s i ->
      Int.fold 0 (last_node chart) s (fun s j ->
        Int.fold 0 (max_cost chart) s (fun s cost ->
          let layer = layer chart i j cost in
          Xlist.fold (find chart i j cost) s (fun s (symbol,sem) ->
              f s (symbol,i,j,cost,sem,layer)))))

let copy chart =
  let a = Array.copy chart in
  Int.iter 0 (last_node chart) (fun i ->
    let b = Array.copy a.(i) in
    Int.iter 0 (last_node chart) (fun j ->
      let c = Array.copy b.(j) in
      b.(j) <- c);
    a.(i) <- b);
  a
              
let rec find_paths_rec chart last i =
  if i = last then [[]] else
    Int.fold (i+1) last [] (fun paths j ->
      Int.fold 0 (max_cost chart) paths (fun paths cost ->
        if find chart i j cost = [] then paths else
          let tails = find_paths_rec chart last j in
          if Xlist.size tails > 1000000 then failwith "find_paths_rec: to many paths" else
            Xlist.fold tails paths (fun paths tail -> (find chart i j cost :: tail) :: paths)))

let find_paths chart =
  let last = last_node chart - 1 in
  find_paths_rec chart last 0

let get_no_entries chart =
  Int.fold 0 (last_node chart) 0 (fun n i ->
      Int.fold 0 (last_node chart) n (fun n j ->
        Int.fold 0 (max_cost chart) n (fun n cost ->
          n + (Xlist.size (find chart i j cost)))))

(* Pod referencją 0 będzie trzymany korzeń termu *)
let lazify chart =
  let new_chart = make (last_node chart) (max_cost chart) in
  let references = ExtArray.make (2 * last_node chart) Dot in
  let _ = ExtArray.add references Dot in (* to jest potrzebne by na pozycji 0 umieścić korzeń termu *)
  let new_chart = fold chart new_chart (fun new_chart (symbol,i,j,cost,sem,layer) ->
      let n = ExtArray.add references sem in
      add_inc new_chart i j cost (symbol,Ref n) layer) in
  new_chart, references

let rec dep_lazify_rec references (DepNode(id,left,l,right)) =
  (* Printf.printf "dep_lazify_rec %s\n" id; *)
  let l = Xlist.rev_map l LCGrules.flatten_functor in
  let l = Xlist.fold l [] (fun l (symbol,sem) ->
      let n = ExtArray.add references sem in
      (symbol,Ref n) :: l) in
  let left = Xlist.map left (dep_lazify_rec references) in
  let right = Xlist.map right (dep_lazify_rec references) in
  DepNode(id,left,l,right)

let dep_lazify dep_chart =
  let references = ExtArray.make 100 Dot in
  let _ = ExtArray.add references Dot in (* to jest potrzebne by na pozycji 0 umieścić korzeń termu *)
  dep_lazify_rec references dep_chart, references

let merge_sems l = (* FIXME: dodać warianty *)
  let map = Xlist.fold l SymbolMap.empty (fun map (t,sem) -> SymbolMap.add_inc map t [sem] (fun l -> sem :: l)) in
  SymbolMap.fold map [] (fun l t sems -> (t,LCGrules.make_variant sems) :: l)

(*let merge_sems l = (* FIXME: dodać warianty *)
  let map = Xlist.fold l SymbolMap.empty (fun map (cost,(t,sem)) -> SymbolMap.add_inc map t (cost,[sem]) (fun (min_cost,l) ->
    if min_cost < cost then min_cost,l else
    if min_cost > cost then cost,[sem] else
    cost, sem :: l)) in
  SymbolMap.fold map [] (fun l t (cost,sems) -> (cost,(t,LCGrules.make_variant sems)) :: l)*)

let make_unique chart i j =
  Int.iter 0 (max_cost chart) (fun cost ->
    let l,layer = chart.(i).(j).(cost) in
    let l = merge_sems l in
    chart.(i).(j).(cost) <- l, layer);
  chart

(*let parse rules chart references timeout time_fun =
  (* print_endline "parse 1"; *)
  (* LCGrules.references := refs;
     LCGrules.next_reference := next_ref; *)
  let start_time = time_fun () in
  let last_node = last_node chart in
  let chart = Int.fold 2 last_node chart (fun chart len ->
      Int.fold 0 (last_node - len) chart (fun chart i ->
          let k = i + len in
          Int.fold 1 (len - 1) chart (fun chart d ->
              let time = time_fun () in
              if time -. start_time > timeout then raise (Timeout(time -. start_time)) else
                let j = i + d in
                let l,lay = Xlist.fold rules (find chart i k,layer chart i k) (fun (l,lay) (cost,rule) ->
                    (rule references (find chart i j) (find chart j k)) @ l,
                    (*          Xlist.fold (find chart i j) l (fun l a ->
                                 Xlist.fold (find chart j k) l (fun l b ->
                                   (rule (a,b)) @ l)),*)
                    max lay ((max (layer chart i j) (layer chart j k)) + 1)) in
                (* print_int i; print_string " "; print_int j; print_string " "; print_int k; print_newline (); *)
                (*         let l = LCGreductions.merge_symbols l in *)
                (*          if Xlist.size l > 0 then Printf.printf "parse: %d-%d |l|=%d\n%!" i k (Xlist.size l); *)
                make_unique (add_list chart i k l lay) i k))) in
  (* print_endline "parse 2"; *)
  chart*)

let parse rules chart references timeout time_fun =
(*   Printf.printf "parse 1: %d\n" (ExtArray.size references); *)
  (* LCGrules.references := refs;
     LCGrules.next_reference := next_ref; *)
  let start_time = time_fun () in
  let last_node = last_node chart in
  let max_cost = max_cost chart in
  let chart = Int.fold 2 last_node chart (fun chart len ->
      Int.fold 0 (last_node - len) chart (fun chart i ->
          let k = i + len in
          Int.fold 1 (len - 1) chart (fun chart d ->
              let time = time_fun () in
              if time -. start_time > timeout then raise (Timeout(time -. start_time)) else
              let j = i + d in
(*               Printf.printf "parse 2: %d i=%d j=%d k=%d\n" (ExtArray.size references) i j k; *)
              let chart (*l,lay*) = Xlist.fold rules chart(*find chart i k,layer chart i k*) (fun chart(*l,lay*) (cost,rule) ->
                Int.fold 0 max_cost chart (fun chart cost1 ->
                  Int.fold 0 max_cost chart (fun chart cost2 ->
                    let t1 = find chart i j cost1 in
                    let t2 = find chart j k cost2 in
                    if cost1 + cost2 + cost > max_cost then chart(*l,lay*) else (
(*                     let size1 = ExtArray.size references in *)
(*                     print_endline "parse 1"; *)
                    let chart = add_inc_list chart i k (cost1 + cost2 + cost) (rule references t1 t2)
                      ((max (layer chart i j cost1) (layer chart j k cost2)) + 1) in
(*                     print_endline "parse 2"; *)
(*                     let size2 = ExtArray.size references in *)
(*                     if size1 < size2 then Printf.printf "parse 3: %d %d i=%d j=%d k=%d\n" size1 size2 i j k; *)
                    chart)))) in
              make_unique chart i k))) in
(*   Printf.printf "parse 4: %d\n" (ExtArray.size references); *)
  chart

let assign_not_parsed left right (t,sem) =
  let sem = if left = [] then sem else (print_endline "assign_not_parsed: ni 1"; sem) in
  let sem = if right = [] then sem else (print_endline "assign_not_parsed: ni 2"; sem) in
  t, sem

let rec dep_parse_rec references start_time timeout time_fun (DepNode(id,left,funct,right)) =
  (* printf "dep_parse_rec 1 id=%d\n%!" id; *)
  let time = time_fun () in
  if time -. start_time > timeout then raise (Timeout(time -. start_time)) else
    let left = Xlist.map left (dep_parse_rec references start_time timeout time_fun) in
    let right = Xlist.map right (dep_parse_rec references start_time timeout time_fun) in
    (* printf "dep_parse_rec 2 id=%d\n%!" id; *)
    let funct,left = Xlist.fold left (funct,[]) (fun (funct,left) arg ->
        (* printf "larg: %s\n" (LCGstringOf.symbol_sem_list arg);
           printf "funct: %s\n" (LCGstringOf.symbol_sem_list funct); *)
        match LCGrules.backward_application_conll references arg funct with
          [] -> (*printf "NOT PARSED\n";*) funct, arg :: left
        | funct -> merge_sems funct, left) in
    (* printf "dep_parse_rec 3 |right|=%d \n%!" (Xlist.size right); *)
    let funct,right = Xlist.fold right (funct,[]) (fun (funct,right) arg ->
        (* printf "funct: %s\n" (LCGstringOf.symbol_sem_list funct);
           printf "rarg: %s\n" (LCGstringOf.symbol_sem_list arg);  *)
        match LCGrules.forward_application_conll references funct arg with
          [] -> (*printf "NOT PARSED\n";*) funct, arg :: right
        | funct -> merge_sems funct, right) in
    (* printf "dep_parse_rec 4\n%!"; *)
    if left = [] && right = [] then funct else (
      let xleft = Xlist.rev_map left (fun arg -> Xlist.rev_map arg LCGrules.set_x_type) in
      let xright = Xlist.rev_map right (fun arg -> Xlist.rev_map arg LCGrules.set_x_type) in
      (* printf "dep_parse_rec 5\n%!"; *)
      let xfunct,xleft = Xlist.fold xleft (funct,[]) (fun (xfunct,left) arg ->
          (* printf "larg: %s\n" (LCGstringOf.symbol_sem_list arg);
             printf "funct: %s\n" (LCGstringOf.symbol_sem_list xfunct); *)
          match LCGrules.backward_application_conll references arg xfunct with
            [] -> (*printf "NOT PARSED\n";*) xfunct, arg :: left
          | xfunct -> merge_sems xfunct, left) in
      (* printf "dep_parse_rec 6\n%!"; *)
      let xfunct,xright = Xlist.fold xright (xfunct,[]) (fun (xfunct,right) arg ->
          (* printf "funct: %s\n" (LCGstringOf.symbol_sem_list xfunct);
             printf "rarg: %s\n" (LCGstringOf.symbol_sem_list arg); *)
          match LCGrules.forward_application_conll references xfunct arg with
            [] -> (*printf "NOT PARSED\n";*) xfunct, arg :: right
          | xfunct -> merge_sems xfunct, right) in
      (* printf "dep_parse_rec 7\n%!"; *)
      if xleft = [] && xright = [] then xfunct else
        raise (NotDepParsed(id,left,funct,right)))

let dep_parse dep_chart references timeout time_fun =
  (*   print_endline "dep_parse"; *)
  let start_time = time_fun () in
  let parsed_dep_chart = dep_parse_rec references start_time timeout time_fun dep_chart in
  parsed_dep_chart

let is_parsed_cost chart cost =
  let n = last_node chart in
  Xlist.fold (find chart 0 n cost) false (fun b -> function
        Bracket(true,true,Tensor[Atom "<root>"]), _ -> true
      (* | Bracket(true,true,Tensor[Atom "<ors-sentence>"]), _ -> true *)
      | _ -> b)

let is_parsed chart =
  Int.fold 0 (max_cost chart) false (fun b cost ->
    b || is_parsed_cost chart cost)

let rec get_parsed_cost_rec chart cost =
  if is_parsed_cost chart cost then cost else
  get_parsed_cost_rec chart (cost+1)

let get_parsed_cost chart = (* zakładam, że is_parsed chart = true *)
  get_parsed_cost_rec chart 0

let is_dep_parsed = function
    [] -> false
  | [Bracket(false,false,Tensor[Atom "<conll_root>"]),_] -> true
  | [Bracket(false,false,Imp(Tensor[Atom("<conll_root>")],Forward,Maybe _)),sem]-> true
  | _ -> false
  (* | [t,_] -> print_endline @@ LCGstringOf.grammar_symbol_prime t; failwith "is_dep_parsed"
  | l -> failwith ("is_dep_parsed " ^ (String.concat " " @@ List.map (fun x -> LCGstringOf.grammar_symbol 0 @@ fst x) l)) *)
(*  | l -> failwith ("is_dep_parsed " ^ (string_of_int @@ List.length l))
*)
let get_parsed_term chart =
  let n = last_node chart in
  let l = Xlist.fold (find chart 0 n (get_parsed_cost chart)) [] (fun l -> function
        Bracket(true,true,Tensor[Atom "<root>"]), sem -> (Cut(Tuple[sem])) :: l
      | Bracket(false,false,Imp(Tensor[Atom("<conll_root>")],Forward,Maybe _)) as t,sem->
        let sem = List.hd (LCGrules.deduce_optarg sem t) in
        (Cut(Tuple[sem])) :: l
      (* | Bracket(true,true,Tensor[Atom "<ors-sentence>"]), sem -> (Cut (Tuple[sem])) :: l *)
      | _ -> l) in
  Node{LCGrenderer.empty_node with lemma="<root>"; pos="<root>"; args=LCGrules.make_variant l}

let get_dep_parsed_term = function
    [Bracket(false,false,Tensor[Atom "<conll_root>"]),sem] ->
    let l = [Cut (Tuple[sem])] in
    Node{LCGrenderer.empty_node with lemma="<root>"; pos="<root>"; args=LCGrules.make_variant l}
  | _ -> failwith "get_dep_parsed_term"

(* FIXME: poprawić poniższe *)
(*
let get_parsed_term tokens lex_sems chart =
  let n = last_node chart in
  let l = Xlist.fold (find chart 0 n) [] (fun l -> function
        Bracket(true,true,Tensor[Atom "<root>"]), sem -> (Cut(Tuple[sem])) :: l
      | Bracket(false,false,Imp(Tensor[Atom("<conll_root>")],Forward,Maybe _)) as t,sem->
        let sem = List.hd (LCGrules.deduce_optarg sem t) in
        (Cut(Tuple[sem])) :: l
      (* | Bracket(true,true,Tensor[Atom "<ors-sentence>"]), sem -> (Cut (Tuple[sem])) :: l *)
      | _ -> l) in
  let id = ExtArray.add tokens {empty_token with token=Lemma("<root>","interp",[])} in
  let _ = ExtArray.add lex_sems empty_lex_sem in
  Node{LCGrenderer.empty_node with
                pred="<root>";
                cat="interp";
                id=id;
                agf=WalTypes.NOSEM;
                args=LCGrules.make_variant l}

let get_dep_parsed_term tokens lex_sems = function
    [Bracket(false,false,Tensor[Atom "<conll_root>"]),sem] ->
    let id = ExtArray.add tokens {empty_token with token=Lemma("<root>","interp",[])} in
    let _ = ExtArray.add lex_sems empty_lex_sem in
    let l = [Cut (Tuple[sem])] in
    Node{LCGrenderer.empty_node with
                  pred="<root>";
                  cat="interp";
                  id=id;
                  agf=WalTypes.NOSEM;
                  args=LCGrules.make_variant l}
  | _ -> failwith "get_dep_parsed_term"
*)

let select_best_paths l =
  (* print_endline "select_best_paths"; *)
  Xlist.fold l (max_int,max_int,max_int,max_int,[]) (fun (min_len,min_len2,min_args,min_ee,paths) (len,len2,args,ee,path) ->
    if min_ee < ee then min_len,min_len2,min_args,min_ee,paths else
    if min_ee > ee then len,len2,args,ee,[path] else
    if min_args < args then min_len,min_len2,min_args,min_ee,paths else
    if min_args > args then len,len2,args,ee,[path] else
    if min_len < len then min_len,min_len2,min_args,min_ee,paths else
    if min_len > len then len,len2,args,ee,[path] else
    len,len2,args,ee,path :: paths)

let rec is_excluded = function
    Tensor[Atom "<root>"] -> true
  | Imp(Tensor[Atom "<sentence>";_],_,_) -> true
  | Imp(Tensor[Atom "s";_;_],_,Maybe _) -> false
  | Imp(Tensor[Atom "s";_;_],_,_) -> true
  | Tensor l -> false
  | Imp(t,d,t2) -> is_excluded t
  | One -> false
  | ImpSet(t,l) -> is_excluded t
  | WithVar(v,g,e,t) -> is_excluded t
  | Bracket(lf,rf,t) -> is_excluded t
  | BracketSet t -> true
  | Star _ -> false
  | Preconj -> false
  | Conj _ -> false
  | Maybe _ -> false
  | t -> failwith ("is_excluded: " ^ LCGstringOf.grammar_symbol_prime t)

let select_best_symbol references symbol_sem_list =
  let fv = LCGrules.add_fv LCGrules.empty_fv "node" (Atom "concept","") in
  let fv = LCGrules.add_fv fv "coerced" (Atom "NULL","") in
  let fv = LCGrules.add_fv fv "role" (Atom "Concept","") in
(*   print_endline "select_best_symbol"; *)
  Xlist.fold symbol_sem_list (max_int,[]) (fun (min_args,sem_list) (symbol,sem) ->
    (* print_endline ("select_best_symbol 1: " ^ LCGstringOf.grammar_symbol 0 symbol); *)
    let args = LCGrenderer.count_req_args symbol in
    (* print_endline ("select_best_symbol 2: args=" ^ string_of_int args); *)
    let symbol,sem = if is_excluded symbol then One,Dot else symbol,sem in
    if min_args < args then min_args,sem_list else
    if min_args > args then args,[snd (LCGrenderer.apply_args references (*LCGrules.empty_*)fv (symbol,sem))] else
    args, (snd (LCGrenderer.apply_args references (*LCGrules.empty_*)fv (symbol,sem))) :: sem_list)

let rec is_irrelevant = function
    Node t -> 
      (try
        let cat = Xlist.assoc t.attrs "CAT" in
        if cat = Val "Token" || cat = Val "Interp" then true else false
      with _ -> true)
  | Variant(e,l) -> Xlist.fold l false (fun b (i,t) -> b || is_irrelevant t)
  | Dot -> true
  | _ -> true
(*   | t -> failwith ("is_irrelevant: " ^ LCGstringOf.linear_term 0 t) *)
    
let merge_paths (len,len2,args,ee,paths) (args2,sem_list) =
(*   print_endline "merge_paths"; *)
  let sem = LCGrules.make_variant sem_list in
  let a = if is_irrelevant sem then 0 else 1 in
  len+1, len2+a, args+args2, ee, Tuple[Cut(Tuple[sem]); LCGrules.make_variant paths]
  
(*let get_text_fragment text_fragments node1 node2 = (* FIXME: kopia z Visualization *)
  try IntMap.find text_fragments.(node1) node2
  with (*Not_found*)_ -> "???"(*failwith (Printf.sprintf "chart: text_fragment not found %d-%d" node1 node2)*)*)

let add_empty_edge par_string node_mapping i (len,len2,args,ee,paths) =
(*   print_endline "add_empty_edge"; *)
  let s = MarkedHTMLof.get_text_fragment par_string node_mapping i (i+1) in
  let sem = Node {LCGrenderer.empty_node with
    lemma=s;
    pos="<raw>";
    arg_symbol=Tuple[Val "<raw>"];
    arg_dir="both";
    attrs=["CAT",Val "Token";"NODE-ID",Val(string_of_int i);"ROLE",Val "Token";"NODE", Val "concept"]} in
  len+1, len2, args, ee+1, Tuple[Cut(sem); LCGrules.make_variant paths]
  
let make_root_symbol (_,_,_,_,paths) =
(*   print_endline "make_root_symbol"; *)
  let sem = Node {LCGrenderer.empty_node with
    lemma="<merge>";
    pos="<merge>";
    arg_symbol=Tuple[Val "<merge>"];
    arg_dir="both";
    args=LCGrules.make_variant paths;
    attrs=["NODE", Val "concept";"ROLE",Val "CORE"]} in
  Bracket(true,true,Tensor[Atom "<root>"]), sem

let select_symbols symbol_sem_list =
  Xlist.fold symbol_sem_list [] (fun symbol_sem_list -> function 
      Bracket(_,_,Star _),_ -> symbol_sem_list
    | Bracket(_,_,Preconj),_ -> symbol_sem_list
    | Bracket(_,_,Conj _),_ -> symbol_sem_list
    | Bracket(_,_,Maybe _),_ -> symbol_sem_list
    | x -> x :: symbol_sem_list)
  
let get_len (len,len2,_,_,_) = max len2 1
  
let merge par_string node_mapping chart references =
  let n = last_node chart in
  let a = Array.make (n+1) [] in
(*   print_endline "merge 1"; *)
  a.(0) <- [0,0,0,0,Dot];
  Int.iter 0 (n - 1) (fun i ->
    let paths = select_best_paths a.(i) in
    a.(i+1) <- add_empty_edge par_string node_mapping i paths :: a.(i+1);
    Int.iter i n (fun j ->
      let symbol_sem_list =
        select_symbols (Int.fold 0 (max_cost chart) [] (fun l cost -> (find chart i j cost) @ l)) in
      if symbol_sem_list <> [] then a.(j) <- merge_paths paths (select_best_symbol references symbol_sem_list) :: a.(j)));
(*   print_endline "merge 2"; *)
  let paths = select_best_paths a.(n) in
(*   print_endline "merge 3"; *)
  add_inc chart 0 n 0 (make_root_symbol paths) 0, get_len paths

let select_maximal chart =
  let last = last_node chart in
  let a = Array.make last (-1,[],-1) in
  let _ = fold chart () (fun chart (symbol,i,j,cost,sem,layer) ->
    let j0,l,_ = a.(i) in
    if j > j0 then a.(i) <- j,[symbol,sem],layer else
    if j < j0 then () else
    a.(i) <- j,(symbol,sem) :: l,layer) in
  let chart = make last 0 in
  snd (Int.fold 0 (last-1) (-1,chart) (fun (j0,chart) i ->
    let j,l,layer = a.(i) in
    if j <= j0 then j0,chart else
    j,add_list chart i j 0 l layer))

(*FIXME:  Bębni na maszynie do pisania.
          Na myśl o czym brykasz?*)

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

let rec internal_substitute var_name t = function
  | Atom x -> Atom x
  | AVar x -> if x = var_name then t else AVar x
  | With l -> With (Xlist.map l (internal_substitute var_name t))
  | Zero -> Zero
  | Top -> Top

let rec substitute var_name t = function
  | Tensor l -> Tensor (Xlist.map l (internal_substitute var_name t))
  | Plus l -> Plus (Xlist.map l (substitute var_name t))
  | StarWith l -> StarWith (Xlist.map l (substitute var_name t))
  | Imp(s,d,t2) -> Imp(substitute var_name t s,d,substitute var_name t t2)
  | One -> One
  | ImpSet(s,l) -> ImpSet(substitute var_name t s, Xlist.map l (fun (d,s) -> d, substitute var_name t s))
  | WithVar(v,g,e,s) -> if v = var_name then WithVar(v,g,e,s) else WithVar(v,internal_substitute var_name t g,e,substitute var_name t s)
  | Star(s,s2) -> Star(substitute var_name t s,substitute var_name t s2)
  | Conj s -> Conj(substitute var_name t s)
  | Preconj -> Preconj
  | Bracket(lf,rf,s) -> Bracket(lf,rf,substitute var_name t s)
  | BracketSet d -> BracketSet d
  | Maybe s -> Maybe (substitute var_name t s)

let rec substitute_schema var_name t = function
  | Tensor l -> Tensor l
  | Plus l -> Plus (Xlist.map l (substitute_schema var_name t))
  | StarWith l -> StarWith (Xlist.map l (substitute_schema var_name t))
  | Imp(s,d,t2) -> Imp(substitute_schema var_name t s,d,substitute_schema var_name t t2)
  | One -> One
  | ImpSet(s,l) -> ImpSet(substitute_schema var_name t s, List.flatten (Xlist.map l (function
        Both,Tensor[AVar var_name2] -> if var_name = var_name2 then t else [Both,Tensor[AVar var_name2]]
      | d,s -> [d, substitute_schema var_name t s])))
  | WithVar(v,g,e,s) -> WithVar(v,g,e,substitute_schema var_name t s)
  | Star(s,s2) -> Star(substitute_schema var_name t s,substitute_schema var_name t s2)
  | Conj s -> Conj (substitute_schema var_name t s)
  | Preconj -> Preconj
  | Bracket(lf,rf,s) -> Bracket(lf,rf,substitute_schema var_name t s)
  | BracketSet d -> BracketSet d
  | Maybe s -> Maybe (substitute_schema var_name t s)

let rec internal_count_avar var_name = function
    Atom _ -> 0
  | AVar x -> if x = var_name then 1 else 0
  | With l -> Xlist.fold l 0 (fun b t -> internal_count_avar var_name t + b)
  | Zero -> 0
  | Top -> 0

let rec count_avar var_name = function
  | Tensor l -> Xlist.fold l 0 (fun b t -> internal_count_avar var_name t + b)
  | Plus l -> Xlist.fold l 0 (fun b t -> count_avar var_name t + b)
  | StarWith l -> Xlist.fold l 0 (fun b t -> count_avar var_name t + b)
  | Imp(s,d,t2) -> count_avar var_name s + count_avar var_name t2
  | One -> 0
  | ImpSet(s,l) -> count_avar var_name s + Xlist.fold l 0 (fun b (_,t) -> count_avar var_name t + b)
  | WithVar(v,g,e,s) -> if v = var_name then 0 else count_avar var_name s +  internal_count_avar var_name g
  | Star(t,t2) -> count_avar var_name t + count_avar var_name t2
  | Conj t -> count_avar var_name t
  | Preconj -> 0
  | Bracket(lf,rf,s) -> count_avar var_name s
  | BracketSet _ -> 0
  | Maybe t -> count_avar var_name t

let rec substitute_substvar v g = function
    Var v as t -> t
  | Tuple l -> Tuple(Xlist.map l (substitute_substvar v g))
  (*   | LetIn(l,s,t) -> LetIn(l,substitute_substvar v g s,substitute_substvar v g t) *)
  | Variant(e,l) -> Variant(e,Xlist.map l (fun (i,t) -> i,substitute_substvar v g t))
  | VariantVar(v2,t) -> if v2 = v then VariantVar(v2,t) else VariantVar(v2,substitute_substvar v g t)
  | SubstVar v2 -> if v2 = v then g else SubstVar v2
  | Case(t,l) -> Case(substitute_substvar v g t,Xlist.map l (fun (x,t) -> x,substitute_substvar v g t))
  | App(s,t) -> App(substitute_substvar v g s,substitute_substvar v g t)
  | Lambda(v2,t) -> Lambda(v2,substitute_substvar v g t)
  | LambdaSet(l,t) -> LambdaSet(l,substitute_substvar v g t)
  | Dot -> Dot
  | Val s -> Val s
  | SetAttr(e,s,t) -> SetAttr(e,substitute_substvar v g s,substitute_substvar v g t)
  | Fix(s,t) -> Fix(substitute_substvar v g s,substitute_substvar v g t)
  | Node t -> Node{t with attrs=Xlist.map t.attrs (fun (e,t) -> e, substitute_substvar v g t);
                          symbol=substitute_substvar v g t.symbol;
                          arg_symbol=substitute_substvar v g t.arg_symbol;
                          args=substitute_substvar v g t.args}
  | Cut t -> Cut(substitute_substvar v g t)
  | t -> failwith ("substitute_substvar: " ^ LCGstringOf.linear_term 0 t)


let empty_node = {
  orth=""; lemma=""; pos=""; weight=0.; id=0; symbol=Dot; arg_symbol=Dot; arg_dir=""; attrs=[]; args=Dot;}

let variable_num_ref = ref 0

let reset_variable_numbers () =
  variable_num_ref := 0

let add_variable_numbers () =
  incr variable_num_ref

let variable_name_ref = ref []

let reset_variable_names () =
  variable_name_ref := []

let rec add_variable_name = function
    [] -> ["a"]
  | "z" :: l -> "a" :: add_variable_name l
  | s :: l -> String.make 1 (Char.chr (Char.code (String.get s 0) + 1)) :: l

let get_variable_name () =
  variable_name_ref := add_variable_name (!variable_name_ref);
  String.concat "" (List.rev (!variable_name_ref)) ^ (string_of_int !variable_num_ref)

let make_arg_symbol l =
  Tuple(Xlist.map l (function
        Atom s -> Val s
      | AVar s -> Val s
      | Top -> Val "T"
      | _ -> failwith "make_arg_symbol"))

let string_of_direction = function
    Forward -> "forward"
  | Backward -> "backward"
  | Both -> "both"

let rec make_term_arg pro_fun dir = function
    Tensor(Atom "pro" :: l) -> get_variable_name (), Cut(Node {empty_node with lemma="pro"; pos="pro"; attrs=pro_fun l})
  | Tensor l ->
    let v = get_variable_name () in
    v, Cut(SetAttr("ARG_DIR",Val (string_of_direction dir),
                   SetAttr("ARG_SYMBOL",make_arg_symbol l,Var v)))
  | Plus l -> let v = get_variable_name () in v, Case(Var v,Xlist.map l (make_term_arg pro_fun dir))
  (* | Imp(s,d,t2) -> *)
  | One -> get_variable_name (), Dot
  | Maybe s ->
    let v,arg = make_term_arg pro_fun dir s in
    let w = get_variable_name () in
    w, Fix(Var w,Lambda(v,arg))
  | c -> failwith ("make_term_arg: " ^ LCGstringOf.grammar_symbol_prime c)

let add_args node args =
  {node with args=Tuple(node.args :: args)}

let make_raised_arg_symbol = function
    Imp(Tensor l,_,_) -> make_arg_symbol l
  | c -> failwith ("make_raised_arg_symbol: " ^ LCGstringOf.grammar_symbol_prime c)

let rec make_raised_term_imp inner_node outer_node arg_symbol arg_dir = function
  | Imp(s,d,t2) ->
    let v = get_variable_name () in
    let arg_symbol = make_raised_arg_symbol t2 in
    Lambda(v,make_raised_term_imp (App(Var v,inner_node)) outer_node arg_symbol d s)
  | ImpSet(s,[d,t2]) ->
    let v = get_variable_name () in
    let arg_symbol = make_raised_arg_symbol t2 in
    LambdaSet([v],make_raised_term_imp (App(Var v,inner_node)) outer_node arg_symbol d s)
  | ImpSet(s,[d1,t1;d2,t2]) ->
    let v1 = get_variable_name () in
    let v2 = get_variable_name () in
    let arg_symbol = make_raised_arg_symbol t2 in
    LambdaSet([v1;v2],make_raised_term_imp (App(Var v1,(App(Var v2,inner_node)))) outer_node arg_symbol d2 s)
  | Tensor l ->
    if outer_node.lemma="" then inner_node else
    Node (add_args outer_node [Cut(SetAttr("ARG_DIR",Val (string_of_direction arg_dir),
                   SetAttr("ARG_SYMBOL",arg_symbol,inner_node)))])
  | c -> (print_endline (LCGstringOf.grammar_symbol_prime c); failwith "make_raised_term_imp")

let is_raised = function
    [_,Imp(_,_,_)] -> true
  | [_;_,Imp(_,_,_)] -> true
  | [_,Imp(_,_,_);_] -> true
  | _ -> false

let rec make_term_imp pro_fun node outer_node = function
  | Imp(s,d,t2) ->
    if is_raised [d,t2] then make_raised_term_imp (Node node) outer_node Dot Both (Imp(s,d,t2)) else
    let v,arg = make_term_arg pro_fun d t2 in
    Lambda(v,make_term_imp pro_fun (add_args node [arg]) outer_node s)
  | ImpSet(s,l) ->
    if is_raised l then make_raised_term_imp (Node node) outer_node Dot Both (ImpSet(s,l)) else
    let vars,args = List.split (Xlist.map l (fun (d,t) -> make_term_arg pro_fun d t)) in
    LambdaSet(vars,make_term_imp pro_fun (add_args node args) outer_node s)
  | Tensor l -> Node node
  | _ -> failwith "make_term_imp"

let rec make_term_withvar_conj pro_fun node outer_node = function
    WithVar(category,_,_,t) ->
      let a,b = make_term_withvar_conj pro_fun node outer_node t in
      VariantVar(category,a),b
  | Imp(s,d,t2) ->
    (* if is_raised [d,t2] then make_raised_term_imp (Node node) outer_node Dot Both (Imp(s,d,t2)) else *)
    let v,arg = make_term_arg pro_fun d t2 in
    let x = LCGrules.get_new_variable () in
    Lambda(x,make_term_imp pro_fun (add_args node [Var x]) outer_node s),Lambda(v,arg)
  | t -> failwith "make_term_withvar_conj"

let rec make_term_withvar pro_fun node outer_node = function
    WithVar(category,_,_,t) -> VariantVar(category,make_term_withvar pro_fun node outer_node t)
  | Bracket(_,_,t) -> make_term_withvar pro_fun node outer_node t
  | Conj t ->
      let x1 = LCGrules.get_new_variable () in
      let x2 = LCGrules.get_new_variable () in
      let a,b = make_term_withvar_conj pro_fun node outer_node t in
      Lambda(x1,Lambda(x2,Coord([Inj(1,Var x2);Inj(2,Var x1)],a,b)))
  | Preconj -> Dot
  | t -> make_term_imp pro_fun node outer_node t

let rec add_projections node = function
    WithVar(category,_,_,t) -> 
(*       print_endline ("add_projections: " ^ category); *)
      VariantVar(category,add_projections node t)
  | Bracket(_,_,t) -> add_projections node t
  | t ->
(*       print_endline ("add_projections: " ^ LCGstringOf.grammar_symbol_prime t); *)
      node

let make_term pro_fun node = make_term_withvar pro_fun node empty_node
let make_raised_term pro_fun node outer_node = make_term_withvar pro_fun node outer_node

let rec make_symbol = function
  | Tensor l ->  Tuple(Xlist.map l (function
        Atom s -> Val s
      | AVar s -> SubstVar s
      | Top -> Val "T"
      | _ -> failwith "make_symbol"))
  | Plus l -> failwith "make_symbol"
  | StarWith _ -> failwith "make_symbol"
  | Imp(s,d,t2) -> make_symbol s
  | One -> failwith "make_symbol"
  | ImpSet(s,l) -> make_symbol s
  | WithVar(v,g,e,s) -> make_symbol s
  | Star _ -> failwith "make_symbol"
  | Conj s -> make_symbol s
  | Preconj -> Val "preconj"
  | Bracket(lf,rf,s) -> make_symbol s
  | BracketSet _ -> failwith "make_symbol"
  | Maybe t -> failwith "make_symbol"

let make_raised_symbol_arg = function
    [_,Imp(_,_,Tensor l)] ->
      Tuple(Xlist.map l (function
        Atom s -> Val s
      | AVar s -> SubstVar s
      | _ -> failwith "make_raised_symbol_arg 1"))
  | [_,Imp(_,_,x1);_,Imp(x2,_,Tensor l)] ->
      if x1 = x2 then
        Tuple(Xlist.map l (function
          Atom s -> Val s
        | AVar s -> SubstVar s
        | _ -> failwith "make_raised_symbol_arg 2a"))
      else failwith "make_raised_symbol_arg 2b"
  | [_;_,Imp(_,_,Tensor l)] ->
      Tuple(Xlist.map l (function
        Atom s -> Val s
      | AVar s -> SubstVar s
      | _ -> failwith "make_raised_symbol_arg 3"))
  | _ -> failwith "make_raised_symbol_arg 4"

let rec make_raised_symbol = function
  | Tensor l -> failwith "make_raised_symbol"
  | Plus l -> failwith "make_raised_symbol"
  | StarWith _ -> failwith "make_raised_symbol"
  | Imp(s,d,t2) -> if is_raised [d,t2] then make_raised_symbol_arg [d,t2] else make_raised_symbol s
  | One -> failwith "make_raised_symbol"
  | ImpSet(s,l) -> if is_raised l then make_raised_symbol_arg l else make_raised_symbol s
  | WithVar(v,g,e,s) -> make_raised_symbol s
  | Star _ -> failwith "make_raised_symbol"
  | Conj _ -> failwith "make_raised_symbol"
  | Preconj -> failwith "make_raised_symbol"
  | Bracket(lf,rf,s) -> make_raised_symbol s
  | BracketSet _ -> failwith "make_raised_symbol"
  | Maybe t -> failwith "make_raised_symbol"

let rec remove_pro = function
    Tensor(Atom "pro" :: _) -> One
  | Tensor l -> Tensor l
  | Plus l -> Plus (Xlist.map l remove_pro)
  | StarWith l -> StarWith (Xlist.map l remove_pro)
  | Imp(s,d,t2) -> Imp(remove_pro s,d,remove_pro t2)
  | One -> One
  | ImpSet(s,l) -> ImpSet(remove_pro s, Xlist.map l (fun (d,s) -> d, remove_pro s))
  | WithVar(v,g,e,s) -> WithVar(v,g,e,remove_pro s)
  | Star(s,s2) -> Star(remove_pro s,remove_pro s2)
  | Conj s -> Conj(remove_pro s)
  | Preconj -> Preconj
  | Bracket(lf,rf,s) -> Bracket(lf,rf,remove_pro s)
  | BracketSet d -> BracketSet d
  | Maybe s -> Maybe (remove_pro s)

let rec simplify = function
    ImpSet(s,[]),LambdaSet([],t) -> simplify (s,t)
  | ImpSet(s,[d,a]),LambdaSet([v],t) -> let s,t = simplify (s,t) in Imp(s,d,a),Lambda(v,t)
  | ImpSet(s,l),LambdaSet(vl,t) -> let s,t = simplify (s,t) in ImpSet(s,l),LambdaSet(vl,t)
  | WithVar(v,Atom g,e,s),VariantVar(_,t) -> simplify (substitute v (Atom g) s, substitute_substvar v (LCGrules.make_subst e (Atom g)) t)
  | WithVar(v,g,e,s),VariantVar(v2,t) ->
    if count_avar v s = 0 then
      simplify (s, substitute_substvar v (LCGrules.make_subst e g) t)
    else let s,t = simplify (s,t) in WithVar(v,g,e,s),VariantVar(v2,t)
  | Bracket(lf,rf,s),t -> let s,t = simplify (s,t) in Bracket(lf,rf,s),t
  | s,t -> s,t

let make_quant_restriction = function
    [] -> Zero
  | [s] -> Atom s
  | l -> With(Xlist.map l (fun s -> Atom s))

let rec count_req_args2 = function
    Tensor l -> 1
  | Plus l -> Xlist.fold l max_int (fun min_args t -> min min_args (count_req_args2 t))
  | StarWith l -> Xlist.fold l max_int (fun min_args t -> min min_args (count_req_args2 t))
  | Imp(s,d,t2) -> 100
  | One -> 0
  | ImpSet(s,l) -> 100
  | WithVar(v,g,e,s) -> count_req_args2 s
  | Star(t,t2) -> count_req_args2 t + count_req_args2 t2
  | Conj t -> count_req_args2 t
  | Preconj -> 0
  | Bracket(lf,rf,s) -> count_req_args2 s
  | BracketSet _ -> 10000
  | Maybe t -> 0

let rec count_req_args = function
    Tensor[Atom "<root>"] -> 0
  | Tensor[Atom "<conll_root>"] -> 0
  | Tensor[Atom "<colon>"] -> 0
  | Tensor[Atom "<speaker>"] -> 0
  | Tensor[Atom "<speaker-end>"] -> 0
  | Tensor[Atom "<squery>"] -> 0
  | Tensor[Atom s] when String.get s 0 = '<' -> 1000
  | Tensor l -> 0
  | Plus l -> Xlist.fold l max_int (fun min_args t -> min min_args (count_req_args t)) (* FIXME: kiedy ta funkcja jest odpalana??*)
  | StarWith l -> Xlist.fold l max_int (fun min_args t -> min min_args (count_req_args t))
  (* | Plus l -> failwith "count_req_args" *)
  | Imp(s,d,t2) -> count_req_args2 t2 + count_req_args s
  | One -> 0
  | ImpSet(s,l) -> Xlist.fold l 0 (fun n (_,t) -> n + count_req_args2 t) + count_req_args s
  | WithVar(v,g,e,s) -> count_req_args s
  | Star(t,t2) -> count_req_args t + count_req_args t2
  | Conj t -> count_req_args t
  | Preconj -> 0
  | Bracket(lf,rf,s) -> count_req_args s
  | BracketSet _ -> 1000
  | Maybe t -> failwith "count_req_args"

let rec get_arg_symbol = function
    Tensor l -> Tensor l
  | Plus [] -> failwith "get_arg_symbol"
  | Plus l -> if count_req_args2 (Plus l) = 0 then One else get_arg_symbol (List.hd l)
  | StarWith _ -> failwith "get_arg_symbol"
  | Imp(t,d,t2) -> Imp(t,d,t2)
  | One -> One
  | ImpSet(t,l) -> failwith "get_arg_symbol"
  | WithVar(v,g,e,s) -> failwith "get_arg_symbol"
  | Star _ -> failwith "get_arg_symbol"
  | Conj t -> failwith "get_arg_symbol"
  | Preconj -> failwith "get_arg_symbol"
  | Bracket(lf,rf,s) -> failwith "get_arg_symbol"
  | BracketSet t -> failwith "get_arg_symbol"
  | Maybe t -> One

let make_arg_sem = function
    Tensor l -> Node{empty_node with lemma="pro-arg"; pos="pro"; attrs=["CAT", Val "X";"ROLE", Val "Arg"(*;"NODE", Val "concept"*)]}
  | Imp(t,d,t2) -> Node{empty_node with lemma="pro-raised-arg"; pos="pro"; attrs=["CAT", Val "X";"ROLE", Val "Arg"(*;"NODE", Val "concept"*)]}
      (* let v = get_variable_name () in
      Lambda(v,Node{empty_node with lemma="???";args=Variable v}) *)
  | One -> Dot
  | _ -> failwith "get_arg_symbol"

let apply_arg fv s t =
  let a = get_arg_symbol t in
  let asem = make_arg_sem a in
  let l = LCGrules.deduce_matching (ref 0)(*LCGrules.empty_*)fv (*LCGrules.empty_*)fv asem (a,t) in
  match l with
    [] -> failwith "apply_arg"
  | (_,_,sem) :: _ -> App(s, sem)

let rec apply_arg_list fv s = function
    [] -> s
  | (_,t) :: l -> apply_arg_list fv (apply_arg fv (LambdaRot(1,s)) t) l

let rec apply_args references fv = function
    Tensor l,s -> Tensor l,SetAttr("ARG_DIR",Val "both",
                   SetAttr("ARG_SYMBOL",Tuple[Val "fragment"],s))
  | Plus l, _ -> failwith ("apply_args: " ^ LCGstringOf.grammar_symbol_prime (Plus l))
  | StarWith l, _ -> failwith ("apply_args: " ^ LCGstringOf.grammar_symbol_prime (StarWith l))
  | Imp(t,d,t2), s -> apply_args references fv (t, apply_arg fv s t2)
  | One, s -> One, s
  | ImpSet(t,l),s -> apply_args references fv (t, apply_arg_list fv s l)
  | WithVar(v,g,e,t),s -> 
      let fv = if LCGrules.mem_fv fv v then fv else LCGrules.add_fv fv v (g,e) in
      let t,s = apply_args references fv (t,ProjVar(v,s)) in 
(*      let g = match e with 
          "node" -> Atom "concept"
        | "coerced" -> Atom "Dot"
        | _ -> g in*)
      t,Subst(s,v,LCGrules.make_subst e (fst (LCGrules.find_fv fv v))) (*WithVar(v,g,e,t),VariantVar(v,s)*)
(*  | WithVar(v,g,e,s),VariantVar(v2,t) -> let s,t = apply_args references (LCGrules.add_fv fv v (g,e)) (s,t) in WithVar(v,g,e,s),VariantVar(v2,t)
  | WithVar(v,g,e,s),Ref id -> apply_args references fv (WithVar(v,g,e,s),ExtArray.get references id)
  | WithVar(v,g,e,s),Variant(e2,l) ->
       let s,l = Xlist.fold l (WithVar(v,g,e,s),[]) (fun (s,l) (i,t) ->
         let s,t = apply_args references fv (WithVar(v,g,e,s),t) in
         s, (i,t) :: l) in
       s, Variant(e2,List.rev l)*)
  | Star(t,t2), s -> failwith ("apply_args: " ^ LCGstringOf.grammar_symbol_prime (Star(t,t2)))
  | Conj t, s -> failwith ("apply_args: " ^ LCGstringOf.grammar_symbol_prime (Conj t))
  | Preconj, s -> failwith ("apply_args: " ^ LCGstringOf.grammar_symbol_prime Preconj)
  | Bracket(lf,rf,s),t -> let s,t = apply_args references fv (s,t) in Bracket(lf,rf,s),t
  | BracketSet t, s -> BracketSet t, s
  | Maybe t, _ -> failwith ("apply_args: " ^ LCGstringOf.grammar_symbol_prime (Maybe t))
  (* | t, s -> failwith ("apply_args: " ^ LCGstringOf.grammar_symbol_prime t ^ " : " ^ LCGstringOf.linear_term 0 s) *)

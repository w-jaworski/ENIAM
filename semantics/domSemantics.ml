(*
 *  ENIAMsemantics implements semantic processing for ENIAM
 *  Copyright (C) 2017-2018 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
 *  Copyright (C) 2017-2018 Institute of Informatics, University of Warsaw
 *  Copyright (C) 2017-2018 SELIDOR - T. Puza, Ł. Wasilewski Sp.J.
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
open SemTypes

(*let empty_concept =
  {c_sense=Dot;c_gsense=Dot;c_orth=Dot;c_name=Dot;(* c_variable: string; c_visible_var: bool;*) c_quant=Dot; c_local_quant=true; (*c_modalities: (string * type_term) list;
   c_left_input_pos: int; c_right_input_pos: int;*) c_relations=Dot; c_variable="",""; c_pos=(-1); c_cat=Dot; c_label=""; c_def_label=""}

let empty_context = {cx_sense=Dot; cx_contents=Dot; cx_relations=Dot; cx_variable="",""; cx_pos=(-1); cx_cat=Dot; cx_label=""; cx_def_label=""}*)

let rec make_args_list = function
    Tuple l -> List.flatten (Xlist.map l make_args_list)
  | Dot -> []
  | t -> [t]

(*let symbols = StringSet.of_list [
  "symbol"; "date"; "date-interval"; "hour-minute"; "hour"; "hour-minute-interval"; "hour-interval";
  "year"; "year-interval"; "day"; "day-interval"; "day-month"; "day-month-interval"; "month-interval"; "roman"; "roman-interval";
  "match-result"; "url"; "email"; "phone-number"; "obj-id"; "building-number";
  "month-lex"; "day-lex"]*)

let admods = StringSet.of_list ["jak";"nieco";"trochę";"zbyt";"niezbyt"]

let rec get_person = function
   ("PERS", Val s) :: _ -> s
 | ("PERS", _) :: _-> failwith "get_person"
 | _ :: l -> get_person l
 | [] -> ""

(* let make_relation t c =
  match t.gf with
    "subj" | "obj" | "arg" ->
      Relation(t.role,t.role_attr,c)
  | "adjunct" ->
      if t.arev then RevRelation(t.arole,t.arole_attr,c) else
      Relation(t.arole,t.arole_attr,c)
  | "core" -> Relation("CORE","",c)
  | s -> failwith ("make_relation: " ^ s) *)

(*let make_make_triple_relation t c =
  match t.gf with
    "subj" | "obj" | "arg" ->
      MakeTripleRelation(t.role,t.role_attr,c)
  | "adjunct" -> MakeTripleRelation(t.arole,t.arole_attr,c)
  | s -> failwith ("make_make_triple_relation: " ^ s)*)

let load_coerced_map filename map =
  File.fold_tab filename map (fun map -> function
    cat1 :: cat2 :: concepts ->
      let concepts = Xlist.map concepts (fun s ->
        match Xstring.split "," s with
          [relation;cat;sense] -> relation,cat,sense
        | _ -> failwith ("load_coerced_map 2: " ^ String.concat "\t" (cat1 :: cat2 :: concepts))) in
    StringMap.add_inc map (cat1 ^ "#" ^ cat2) [concepts] (fun l -> concepts :: l)
  | l -> failwith ("load_coerced_map 1: " ^ String.concat "\t" l))

let coerced_map = ref StringMap.empty

let rec make_list = function
    Dot -> []
  | Val s -> [s]
  | Variant(_,l) -> List.flatten (Xlist.rev_map l (fun (_,t) -> make_list t))
  | t -> failwith ("make_list: " ^ SemStringOf.linear_term 0 t)

let rec make_list2 = function
    Dot -> []
  | Val s -> [s]
  | Variant(_,l) -> List.flatten (Xlist.rev_map l (fun (_,t) -> make_list2 t))
  | Tuple l -> List.flatten (Xlist.rev_map l make_list2)
  | t -> failwith ("make_list2: " ^ SemStringOf.linear_term 0 t)

let make_variant = function
    [] -> failwith "make_variant"
  | [t] -> t
  | l ->
    let e = LCGreductions.get_variant_label () in
    let l,_ = Xlist.fold l ([],1) (fun (l,i) -> function
          t -> (string_of_int i,t) :: l, i+1) in
    Variant(e,l)

let add_coerced cat coerced c =
  let cat_coerced = (*Xlist.fold (make_list cat) [] (fun l cat ->*)
    Xlist.fold (make_list coerced) [] (fun l coerced ->
      if cat = coerced then l else
      (cat ^ "#" ^ coerced) :: l) in
  if cat_coerced = [] then c else
  let l = Xlist.fold cat_coerced [] (fun l cat_coerced ->
    let paths = try StringMap.find !coerced_map cat_coerced
      with Not_found -> failwith ("add_coerced: " ^ cat_coerced) in
    Xlist.fold paths l (fun l path ->
      Xlist.fold path c (fun c (relation,cat,sense) ->
        Concept{empty_concept with sense=sense; cat=cat;
          (*c_quant=Val "pro";*) relations=Tuple[SingleRelation(Val "pro");Relation(relation,"",c)]}) :: l)) in
  make_variant l

let initialize () =
  coerced_map := load_coerced_map LexSemanticsTypes.coercions_filename StringMap.empty;
  coerced_map := 
    Xlist.fold !SubsyntaxTypes.theories !coerced_map (fun map theory ->
      File.catch_no_file (load_coerced_map (SubsyntaxTypes.theories_path ^ theory ^ "/coercions.tab")) map);
  coerced_map := 
    Xlist.fold !SubsyntaxTypes.user_theories !coerced_map (fun map theory ->
      File.catch_no_file (load_coerced_map (SubsyntaxTypes.theories_path ^ theory ^ "/coercions.tab")) map);
  ()

(* let add_coerced coerced c =
  if coerced = Dot then Concept c else
  Concept{empty_concept with c_cat=coerced; c_relations=Tuple[Relation("Has","",Concept{c with c_relations=Dot});c.c_relations]} *)
(* let add_coerced coerced c =
  if coerced = Dot then Concept c else
  Concept{empty_concept with c_cat=coerced; c_relations=Relation("Has","",Concept c)} (* FIXME: trzeba dodać concept do tokenów *) *)
(* let add_coerced coerced c =
  if coerced = Dot then Concept c else
  let coerced_rels,c_rels = split_relations c.c_relations in
  Concept{empty_concept with c_cat=coerced; c_relations=Tuple[Relation("Has","",Concept{c with c_relations=c_rels});coerced_rels]} *)
(* let add_coerced2 coerced c =
  if coerced = Dot then c else
  Concept{empty_concept with c_cat=coerced; c_relations=Relation("Has","",c)} (* FIXME: trzeba dodać concept do tokenów *) *)

(*let get_sense c =
  match make_list c.c_sense with
    [s] -> s
  | _ -> failwith "get_sense"*)

(*let get_pos = function
    Concept{c_pos=p} -> p
  | _ -> (-1) (* FIXME: todo *)*)

let create_context_and_relation tokens lex_sems t cat coerced c =
  let c2 = add_coerced cat coerced c in
  match t.snode with
      "sit" ->
(*         let id = ExtArray.add tokens SubsyntaxTypes.empty_token_env in *)
        let _ = ExtArray.add lex_sems LexSemanticsTypes.empty_lex_sem in
(*         Relation(t.role ^ t.coord_arg,"",Context{empty_context with cx_contents=c2; cx_variable=string_of_int id,""; cx_pos=get_pos c}) *)
        Relation(t.role (*^ t.coord_arg*),"",Concept{empty_concept with contents=c2; (*cx_variable=string_of_int id,""; cx_pos=get_pos c*)})
    | "concept" ->
        Relation(t.role (*^ t.coord_arg*),"",c2)
    | "dot" -> Dot
    | "relations" -> Dot
    | s -> failwith ("create_context_and_relation: " ^ s)


let create_normal_concept tokens lex_sems t cat coerced =
  (*if t.agf = WalTypes.NOSEM then t.args else*)
  let cat,coerced = if !user_ontology_flag then cat,coerced else "",Dot in
  let coerced = if coerced = Val cat || coerced = Val "NULL" then Dot else coerced in
  let c = {empty_concept with
    sense =  (*if !user_ontology_flag then Val t.lemma else*) (*if t.lemma = "<root>" then Dot else*) t.lemma;
(*     c_gsense = if t.pos <> "adj" then Val t.lemma else Val (ENAMexec.convert_adj_form_gender t.lemma "f"); (* FIXME *) *)
(*     c_orth = (try Val (ExtArray.get tokens t.id).SubsyntaxTypes.orth with _ -> Dot); *)
    relations=t.args;
(*    c_quant=if t.label = "" then t.sem_args else Dot; (* FIXME: zakładam że t.label <> "" występuje tylko dla pro *)
    c_variable=string_of_int t.id,"";
    c_pos=(ExtArray.get tokens t.id).SubsyntaxTypes.beg;
    c_local_quant=true;*)
    cat=cat;
    label=t.n_label;
    atrs=("ORTH",Val t.orth) :: ("POS",Val t.pos) :: t.attrs;
    def_label=t.n_def_label} in
  let c = if t.role="Concept" then {c with relations=Tuple[c.relations;SingleRelation (Val "root")]} else c in
  if t.pos = "subst" || t.pos = "depr" || t.pos = "ger" || t.pos = "unk" || t.pos = "fixed" (*|| StringSet.mem symbols t.pos*) then (* FIXME: wykrywanie plurale tantum *)
(*     let c = {c with c_local_quant=false} in *)
    let c,measure,cx_flag = Xlist.fold t.attrs (c,false,false) (fun (c,measure,cx_flag) -> function
        "NSYN",Val "common" -> c,measure,cx_flag
      | "NSYN",Val "proper" -> c(*{c with c_name=Val t.lemma; c_sense=Dot(*t.sense*)(*c_sense=if Val t.pred=c.c_sense then Dot else c.c_sense*)}*),measure,cx_flag; (* FIXME: zaślepka na potrzeby gramatyk semantycznych *)  (* Rozpoznawanie propoer names nieznanego typu - ryzykowne ale proste *)
      | "NSYN",Val "pronoun" -> c(*{c with c_quant=Tuple[c.c_quant;Val "indexical"]}*),measure,cx_flag
(*       | "NSEM",Val "count" -> c(*{c with c_quant=Tuple[c.c_quant;Val "count"]}*),measure,cx_flag *)
      | "NSEM",Val "time" -> c,measure,cx_flag(*failwith "create_normal_concept: time"*)
      | "NSEM",t -> {c with  relations=Tuple[c.relations;SingleRelation t]},measure,cx_flag
(*      | "NSEM",Val "mass" -> {c with c_quant=Tuple[c.c_quant;Val "mass"]},measure,cx_flag
      | "NSEM",Variant(e,[a,Val "mass";b,Val "count"]) -> {c with c_quant=Tuple[c.c_quant;Variant(e,[a,Val "mass";b,Val "count"])]},measure,cx_flag (* FIXME: tu by należało podzielić to na dwa pudełka *)
      | "NSEM",Variant(e,[a,Val "count";b,Val "mass"]) -> {c with c_quant=Tuple[c.c_quant;Variant(e,[a,Val "count";b,Val "mass"])]},measure,cx_flag
      | "NSEM",Val "measure" -> c,true,cx_flag*)
      | "NUM",t -> {c with relations=Tuple[c.relations;SingleRelation t](*c_quant=Tuple[c.c_quant;t]*)},measure,cx_flag
      | "CASE",_ -> c,measure,cx_flag
      | "GEND",_ -> c,measure,cx_flag
      | "PERS",Val "ter" -> c,measure,cx_flag
      | "PERS",Val "sec" -> {c with relations=Tuple[c.relations;SingleRelation(Val "impt")]},measure,true
      | "ASPECT",_ -> c,measure,cx_flag
      | "NEGATION",Val "aff" -> c,measure,cx_flag
      | "NEGATION",Val "neg" -> {c with (*c_quant=Tuple[c.c_quant;Val "nie"]*)relations=Tuple[c.relations;SingleRelation(Val "nie")]},measure,cx_flag
      | "controller",_ -> c,measure,cx_flag
      | "GRAD",Val "pos" -> c,measure,cx_flag
      | "PT",Val "pt" -> {c with (*c_quant=Tuple[c.c_quant;Val "pt"]*)relations=Tuple[c.relations;SingleRelation(Val "pt")]},measure,cx_flag
      | "PT",Val "npt" -> c,measure,cx_flag
      | "ACM",_ -> c,measure,cx_flag
      (* | "INCLUSION",_ -> c,measure ,cx_flag
      | "QUOT",Val "+" -> {c with c_relations=Tuple[c.c_relations;SingleRelation(Val "quot")]},measure,cx_flag
      | "LEX",_ -> c,measure,cx_flag (* FIXME *) *)
(*       | "TYPE",Val "int" -> {c with c_quant=Tuple[c.c_quant;Val "interrogative"]},measure *)
      (* | "TYPE",_ -> c,measure,cx_flag (* FIXME *) *)
      | e,t -> failwith ("create_normal_concept noun: " ^ e ^ ": " ^ SemStringOf.linear_term 0 t)) in
    (* let c = if t.pos = "depr" then {c with c_relations=Tuple[c.c_relations;SingleRelation(Val "depr")]} else c in *)
    (*let role,c = match t.pos with
        "hour-minute" -> expand_hour_minute t.role c
      | "hour-interval" -> expand_hour_interval t.role c
      | "hour-minute-interval" -> expand_hour_minute_interval t.role c
      | "date" -> expand_date t.role c
      | _ -> t.role,Concept c in
    create_context_and_relation tokens lex_sems {t with role=role} cat coerced c else *)
    create_context_and_relation tokens lex_sems t cat coerced (Concept c) else
  if t.pos = "symbol" then (* FIXME: wykrywanie plurale tantum *)
(*     let c = {c with c_local_quant=false} in *)
    let _(*mode*) = Xlist.fold t.attrs "" (fun mode -> function
        "MODE",Val mode -> mode
      | "NUM",_ -> mode
      | "CASE",_ -> mode
      | "GEND",_ -> mode
      | "PERS",_ -> mode
      | "NSEM",_ -> mode
      | "ACM",_ -> mode
      | "GRAD",_ -> mode
      | e,t -> failwith ("create_normal_concept symbol: " ^ e ^ ": " ^ SemStringOf.linear_term 0 t)) in
(*     let c = {c with sense="expand_compound"; contents=Tuple [Val mode; Val c.sense]} in *)
(*      | "day-month" -> {c with sense="expand_compound"; contents=Tuple [Val "day-month"; Val c.sense]}
      | "hour-minute" -> {c with sense="expand_compound"; contents=Tuple [Val "hour-minute"; Val c.sense]}
      | "match-result" -> {c with sense="expand_compound"; contents=Tuple [Val "match-result"; Val c.sense]}
      | "intnum-interval" -> {c with sense="expand_compound"; contents=Tuple [Val "intnum-interval"; Val c.sense]}
      | "roman-interval" -> {c with sense="expand_compound"; contents=Tuple [Val "roman-interval"; Val c.sense]}
      | "realnum-interval" -> {c with sense="expand_compound"; contents=Tuple [Val "realnum-interval"; Val c.sense]}
      | "date-interval" -> {c with sense="expand_compound"; contents=Tuple [Val "date-interval"; Val c.sense]}
      | "day-month-interval" -> {c with sense="expand_compound"; contents=Tuple [Val "day-month-interval"; Val c.sense]}
      | "day-interval" -> {c with sense="expand_compound"; contents=Tuple [Val "day-interval"; Val c.sense]}
      | "month-interval" -> {c with sense="expand_compound"; contents=Tuple [Val "month-interval"; Val c.sense]}
      | "year-interval" -> {c with sense="expand_compound"; contents=Tuple [Val "year-interval"; Val c.sense]}
      | "hour-minute-interval" -> {c with sense="expand_compound"; contents=Tuple [Val "hour-minute-interval"; Val c.sense]}
      | "hour-interval" -> {c with sense="expand_compound"; contents=Tuple [Val "hour-interval"; Val c.sense]}
      | "minute-interval" -> {c with sense="expand_compound"; contents=Tuple [Val "minute-interval"; Val c.sense]}
      | _ -> c in*)
    create_context_and_relation tokens lex_sems t cat coerced (Concept c) else
  if t.pos = "prep" || t.pos = "compar" || (t.lemma = "to" && t.pos = "qub") then
    (* if t.arole = "NOSEM" then make_relation t (t.args) else *)
    let c,is_sem = Xlist.fold t.attrs (c,false) (fun (c,is_sem) -> function
      | "CASE",_ -> c,is_sem
      | "GEND",_ -> c,is_sem
      | "NUM",_ -> c,is_sem
      | "GRAD",_ -> c,is_sem
(*      | "PSEM",Val "sem" -> c,true
      | "PSEM",Val "nosem" -> c,false*)
      | e,t -> failwith ("create_normal_concept prep: " ^ e)) in
    (* make_make_triple_relation t (Concept c) else *)
    (* if is_sem then Relation(t.role,"",add_coerced2 coerced (CreateContext({empty_context with cx_sense=c.c_sense; cx_variable=c.c_variable; cx_pos=c.c_pos; cx_cat=c.c_cat},c.c_relations))) *)
    (*if is_sem then (*create_context_and_relation tokens lex_sems t coerced (CreateContext({empty_context with cx_sense=c.c_sense; cx_variable=c.c_variable; cx_pos=c.c_pos; cx_cat=c.c_cat},c.c_relations))*)
      create_context_and_relation tokens lex_sems t cat coerced (Concept c)
    else*) Relation(t.role (*^ t.coord_arg*),"",RemoveRelation("CORE","",c.relations)) else
  if t.pos = "fin" || t.pos = "bedzie" || t.pos = "praet" || t.pos = "winien" || t.pos = "impt" ||
     t.pos = "imps" || t.pos = "pred" || t.pos = "inf" || t.lemma = "pro-komunikować" ||
     t.pos = "adj" || t.pos = "adjc" || t.pos = "adjp" || t.pos = "adja" || t.pos = "pact" ||
     t.pos = "ppas" || t.pos = "pcon" || t.pos = "pant" || t.pos = "apron" ||
     t.pos = "ordnum" || t.pos = "roman-adj" || t.pos = "adv" || t.pos = "x" ||
     (t.pos = "part" && t.lemma = "nie") then
(*     let c = {c with c_local_quant=false} in *)
(*    let c = if t.pos = "fin" || t.pos = "bedzie" || t.pos = "praet" || t.pos = "winien" || t.pos = "impt" ||
               t.pos = "imps" || t.pos = "pred" || t.pos = "inf" || t.lemma = "pro-komunikować" ||
               t.pos = "pact" || t.pos = "ppas" || t.pos = "pcon" || t.pos = "pant" then {c with c_local_quant=false} else c in*)
    let c = Xlist.fold t.attrs c (fun c -> function
(*         "SENSE",t -> {c with c_sense=Tuple[c.c_sense;t]} *)
      | "NUM",t -> c
      | "CASE",_ -> c
      | "GEND",_ -> c
      | "PERS",_ -> c
      | "ASPECT",_ -> c
      (* | "CTYPE",_ -> c (* FIXME *) *)
      | "TENSE",t -> {c with relations=Tuple[c.relations;SingleRelation t]}
      | "MOOD",Val "indicative" -> c
      | "MOOD",Val "conditional" -> {c with relations=Tuple[c.relations;SingleRelation(Val "cond")]} (* FIXME *)
      | "MOOD",Val "imperative" -> {c with relations=Tuple[c.relations;SingleRelation(Val "impt")]} (* FIXME *)
      | "NEGATION",Val "aff" -> c
      | "NEGATION",Val "neg" -> {c with (*c_quant=Tuple[c.c_quant;Val "nie"]*)relations=Tuple[c.relations;SingleRelation(Val "nie")]}
      | "SYN",Val "common" -> c
      | "SYN",Val "pronoun" -> c(*{c with c_quant=Tuple[c.c_quant;Val "indexical"]}*)
      | "SYN",Val "proper" -> if t.pos = "roman-adj" then c else failwith "create_normal_concept adj: SYN=proper"
      | "NSEM",Val "count" -> (*if t.pos = "roman-adj" then*) c (*else failwith "create_normal_concept adj: NSEM=count"*)
      | "GRAD",Val "pos" -> c
      | "GRAD",Val "com" -> if StringSet.mem admods t.lemma then c else {c with relations=Tuple[c.relations;SingleRelation (Val "com")]}
      | "GRAD",Val "sup" -> if StringSet.mem admods t.lemma then c else {c with relations=Tuple[c.relations;SingleRelation (Val "sup")]}
      | "CTYPE",_ -> c (* FIXME1: trzeba zaznaczyć pytajność w grafie, CTYPE pojawia się w dwu węzłach *)
(*       | "TYPE",Val "int" -> {c with c_quant=Tuple[c.c_quant;Val "interrogative"]} *)
      | "TYPE",_ -> c (* FIXME *)
      | "LEX",_ -> c (* FIXME *)
      | "ACM",_ -> c
      | "MODE",_ -> c
      | e,t -> failwith ("create_normal_concept 2: " ^ e)) in
    let c = if t.lemma = "pro-komunikować" then {c with relations=Relation("Theme","",c.relations)} else c in (* FIXME: to by trzeba przesunąć na wcześniej *)
    create_context_and_relation tokens lex_sems t cat coerced (Concept c) else
  if t.pos = "num" || t.pos = "intnum" || t.pos = "realnum" || t.pos = "intnum-interval" || t.pos = "realnum-interval" then
    let c = Xlist.fold t.attrs c (fun c -> function
(*         "SENSE",t -> {c with c_sense=Tuple[c.c_sense;t]} *)
      | "ACM",_ -> c
      | "NUM",_ -> c
      | "CASE",_ -> c
      | "GEND",_ -> c
      | "PERS",_ -> c
      | "NSYN",_ -> c
      | "NSEM",_ -> c
      | e,t -> failwith ("create_normal_concept num: " ^ e)) in
    create_context_and_relation tokens lex_sems t cat coerced (Concept c) else
  if coerced <> Dot then failwith ("create_normal_concept coerced: " ^ t.lemma) else
  if t.pos = "pro" || t.pos = "ppron12" || t.pos = "ppron3" || t.pos = "siebie" then (* FIXME: indexicalność *)
(*     let c = {c with c_local_quant=false} in *)
    let c = Xlist.fold t.attrs c (fun c -> function
        (* "NUM",t -> {c with c_relations=Tuple[c.c_relations;SingleRelation t]}
      | "GEND",t -> {c with c_relations=Tuple[c.c_relations;SingleRelation t]}
      | "PERS",t2 -> if t.pos = "siebie" then c else {c with c_relations=Tuple[c.c_relations;SingleRelation t2]} *)
        "NUM",t2 -> if t.pos = "siebie" then c else {c with relations=Tuple[c.relations;SingleRelation t2](*c_quant=Tuple[c.c_quant;t]*)}
      | "GEND",t2 -> if t.pos = "siebie" then c else {c with (*c_quant=Tuple[c.c_quant;t]*)relations=Tuple[c.relations;SingleRelation t2]}
      | "PERS",t2 -> if t.pos = "siebie" then c else {c with (*c_quant=Tuple[c.c_quant;t2]*)relations=Tuple[c.relations;SingleRelation t2]}
      | "CASE",_ -> c
      | "SYN",_ -> c
      | "NSEM",_ -> c
      | "ACM",_ -> c
      | "controller",_ -> c
      | "controllee",_ -> c
      (* | "coref",t -> {c with c_relations=Tuple[c.c_relations;SingleRelation (Val "coref")]} (* FIXME: zaślepka do poprawienia przy implementacji kontroli *) *)
      | e,t -> failwith ("create_normal_concept pron: " ^ e)) in
    create_context_and_relation tokens lex_sems t cat coerced (Concept c) else
  if t.pos = "part" && t.lemma="się" then
    (*let c = {c with c_quant=Tuple[c.c_quant;Val "coreferential"]} in*)
    create_context_and_relation tokens lex_sems t cat coerced (Concept c) else
  if t.pos = "part" && (t.lemma="czy" || t.lemma="gdyby") then
    Relation(t.role (*^ t.coord_arg*),"",SetContextName(c.sense,RemoveRelation(t.role (*^ t.coord_arg*),"",c.relations))) else
  if t.pos = "part" (*&& (t.lemma="nie" || t.lemma="by")*) then
    create_context_and_relation tokens lex_sems t cat coerced (Concept c) else
  if t.pos = "qub" then
    let c = Xlist.fold t.attrs c (fun c -> function
      | "NSEM",_ -> c
      | "NUM",_ -> c
      | "CASE",_ -> c
      | "GEND",_ -> c
      | "PERS",_ -> c
      | "ACM",_ -> c
      | "GRAD",_ -> c
(*      | "TYPE",Val "int" -> {c with c_quant=Tuple[c.c_quant;Val "interrogative"]}
      | "TYPE",_ -> c*)
      | e,t2 -> failwith ("create_normal_concept qub: " ^ e ^ " in " ^ t.lemma)) in
    create_context_and_relation tokens lex_sems t cat coerced (Concept c) else
  if t.pos = "aglt" then create_context_and_relation tokens lex_sems t cat coerced (Concept c) else
  if t.pos = "comp" then
    Relation(t.role (*^ t.coord_arg*),"",SetContextName(c.sense,RemoveRelation("CORE","",c.relations))) else
  if t.pos = "conj" then
    if t.lemma = "+" then Dot else (* FIXME: proteza na potrzeby inn *)
    let c = {empty_concept with sense=c.sense; contents=(*make_args_list*) t.args; (*cx_variable=c.c_variable; cx_pos=c.c_pos;*) cat=c.cat; def_label=c.def_label; label=c.label} in
    (* let c = {empty_context with cx_sense=c.c_sense; cx_contents=t.args; cx_variable=c.c_variable; cx_pos=c.c_pos; cx_cat=c.c_cat; cx_def_label=c.c_def_label; cx_label=c.c_label} in *)
    let c = Xlist.fold t.attrs c (fun c -> function
      | "NUM",_ -> c
      | "CASE",_ -> c
      | "GEND",_ -> c
      | "GRAD",_ -> c
      | "PERS",_ -> c
      | "ASPECT",_ -> c
      | "NSEM",_ -> c
      | "ACM",_ -> c
      | "controller",_ -> c
      | "controllee",_ -> c
      | e,t -> failwith ("create_normal_concept conj: " ^ e)) in
(*     create_context_and_relation tokens lex_sems t cat coerced (Concept c) else *)
    ManageCoordination({t with attrs=[]; args=Dot},Concept c) else
  (* if t.pos = "interj" then
    let c = Xlist.fold t.attrs c (fun c -> function
      | e,t -> failwith ("create_normal_concept interj: " ^ e)) in
    make_relation t (Concept c) else *)
  if t.pos = "sinterj" || t.pos = "interj" then
    let c = Xlist.fold t.attrs c (fun c -> function
      | e,t -> failwith ("create_normal_concept sinterj: " ^ e)) in
    create_context_and_relation tokens lex_sems t cat coerced (Concept c) else
(*    let id = ExtArray.add tokens SubsyntaxTypes.empty_token_env in
    let _ = ExtArray.add lex_sems LexSemanticsTypes.empty_lex_sem in
    let cx = {empty_context with cx_contents=add_coerced coerced c; cx_variable=string_of_int id,""; cx_pos=c.c_pos; cx_cat=Val "Situation"} in
    make_relation t (Context cx) else*)
  if t.pos = "<raw>" then
    let c = Xlist.fold t.attrs c (fun c -> function
      | "NODE-ID",Val s -> {c with (*c_quant=Tuple[Val s;c.c_quant]*)label=s}
      | e,t2 -> failwith ("create_normal_concept <raw>: " ^ e ^ " in " ^ t.lemma)) in
    create_context_and_relation tokens lex_sems t cat coerced (Concept c) else
  if t.pos = "<merge>" then
    let c = {empty_concept with sense=c.sense; contents=RemoveRelation("","",t.args); (*cx_variable=c.c_variable; cx_pos=c.c_pos;*) cat=c.cat; def_label=c.def_label; label=c.label} in
    Concept c else
(*    let c = {empty_context with cx_sense=c.sense; cx_contents=RemoveRelation("","",t.args); cx_variable=c.c_variable; cx_pos=c.c_pos; cx_cat=c.c_cat; cx_def_label=c.c_def_label; cx_label=c.c_label} in
    Context c else*)
(*     RemoveRelation("null","",ManageCoordination({t with attrs=[]; args=Dot},Context c)) else *)
 (*   RemoveRelation("CORE","",create_context_and_relation tokens lex_sems t cat coerced (Concept c)) else*)
  if t.lemma = "<root>" then t.args else
(*   if t.lemma = "<merge>" then RemoveRelation("null","",t.args) else *)
  (* if t.pos = "interp" && t.lemma = "?" && t.args = Dot then SingleRelation(Val "int") else *)
  if t.pos = "interp" && (t.lemma = "</coord1>" || t.lemma = "<coord1>" || t.lemma = "</coord1comp>" || t.lemma = "<coord1comp>") then t.args else
  if t.pos = "interp" && (t.lemma = "</coord2>" || t.lemma = "<coord2>" || t.lemma = "</coord2comp>" || t.lemma = "<coord2comp>") then t.args else
  if t.pos = "interp" && t.lemma = "?" then
    Relation(t.role (*^ t.coord_arg*),"",AddSingleRelation(Val "int",RemoveRelation("null","",t.args))) else (* FIXME1: to powinno tworzyć kontekst i zaznaczać ze jest interrogative *)
  if t.pos = "interp" && t.lemma = ":" then
    if t.snode = "dot" then Dot else t.args
    (*Relation(t.role (*^ t.coord_arg*),"",RemoveRelation("CORE","",t.args))*) else
  if t.pos = "interp" && (t.lemma = "," || t.lemma = "¶" || t.lemma = "‚") then Dot else
  if t.pos = "interp" && t.lemma = "</sentence>" then
    if t.args = Dot then Dot else
    if t.role = "Concept" then 
      let c = {empty_concept with sense=c.sense; contents=(*List.rev (Xlist.rev_map (make_args_list t.args (fun t ->*) RemoveRelation("null","",t.args); (*cx_variable=c.c_variable; cx_pos=c.c_pos;*) cat=c.cat; def_label=c.def_label; label=c.label} in
      Relation("null","",Concept c) else
(*      let c = {empty_context with cx_sense=c.c_sense; cx_contents=RemoveRelation("null","",t.args); cx_variable=c.c_variable; cx_pos=c.c_pos; cx_cat=c.c_cat; cx_def_label=c.c_def_label; cx_label=c.c_label} in
      Relation("null","",Context c) else*)
   (* let l = (*List.rev*) (make_args_list t.args) in
    Xlist.fold (List.tl l) (RemoveRelation("null","",List.hd l)) (fun t s -> AddRelation(t,"Next","Clause",RemoveRelation("null","",s))) else *)
    let c = {empty_concept with sense=c.sense; contents=(*make_args_list*) t.args; (*cx_variable=c.c_variable; cx_pos=c.c_pos;*) cat=c.cat; def_label=c.def_label; label=c.label} in
    let c = Xlist.fold t.attrs c (fun c -> function
      (* | "NUM",_ -> c
      | "CASE",_ -> c
      | "GEND",_ -> c
      | "PERS",_ -> c
      | "ASPECT",_ -> c
      | "controller",_ -> c
      | "controllee",_ -> c *)
      | e,t -> failwith ("create_normal_concept conj: " ^ e)) in
    if t.role = "Concept" then Concept c else
    RemoveRelation("null","",ManageCoordination({t with attrs=[]; args=Dot},Concept c)) else
  if t.pos = "interp" && t.lemma = "<sentence>" then t.args else
  if t.pos = "interp" && t.lemma = "</query>" then
    let l = (*List.rev*) (make_args_list t.args) in
    if l = [] then failwith "create_normal_concept: empty list" else
    Xlist.fold (List.tl l) (List.hd l) (fun t s -> AddRelation(t,"Next","Sentence",s)) else
  if t.pos = "interp" && t.lemma = "<query>" then t.args else
  if t.pos = "interp" && (t.lemma = "(" || t.lemma = "-") then
    Relation(t.role (*^ t.coord_arg*),"",RemoveRelation("","",c.relations)) else
  if t.pos = "interp" && t.lemma = ")" then
    Dot else
(*  if t.pos = "interp" && t.lemma = "”s" then
    let l = List.rev (make_args_list t.args) in
    let x = Xlist.fold (List.tl l) (List.hd l) (fun t s -> AddRelation(RemoveRelation t,"Next","Sentence",RemoveRelation s)) in
    Relation(t.arole,t.arole_attr,x) else (* FIXME: czy na pewno tu i w następnych arole a nie position.role? *)
  if t.pos = "interp" && t.lemma = "<or>" then
    Relation(t.arole,t.arole_attr,t.args) else
  if t.pos = "interp" && t.lemma = "<speaker>" then
    Relation(t.arole,t.arole_attr,RemoveRelation t.args) else
  if t.pos = "interp" && t.lemma = "</query>" then
    let l = List.rev (make_args_list t.args) in
    let x = Xlist.fold (List.tl l) (List.hd l) (fun t s -> AddRelation(RemoveRelation t,"Next","Sentence",RemoveRelation s)) in
    if t.gf = "obj" then Relation(t.arole,t.arole_attr,x) else x else
  if t.pos = "interp" && t.lemma = "<query1>" then t.args else
  if t.pos = "interp" && t.lemma = "<query2>" then t.args else
  if t.pos = "interp" && t.lemma = "<query4>" then t.args else
  if t.pos = "interp" && t.lemma = "<query5>" then
    let l = List.rev (make_args_list t.args) in
    Xlist.fold (List.tl l) (List.hd l) (fun t s -> AddRelation(RemoveRelation t,"Next","Sentence",RemoveRelation s)) else
  if t.pos = "interp" && t.lemma = "<query6>" then
    let l = List.rev (make_args_list t.args) in
    Xlist.fold (List.tl l) (List.hd l) (fun t s -> AddRelation(RemoveRelation t,"Next","Sentence",RemoveRelation s)) else
  if t.pos = "interp" && t.lemma = "?" then SingleRelation(Val "int") else
  if t.pos = "interp" && t.lemma = "„" then
    make_relation t (RemoveRelation t.args) else
  if t.pos = "interp" || t.lemma = "</or-sentence>" then make_relation t (t.args) else*) (
  if t.pos = "interp" && t.lemma = "." then (* Na potrzeby partial parsed *)
    Dot (*create_context_and_relation tokens lex_sems t cat coerced (Concept c)*) else
  if t.pos = "interp" && (t.lemma = "„" || t.lemma = "”" || t.lemma = ";" || t.lemma = "#" || t.lemma = "[" || t.lemma = "]") then Dot else (* Na potrzeby partial parsed *) (* FIXME: symbole do escapowania w inference *)
  if t.pos = "interp" && (t.lemma = "!" || t.lemma = "=" || t.lemma = "/" || t.lemma = "®") then (* Na potrzeby partial parsed *)
    create_context_and_relation tokens lex_sems t cat coerced (Concept c) else
  if t.pos = "interp" || t.arg_symbol = Val "fragment" then Dot else
  if t.pos = "interp" then Node t else
  (*if t.pos = "" then make_relation t (t.args) else*)
  (* print_endline t.lemma; *)
  Node t)

let rec translate_node tokens lex_sems t =
  let attrs = Xlist.map t.LCGtypes.attrs (fun (k,t) -> k, create_concepts tokens lex_sems t) in
  let t = {
    orth=t.LCGtypes.orth; lemma=t.LCGtypes.lemma; pos=t.LCGtypes.pos; weight=t.LCGtypes.weight;
    id=t.LCGtypes.id; symbol=create_concepts tokens lex_sems t.LCGtypes.symbol; arg_symbol=create_concepts tokens lex_sems t.LCGtypes.arg_symbol;
    arg_dir=t.LCGtypes.arg_dir;
    attrs=[]; n_label=""; n_def_label="";snode="";
    args=create_concepts tokens lex_sems t.LCGtypes.args;
    gf=""; role=""; role_attr=""; coord_arg=0; selprefs=Dot; (*sense=Dot;*) arole=""; arole_attr=""; arev=false; sem_args=Dot;
    (*cat=Dot;coerced=Dot*)} in
  let t,attrs,cat,coerced = Xlist.fold attrs (t,[],Dot,Dot) (fun (t,attrs,cat,coerced) -> function
(*      "gf",Val s -> {t with gf=s},attrs,cat,coerced
    | "role",Val s -> {t with role=s},attrs,cat,coerced
    | "role-attr",Val s -> {t with role_attr=s},attrs,cat,coerced
    | "selprefs",s -> {t with selprefs=s},attrs,cat,coerced
    | "sense",s -> {t with sense=s},attrs,cat,coerced
    | "hipero",_ -> t,attrs,cat,coerced
    | "arole",Val s -> {t with arole=s},attrs,cat,coerced
    | "arole-attr",Val s -> {t with arole_attr=s},attrs,cat,coerced
    | "arev",Val "-" -> {t with arev=false},attrs,cat,coerced
    | "arev",Val "+" -> {t with arev=true},attrs,cat,coerced
    | "agf",Val s -> t,attrs,cat,coerced
    | "sem-args",s -> {t with sem_args=s},attrs,cat,coerced
    | "rev-hipero",_ -> t,attrs,cat,coerced
    | "fopinion",_ -> t,attrs,cat,coerced
    | "sopinion",_ -> t,attrs,cat,coerced*)
    | "NODE",Val s -> {t with snode=s},attrs,cat,coerced
    | "ROLE",Val s -> {t with role=s},attrs,cat,coerced
    | "ROLE",Dot -> {t with role=""},attrs,cat,coerced
    | "COORD_ARG",Val s -> {t with coord_arg=try int_of_string s with  _ -> failwith "translate_node: COORD_ARG"},attrs,cat,coerced
    | "ACM",s -> t,("ACM",s) :: attrs,cat,coerced
    | "ASPECT",s -> t,("ASPECT",s) :: attrs,cat,coerced
    | "NEGATION",s -> t,("NEGATION",s) :: attrs,cat,coerced
    | "MOOD",s -> t,("MOOD",s) :: attrs,cat,coerced
    | "TENSE",s -> t,("TENSE",s) :: attrs,cat,coerced
    | "CTYPE",s -> t,("CTYPE",s) :: attrs,cat,coerced
    | "controller",s -> t,("controller",s) :: attrs,cat,coerced
    | "controllee",s -> t,("controllee",s) :: attrs,cat,coerced
    | "coref",s -> t,attrs,cat,coerced
    | "label",Val s -> {t with n_label=s},attrs,cat,coerced
    | "def-label",Val s -> {t with n_def_label=s},attrs,cat,coerced
    | "CAT",s -> t,attrs,s,coerced
    | "COERCED",s -> t,attrs,cat,s
    | "NUM",s -> t,("NUM",s) :: attrs,cat,coerced
    | "CASE",s -> t,("CASE",s) :: attrs,cat,coerced
    | "GEND",s -> t,("GEND",s) :: attrs,cat,coerced
    | "PERS",s -> t,("PERS",s) :: attrs,cat,coerced
    | "NSYN",s -> t,("NSYN",s) :: attrs,cat,coerced
    | "NSEM",s -> t,("NSEM",s) :: attrs,cat,coerced
    | "MODE",s -> t,("MODE",s) :: attrs,cat,coerced
    | "GRAD",s -> t,("GRAD",s) :: attrs,cat,coerced
    | "PSEM",s -> t,("PSEM",s) :: attrs,cat,coerced
    | "PT",s -> t,("PT",s) :: attrs,cat,coerced
    | "NODE-ID",s -> t,("NODE-ID",s) :: attrs,cat,coerced
    | "COL",s -> t,attrs,cat,coerced
    | "PHRASE",s -> t,attrs,cat,coerced
    (* | k,v -> printf "translate_node: %s %s\n%!" k (SemStringOf.linear_term 0 v); t, (k,v) :: attrs,cat,coerced) in *)
    | k,v -> failwith (Printf.sprintf "translate_node: %s %s\n%!" k (SemStringOf.linear_term 0 v))) in
  {t with attrs=attrs},cat,coerced

and create_concepts tokens lex_sems = function
    LCGtypes.Node t ->
      let t,cat,coerced = translate_node tokens lex_sems t in
      (match cat with
        Dot -> create_normal_concept tokens lex_sems t "NULL" coerced
      | Val cat -> create_normal_concept tokens lex_sems t cat coerced
      | Variant(e,l) -> 
          (Variant(e,Xlist.map l (function 
              (i,Val cat) -> i, create_normal_concept tokens lex_sems t cat coerced
			| (i,s) -> failwith ("create_concepts: " ^ SemStringOf.linear_term 0 s))))
	  | _-> failwith "create_concepts")
  | LCGtypes.Tuple l -> Tuple(Xlist.map l (create_concepts tokens lex_sems))
  | LCGtypes.Variant(e,l) ->
       (* if e = "" then print_endline "create_concepts: empty variant label" else print_endline ("create_concepts: variant label " ^ e); *)
       Variant(e,Xlist.map l (fun (i,t) -> i, create_concepts tokens lex_sems t))
  | LCGtypes.Dot -> Dot
  | LCGtypes.Val s -> Val s
  | LCGtypes.Ref i -> Ref i
  (* | Choice choices -> Choice(StringMap.map choices (create_concepts tokens lex_sems)) *)
  | t -> failwith ("create_concepts: " ^ LCGstringOf.linear_term 0 t)


let translate tokens lex_sems term =
  let sem = Array.make (Array.length term) Dot in
  Int.iter 0 (Array.length sem - 1) (fun i ->
    sem.(i) <- create_concepts tokens lex_sems term.(i));
  sem


(*******************************************************************************************)
(*
let split_day_month s =
  match Xstring.split "\\." s with
    [d;m] -> d,m
  | _ -> failwith "split_day_month"

let split_hour_minute s =
  match Xstring.split ":" s with
    [h;m] -> h,m
  | _ -> try split_day_month s with _ -> failwith "split_hour_minute"

let split_interval s =
  match Xstring.split "-" s with
    [h1;h2] -> h1,h2
  | _ -> failwith "split_interval"

let split_date s =
  let s,f =
    match Xstring.split " " s with
      [s;"roku"] -> s,true
    | [s] -> s,false
    | _ -> failwith "split_date" in
  match Xstring.split "\\." s with
    [d;m;y] -> d,m,y,f
  | _ -> failwith "split_date"

let make_minute rel m =
  Relation(rel,"",Concept {empty_concept with sense="minuta"; cat="Minute"; relations=
    Tuple[SingleRelation(Val "pro");Relation("Count","",Concept {empty_concept with sense=m; cat="Number"})]})

let modify_minute rel c h =
  Relation(rel,"",Concept {empty_concept with sense="minute"; cat="Minute"; relations=
    Tuple[SingleRelation(Val "pro");Relation("Count","",Concept {c with sense=h})]})

let make_hour h args =
  Relation("Arg","",Concept {empty_concept with sense="godzina"; cat="Hour"; relations=
    Tuple(SingleRelation(Val "pro") :: Relation("Attr","",Concept {empty_concept with sense=h; cat="HourNumber"}) :: args)})

let modify_hour c h args =
  Relation("Arg","",Concept {empty_concept with sense="godzina"; cat="Hour"; relations=
    Tuple(SingleRelation(Val "pro") :: Relation("Attr","",Concept {c with sense=h}) :: args)})

let make_day h args =
  Relation("Arg","",Concept {empty_concept with sense="dzień"; cat="Day"; relations=
    Tuple(SingleRelation(Val "pro") :: Relation("Attr","",Concept {empty_concept with sense=h; cat="DayNumber"}) :: args)})

let modify_day c h args =
  Relation("Arg","",Concept {empty_concept with sense="dzień"; cat="Day"; relations=
    Tuple(SingleRelation(Val "pro") :: Relation("Attr","",Concept {c with sense=h}) :: args)})

let make_month rel h args =
  Relation(rel,"",Concept {empty_concept with sense="miesiąc"; cat="Month"; relations=
    Tuple(SingleRelation(Val "pro") :: Relation("Attr","",Concept {empty_concept with sense=h; cat="MonthNumber"}) :: args)})

let modify_month rel c h args =
  Relation(rel,"",Concept {empty_concept with sense="miesiąc"; cat="Month"; relations=
    Tuple(SingleRelation(Val "pro") :: Relation("Attr","",Concept {c with sense=h}) :: args)})

let make_year rel h args =
  Relation(rel,"",Concept {empty_concept with sense="rok"; cat="Year"; relations=
    Tuple(SingleRelation(Val "pro") :: Relation("Attr","",Concept {empty_concept with sense=h; cat="YearNumber"}) :: args)})

let modify_year rel c h args =
  Relation(rel,"",Concept {empty_concept with sense="rok"; cat="Year"; relations=
    Tuple(SingleRelation(Val "pro") :: Relation("Attr","",Concept {c with sense=h}) :: args)})

let expand_hour_minute c =
  let h,m = split_hour_minute c.sense in
  AddParentRelation(make_minute "RevPart" m,Concept {c with sense=h})

let expand_day_month c =
  let d,m = split_day_month c.sense in
  AddParentRelation(make_month "Part" m [],Concept {c with sense=d})

let expand_intnum_interval c =
  let m1,m2 = split_interval c.sense in
  let t1 = Relation("Attr","",Concept {empty_concept with sense="do"; cat="Number"; relations=make_minute "Arg" m2}) in
  let t2 = Relation("Attr","",Concept {empty_concept with sense="od"; cat="Number"; relations=Tuple[t1;modify_minute "Arg" c m1]}) in
  AddParentRelation(t2,Dot)

let expand_minute_interval c =
  let m1,m2 = split_interval c.sense in
  let t1 = Relation("Attr","",Concept {empty_concept with sense="do"; cat="HourAttr"; relations=make_minute "Arg" m2}) in
  let t2 = Relation("Attr","",Concept {empty_concept with sense="od"; cat="HourAttr"; relations=Tuple[t1;modify_minute "Arg" c m1]}) in
  AddParentRelation(t2,Dot)

let expand_hour_interval c =
  let h1,h2 = split_interval c.sense in
  let t1 = Relation("Attr","",Concept {empty_concept with sense="do"; cat="HourAttr"; relations=make_hour h2 []}) in
  let t2 = Relation("Attr","",Concept {empty_concept with sense="od"; cat="HourAttr"; relations=Tuple[t1;modify_hour c h1 []]}) in
  AddParentRelation(t2,Dot)

let expand_hour_minute_interval c =
  let hm1,hm2 = split_interval c.sense in
  let h1,m1 = split_hour_minute hm1 in
  let h2,m2 = split_hour_minute hm2 in
  let t1 = Relation("Attr","",Concept {empty_concept with sense="do"; cat="HourAttr"; relations=make_hour h2 [make_minute "RevPart" m2]}) in
  let t2 = Relation("Attr","",Concept {empty_concept with sense="od"; cat="HourAttr"; relations=Tuple[t1;modify_hour c h1 [make_minute "RevPart" m1]]}) in
  AddParentRelation(t2,Dot)

let expand_day_interval c =
  let h1,h2 = split_interval c.sense in
  let t1 = Relation("Attr","",Concept {empty_concept with sense="do"; cat="DayAttr"; relations=make_day h2 []}) in
  let t2 = Relation("Attr","",Concept {empty_concept with sense="od"; cat="DayAttr"; relations=Tuple[t1;modify_day c h1 []]}) in
  AddParentRelation(t2,Dot)

let expand_day_month_interval c =
  let dm1,dm2 = split_interval c.sense in
  let d1,m1 = split_day_month dm1 in
  let d2,m2 = split_day_month dm2 in
  let t1 = Relation("Attr","",Concept {empty_concept with sense="do"; cat="DayAttr"; relations=make_day d2 [make_month "Part" m2 []]}) in
  let t2 = Relation("Attr","",Concept {empty_concept with sense="od"; cat="DayAttr"; relations=Tuple[t1;modify_day c d1 [make_month "Part" m1 []]]}) in
  AddParentRelation(t2,Dot)

let expand_month_interval c =
  let h1,h2 = split_interval c.sense in
  let t1 = Relation("Attr","",Concept {empty_concept with sense="do"; cat="MonthAttr"; relations=make_month "Arg" h2 []}) in
  let t2 = Relation("Attr","",Concept {empty_concept with sense="od"; cat="MonthAttr"; relations=Tuple[t1;modify_month "Arg" c h1 []]}) in
  AddParentRelation(t2,Dot)

let expand_year_interval c =
  let h1,h2 = split_interval c.sense in
  let t1 = Relation("Attr","",Concept {empty_concept with sense="do"; cat="YearAttr"; relations=make_year "Arg" h2 []}) in
  let t2 = Relation("Attr","",Concept {empty_concept with sense="od"; cat="YearAttr"; relations=Tuple[t1;modify_year "Arg" c h1 []]}) in
  AddParentRelation(t2,Dot)

(*let rec expand_date_relations_rec rev = function
    Tuple l -> Xlist.fold l (Dot,rev) (fun (year,rev) t ->
      print_endline ("expand_date_relations_rec 1: " ^ SemStringOf.linear_term 0 t);
      let year2,rev = expand_date_relations_rec rev t in
      print_endline ("expand_date_relations_rec 2: " ^ SemStringOf.linear_term 0 year2);
      (match year,year2 with
        Dot,t -> t
      | t,Dot -> t
      | _ -> failwith "expand_date_relations_rec"),rev)
  | Dot -> print_endline "expand_date_relations_rec: Dot"; Dot, rev
  | Relation("Part","",(Concept{c_sense=Val "rok"} as c)) -> print_endline "rok"; c, rev
  | Relation _ as t ->
       print_endline ("expand_date_relations_rec: " ^ SemStringOf.linear_term 0 t);
       Dot, t :: rev
  | t -> failwith ("expand_date_relations_rec: " ^ SemStringOf.linear_term 0 t)

let expand_date_relations t =
  print_endline ("expand_date_relations: " ^ SemStringOf.linear_term 0 t);
  let year,rev = expand_date_relations_rec [] t in
  year,match rev with
    [] -> Dot
  | [t] -> t
  | l -> Tuple(List.rev l)*)

let expand_date c =
  (* print_endline ("expand_date: " ^ SemStringOf.linear_term 0 (Concept c)); *)
  (* if role <> "Number" then failwith ("expand_date: " ^ role) else *)
  let d,m,y,f = split_date c.sense in
  (* let year,relations = expand_date_relations c.c_relations in
  let year = if year = Dot then
    Concept {empty_concept with c_sense=Val "rok"; c_cat="Year"; c_relations=
      Relation("Number","",Concept {empty_concept with c_sense=Val y; c_cat="YearNumber"})}
  else
     AddRelation(year,"Number","",Concept {empty_concept with c_sense=Val y; c_cat=Val "YearNumber"}) in *)
  AddParentRelation(
    make_month "Part" m
      [Relation("Part","",Concept {empty_concept with sense="rok"; (*c_quant=if f then Val "sg" else Val "pro";*) cat="Year"; relations=
        Tuple[SingleRelation(if f then Val "sg" else Val "pro");Relation("Attr","",Concept {empty_concept with sense=y; cat="YearNumber"})]})],
    Concept {c with sense=d(*; c_relations=relations*)})

let expand_date_interval c =
  let dmy1,dmy2 = split_interval c.sense in
  let d1,m1,y1,_ = split_date dmy1 in
  let d2,m2,y2,_ = split_date dmy2 in
  let t1 = Relation("Attr","",Concept {empty_concept with sense="do"; cat="DayAttr"; relations=
    make_day d2 [make_month "Part" m2 [make_year "Part" y2 []]]}) in
  let t2 = Relation("Attr","",Concept {empty_concept with sense="od"; cat="DayAttr"; relations=
    Tuple[t1;modify_day c d1 [make_month "Part" m1 [make_year "Part" y1 []]]]}) in
  AddParentRelation(t2,Dot)


let expand_compound_concept c = function
        "date" -> expand_date c
      | "day-month" -> expand_day_month c
      | "hour-minute" -> expand_hour_minute c
      | "match-result" -> failwith "expand_compound_concept: ni"
      | "intnum-interval" -> expand_intnum_interval c
      | "roman-interval" -> failwith "expand_compound_concept: ni"
      | "realnum-interval" -> failwith "expand_compound_concept: ni"
      | "date-interval" -> expand_date_interval c
      | "day-month-interval" -> expand_day_month_interval c
      | "day-interval" -> expand_day_interval c
      | "month-interval" -> expand_month_interval c
      | "year-interval" -> expand_year_interval c
      | "hour-minute-interval" -> expand_hour_minute_interval c
      | "hour-interval" -> expand_hour_interval c
      | "minute-interval" -> expand_minute_interval c
      | "hour" | "year" | "day" | "intnum" | "realnum" | "realnum-en" -> Concept c
      | s -> failwith ("expand_compound_concept: " ^ s)

let rec expand_compound_concepts = function
    Node t -> Node{t with args=expand_compound_concepts t.args}
  | Concept c ->
      if c.sense="expand_compound" then 
        let pos,sense = match c.contents with
            Tuple[Val pos; Val sense] -> pos, sense
		  | _ -> failwith "expand_compound_concepts" in
		expand_compound_concepts (expand_compound_concept {c with sense=sense; contents=Dot} pos) 
	  else Concept {c with relations=expand_compound_concepts c.relations; contents=(*List.rev (Xlist.rev_map c.contents*) expand_compound_concepts c.contents}
(*      let c = {c with relations=expand_compound_concepts c.relations; contents=List.rev (Xlist.rev_map c.contents expand_compound_concepts)} in
      (match c.sense with
        Tuple[Val v;sense] -> expand_compound_concept {c with sense=sense} v
      | _ -> Concept c)*)
(*   | Context c -> Context{c with cx_contents=expand_compound_concepts c.cx_contents; cx_relations=expand_compound_concepts c.cx_relations} *)
  | Relation(r,a,t) -> Relation(r,a,expand_compound_concepts t)
  | RevRelation(r,a,t) -> RevRelation(r,a,expand_compound_concepts t)
  | SingleRelation r  -> SingleRelation r
  | AddRelation(t,r,a,s) -> AddRelation(expand_compound_concepts t,r,a,expand_compound_concepts s)
  | AddParentRelation(t,s) -> AddParentRelation(expand_compound_concepts t, expand_compound_concepts s)
  | AddSingleRelation(r,s) -> AddSingleRelation(r,expand_compound_concepts s)
  | RemoveRelation(a,b,t) -> RemoveRelation(a,b,expand_compound_concepts t)
  | SetContextName(s,t) -> SetContextName(s,expand_compound_concepts t)
  | CreateContext(s,t) -> CreateContext(s,expand_compound_concepts t)
  | ManageCoordination(s,t) -> ManageCoordination(s,expand_compound_concepts t)
  | Tuple l -> Tuple(List.rev (Xlist.rev_map l expand_compound_concepts))
  | Variant(e,l) -> Variant(e, List.rev (Xlist.rev_map l (fun (i,t) -> i, expand_compound_concepts t)))
  | Dot -> Dot
  | t -> failwith ("expand_compound_concepts: " ^ SemStringOf.linear_term 0 t)
*)
(*******************************************************************************************)

(**let rec split_apoz_relations = function
    Tuple l ->
      let pl,cl = Xlist.fold l ([],[]) (fun (pl,cl) t ->
        let p,c = split_apoz_relations t in
        p :: pl, c :: cl) in
      Tuple(List.rev pl), Tuple(List.rev cl)
  | Variant(i,l) ->
      let pl,cl = Xlist.fold l ([],[]) (fun (pl,cl) (e,t) ->
        let p,c = split_apoz_relations t in
        (e,p) :: pl, (e,c) :: cl) in
      Variant(i,List.rev pl), Variant(i,List.rev cl)
  | Dot -> Dot, Dot
  | Relation("Apoz","",t) -> t, Dot
  | Relation _ as t -> Dot, t
  | RevRelation("Apoz","",t) -> t, Dot
  | RevRelation _ as t -> Dot, t
  | SingleRelation _ as t -> Dot, t
  (* | TripleRelation("Apoz","",_,_) -> failwith ("split_apoz_relations: ni")
  | TripleRelation _ as t -> Dot, t *)
  | t -> failwith ("split_apoz_relations: " ^ SemStringOf.linear_term 0 t)

let rec merge_linear_term = function
    Dot,t -> t
  | t,Dot -> t
  | Val s,Val t -> if s = t then Val s else Tuple[Val s;Val t]
  | s, t -> failwith ("merge_linear_term: " ^ SemStringOf.linear_term 0 s ^ "   " ^ SemStringOf.linear_term 0 t)


let rec merge_apoz_concepts = function
    c, Tuple l -> Xlist.fold l c (fun c t -> merge_apoz_concepts (c,t))
  | c, Dot -> c
  | c, Variant(e,l) -> Variant(e,Xlist.map l (fun (e,t) -> e, merge_apoz_concepts (c,t)))
  | Concept c, Concept d -> Concept{
      c_sense=merge_linear_term (c.c_sense,d.c_sense);
      c_gsense=merge_linear_term (c.c_gsense,d.c_gsense);
      c_orth=merge_linear_term (c.c_orth,d.c_orth); (* FIXME: powinna być konkatenacja *)
      c_name=merge_linear_term (c.c_name,d.c_name);
      c_quant=merge_linear_term (c.c_quant,d.c_quant);
      c_local_quant=c.c_local_quant;
      c_relations=Tuple[c.c_relations;d.c_relations];
      c_variable=c.c_variable;
      c_label=if c.c_label="" || d.c_label="" then c.c_label ^ d.c_label else c.c_label ^ "," ^ d.c_label; (* FIXME *)
      c_def_label=if c.c_def_label="" || d.c_def_label="" then c.c_def_label ^ d.c_def_label else c.c_def_label ^ "," ^ d.c_def_label; (* FIXME *)
      c_pos=c.c_pos;
      c_cat=merge_linear_term (c.c_cat,d.c_cat)}
  | c, t -> failwith ("merge_apoz_concepts: " ^ SemStringOf.linear_term 0 c ^ "   " ^ SemStringOf.linear_term 0 t)

let rec merge_apoz = function
    (* Node t -> Node{t with args=merge_apoz t.args} *)
  | Concept c ->
       (* print_endline ("merge_apoz: " ^ c.c_name); *)
       let apoz_rels,c_rels = split_apoz_relations (merge_apoz c.c_relations) in
       merge_apoz_concepts (Concept{c with c_relations=c_rels},apoz_rels)
  | Context c -> Context{c with cx_contents=merge_apoz c.cx_contents; cx_relations=merge_apoz c.cx_relations}
  | Relation(r,a,t) -> Relation(r,a,merge_apoz t)
  | RevRelation(r,a,t) -> RevRelation(r,a,merge_apoz t)
  | SingleRelation r  -> SingleRelation r
  (* | TripleRelation(r,a,s,t) -> TripleRelation(r,a,merge_apoz s,merge_apoz t) *)
  (* | AddRelation(t,r,a,s) -> AddRelation(merge_apoz t,r,a,merge_apoz s)
  | RemoveRelation t -> RemoveRelation(merge_apoz t)
  | MakeTripleRelation(r,a,t) -> MakeTripleRelation(r,a,merge_apoz t) *)
  | Tuple l -> Tuple(Xlist.map l merge_apoz)
  | Variant(e,l) -> Variant(e,Xlist.map l (fun (i,t) -> i, merge_apoz t))
  | Dot -> Dot
  | Val s -> Val s
  | t -> failwith ("merge_apoz: " ^ SemStringOf.linear_term 0 t)

let rec split_parent_relations = function
    Tuple l ->
      let pl,cl = Xlist.fold l ([],[]) (fun (pl,cl) t ->
        let p,c = split_parent_relations t in
        p :: pl, c :: cl) in
      Tuple(List.rev pl), Tuple(List.rev cl)
  | Variant(i,l) ->
      let pl,cl = Xlist.fold l ([],[]) (fun (pl,cl) (e,t) ->
        let p,c = split_parent_relations t in
        (e,p) :: pl, (e,c) :: cl) in
      Variant(i,List.rev pl), Variant(i,List.rev cl)
  | Dot -> Dot, Dot
  | Relation("PHas","",t) -> Relation("Has","",t), Dot
  | Relation("PApoz","",t) -> Relation("Apoz","",t), Dot
  | Relation("PPart","",t) -> Relation("Part","",t), Dot
  | Relation("PAttr","",t) -> Relation("Attr","",t), Dot
  | Relation("PQuant","",t) -> Relation("Quant","",t), Dot
  | Relation("PRevPart","",t) -> Relation("RevPart","",t), Dot
  | Relation _ as t -> Dot, t
  | RevRelation("PHas","",t) -> failwith ("split_parent_relations ni: ni")
  | RevRelation("PApoz","",t) -> failwith ("split_parent_relations ni: ni")
  | RevRelation _ as t -> Dot, t
  | SingleRelation _ as t -> Dot, t
  (* | TripleRelation("PHas","",s,t) -> TripleRelation("Has","",s,t), Dot
  | TripleRelation("PApoz","",s,t) -> TripleRelation("Apoz","",s,t), Dot
  | TripleRelation _ as t -> Dot, t *)
  | t -> failwith ("split_parent_relations: " ^ SemStringOf.linear_term 0 t)

let rec find_parent_relations re = function
    (* Node t -> Node{t with args=find_parent_relations re t.args} *)
  | Concept c ->
       let coerced_rels,c_rels = split_parent_relations c.c_relations in
       re := coerced_rels :: !re;
       Concept{c with c_relations=c_rels}
  | Context c -> Context{c with cx_contents=find_parent_relations re c.cx_contents; cx_relations=find_parent_relations re c.cx_relations}
  | Relation(r,a,t) -> Relation(r,a,find_parent_relations re t)
  | RevRelation(r,a,t) -> RevRelation(r,a,find_parent_relations re t)
  | SingleRelation r  -> SingleRelation r
  (* | TripleRelation(r,a,s,t) -> TripleRelation(r,a,find_parent_relations re s,find_parent_relations re t) *)
  (* | AddRelation(t,r,a,s) -> AddRelation(find_parent_relations re t,r,a,find_parent_relations re s)
  | RemoveRelation t -> RemoveRelation(find_parent_relations re t)
  | MakeTripleRelation(r,a,t) -> MakeTripleRelation(r,a,find_parent_relations re t) *)
  | Tuple l -> Tuple(Xlist.map l (find_parent_relations re))
  | Variant(e,l) ->
       let l1,l2 = Xlist.fold l ([],[]) (fun (l1,l2) (i,t) ->
         let r = ref [] in
         let t = find_parent_relations r t in
         (i,t) :: l1, (i,Tuple(!r)) :: l2) in
       re := Variant(e,l2) :: !re;
       Variant(e,l1)
  | Dot -> Dot
  | Val s -> Val s
  | t -> failwith ("find_parent_relations: " ^ SemStringOf.linear_term 0 t)

let rec has_relation = function
    (* Node t -> Node{t with args=has_relation t.args} *)
  (* | Concept c ->
  | Context c ->  *)
  | Relation(r,a,t) -> true
  | RevRelation(r,a,t) -> true
  | SingleRelation r  -> true
  (* | TripleRelation(r,a,s,t) -> true *)
  (* | AddRelation(t,r,a,s) -> AddRelation(has_relation t,r,a,has_relation s)
  | RemoveRelation t -> RemoveRelation(has_relation t)
  | MakeTripleRelation(r,a,t) -> MakeTripleRelation(r,a,has_relation t) *)
  | Tuple l -> Xlist.fold l false (fun b t -> b || has_relation t)
  | Variant(_,l) -> Xlist.fold l false (fun b (_,t) -> b || has_relation t)
  | Dot -> false
  | Val s -> false
  | t -> failwith ("has_relation: " ^ SemStringOf.linear_term 0 t)

let rec shift_parent_relations = function
    (* Node t -> Node{t with args=shift_parent_relations t.args} *)
  | Concept c ->
       let coerced_rels = ref [] in
       let c_rels = find_parent_relations coerced_rels c.c_relations in
       if not (has_relation (Tuple(!coerced_rels))) then Concept{c with c_relations=shift_parent_relations c_rels} else
       shift_parent_relations (Concept{c with c_relations=Tuple(c_rels :: !coerced_rels)})
  | Context c -> Context{c with cx_contents=shift_parent_relations c.cx_contents; cx_relations=shift_parent_relations c.cx_relations}
  | Relation(r,a,t) -> Relation(r,a,shift_parent_relations t)
  | RevRelation(r,a,t) -> RevRelation(r,a,shift_parent_relations t)
  | SingleRelation r  -> SingleRelation r
  (* | TripleRelation(r,a,s,t) -> TripleRelation(r,a,shift_parent_relations s,shift_parent_relations t) *)
  (* | AddRelation(t,r,a,s) -> AddRelation(shift_parent_relations t,r,a,shift_parent_relations s)
  | RemoveRelation t -> RemoveRelation(shift_parent_relations t)
  | MakeTripleRelation(r,a,t) -> MakeTripleRelation(r,a,shift_parent_relations t) *)
  | Tuple l -> Tuple(Xlist.map l shift_parent_relations)
  | Variant(e,l) -> Variant(e,Xlist.map l (fun (i,t) -> i, shift_parent_relations t))
  | Dot -> Dot
  | Val s -> Val s
  | t -> failwith ("shift_parent_relations: " ^ SemStringOf.linear_term 0 t)**)

(*let rec simplify_tree = function
    Node t -> Node{t with args=simplify_tree t.args}
  | Concept c -> Concept{c with
      c_sense=simplify_tree c.c_sense;
      c_name=simplify_tree c.c_name;
      c_quant=simplify_tree c.c_quant;
      c_cat=simplify_tree c.c_cat;
      c_relations=simplify_tree c.c_relations}
  | Context c -> Context{c with
      cx_sense=simplify_tree c.cx_sense;
      cx_contents=simplify_tree c.cx_contents;
      cx_relations=simplify_tree c.cx_relations}
  | Relation(r,a,t) -> Relation(r,a,simplify_tree t)
  | RevRelation(r,a,t) -> RevRelation(r,a,simplify_tree t)
  | SingleRelation r  -> SingleRelation r
  (* | TripleRelation(r,a,s,t) -> TripleRelation(r,a,simplify_tree s,simplify_tree t) *)
  | Tuple l ->
      let l = Xlist.fold l [] (fun l t ->
        match simplify_tree t with
          Dot -> l
        | t -> t :: l) in
      (match l with
        [] -> Dot
      | [t] -> t
      | l -> Tuple(List.rev l))
  | Variant(_,[_,t]) -> simplify_tree t
  | Variant(e,l) ->
      let l = Xlist.map l (fun (i,t) -> i, simplify_tree t) in
      let _,t = List.hd l in
      let b = Xlist.fold (List.tl l) true (fun b (_,s) -> if s = t then b else false) in
      if b then t else
      (try
        (match t with
           Concept c ->
             let lt = Xlist.fold l [] (fun lt -> function
                 i,Concept c2 -> if c.c_sense = c2.c_sense && c.c_quant = c2.c_quant then (i,c2.c_relations) :: lt else raise Not_found
               | _ -> raise Not_found) in
             Concept{c with c_relations = simplify_tree (Variant(e,lt))}
         | Context c ->
             let lt1,lt2 = Xlist.fold l ([],[]) (fun (lt1,lt2) -> function
                 i,Context c2 -> (i,c2.cx_contents) :: lt1, (i,c2.cx_relations) :: lt2
               | _ -> raise Not_found) in
             Context{c with cx_contents= simplify_tree (Variant(e,lt1)); cx_relations = simplify_tree (Variant(e,lt2))}
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
        | Tuple tl ->
(*             print_endline ("V3: " ^ LCGstringOf.linear_term 0 (Variant l));  *)
            let n = Xlist.size tl in
            let lt = Xlist.fold l [] (fun lt -> function
              i,Tuple tl -> if n = Xlist.size tl then (i,tl) :: lt else raise Not_found
            | _ -> raise Not_found) in
            let t = Tuple(SemGraph.transpose_tuple_variant e lt) in
(*             print_endline ("V4: " ^ LCGstringOf.linear_term 0 t); *)
            simplify_tree t
         | Dot -> if Xlist.fold l true (fun b -> function
              _,Dot -> b
            | _ -> false) then Dot else raise Not_found
         | _ -> raise Not_found)
      with Not_found -> Variant(e,l))
(*   Variant(e,Xlist.map l (fun (i,t) -> i, simplify_tree t)) *)
  | Dot -> Dot
  | Val s -> Val s
  | t -> failwith ("simplify_tree: " ^ SemStringOf.linear_term 0 t)*)

(*******************************************************************************************)

(**let load_ontology filename =
  File.fold_tab filename StringMap.empty (fun onto -> function
    [parent;rel;child] ->
       let onto2 = try StringMap.find onto parent with Not_found -> StringMap.empty in
       let onto2 = StringMap.add_inc onto2 rel (StringSet.singleton child) (fun set -> StringSet.add set child) in
       StringMap.add onto parent onto2
  | line -> failwith ("load_ontology: " ^ String.concat "\t" line))

let ontology = load_ontology "data/ontology.dic"

let rec string_of_val = function
    Val s -> [s]
  | Dot -> [""]
  | Variant(_,l) -> List.flatten (Xlist.map l (fun (_,t) -> string_of_val t))
  | t -> failwith ("string_of_val: " ^ SemStringOf.linear_term 0 t)

let rec validate_ontology_concept r parent rel onto = function
    Concept c ->
       Xlist.iter (string_of_val c.c_cat) (fun v ->
         if not (StringSet.mem onto v) then r := ("validate_ontology_concept: unknown child concept " ^ parent ^ " -> " ^ rel ^ " -> " ^ v) :: !r);
       validate_ontology r ontology (Concept c)
  | Context c ->
       Xlist.iter (string_of_val c.cx_cat) (fun v ->
         if not (StringSet.mem onto v) then r := ("validate_ontology_concept: unknown child concept " ^ parent ^ " -> " ^ rel ^ " -> " ^ v) :: !r);
       validate_ontology r ontology (Context c)
  | Tuple l -> Xlist.iter l (validate_ontology_concept r parent rel onto)
  | Variant(e,l) ->
       if e = "" then r := ("validate_ontology: empty variant label") :: !r;
       Xlist.iter l (fun (_,t) -> validate_ontology_concept r parent rel onto t)
  | t -> failwith ("validate_ontology_concept: " ^ SemStringOf.linear_term 0 t)

and validate_ontology_rel r cat onto = function
    Relation(rel,a,t) ->  (* FIXME: role attribute *)
      (try
        let onto = StringMap.find onto rel in
        validate_ontology_concept r cat rel onto t
      with Not_found -> (
        r := ("validate_ontology_rel: unknown relation " ^ cat ^ " -> " ^ rel) :: !r;
        validate_ontology r ontology t))
  | RevRelation(rel,a,t) -> () (* FIXME: zaimplementować *)
  | SingleRelation rel  -> () (* FIXME: zaimplementować *)
  (* | TripleRelation(rel,a,s,t) ->
      validate_ontology r ontology s;
      (try
        let onto = StringMap.find onto rel in
        validate_ontology_concept r cat rel onto t
      with Not_found -> (
        r := ("validate_ontology_rel: unknown relation " ^ cat ^ " -> " ^ rel) :: !r;
        validate_ontology r ontology t)) *)
  | Tuple l -> Xlist.iter l (validate_ontology_rel r cat onto)
  | Variant(e,l) ->
       if e = "" then r := ("validate_ontology_rel: empty variant label") :: !r;
       Xlist.iter l (fun (_,t) -> validate_ontology_rel r cat onto t)
  | Dot -> ()
  | t -> failwith ("validate_ontology_rel: " ^ cat ^ " " ^ SemStringOf.linear_term 0 t)

and validate_ontology r onto = function
    Concept c ->
       Xlist.iter (string_of_val c.c_cat) (fun v ->
         let onto = try StringMap.find onto v with Not_found -> StringMap.empty in
         validate_ontology_rel r v onto c.c_relations)
  | Context c ->
(*        print_endline "validate_ontology"; *)
       validate_ontology r onto c.cx_contents;
       Xlist.iter (string_of_val c.cx_cat) (fun v ->
         let onto = try StringMap.find onto v with Not_found -> StringMap.empty in
         validate_ontology_rel r v onto c.cx_relations)
  | Tuple l -> Xlist.iter l (validate_ontology r onto)
  | Variant(e,l) ->
       if e = "" then r := ("validate_ontology: empty variant label") :: !r;
       Xlist.iter l (fun (_,t) -> validate_ontology r onto t)
  | Dot -> ()
  | t -> failwith ("validate_ontology: " ^ SemStringOf.linear_term 0 t)**)



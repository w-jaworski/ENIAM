(*
 *  ENIAMvalence is a library that assigns tokens with lexicosemantic information.
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

open WalTypes
open Xstd

let transform_zeby = function
    Aff -> [Comp "że"]
  | Negation -> [Comp "że";Comp "żeby"]
  | NegationUndef -> [Comp "że";Comp "żeby"]

let transform_gdy = function
    "indicative" -> [Comp "gdy"]
  | "imperative" -> [Comp "gdy"]
  | "conditional" -> [Comp "gdyby"]
  | "gerundial" -> [Comp "gdy"]
  | "no-subj" -> [Comp "gdy"]
  | "" -> [Comp "gdy";Comp "gdyby"]
  | s -> failwith ("transform_gdy: " ^ s)

let transform_comp negation mood = function
    Comp comp -> [Comp comp]
  | Zeby -> transform_zeby negation
  | Gdy -> transform_gdy mood
  | CompUndef -> [CompUndef]

let transform_str mood negation =
  if mood = "gerundial" then [Case "gen"] else
  match negation with
    Aff -> [Case "acc"]
  | Negation -> [Case "gen"]
  | NegationUndef -> [Case "acc";Case "gen"]

let transform_phrase pos lemma = function
  | CP(ctype,comp) as morf -> [morf]
  | PrepNP _ as morf -> [morf]
  | PrepFixed _ as morf -> [morf]
  | PrepAdjP _ as morf -> [morf]
  | ComprepNP _ as morf -> [morf]
  | ComparP _ as morf -> [morf]
  | PrepNCP _ as morf -> [morf]
  | InfP _ as morf -> [morf]
  | AdvP _ as morf -> [morf]
  | XP as morf -> [morf]
  | IP as morf -> [morf]
  | AdjA as morf -> [morf]
  | PadvP as morf -> [morf]
  | AdMod _ as morf -> [morf]
  | Qub as morf -> [morf]
  | FixedP _ as morf -> [morf]
  | SymbolP as morf -> [morf]
  | ColonP as morf -> [morf]
  | Inclusion as morf -> [morf]
  | Or as morf -> [morf]
  | Pro as morf -> [morf]
  | Null as morf -> [morf]
  | Head -> [Null]
  | morf -> failwith ("transform_phrase: " ^ lemma ^ " " ^ pos ^ " " ^ WalStringOf.phrase morf)

let transform_noun_phrase lemma = function
    NP(Case case) -> [NP(Case case)(*;NumP(Case case)*)]
  | NP(CaseAgr) -> [NP(CaseAgr)(*;NumP(CaseAgr)*)]
  | NPA(CaseAgr) -> [NPA(CaseAgr)(*;NumP(CaseAgr)*)]
  | NCP(Case c,ctype,comp) -> [NCP(Case c,ctype,comp)]
  | NP(CaseUndef) -> [NP(CaseUndef)(*;NumP(Case case)*)]
  | AdjP(Case _) as morf -> [morf] (* tylko 'zagłada adjp(postp)' *)
  | AdjP(CaseAgr) -> [AdjP(AllAgr)]
  | AdjP(Str) -> [AdjP(AllAgr)] (* chyba błąd walentego, tylko 'barwa', 'bieda', 'głód', 'kolor', 'nędza', 'śmierć', 'usta' *)
  | morf -> transform_phrase "noun" lemma morf

let transform_noun_pos lemma = function
  | SUBST(_,Case _) as morf -> [morf]
  | PPRON3(_,Case _) as morf -> [morf]
  | SUBST(_,CaseAgr) as morf -> [morf]
  | SUBST(n,Str) -> [ADJ(n,AllAgr,GenderUndef,Grad "pos")] (* błąd walentym: 'zła godzina' *)
  | ADJ(_,Case _,_,_) as morf -> [morf]
  | ADJ(n,CaseAgr,GenderAgr,gr) -> [ADJ(n,AllAgr,GenderAgr,gr)]
  | PACT(n,CaseAgr,g,a,neg) -> [PACT(n,AllAgr,g,a,neg)]
  | PPAS(_,Case _,_,_,_) as morf -> [morf]
  | PPAS(n,CaseAgr,g,a,neg) -> [PPAS(n,AllAgr,g,a,neg)]
  | ADJ(n,Str,g,gr) -> [ADJ(n,AllAgr,g,gr)]
  | PPAS(n,Str,g,a,neg) -> [PPAS(n,AllAgr,g,a,neg)]
  | PREP(Case _) as morf -> [morf]
  | ADV _ as morf -> [morf] (* tu trafiają przysłówkowe realizacje *)
  | COMP _ as morf -> [morf]
  | QUB as morf -> [morf]
  | PERS _ as morf -> [morf]
  | pos -> failwith ("transform_noun_pos: " ^ lemma ^ " " ^ WalStringOf.pos pos)

let transform_adj_phrase lemma = function
    NP(Case case) -> [NP(Case case)(*;NumP(Case case)*)]
  | NP(Part) -> [NP(Case "gen");NP(Case "acc")(*;NumP(Case "gen");NumP(Case "acc")*)] (* jedno wystąpienie 'krewny' *)
  | NCP(Case c,ctype,comp) -> [NCP(Case c,ctype,comp)]
  | AdjP(CaseAgr) -> [AdjP(AllAgr)] (* jedno wystąpienie 'cały szczęśliwy', może się przydać podniesienie typu *)
  | morf -> transform_phrase "adj" lemma morf

let transform_adj_pos lemma = function
  | ADJ(n,CaseAgr,g,gr) -> [ADJ(n,AllAgr,g,gr)]
  | PREP(Case _) as morf -> [morf]
  | ADV _ as morf -> [morf]
  | QUB as morf -> [morf]
  | morf -> failwith ("transform_adj_pos: " ^ lemma ^ " " ^ WalStringOf.pos morf)

let transform_adv_phrase lemma = function
    NP(Case case) -> [NP(Case case)]
  | NP(CaseUndef) -> [NP(CaseUndef)]
  | NCP(Case c,ctype,comp) -> [NCP(Case c,ctype,comp)]
  | morf -> transform_phrase "adv" lemma morf

let transform_adv_pos lemma = function
    SUBST(_,Case _) as morf -> [morf]
  (*    | ADJ(_,CaseAgr,_,_) as morf -> [morf]*)
  | COMP _ as morf -> [morf]
  | PREP(Case _) as morf -> [morf]
  | COMPAR _ as morf -> [morf]
  | ADV _ as morf -> [morf] (* tu trafiają przysłówkowe realizacje *)
  | morf -> failwith ("transform_adv_pos: " ^ lemma ^ " " ^ WalStringOf.pos morf)

let transform_prep_phrase lemma = function
    NP(Case case) -> [NP(Case case)]
  | morf -> transform_phrase "prep" lemma morf

let transform_prep_pos lemma = function
  | SUBST(_,Case _) as morf -> [morf]
  | SIEBIE(Case _) as morf -> [morf]
  | PPRON12(_,Case _) as morf -> [morf]
  | PPRON3(_,Case _) as morf -> [morf]
  | SUBST(n,Str) -> [SUBST(n,CaseAgr)]
  | NUM(Case _,_) as morf -> [morf]
  | ADJ(_,Case _,_,_) as morf -> [morf]
  | GER(_,Case _,_,_,_) as morf -> [morf]
  | PPAS(_,Case _,_,_,_) as morf -> [morf]
(*  | ADV _ as morf -> [morf]*)
  | QUB as morf -> [morf]
  | pos -> failwith ("transform_prep_pos: " ^ lemma ^ " " ^ WalStringOf.pos pos)

let transform_comprep_phrase lemma = function
    NP(Case case) -> [NP(Case case)(*;NumP(Case case)*)]
  | NCP(Case c,ctype,comp) -> [NCP(Case c,ctype,comp)]
  | morf -> transform_phrase "comprep" lemma morf

let transform_comprep_pos lemma = function
  | SUBST(_,Case _) as morf -> [morf]
(*  | SUBST(n,Str) -> [SUBST(n,CaseAgr)]*)
  | NUM(Case _,_) as morf -> [morf]
(*  | ADJ(_,Case _,_,_) as morf -> [morf]
  | GER(_,Case _,_,_,_,_) as morf -> [morf]
  | PPAS(_,Case _,_,_,_) as morf -> [morf]
  | ADV _ as morf -> [morf]
    | QUB as morf -> [morf]*)
  | pos -> failwith ("transform_comprep_pos: " ^ lemma ^ " " ^ WalStringOf.pos pos)

let transform_compar_phrase lemma = function
  | NP(Str) -> Xlist.map ["nom";"gen";"dat";"acc";"inst"] (fun case -> NP(Case case)) (* FIXME: sprawdzić kto kontroluje! *) (* FIXME: uzgodnić a komparatywem *)
  | morf -> transform_phrase "compar" lemma morf

let transform_compar_pos lemma = function
  | SUBST(_,Case _) as morf -> [morf]
  | ADJ(_,Case _,_,_) as morf -> [morf]
  | PREP(Case _) as morf -> [morf]
  | SUBST(Number n,Str) -> [SUBST(Number n,CaseUndef)]
  | SUBST(NumberAgr,Str) -> [SUBST(NumberUndef,CaseUndef)]
  | SUBST(NumberUndef,Str) -> [SUBST(NumberUndef,CaseUndef)]
  | PPAS(_,Case _,_,_,_) as morf -> [morf]
  | PPAS(NumberAgr,Str,GenderAgr,a,neg) -> [PPAS(NumberUndef,CaseUndef,GenderUndef,a,neg)]  (* FIXME: ta sama sytuacja co w "jako" *)
  | PPAS(NumberAgr,CaseAgr,GenderAgr,a,neg) -> [PPAS(NumberUndef,CaseUndef,GenderUndef,a,neg)] (* FIXME: ta sama sytuacja co w "jako" *)
  | ADJ(NumberAgr,Str,GenderAgr,gr) -> [ADJ(NumberUndef,CaseUndef,GenderUndef,gr)] (* FIXME: ta sama sytuacja co w "jako" *)
  | ADJ(NumberAgr,CaseAgr,GenderAgr,gr) -> [ADJ(NumberUndef,CaseUndef,GenderUndef,gr)] (* FIXME: ta sama sytuacja co w "jako" *)
  | NUM(Case _,_) as morf -> [morf]
  | pos -> failwith ("transform_compar_pos: " ^ lemma ^ " " ^ WalStringOf.pos pos)

let transform_comp_pos lemma = function
  | PERS _ as morf -> [morf]
  | pos -> failwith ("transform_comp_pos: " ^ lemma ^ " " ^ WalStringOf.pos pos)

let transform_qub_pos lemma = function
  | QUB as morf -> [morf]
  | pos -> failwith ("transform_qub_pos: " ^ lemma ^ " " ^ WalStringOf.pos pos)

let transform_interj_phrase lemma = function
    NP(Case "nom") as morf -> [morf]
  | morf -> transform_phrase "interj" lemma morf

let transform_interj_pos lemma = function
  | pos -> failwith ("transform_interj_pos: " ^ lemma ^ " " ^ WalStringOf.pos pos)

let transform_sinterj_pos lemma = function
  | pos -> failwith ("transform_sinterj_pos: " ^ lemma ^ " " ^ WalStringOf.pos pos)

let transform_aglt_pos lemma = function
  | pos -> failwith ("transform_aglt_pos: " ^ lemma ^ " " ^ WalStringOf.pos pos)

let transform_siebie_pos lemma = function
  | ADJ(NumberAgr,CaseAgr,GenderAgr,gr) -> [ADJ(NumberAgr,AllAgr,GenderAgr,gr)]
  | pos -> failwith ("transform_siebie_pos: " ^ lemma ^ " " ^ WalStringOf.pos pos)

let transform_pers_subj_phrase lemma negation mood = function (* FIXME: prepnp(na,loc) *)
  | NP(Str) -> [NP(NomAgr);NP(VocAgr)(*;NumP(NomAgr)*)]
  | NP(Part) -> [NP(Case "gen");NP(Case "acc")(*;NumP(Case "gen");NumP(Case "acc")*)] (* tylko w 'nalewać', 'nalać', 'ponalewać', 'najechać','uzbierać' *)
  | NCP(Str,ctype,comp) -> [NCP(NomAgr,ctype,comp);NCP(VocAgr,ctype,comp)]
  | Pro -> [ProNG]
  | morf -> transform_phrase "pers_subj" lemma morf

let transform_pers_subj_pos lemma negation mood = function
  (*    COMP _ as morf -> [morf]*)
  | SUBST(n,Str) -> [SUBST(n,NomAgr)]
  | SUBST(n,Case "nom") -> [SUBST(n,NomAgr)] (* wygląda na błąd Walentego, ale nie ma znaczenia *)
  | NUM(Str,g) -> [NUM(NomAgr,g)]
  | NUM(Case "nom",g) -> [NUM(NomAgr,g)]
(*      | ADJ(n,Str,g,gr) -> [ADJ(n,NomAgr,g,gr)]*)
  | morf -> failwith ("transform_pers_subj_pos: " ^ lemma ^ " " ^ WalStringOf.pos morf)

let transform_ger_subj_phrase lemma negation mood control = function
  | NP(Str) -> [NP(Case "gen");PrepNP("przez",Case "acc")(*;NumP(Case "gen")*)(*;PrepNumP("przez",Case "acc")*)] (* FIXME: czy przez:acc jest możliwe? *)
  | NP(Part) -> [NP(Case "gen")(*;NP(Case "acc")*)(*;NumP(Case "gen");NumP(Case "acc")*)]
  | NCP(Str,ctype,comp) -> [NCP(Case "gen",ctype,comp);PrepNCP("przez",Case "acc",ctype,comp)] (* FIXME: czy przez:acc jest możliwe? *)
  | Pro -> if control then [Pro] else [Null]
  | morf -> transform_phrase "ger_subj" lemma morf

let transform_ger_subj_pos lemma negation mood = function (* FIXME: ADV(_) *)
  (*    COMP _ as morf -> [morf] (* FIXME: czy to jest możliwe? *)*)
  | SUBST(n,Str) -> [SUBST(n,Case "gen")]
  | SUBST(n,Case "nom") -> [SUBST(n,Case "gen")] (* wygląda na błąd Walentego, ale nie ma znaczenia *)
  | NUM(Str,g) -> [NUM(Case "gen",g)]
  | NUM(Case "nom",g) -> [NUM(Case "gen",g)]
(* | ADJ(n,Str,g,gr) -> [ADJ(n,Case "gen",g,gr)]*)
  | morf -> failwith ("transform_pers_subj_pos: " ^ lemma ^ " " ^ WalStringOf.pos morf)

let transform_ppas_subj_phrase lemma negation mood control = function
  | NP(Str) -> [PrepNP("przez",Case "acc")(*;PrepNumP("przez",Case "acc")*)]
  | NCP(Str,ctype,comp) -> [PrepNCP("przez",Case "acc",ctype,comp)]
  | Pro -> if control then [Pro] else [Null]
  | morf -> transform_phrase "ppas_subj" lemma morf

let transform_pers_phrase lemma negation mood = function
  | NP(Str) -> List.flatten (Xlist.map (transform_str mood negation) (fun case -> [NP case(*;NumP(case)*)]))
  | NP(Part) -> [NP(Case "gen")] @ (if mood = "gerundial" then [] else [NP(Case "acc")(*;NumP(Case "gen");NumP(Case "acc")*)])
  | NP(Case case) -> [NP(Case case)(*;NumP(Case case)*)]
  | NP(CaseUndef) -> [NP(CaseUndef)(*;NumP(Case case)*)]
  | NCP(Str,ctype,comp) -> List.flatten (Xlist.map (transform_str mood negation) (fun case -> [NCP(case,ctype,comp)]))
  | NCP(Part,ctype,comp) -> List.flatten (Xlist.map (transform_str mood negation) (fun case -> [NCP(case,ctype,comp)]))
  | NCP(Case case,ctype,comp) -> [NCP(Case case,ctype,comp)]
  | AdjP(Str) -> Xlist.map (transform_str mood negation) (fun case -> AdjP case) (* FIXME: pomijam uzgadnianie liczby i rodzaju - wykonalne za pomocą kontroli *)
  | AdjP CaseAgr (*as morf*) -> if mood = "gerundial" then [AdjP AllAgr] else [](*failwith ("transform_pers_phrase2: " ^ lemma ^ " " ^ WalStringOf.phrase morf)*) (* FIXME: trzeba by zrobić mapowanie między przymiotnikami i przysłówkami *)
  | AdjP(Case _) as morf -> [morf] (* FIXME: pomijam uzgadnianie liczby i rodzaju - wykonalne za pomocą kontroli *)
  | AdjP(NomAgr) as morf -> if mood = "no-subj" then [AdjP(Case "nom")] else [morf]
  | AdvP "misc" -> if mood = "gerundial" then [AdjP AllAgr] else [AdvP "misc"]
  | AdvP "mod" -> if mood = "gerundial" then [AdjP AllAgr] else [AdvP "mod"]
  | morf -> transform_phrase "pers" lemma morf

let transform_pers_pos lemma negation mood = function
  | SUBST(n,Str) -> Xlist.map (transform_str mood negation) (fun case -> SUBST(n,case))
  | PPRON12(n,Str) -> Xlist.map (transform_str mood negation) (fun case -> PPRON12(n,case))
  | PPRON3(n,Str) -> Xlist.map (transform_str mood negation) (fun case -> PPRON3(n,case))
  | SIEBIE(Str) -> Xlist.map (transform_str mood negation) (fun case -> SIEBIE(case))
  | NUM(Str,g) -> Xlist.map (transform_str mood negation) (fun case -> NUM(case,g))
  | ADJ(n,Str,g,gr) -> Xlist.map (transform_str mood negation) (fun case -> ADJ(n,case,g,gr))
(*     | PPAS(n,Str,g,a,neg) -> Xlist.map (transform_str negation) (fun case -> PPAS(n,Str,g,a,neg))*)
  | SUBST(n,Part) -> [SUBST(n,Case "gen");SUBST(n,Case "acc")]
  | ADJ(n,Part,g,gr) -> [ADJ(n,Case "gen",g,gr);ADJ(n,Case "acc",g,gr)]
  | ADJ(n,CaseAgr,g,gr) as morf -> if lemma = "siedzieć" then [ADJ(n,AllAgr,g,gr)] else (failwith ("transform_pers_pos2: " ^ lemma ^ " " ^ WalStringOf.pos morf)) (* FIXME *)
  | SUBST(_,Case _) as morf -> [morf]
  | PPRON12(_,Case _) as morf -> [morf]
  | PPRON3(_,Case _) as morf -> [morf]
  | SIEBIE(Case _) as morf -> [morf]
  | NUM(Case _,_) as morf -> [morf]
  | PREP _ as morf -> [morf]
  | ADJ(_,Case _,_,_) as morf -> [morf]
  | PPAS(_,Case _,_,_,_) as morf -> [morf]
(*  | SUBST(n,CaseAgr) -> Xlist.map ["nom";"gen";"dat";"acc";"inst"] (fun case -> SUBST(n,Case case)) (* FIXME: sprawdzić kto kontroluje! *)
    | ADJ(n,CaseAgr,g,gr) -> Xlist.map ["nom";"gen";"dat";"acc";"inst"] (fun case -> ADJ(n,Case case,g,gr))  (* FIXME: sprawdzić kto kontroluje! *)*)
  | COMPAR _ as morf -> [morf]
  | COMP _ as morf -> [morf]
  | INF _ as morf -> [morf]
  | QUB as morf -> [morf]
  | ADV grad -> (*if mood = "gerundial" then [ADJ(NumberAgr,AllAgr,GenderAgr,grad)] else*) [ADV grad]  (* FIXME: to nie poprawi lematu *)
  | PERS _ as morf -> [morf]
  | morf -> failwith ("transform_pers_pos: " ^ lemma ^ " " ^ WalStringOf.pos morf)

let rec transform_comps negation mood = function
  | CP(ctype,comp) -> Xlist.map (transform_comp negation mood comp) (fun comp -> CP(ctype,comp))
  | NCP(case,ctype,comp) -> Xlist.map (transform_comp negation mood comp) (fun comp -> NCP(case,ctype,comp))
  | PrepNCP(prep,case,ctype,comp) -> Xlist.map (transform_comp negation mood comp) (fun comp -> PrepNCP(prep,case,ctype,comp))
  | E phrase -> Xlist.map (transform_comps negation mood phrase) (fun phrase -> E phrase)
  | morf -> [morf]

let compars = StringSet.of_list ["jak"; "jako"; "niż"; "niczym" ;"niby"; "co"; "zamiast"]

let is_compar lex = StringSet.mem compars lex

(* FIXME: pomijam uzgadnianie przypadku, liczby i rodzaju - wykonalne za pomocą kontroli *)
let transform_preps morf =
  let morf = match morf with
      | LexArg(id,lex,PREP c) -> if is_compar lex then LexArg(id,lex,COMPAR c) else LexArg(id,lex,PREP c)
      | SimpleLexArg(lex,PREP c) -> if is_compar lex then SimpleLexArg(lex,COMPAR c) else SimpleLexArg(lex,PREP c)
      | PrepNP(prep,c) -> if is_compar prep then ComparP(prep,c) else PrepNP(prep,c)
      | PrepAdjP(prep,c) -> if is_compar prep then ComparP(prep,c) else PrepAdjP(prep,c)
      | PrepNCP(prep,case,ctype,comp) as morf -> if is_compar prep then failwith "transform_preps 1" else morf
      | morf -> morf in
  match morf with
    | ComparP(prep,Str) -> Xlist.map ["nom";"gen";"dat";"acc";"inst";"postp"] (fun case -> ComparP(prep,Case case))
    | ComparP(_,CaseUndef) as morf -> [morf]
    | ComparP(_,Case _) as morf -> [morf]
    | LexArg(id,lex,COMPAR Str) -> Xlist.map ["nom";"gen";"dat";"acc";"inst";"postp"] (fun case -> LexArg(id,lex,COMPAR (Case case)))
    | SimpleLexArg(lex,COMPAR Str) -> Xlist.map ["nom";"gen";"dat";"acc";"inst";"postp"] (fun case -> SimpleLexArg(lex,COMPAR (Case case)))
    | LexArg(id,lex,COMPAR (Case _)) as morf -> [morf]
    | SimpleLexArg(lex,COMPAR (Case _)) as morf -> [morf]
    | LexArg(id,lex,COMPAR _) -> failwith "transform_preps 3"
    | SimpleLexArg(lex,COMPAR _) -> failwith "transform_preps 4"
    | PrepNP("per",Str) -> [PrepNP("per",Case "nom");PrepNP("per",Case "voc")] (* FIXME: voc do poprawienie w leksykonie *)
    | PrepNP(_,Case _) as morf -> [morf]
    | PrepAdjP(_,Case _) as morf -> [morf]
    | PrepNCP(_,Case _,_,_) as morf -> [morf]
    | PrepNP(_,CaseUndef) as morf -> [morf]
    | PrepNP _ as morf -> failwith ("transform_preps 5: " ^ WalStringOf.phrase morf)
    | PrepAdjP _ -> failwith "transform_preps 6"
    | PrepNCP _ -> failwith "transform_preps 7"
    | LexArg(id,"w",PREP Str) -> [LexArg(id,"w",PREP (Case "acc"));LexArg(id,"w",PREP (Case "loc"));]
    | SimpleLexArg("w",PREP Str) -> [SimpleLexArg("w",PREP (Case "acc"));SimpleLexArg("w",PREP (Case "loc"))]
    | LexArg(id,lex,PREP (Case _)) as morf -> [morf]
    | SimpleLexArg(lex,PREP (Case _)) as morf -> [morf]
    | LexArg(id,lex,PREP _) -> failwith "transform_preps 8"
    | SimpleLexArg(lex,PREP _) -> failwith "transform_preps 9"
    | morf -> [morf]

let transform_pers_schema lemma negation mood schema =
  Xlist.map schema (fun s ->
      {s with morfs = (
                let morfs = List.flatten (Xlist.map s.morfs (transform_comps negation mood)) in
                (* Printf.printf "A %s\n" (String.concat " " (Xlist.map morfs WalStringOf.phrase)); *)
                let morfs = List.flatten (Xlist.map morfs transform_preps) in
                (* Printf.printf "B %s\n" (String.concat " " (Xlist.map morfs WalStringOf.phrase)); *)
                let morfs = if s.gf = SUBJ then List.flatten (Xlist.map morfs (function
                    | E phrase -> Xlist.map (transform_pers_subj_phrase lemma negation mood phrase) (fun phrase -> E phrase)
                    | LexArg(id,lex,pos) -> Xlist.map (transform_pers_subj_pos lemma negation mood pos) (fun pos -> LexArg(id,lex,pos))
                    | SimpleLexArg(lex,pos) -> Xlist.map (transform_pers_subj_pos lemma negation mood pos) (fun pos -> SimpleLexArg(lex,pos))
                    | phrase -> transform_pers_subj_phrase lemma negation mood phrase))
                else List.flatten (Xlist.map morfs (function
                    | LexArg(id,lex,pos) -> Xlist.map (transform_pers_pos lemma negation mood pos) (fun pos -> LexArg(id,lex,pos))
                    | SimpleLexArg(lex,pos) -> Xlist.map (transform_pers_pos lemma negation mood pos) (fun pos -> SimpleLexArg(lex,pos))
                    | phrase -> transform_pers_phrase lemma negation mood phrase)) in
                (* Printf.printf "C %s\n" (String.concat " " (Xlist.map morfs WalStringOf.phrase)); *)
                morfs)})

let transform_nosubj_schema lemma pro negation mood schema =
  Xlist.map schema (fun s ->
      {s with morfs =
                let morfs = List.flatten (Xlist.map s.morfs (transform_comps negation mood)) in
(*                 Printf.printf "A %s\n" (String.concat " " (Xlist.map morfs WalStringOf.phrase)); *)
                let morfs = List.flatten (Xlist.map morfs transform_preps) in
(*                 Printf.printf "B %s\n" (String.concat " " (Xlist.map morfs WalStringOf.phrase)); *)
                if s.gf = SUBJ then [pro]
                else List.flatten (Xlist.map morfs (function
                    | LexArg(id,lex,pos) -> Xlist.map (transform_pers_pos lemma negation mood pos) (fun pos -> LexArg(id,lex,pos))
                    | SimpleLexArg(lex,pos) -> Xlist.map (transform_pers_pos lemma negation mood pos) (fun pos -> SimpleLexArg(lex,pos))
                    | phrase -> transform_pers_phrase lemma negation mood phrase))})

(* let transform_ger_adv_lex = function
  | s -> print_endline ("transform_ger_adv_lex: " ^ s); s

let transform_ger_adv_pos = function
  | LexArg(id,lex,ADV grad) -> LexArg(id,transform_ger_adv_lex lex,ADJ(NumberAgr,AllAgr,GenderAgr,grad))
  | SimpleLexArg(lex,ADV grad) -> SimpleLexArg(transform_ger_adv_lex lex,ADJ(NumberAgr,AllAgr,GenderAgr,grad))
  | morf -> morf *)

let transform_ger_schema lemma negation schema = (* FIXME: zakładam, że ger zeruje mood, czy to prawda? *)
  Xlist.map schema (fun s ->
      {s with morfs =
                let morfs = List.flatten (Xlist.map s.morfs (transform_comps negation "gerundial")) in
                let morfs = List.flatten (Xlist.map morfs transform_preps) in
                (* let morfs = Xlist.map morfs transform_ger_adv_pos in *)
                if s.gf = SUBJ then List.flatten (Xlist.map morfs (function
                    | E phrase -> Xlist.map (transform_ger_subj_phrase lemma negation "gerundial" (s.cr <> [] || s.ce <> []) phrase) (fun phrase -> E phrase)
                    | LexArg(id,lex,pos) -> Xlist.map (transform_ger_subj_pos lemma negation "gerundial" pos) (fun pos -> LexArg(id,lex,pos))
                    | SimpleLexArg(lex,pos) -> Xlist.map (transform_ger_subj_pos lemma negation "gerundial" pos) (fun pos -> SimpleLexArg(lex,pos))
                    | phrase -> transform_ger_subj_phrase lemma negation "gerundial" (s.cr <> [] || s.ce <> []) phrase))
                else List.flatten (Xlist.map morfs (function
                    | LexArg(id,lex,pos) -> Xlist.map (transform_pers_pos lemma negation "gerundial" pos) (fun pos -> LexArg(id,lex,pos))
                    | SimpleLexArg(lex,pos) -> Xlist.map (transform_pers_pos lemma negation "gerundial" pos) (fun pos -> SimpleLexArg(lex,pos))
                    | phrase -> transform_pers_phrase lemma negation "gerundial" phrase))})

let transform_ppas_schema lemma negation mood schema =
  if not (Xlist.fold schema false (fun b p -> if p.gf = OBJ then true else b)) then
    (*failwith ("transform_ppas_schema: attempt to make ppas schema for lemma " ^ lemma ^ "without OBJ arg")*)raise Not_found else
  Xlist.map schema (fun s ->
        let morfs = List.flatten (Xlist.map s.morfs (transform_comps negation mood)) in
        let morfs = List.flatten (Xlist.map morfs transform_preps) in
        {s with morfs =
                if s.gf = OBJ then [Null] else
                if s.gf = SUBJ then List.flatten (Xlist.map morfs (function
                    | E phrase ->  raise Not_found (* tylko 'obladzać' i 'oblodzić', chyba błąd *)
                    | LexArg(id,lex,SUBST(n,Str)) -> raise Not_found (* FIXME!!! *)
                    | SimpleLexArg(lex,SUBST(n,Str)) -> raise Not_found (* FIXME!!! *)
                    | phrase -> transform_ppas_subj_phrase lemma negation mood (s.cr <> [] || s.ce <> []) phrase))
                else List.flatten (Xlist.map morfs (function
                    | LexArg(id,lex,pos) -> Xlist.map (transform_pers_pos lemma negation mood pos) (fun pos -> LexArg(id,lex,pos))
                    | SimpleLexArg(lex,pos) -> Xlist.map (transform_pers_pos lemma negation mood pos) (fun pos -> SimpleLexArg(lex,pos))
                    | phrase -> transform_pers_phrase lemma negation mood phrase))})

let get_rev_obj_roles schema =
  let roles = Xlist.fold schema [] (fun roles s ->
    if s.gf = OBJ then ("Rev-" ^ s.role) :: roles else roles) in
  if roles = [] then raise Not_found else roles
  
let get_rev_subj_roles schema =
  let roles = Xlist.fold schema [] (fun roles s ->
    if s.gf = SUBJ then ("Rev-" ^ s.role) :: roles else roles) in
  if roles = [] then raise Not_found else roles

let get_rev_head_roles schema =
  Xlist.fold schema [] (fun roles s ->
    Xlist.fold s.morfs roles (fun roles -> function
        Head -> ("Rev-" ^ s.role) :: roles
      | _ -> roles))

                    
let transform_num_schema acm schema =
  Xlist.map schema (fun s ->
      {s with morfs=List.flatten (Xlist.map s.morfs (function
           | Null -> [Null]
           | FixedP _ as morf -> [morf]
           | LexArg(id,lex,SUBST(NumberUndef,CaseUndef)) ->
             (match acm with
                "rec" -> [LexArg(id,lex,SUBST(NumberUndef,GenAgr))]
              | "congr" -> [LexArg(id,lex,SUBST(NumberUndef,AllAgr))]
              | _ -> failwith "transform_num_schema")
           | SimpleLexArg(lex,SUBST(NumberUndef,CaseUndef)) ->
             (match acm with
                "rec" -> [SimpleLexArg(lex,SUBST(NumberUndef,GenAgr))]
              | "congr" -> [SimpleLexArg(lex,SUBST(NumberUndef,AllAgr))]
              | _ -> failwith "transform_num_schema")
           | morf -> failwith ("transform_num_schema: " ^ WalStringOf.phrase morf)))})

let transform_schema pos lemma schema =
  let phrase_fun,pos_fun = match pos with
      "subst" -> transform_noun_phrase,transform_noun_pos
    | "adj" -> transform_adj_phrase,transform_adj_pos
    | "adv" -> transform_adv_phrase,transform_adv_pos
    | "prep" | "x" -> transform_prep_phrase,transform_prep_pos
    | "comprep" -> transform_comprep_phrase,transform_comprep_pos
    | "compar" -> transform_compar_phrase,transform_compar_pos
    | "comp" -> transform_phrase "comp",transform_comp_pos
    | "qub" -> transform_phrase "qub",transform_qub_pos
    | "siebie" -> transform_phrase "siebie",transform_siebie_pos
    | "interj" -> transform_interj_phrase,transform_interj_pos
    | "sinterj" -> transform_phrase "sinterj",transform_interj_pos
    | "aglt" -> transform_phrase "aglt",transform_interj_pos
    | _ -> failwith "transform_schema"
  in
  Xlist.map schema (fun s ->
      let morfs = List.flatten (Xlist.map s.morfs (transform_comps NegationUndef "")) in (* FIXME: zależność od trybu warunkowego i negacji *)
      let morfs = List.flatten (Xlist.map morfs transform_preps) in
      {s with morfs=List.flatten (Xlist.map morfs (function
             LexArg(id,lex,pos) -> Xlist.map (pos_fun lemma pos) (fun pos -> LexArg(id,lex,pos))
           | SimpleLexArg(lex,pos) -> Xlist.map (pos_fun lemma pos) (fun pos -> SimpleLexArg(lex,pos))
           | phrase -> phrase_fun lemma phrase))})

let rec remove_adj_agr = function
    [] -> []
  | {morfs=[Null;AdjP(CaseAgr)]} :: l -> remove_adj_agr l
  | {morfs=[Null;AdjP(Part)]} :: l -> remove_adj_agr l
  | s :: l -> (*print_endline (WalStringOf.schema [s]);*) s :: (remove_adj_agr l)

let rec get_role gf = function
    [] -> raise Not_found
  | s :: l -> if s.gf = gf then s.role,s.role_attr else get_role gf l

let expand_negation = function
    Negation -> [Negation]
  | Aff -> [Aff]
  | NegationUndef -> [Negation;Aff]

let expand_aspect = function
    Aspect s -> [Aspect s]
  | AspectUndef -> [Aspect "imperf";Aspect "perf"]

let aspect_sel = function
    Aspect s -> [LCGlexiconTypes.Aspect,LCGlexiconTypes.Eq,[s]]
  | AspectUndef -> []

open LCGlexiconTypes

let transform_entry pos lemma negation pred aspect schema =
(*   print_endline "transform_entry: start";  *)
  match pos with
    "subst" | "depr" | "symbol" | (*"year" | "day" | "day-month" | "date" | "date-interval" |
    "hour-minute" | "hour" | "hour-minute-interval" | "hour-interval" |
    "day-interval" | "day-month-interval" | "month-interval" | "year-interval" | "initial" |*) "fixed" | "unk" ->
    if negation <> NegationUndef || pred <> PredFalse || aspect <> AspectUndef then failwith ("transform_entry 1");
    [[],transform_schema "subst" lemma schema]
  | "adj" | "ordnum" | "adja" | "adjc" |"adjp" ->
    if negation <> NegationUndef || aspect <> AspectUndef then failwith ("transform_entry 2");
    let sel =  match pred with PredTrue -> [Case,Eq,["pred"]] | _ -> [] in
    let roles = get_rev_head_roles schema in
    let sel = if roles = [] then sel else [Role,Eq,roles] @ sel in
    [sel,transform_schema "adj" lemma schema]
  | "adv" | "prep" | "comprep" | "comp" | "compar" | "qub" | "siebie" | "x" ->
    if negation <> NegationUndef || (*pred <> PredFalse ||*) aspect <> AspectUndef then failwith ("transform_entry 3"); (* FIXME: typy przysłówków *)
    [[],transform_schema pos lemma schema]
  | _ ->
  if pred <> PredFalse then failwith ("transform_entry 4") else
  if pos = "num" || pos = "intnum" || pos = "intnum-interval" || pos = "realnum" || pos = "realnum-interval" then (
    if negation <> NegationUndef || aspect <> AspectUndef then failwith ("transform_entry 5");
    Xlist.map ["congr";"rec"] (fun acm ->
        [Acm,Eq,[acm]],transform_num_schema acm schema)) else
  if pos = "interj" then (
    if negation <> NegationUndef || pred <> PredFalse || aspect <> AspectUndef then failwith ("transform_entry 6");
    [[],transform_schema "interj" lemma schema]) else
  if (pos = "conj" || pos = "interp" || pos = "ppron12" || pos = "ppron3" || pos = "numcomp" || pos = "realnum" ||
      pos = "intnum-interval" || pos = "realnum-interval" || pos = "symbol" || pos = "ordnum" || pos = "roman" ||
      pos = "roman-interval" || pos = "roman-ordnum" || pos = "match-result" || pos = "url" || pos = "email" ||
      pos = "phone-number" || pos = "postal-code" || pos = "obj-id" || pos = "building-number" || pos = "list-item" ||
      pos = "aglt" || pos = "pacta" || pos = "part" || pos = "conj" || pos = "sinterj" || pos = "burk" ||
      pos = "interp" || pos = "xxx" || pos = "unk" || pos = "html-tag" || pos = "apron" || pos = "x" ||
      pos = "other") && schema = [] then [[],[]] else
  List.flatten (Xlist.map (expand_negation negation) (fun negation ->
  let sel = [Negation,Eq,[WalStringOf.negation negation]] @ aspect_sel aspect in
  if pos = "fin" || pos = "bedzie" then
        [sel @ [Mood,Eq,["indicative"]],transform_pers_schema lemma negation "indicative" schema;
         sel @ [Mood,Eq,["imperative"]],transform_pers_schema lemma negation "imperative" schema] else
  if pos = "praet" || pos = "winien" then
        [sel @ [Mood,Eq,["indicative"]],transform_pers_schema lemma negation "indicative" schema;
         sel @ [Mood,Eq,["conditional"]],transform_pers_schema lemma negation "conditional" schema] else
  if pos = "impt" then
    [sel @ [Mood,Eq,["imperative"]],transform_nosubj_schema lemma ProNG negation "imperative" schema] else
  if pos = "imps" then
    [sel @ [Mood,Eq,["indicative"]],transform_nosubj_schema lemma Pro negation "indicative" schema] else
  if pos = "pred" then
    [sel @ [Mood,Eq,["indicative"]],transform_pers_schema lemma negation "indicative" schema] else
  if pos = "pcon" || pos = "pant" || pos = "inf" then
    let negation = if negation = Aff && pos = "inf" then NegationUndef else negation in
      (* let role,role_attr = try get_role SUBJ schema with Not_found -> "Initiator","" in *)
    [sel, transform_nosubj_schema lemma Pro negation "no-subj" schema] else
  if pos = "pact" then
      try
      (* let role,role_attr = try get_role SUBJ schema with Not_found -> "Initiator","" in *)
        [[Role,Eq,get_rev_subj_roles schema] @ sel, transform_nosubj_schema lemma Null negation "no-subj" schema] 
      with Not_found -> [] else
  if pos = "ppas" then
      try
        (* let role,role_attr = try get_role OBJ schema with Not_found -> "Theme","" in *)
        [[Role,Eq,get_rev_obj_roles schema] @ sel, transform_ppas_schema lemma negation "indicative" schema]
      with Not_found -> [] else
  if pos = "ger" then
    [sel,transform_ger_schema lemma negation schema] else
  (* if schema = [] then (print_endline "transform_entry: empty"; [[],[]]) else *)
  failwith ("transform_entry: " ^ pos)))

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

(* FIXME: trzeba zanalizować interację tej procedury z Pro w schemacie w wersji z walentym i z semantyką dziedzinową *)
let set_necessary pos schema =
  Xlist.map schema (fun p ->
    let nec =
      if p.gf = ADJUNCT then Opt else
      if Xlist.fold p.morfs false (fun b -> function
          SimpleLexArg _ -> true
        | LexArg _ -> true
        | FixedP _ -> true
        | _ -> b) then Req else
      if p.gf <> SUBJ && p.cr = [] (*&& p.ce = []*) then Opt else
      if p.gf = SUBJ && pos = "impt" then ProNG else
      if p.gf = SUBJ && pos = "pact" then Opt else
      if p.gf = OBJ && pos = "ppas" then Opt else
      if Xlist.fold p.morfs false (fun b -> function
          NP NomAgr -> true
        | NCP(NomAgr,_,_) -> true
        | E (NCP(NomAgr,CompTypeUndef,CompUndef)) -> true
        | E (NP(NomAgr)) -> true
        | Pro -> true
        | ProNG -> true (* czy to jest potrzebne? *)
        | _ -> b) then ProNG else Pro in
    {p with is_necessary=nec})

let rec reduce_comp test_lexemes = function
    Comp s -> if test_lexemes s then Comp s else raise Not_found
  | Zeby -> if test_lexemes "żeby" || test_lexemes "że" then Zeby else raise Not_found
  | Gdy -> if test_lexemes "gdy" || test_lexemes "gdyby" then Gdy else raise Not_found
  | CompUndef -> failwith "reduce_comp"

let reduce_phrase (test_comprep_reqs,test_comprep_reqs2,test_lexarg_reqs,test_lexemes) = function
  | PrepNP(prep,case) as phrase -> if test_lexemes prep then phrase else raise Not_found
  | PrepAdjP(prep,case) as phrase -> if test_lexemes prep then phrase else raise Not_found
  | ComprepNP(prep) as phrase  -> if test_comprep_reqs prep && test_comprep_reqs2 prep then phrase else raise Not_found
  | ComparP(prep,case) as phrase  -> if test_lexemes prep then phrase else raise Not_found
  | CP(ctype,comp) -> CP(ctype,reduce_comp test_lexemes comp)
  | NCP(case,ctype,comp) -> if test_lexemes "to" then NCP(case,ctype,reduce_comp test_lexemes comp) else raise Not_found
  | PrepNCP(prep,case,ctype,comp) -> if test_lexemes prep && test_lexemes "to" then PrepNCP(prep,case,ctype,reduce_comp test_lexemes comp) else raise Not_found
  | SimpleLexArg(lemma,_) as phrase  -> if test_lexemes lemma then phrase else raise Not_found
  | LexArg(id,lemma,_) as phrase  -> if test_lexemes lemma && test_lexarg_reqs id then phrase else raise Not_found
  | FixedP lemma as phrase  -> if test_lexemes lemma then phrase else raise Not_found
  | phrase -> phrase

let rec reduce_morfs tests = function
    [] -> []
  | morf :: l -> (try [reduce_phrase tests morf]
                  with Not_found -> []) @ reduce_morfs tests l

let rec reduce_schema2 tests = function
    [] -> []
  | s :: l ->
    let morfs = reduce_morfs tests s.morfs in
    if morfs = [] then reduce_schema2 tests l else
      {s with morfs=morfs} :: reduce_schema2 tests l

let reduce_entries lexemes entries =
  let lexemes = StringSet.add lexemes "" in
  StringMap.map entries (fun entries ->
      StringSet.fold lexemes StringMap.empty (fun reduced lemma ->
          try StringMap.add reduced lemma (StringMap.find entries lemma)
          with Not_found -> reduced))

let merge_schema phrases schema =
  Xlist.map schema (fun p ->
      let morfs = List.flatten (Xlist.map p.morfs (function
            MorfId id -> (try IntMap.find phrases id with Not_found -> failwith "merge_schema")
          | _ -> failwith "merge_schema")) in
      {p with morfs=morfs})

let merge_entries phrases entries =
  Entries.map entries (fun _ _ (opinion,neg,pred,aspect,schema) ->
      opinion,neg,pred,aspect,merge_schema phrases schema)

let create_tests comprep_reqs comprep_reqs2 lexarg_reqs lexemes =
  let lexemes = StringSet.add (StringSet.add lexemes "_") "" in
  (fun s ->
     if StringMap.mem comprep_reqs s then
       not (StringSet.is_empty (StringSet.intersection (StringMap.find comprep_reqs s) lexemes))
     else true),
  (fun s ->
     if StringMap.mem comprep_reqs2 s then
       not (StringSet.is_empty (StringSet.intersection (StringMap.find comprep_reqs2 s) lexemes))
     else failwith "create_tests"),
  (fun s ->
     if IntMap.mem lexarg_reqs s then
       not (StringSet.is_empty (StringSet.intersection (IntMap.find lexarg_reqs s) lexemes))
     else true),
  StringSet.mem lexemes

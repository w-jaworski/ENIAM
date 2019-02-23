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

let opinion = function
    Pewny -> "cer"
  | Potoczny -> "col"
  | Watpliwy -> "unc"
  | Archaiczny -> "dat"
  | Zly -> "bad"
  | Wulgarny -> "vul"
  | Dziedzinowy -> "dom"
  | Nieokreslony -> "unk"
  | Metaforyczny -> "met"
  | Sporadyczny -> "rar"
  (* | OpinionUndef -> failwith "WalStringOf.opinion" *)

let negation = function
    Negation -> "neg"
  | Aff -> "aff"
  | NegationUndef -> "_"
  (* | NegationNA -> "" *)

let pred = function
    (* PredNA -> "" *)
  | PredTrue -> "pred"
  | PredFalse -> "nopred"
  | PredUndef -> "_"

let aspect = function
    Aspect s -> s
  | AspectUndef -> "_"
  (* | AspectNA -> "" *)

let case = function
    Case s -> s
  | Str -> "str"
  | Part -> "part"
  | CaseAgr -> "agr"
  (* | CaseUAgr -> "uagr"
  | AllUAgr -> "alluagr" *)
  | CaseUndef -> "_"
  | AllAgr -> "allagr"
  | NomAgr -> "nomagr"
  | VocAgr -> "vocagr"
  | GenAgr -> "genagr"

let rec comp = function
    Comp s -> s
  | Zeby -> "żeby2"
  | Gdy -> "gdy"
  | CompUndef -> "_"

let rec comp_type = function
   Int -> "int"
 | Rel -> "rel"
 | CompTypeUndef -> "_"

let number = function
    Number s -> s
  | NumberAgr -> "agr"
  | NumberUndef -> "_"

let gender = function
    Gender s -> s
  | GenderUndef -> "_"
  | GenderAgr -> "agr"
  | Genders l -> String.concat "." l

let grad = function
    Grad s -> s
  | GradAgr -> "agr"
  | GradUndef -> "_"

(* let psem = function
    Psem -> "sem"
  | Pnosem -> "nosem" *)


(* let refl = function
    (* ReflEmpty -> "" *)
  | ReflTrue -> "się"
  | ReflFalse -> "nosię"
  | ReflUndef -> "_" *)

(* let acm = function
    Acm s -> s
  | AcmUndef -> "_" *)

let gf = function
    SUBJ -> "subj"
  | OBJ -> "obj"
  | ARG -> "arg"(*""*)
  | ADJUNCT -> "adjunct"
  | CORE -> "core"
  | NOSEM -> "nosem"

let pos = function
    SUBST(n,c) -> "SUBST(" ^ number n ^ "," ^ case c ^ ")"
  | PPRON12(n,c) -> "PPRON12(" ^ number n ^ "," ^ case c ^ ")"
  | PPRON3(n,c) -> "PPRON3(" ^ number n ^ "," ^ case c ^ ")"
  | SIEBIE(c) -> "SIEBIE(" ^ case c ^ ")"
  | PREP(c) -> "PREP(" ^ case c ^ ")"
  | NUM(c,g) -> "NUM(" ^ case c ^ "," ^ gender g ^ ")"
  | ADJ(n,c,g,gr) -> "ADJ(" ^ number n ^ "," ^ case c ^ "," ^ gender g ^ "," ^ grad gr ^ ")"
  | ADV(gr) -> "ADV(" ^ grad gr ^ ")"
  | GER(n,c,g,a,neg) -> "GER(" ^ number n ^ "," ^ case c ^ "," ^ gender g ^ "," ^ aspect a ^ "," ^ negation neg ^ ")"
  | PACT(n,c,g,a,neg) -> "PACT(" ^ number n ^ "," ^ case c ^ "," ^ gender g ^ "," ^ aspect a ^ "," ^ negation neg ^ ")"
  | PPAS(n,c,g,a,neg) -> "PPAS(" ^ number n ^ "," ^ case c ^ "," ^ gender g ^ "," ^ aspect a ^ "," ^ negation neg ^ ")"
  | INF(a,n) -> "INF(" ^ aspect a ^ "," ^ negation n ^ ")"
  | QUB -> "QUB"
  | COMPAR c -> "COMPAR(" ^ case c ^ ")"
  | COMP(c) -> "COMP(" ^ comp_type c ^ ")"
  | PERS(n) -> "PERS(" ^ negation n ^ ")"
  | FIXED -> "FIXED"

let rec phrase = function
    NP c -> "np(" ^ case c ^ ")"
  | NPA c -> "npa(" ^ case c ^ ")"
  | PrepNP(prep,c) -> "prepnp(" ^ (*psem p ^ "," ^*) prep ^ "," ^ case c ^ ")"
  | PrepFixed(prep) -> "prepfixed(" ^ (*psem p ^ "," ^*) prep ^ ")"
  | AdjP c -> "adjp(" ^ case c ^ ")"
  | AdjA -> "adja"
  | PrepAdjP(prep,c) -> "prepadjp(" ^ prep ^ "," ^ case c ^ ")"
  (* | NumP(c) -> "nump(" ^ case c ^ ")"
  | PrepNumP(prep,c) -> "prepnump(" ^ prep ^ "," ^ case c ^ ")" *)
  | ComprepNP(prep) -> "comprepnp(" ^ prep ^ ")"
  | ComparP(prep,c) -> "comparp(" ^(* psem p ^ "," ^*) prep ^ "," ^ case c ^ ")"
  | CP(ct,co) -> "cp(" ^ comp_type ct ^ "," ^ comp co ^ ")"
  | NCP(c,ct,co) -> "ncp(" ^ case c ^ "," ^ comp_type ct ^ "," ^ comp co ^ ")"
  | PrepNCP(prep,c,ct,co) -> "prepncp(" ^ (*psem p ^ "," ^*) prep ^ "," ^ case c ^ "," ^ comp_type ct ^ "," ^ comp co ^ ")"
  | InfP(a) -> "infp(" ^ aspect a (*^ req r*) ^ ")"
  | PadvP -> "padvp"
  | AdvP(m) -> "advp(" ^ m ^ ")"
  | XP -> "xp"
  | IP -> "ip"
  | ColonP -> "colonp"
  | SymbolP -> "symbolp"
  | FixedP s -> "fixed(" ^ s ^ ")"
  (* | Num(c,a) -> "num(" ^ case c ^ "," ^ acm a ^ ")" *)
  | Or -> "or"
  | Qub -> "qub"
  | AdMod g -> "admod(" ^ grad g ^ ")"
  | Inclusion -> "inclusion"
  | Pro -> "pro"
  | ProNG -> "prong"
  | Null -> "null"
  (* | GerP c -> "gerp(" ^ case c ^ ")"
  | PrepGerP(prep,c) -> "prepgerp(" ^ prep ^ "," ^ case c ^ ")"
  | PpasP c -> "ppasp(" ^ case c ^ ")"
  | PrepPpasP(prep,c) -> "prepppasp(" ^ prep ^ "," ^ case c ^ ")"
  | PactP c -> "pactp(" ^ case c ^ ")" *)
  | E p -> "E(" ^ phrase p ^ ")"
  | MorfId id -> Printf.sprintf "id(%d)" id
  | SimpleLexArg(le,p) -> "lex(" ^ le ^ "," ^ pos p ^ ")"
  | LexArg(id,le,p) -> "lex(" ^ string_of_int id ^ "," ^ le ^ "," ^ pos p ^ ")"
  | LCG s -> LCGstringOf.grammar_symbol 0 s


let restr = function
    Natr -> "natr"
  | Atr -> "atr"
  | Ratr -> "ratr"
  | Ratrs -> "ratrs"
  | Atr1 -> "atr1"
  | Ratr1 -> "ratr1"
  | NoRestr -> ""

let controllers l =
  Xlist.map l (function
      "1" -> "controller"
    | n -> "controller" ^ n)

let controllees l =
  Xlist.map l (function
      "1" -> "controllee"
    | n -> "controllee" ^ n)

let necessary = function
    Opt -> "opt"
  | Req -> "req"
  | Pro -> "pro"
  | ProNG -> "prong"
  | Multi -> "multi"

let dir = function
    Both_ -> ""
  | Forward_ -> "/"
  | Backward_ -> "\\"

let rec sel_prefs = function
    SynsetId i -> string_of_int i
  | Predef s -> s
  | SynsetName s -> s
  | RelationRole(a,b,c) -> Printf.sprintf "(%s,%s,%s)" a b c

let rec schema schema =
  String.concat "+" (Xlist.map schema (fun s ->
    String.concat "," (
      (if s.gf = ARG then [] else [gf s.gf])@s.mode@
      (if s.is_necessary = Opt then [] else [necessary s.is_necessary])@
      (if s.role = "" then [] else [s.role])@
      (if s.role_attr = "" then [] else [s.role_attr])@
      (Xlist.map s.sel_prefs sel_prefs)@
      (controllers s.cr)@(controllees s.ce)) ^ (dir s.dir) ^ "{" ^  String.concat ";" (Xlist.map s.morfs phrase) ^ "}"))
(*
and morf = function
    Phrase p -> phrase p
  | LexPhrase(pos_lex,(r,s)) -> "lex([" ^ String.concat ";" (Xlist.map pos_lex (fun (p,le) -> pos p ^ "," ^ lex le)) ^ "]," ^ restr r ^ "[" ^ schema s ^ "])"
  | PhraseAbbr(p,ml) -> phrase_abbr p ^ "[" ^ String.concat ";" (Xlist.map ml morf) ^ "]"
  | PhraseComp(p,(ct,l)) -> phrase_comp p ^ "," ^ comp_type ct ^ "[" ^ String.concat ";" (Xlist.map l comp) ^ "]"

let simple_morf = function
  | MorfId id -> Printf.sprintf "%d" id
  | _ -> failwith "WalStringOf.simple_morf"

let rec simple_schema schema =
  String.concat "+" (Xlist.map schema (fun s ->
      String.concat "," (
        (if s.gf = ARG then [] else [gf s.gf])@
        s.mode@(controllers s.cr)@(controllees s.ce)) ^
      "{" ^  String.concat ";" (Xlist.map s.morfs simple_morf) ^ "}"))

let sel_prefs = function
    SynsetId id -> Printf.sprintf "synset(%d)" id
  | Predef id -> id
  | RelationArgId _ -> failwith "sel_prefs"
  | RelationRole(rel,role,role_attr) ->
    if role_attr = "" then Printf.sprintf "relation(%s,%s)" rel role
      else Printf.sprintf "relation(%s,%s,%s)" rel role role_attr


let sem_frame (s:position) =
  s.role ^
  (if s.role_attr="" then "" else "," ^ s.role_attr) ^
  (if s.sel_prefs = [] then "" else
     "[" ^ String.concat ";" (Xlist.map s.sel_prefs sel_prefs) ^ "]")

let rec connected_schema schema =
  String.concat "+" (Xlist.map schema (fun s ->
      String.concat "," (
        (if s.gf = ARG then [] else [gf s.gf])@
        s.mode@(controllers s.cr)@(controllees s.ce)) ^
      "{" ^  String.concat ";" (Xlist.map s.morfs simple_morf) ^ "}:" ^ sem_frame s))
*)

let sense m =
  m.name ^ "-" ^ m.variant

let lex_entry = function
    SimpleLexEntry(le,p) ->
    Printf.sprintf "lex(%s,%s)" le p
  | LexEntry(id,le,p,NoRestr,s) ->
    Printf.sprintf "lex(%d,%s,%s)\t%s" id le p (schema s)
  | LexEntry(id,le,p,r,[]) ->
    Printf.sprintf "lex(%d,%s,%s)\t%s" id le p (restr r)
  | ComprepNPEntry(le,NoRestr,s) ->
    Printf.sprintf "comprepnp(%s)\t%s" le (schema s)
  | ComprepNPEntry(le,r,[]) ->
    Printf.sprintf "comprepnp(%s)\t%s" le (restr r)
  | _ -> failwith "WalStringOf.lex_entry"

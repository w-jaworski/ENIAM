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

open LCGtypes
open WalTypes

let arg_of_ctype = function
    Int -> Atom "int"
  | Rel -> Atom "rel"
  (* | Sub -> LCGtypes.Atom "sub"
  | Coord -> LCGtypes.Atom "coord" *)
  | CompTypeUndef -> Top
  (* | CompTypeAgr -> LCGtypes.AVar "ctype" *)

let render_number = function
    Number n -> Atom n
  | NumberUndef -> Top
  | NumberAgr -> Top

let render_negation = function
    Negation -> Atom "neg"
  | Aff -> Atom "aff"
  | NegationUndef -> Top

let render_pos_entry = function
    "subst" ->  [Atom "subst"; AVar "number"; AVar "case"; AVar "gender"; AVar "person"]
  | "ppron12" ->  [Atom "ppron12"; AVar "number"; AVar "case"; AVar "gender"; AVar "person"]
  | "ppron3" ->  [Atom "ppron3"; AVar "number"; AVar "case"; AVar "gender"; AVar "person"]
  | "siebie" ->  [Atom "siebie"; AVar "case"]
  | "num" -> [Atom "num"; AVar "number"; AVar "case"; AVar "gender"; AVar "person"]
  | "intnum" -> [Atom "num"; AVar "number"; AVar "case"; AVar "gender"; AVar "person"]
  | "prep" ->  [Atom "prep"; AVar "case"]
  | "adj" -> [Atom "adj"; AVar "number"; AVar "case"; AVar "gender"; AVar "grad"]
  | "adv" -> [Atom "adv"; AVar "grad"]
  | "ger" ->  [Atom "ger"; AVar "number"; AVar "case"; AVar "gender"; AVar "person"; AVar "negation"]
  | "pact" ->  [Atom "pact"; AVar "number"; AVar "case"; AVar "gender"; AVar "negation"]
  | "ppas" ->  [Atom "ppas"; AVar "number"; AVar "case"; AVar "gender"; AVar "negation"]
  | "inf" ->  [Atom "inf"; AVar "aspect"; AVar "negation"]
  | "qub" ->  [Atom "qub"]
  | "compar" ->  [Atom "comparp"; AVar "case"]
  | "comp" ->  [Atom "comp"; AVar "ctype"]
  | "fin" ->  [Atom "pers"; AVar "negation"]
  | "praet" ->  [Atom "pers"; AVar "negation"]
  | "pred" ->  [Atom "pers"; AVar "negation"]
  | "winien" ->  [Atom "pers"; AVar "negation"]
  | "bedzie" ->  [Atom "pers"; AVar "negation"]
  | s -> failwith ("render_pos_entry: " ^ s)

let render_pos = function (* wprowadzam uzgodnienia a nie wartości cech, bo wartości cech są wprowadzane przez leksem a uzgodnienia wiążą je z wartościami u nadrzędnika *)
  | SUBST(number,Case case) -> [Atom "subst"; render_number number; Atom case; Top; Top]
  | SUBST(_,NomAgr) -> [Atom "subst"; AVar "number"; Atom "nom"; AVar "gender"; AVar "person"]
  | SUBST(_,GenAgr) -> [Atom "subst"; AVar "number"; Atom "gen"; AVar "gender"; AVar "person"]
  | SUBST(_,AllAgr) -> [Atom "subst"; AVar "number"; AVar "case"; AVar "gender"; AVar "person"]
  | SUBST(number,CaseAgr) -> [Atom "subst"; render_number number; AVar "case"; Top; Top]
  | SUBST(_,CaseUndef) -> [Atom "subst"; Top; Top; Top; Top]
  | PPRON12(number,Case case) -> [Atom "ppron12"; render_number number; Atom case; Top; Top]
  | PPRON3(number,Case case) -> [Atom "ppron3"; render_number number; Atom case; Top; Top]
  | SIEBIE(Case case) -> [Atom "siebie"; Atom case]
  | NUM(Case case,_) -> [Atom "num"; Top; Atom case; Top; Top]
  | NUM(NomAgr,_) -> [Atom "num"; AVar "number"; Atom "nom"; AVar "gender"; AVar "person"]
(*  | NUM(CaseAgr,_) -> [Atom "num"; Top; AVar "case"; Top; Top]
    | NUM(CaseUndef,_) -> [Atom "num"; Top; Top; Top; Top]*)
  | PREP(Case case) -> [Atom "prep"; Atom case]
  | ADJ(_,Case case,_,Grad grad) -> [Atom "adj"; Top; Atom case; Top; Atom grad]
(*  | ADJ(_,NomAgr,_,_) -> [Atom "adj"; AVar "number"; Atom "nom"; AVar "gender"]
    | ADJ(_,CaseAgr,_,_) -> [Atom "adj"; Top; AVar "case"; Top]*)
  | ADJ(_,CaseUndef,_,Grad grad) -> [Atom "adj"; Top; Top; Top; Atom grad]
  | ADJ(_,AllAgr,_,Grad grad) -> [Atom "adj"; AVar "number"; AVar "case"; AVar "gender"; Atom grad]
  | ADJ(_,AllAgr,_,GradUndef) -> [Atom "adj"; AVar "number"; AVar "case"; AVar "gender"; Top]
  | ADV (Grad grad) -> [Atom "adv"; Atom grad]
  | ADV GradUndef -> [Atom "adv"; Top]
  | GER(_,Case case,_,_,neg) -> [Atom "ger"; Top; Atom case; Top; Top; render_negation neg]
(*  | GER(_,NomAgr,_,_,_) -> [Atom "ger"; AVar "number"; Atom "nom"; AVar "gender"; AVar "person"]
  | GER(_,CaseAgr,_,_,_) -> [Atom "ger"; Top; AVar "case"; Top; Top]
  | GER(_,CaseUndef,_,_,_) -> [Atom "ger"; Top; Top; Top; Top]
  | PACT(_,Case case,_,_,_) -> [Atom "pact"; Top; Atom case; Top]
    | PACT(_,NomAgr,_,_,_) -> [Atom "pact"; AVar "number"; Atom "nom"; AVar "gender"]*)
  | PACT(_,AllAgr,_,_,neg) -> [Atom "pact"; AVar "number"; AVar "case"; AVar "gender"; render_negation neg]
(*    | PACT(_,CaseAgr,_,_,_) -> [Atom "pact"; Top; AVar "case"; Top]*)
  | PPAS(_,Case case,_,_,neg) -> [Atom "ppas"; Top; Atom case; Top; render_negation neg]
  | PPAS(_,CaseUndef,_,_,neg) -> [Atom "ppas"; Top; Top; Top; render_negation neg]
  (*  | PPAS(_,NomAgr,_,_,_) -> [Atom "ppas"; AVar "number"; Atom "nom"; AVar "gender"]*)
  | PPAS(_,AllAgr,_,_,neg) -> [Atom "ppas"; AVar "number"; AVar "case"; AVar "gender"; render_negation neg]
(*    | PPAS(_,CaseAgr,_,_,_) -> [Atom "ppas"; Top; AVar "case"; Top]*)
  | INF(Aspect aspect,neg) -> [Atom "inf"; Atom aspect; render_negation neg]
  | INF(AspectUndef,neg) -> [Atom "inf"; Top; render_negation neg]
  | QUB -> [Atom "qub"]
  | COMPAR (Case case) -> [Atom "compar"; Atom case]
  | COMP ctype -> [Atom "comp"; arg_of_ctype ctype]
  | PERS neg -> [Atom "pers"; render_negation neg]
  | pos -> failwith ("render_pos: " ^ WalStringOf.pos pos)
(**
let render_phrase = function
      NP(Case case) -> Tensor[Atom "np"; Top; Atom case; Top; Top; Top; Top; Top]
    | NP NomAgr -> Tensor[Atom "np"; AVar "number"; Atom "nom"; AVar "gender"; AVar "person"; Top; Top; Top]
    | NP VocAgr -> Tensor[Atom "np"; AVar "number"; Atom "voc"; AVar "gender"; AVar "person"; Top; Top; Top]
(*    | NP GenAgr -> Tensor[Atom "np"; AVar "number"; Atom "gen"; AVar "gender"; AVar "person"]
      | NP AllAgr -> Tensor[Atom "np"; AVar "number"; AVar "case"; AVar "gender"; AVar "person"]*)
    | NP CaseAgr -> Tensor[Atom "np"; Top; AVar "case"; Top; Top; Top; Top; Top]
    | NP CaseUndef -> Tensor[Atom "np"; Top; Top; Top; Top; Top; Top; Top]
    | NPA CaseAgr -> Tensor[Atom "npa"; Top; AVar "case"; Top; Top; Top; Top; Top]
    | PrepNP("",CaseUndef) -> Tensor[Atom "prepnp"; (*Atom "sem";*) Top; Top; Top; Top; Top]
    | PrepNP("_",CaseUndef) -> Tensor[Atom "prepnp";(* Atom "sem";*) Top; Top; Top; Top; Top]
    | PrepNP("_",Case case) -> Tensor[Atom "prepnp"; (*Atom "sem";*) Top; Atom case; Top; Top; Top]
    | PrepNP(prep,CaseUndef) -> Tensor[Atom "prepnp"; (*Atom "sem";*) Atom prep; Top; Top; Top; Top]
    | PrepNP(prep,Case case) -> Tensor[Atom "prepnp"; (*Atom "sem";*) Atom prep; Atom case; Top; Top; Top]
    | AdjP(Case case) -> Tensor[Atom "adjp"; Top; Atom case; Top; Top; Top; Top; Top]
    | AdjP NomAgr -> Tensor[Atom "adjp"; AVar "number"; Atom "nom"; AVar "gender"; Top; Top; Top; Top]
    | AdjP AllAgr -> Tensor[Atom "adjp"; AVar "number"; AVar "case"; AVar "gender"; Top; Top; Top; Top]
(*    | AdjP CaseAgr -> Tensor[Atom "adjp"; Top; AVar "case"; Top]
      | PrepAdjP("",CaseUndef) -> Tensor[Atom "prepnp"; Top; Top]*)
    | AdjA -> Tensor[Atom "adja"; Top; Top; Top]
    | PrepAdjP(prep,Case case) -> Tensor[Atom "prepadjp"; Atom prep; Atom case; Top; Top; Top]
    (* | NumP(Case case) -> Tensor[Atom "nump"; Top; Atom case; Top; Top]
    | NumP NomAgr -> Tensor[Atom "nump"; AVar "number"; Atom "nom"; AVar "gender"; AVar "person"]
    | NumP CaseAgr -> Tensor[Atom "nump"; Top; AVar "case"; Top; Top]
    | NumP CaseUndef -> Tensor[Atom "nump"; Top; Top; Top; Top]
    | PrepNumP(_,"",CaseUndef) -> Tensor[Atom "prepnp"; Top; Top]
    | PrepNumP(_,prep,Case case) -> Tensor[Atom "prepnump"; Atom prep; Atom case] *)
(*      | ComprepNP("") -> Tensor[Atom "comprepnp"; Top]*)
    | ComprepNP(prep) -> Tensor[Atom "comprepnp"; Atom prep; Top; Top; Top]
    | ComparP((*Psem,*)prep,Case case) -> Tensor[Atom "comparp"; (*Atom "sem";*) Atom prep; Atom case; Top; Top; Top]
    (* | ComparP(Pnosem,prep,Case case) -> Tensor[Atom "comparp"; Atom "nosem"; Atom prep; Atom case] *)
    | ComparP((*Psem,*)prep,CaseUndef) -> Tensor[Atom "comparp"; (*Atom "sem";*) Atom prep; Top; Top; Top; Top]
    (* | ComparP(Pnosem,prep,CaseUndef) -> Tensor[Atom "comparp"; Atom "nosem"; Atom prep; Top] *)
    (* | ComparPP(_,prep) -> Tensor[Atom "comparpp"; Atom prep] *)
    (* | IP -> Tensor[Atom "ip";Top;Top;Top] *)
    | CP (ctype,Comp comp) -> Tensor[Atom "cp"; arg_of_ctype ctype; Atom comp; Top; Top; Top]
    (*    | CP (ctype,CompUndef) -> Tensor[Atom "cp"; arg_of_ctype ctype; Top]*)
    | NCP(Case case,ctype,Comp comp) -> Tensor[Atom "ncp"; Top; Atom case; Top; Top; arg_of_ctype ctype; Atom comp; Top; Top; Top]
    | NCP(Case case,CompTypeUndef,CompUndef) -> Tensor[Atom "ncp"; Top; Atom case; Top; Top; Top; Top; Top; Top; Top]
    | NCP(NomAgr,ctype,Comp comp) -> Tensor[Atom "ncp"; AVar "number"; Atom "nom"; AVar "gender"; AVar "person"; arg_of_ctype ctype; Atom comp; Top; Top; Top]
    | NCP(NomAgr,CompTypeUndef,CompUndef) -> Tensor[Atom "ncp"; AVar "number"; Atom "nom"; AVar "gender"; AVar "person"; Top; Top; Top; Top; Top]
    | NCP(VocAgr,ctype,Comp comp) -> Tensor[Atom "ncp"; AVar "number"; Atom "voc"; AVar "gender"; AVar "person"; arg_of_ctype ctype; Atom comp; Top; Top; Top]
    | NCP(VocAgr,CompTypeUndef,CompUndef) -> Tensor[Atom "ncp"; AVar "number"; Atom "voc"; AVar "gender"; AVar "person"; Top; Top; Top; Top; Top]
    | PrepNCP((*Psem,*)prep,Case case,ctype,Comp comp) -> Tensor[Atom "prepncp"; (*Atom "sem";*) Atom prep; Atom case; arg_of_ctype ctype; Atom comp; Top; Top; Top]
    | PrepNCP((*Psem,*)prep,Case case,CompTypeUndef,CompUndef) -> Tensor[Atom "prepncp"; (*Atom "sem";*) Atom prep; Atom case; Top; Top; Top; Top; Top]
    (* | PrepNCP(Pnosem,prep,Case case,ctype,Comp comp) -> Tensor[Atom "prepncp"; Atom "nosem"; Atom prep; Atom case; arg_of_ctype ctype; Atom comp]
    | PrepNCP(Pnosem,prep,Case case,CompTypeUndef,CompUndef) -> Tensor[Atom "prepncp"; Atom "nosem"; Atom prep; Atom case; Top; Top] *)
    | InfP(Aspect aspect) -> Tensor[Atom "infp"; Atom aspect; Top; Top; Top]
    | InfP AspectUndef -> Tensor[Atom "infp"; Top; Top; Top; Top]
    | PadvP -> Tensor[Atom "padvp"; Top; Top; Top]
    | AdvP "misc" -> Tensor[Atom "advp"; Top; Top; Top; Top] (* FIXME: a może Atom "mod" zamiast Top *)
    | AdvP "" -> Tensor[Atom "advp"; Top; Top; Top; Top] (* FIXME: a może Atom "mod" zamiast Top *)
    | AdvP "pos" -> Tensor[Atom "advp"; Atom "pos"; Top; Top; Top] (* FIXME: a może Atom "mod" zamiast Top *)
    | AdvP "com" -> Tensor[Atom "advp"; Atom "com"; Top; Top; Top] (* FIXME: a może Atom "mod" zamiast Top *)
    | AdvP "sup" -> Tensor[Atom "advp"; Atom "sup"; Top; Top; Top] (* FIXME: a może Atom "mod" zamiast Top *)
    | AdvP mode -> Tensor[Atom "advp"; Atom mode; Top; Top; Top]
    | ColonP -> Tensor[Atom "colonp"; Top; Top; Top]
    | FixedP "" -> Tensor[Atom "fixed"; Top; Top; Top; Top]
    | FixedP lex -> Tensor[Atom "fixed"; Atom lex; Top; Top; Top]
    | XP -> Tensor[Atom "xp"; Top; Top; Top]
    | SymbolP -> Tensor[Atom "symbol"; Top; Top; Top]
    | Inclusion -> Tensor[Atom "inclusion"; Top; Top; Top]
    (* | PrepP -> Tensor[Atom "prepp";Top]
    | Prep("",CaseAgr) -> Tensor[Atom "prep"; Top; AVar "case"]
    | Prep("",CaseUAgr) -> Tensor[Atom "prep"; Top; AVar "ucase"]
    | Num(AllAgr,Acm acm) -> Tensor[Atom "num"; AVar "number"; AVar "case"; AVar "gender"; AVar "person"; Atom acm]
    | Measure(AllUAgr) -> Tensor[Atom "measure"; AVar "unumber"; AVar "ucase"; AVar "ugender"; AVar "uperson"] *)
    | Or -> Tensor[Atom "or"; Top; Top; Top]
    | Qub -> Tensor[Atom "qub"; Top; Top; Top]
    (* | Inclusion -> Tensor[Atom "inclusion"]
    | Adja -> Tensor[Atom "adja"]
    | Aglt -> Tensor[Atom "aglt"; AVar "number"; AVar "person"]
    | AuxPast -> Tensor[Atom "aux-past"; AVar "number"; AVar "gender"; AVar "person"]
    | AuxFut -> Tensor[Atom "aux-fut"; AVar "number"; AVar "gender"; AVar "person"]
    | AuxImp -> Tensor[Atom "aux-imp"]
    | Pro -> One
        | ProNG -> One *)
    | E Or -> Tensor[Atom "or"; Top; Top; Top]
    | E (CP(CompTypeUndef,CompUndef)) -> Tensor[Atom "cp"; Top; Top; Top; Top; Top]
    | E (NCP(NomAgr,CompTypeUndef,CompUndef)) -> Tensor[Atom "ncp"; AVar "number"; Atom "nom"; AVar "gender"; AVar "person"; Top; Top; Top; Top; Top]
    | E (NCP(VocAgr,CompTypeUndef,CompUndef)) -> Tensor[Atom "ncp"; AVar "number"; Atom "voc"; AVar "gender"; AVar "person"; Top; Top; Top; Top; Top]
    | E (NP(NomAgr)) -> Tensor[Atom "np"; AVar "number"; Atom "nom"; AVar "gender"; AVar "person"; Top; Top; Top]
    | E (NP(VocAgr)) -> Tensor[Atom "np"; AVar "number"; Atom "voc"; AVar "gender"; AVar "person"; Top; Top; Top]
    | E (PrepNP((*Psem,*)prep,Case case)) -> Tensor[Atom "prepnp"; (*Atom "sem";*) Atom prep; Atom case; Top; Top; Top]
    (* | E (PrepNP(Pnosem,prep,Case case)) -> Tensor[Atom "prepnp"; Atom "nosem"; Atom prep; Atom case] *)
    | E (NP(Case case)) -> Tensor[Atom "np"; Top; Atom case; Top; Top; Top; Top; Top]
    | E (NCP(Case case,CompTypeUndef,CompUndef)) -> Tensor[Atom "ncp"; Top; Atom case; Top; Top; Top; Top; Top; Top; Top]
    | E (PrepNCP((*Psem,*)prep,Case case,CompTypeUndef,CompUndef)) -> Tensor[Atom "prepncp"; (*Atom "sem";*) Atom prep; Atom case; Top; Top; Top; Top; Top]
    (* | E (PrepNCP(Pnosem,prep,Case case,CompTypeUndef,CompUndef)) -> Tensor[Atom "prepncp"; Atom "nosem"; Atom prep; Atom case; Top; Top] *)
    | phrase -> failwith ("render_phrase: " ^ WalStringOf.phrase phrase)
**)
let render_phrase_cat lemma pos cat role node = function
      NP(Case case) -> Tensor[Atom "np"; Top; Atom case; Top; Top; Atom cat; role; Atom node]
    | NP NomAgr -> Tensor[Atom "np"; AVar "number"; Atom "nom"; AVar "gender"; AVar "person"; Atom cat; role; Atom node]
    | NP VocAgr -> Tensor[Atom "np"; AVar "number"; Atom "voc"; AVar "gender"; AVar "person"; Atom cat; role; Atom node]
(*    | NP GenAgr -> Tensor[Atom "np"; AVar "number"; Atom "gen"; AVar "gender"; AVar "person"; Atom cat; role; Atom node]
      | NP AllAgr -> Tensor[Atom "np"; AVar "number"; AVar "case"; AVar "gender"; AVar "person"; Atom cat; role; Atom node]*)
    | NP CaseAgr -> Tensor[Atom "np"; Top; AVar "case"; Top; Top; Atom cat; role; Atom node]
    | NP CaseUndef -> Tensor[Atom "np"; Top; Top; Top; Top; Atom cat; role; Atom node]
    | NPA CaseAgr -> Tensor[Atom "npa"; Top; AVar "case"; Top; Top; Atom cat; role; Atom node]
    | NP AllAgr -> Tensor[Atom "np"; AVar "number"; AVar "case"; AVar "gender"; AVar "person"; Atom cat; role; Atom node]
    | PrepNP("",CaseUndef) -> Tensor[Atom "prepnp"; (*Atom "sem";*) Top; Top; Atom cat; role; Atom node]
    | PrepNP("_",CaseUndef) -> Tensor[Atom "prepnp";(* Atom "sem";*) Top; Top; Atom cat; role; Atom node]
    | PrepNP("_",Case case) -> Tensor[Atom "prepnp"; (*Atom "sem";*) Top; Atom case; Atom cat; role; Atom node]
    | PrepNP(prep,CaseUndef) -> Tensor[Atom "prepnp"; (*Atom "sem";*) Atom prep; Top; Atom cat; role; Atom node]
    | PrepNP(prep,Case case) -> Tensor[Atom "prepnp"; (*Atom "sem";*) Atom prep; Atom case; Atom cat; role; Atom node]
    | PrepFixed(prep) -> Tensor[Atom "prepfixed"; (*Atom "sem";*) Atom prep; Atom cat; role; Atom node]
    (* | PrepNP(Pnosem,"",CaseUndef) -> Tensor[Atom "prepnp"; Atom "nosem"; Top; Top; Atom cat; role; Atom node]
    | PrepNP(Pnosem,"_",CaseUndef) -> Tensor[Atom "prepnp"; Atom "nosem"; Top; Top; Atom cat; role; Atom node]
    | PrepNP(Pnosem,"_",Case case) -> Tensor[Atom "prepnp"; Atom "nosem"; Top; Atom case; Atom cat; role; Atom node]
    | PrepNP(Pnosem,prep,CaseUndef) -> Tensor[Atom "prepnp"; Atom "nosem"; Atom prep; Top; Atom cat; role; Atom node]
    | PrepNP(Pnosem,prep,Case case) -> Tensor[Atom "prepnp"; Atom "nosem"; Atom prep; Atom case; Atom cat; role; Atom node] *)
    | AdjP(Case "postp") -> 
        if pos <> "x" && pos <> "prep" then failwith ("render_phrase_cat: pos=" ^ pos) else
        (match lemma with
          "z" | "od" | "do" -> Tensor[Atom "adjp"; Atom "sg"; Atom "nom"; Atom "f"; Atom "pos"; Atom cat; role; Atom node]
        | "po" -> Tensor[Atom "adjp"; Top; Atom "postp"; Top; Atom "pos"; Atom cat; role; Atom node]
        | _ -> failwith ("render_phrase_cat: lemma=" ^ lemma))
    | AdjP(Case case) -> Tensor[Atom "adjp"; Top; Atom case; Top; Top; Atom cat; role; Atom node]
    | AdjP NomAgr -> Tensor[Atom "adjp"; AVar "number"; Atom "nom"; AVar "gender"; Top; Atom cat; role; Atom node]
    | AdjP AllAgr -> Tensor[Atom "adjp"; AVar "number"; AVar "case"; AVar "gender"; Top; Atom cat; role; Atom node]
(*    | AdjP CaseAgr -> Tensor[Atom "adjp"; Top; AVar "case"; Top; Top; Atom cat; role; Atom node]
      | PrepAdjP("",CaseUndef) -> Tensor[Atom "prepnp"; Top; Top; Atom cat; role; Atom node]*)
    | AdjA -> Tensor[Atom "adja"; Atom cat; role; Atom node]
    | PrepAdjP(prep,Case case) -> Tensor[Atom "prepadjp"; Atom prep; Atom case; Atom cat; role; Atom node]
    (* | NumP(Case case) -> Tensor[Atom "nump"; Top; Atom case; Top; Atom node]
    | NumP NomAgr -> Tensor[Atom "nump"; AVar "number"; Atom "nom"; AVar "gender"; AVar "person"]
    | NumP CaseAgr -> Tensor[Atom "nump"; Top; AVar "case"; Top; Top]
    | NumP CaseUndef -> Tensor[Atom "nump"; Top; Top; Top; Top]
    | PrepNumP(_,"",CaseUndef) -> Tensor[Atom "prepnp"; Top; Top]
    | PrepNumP(_,prep,Case case) -> Tensor[Atom "prepnump"; Atom prep; Atom case] *)
(*      | ComprepNP("") -> Tensor[Atom "comprepnp"; Top; Atom cat; role; Atom node]*)
    | ComprepNP(prep) -> Tensor[Atom "comprepnp"; Atom prep; Atom cat; role; Atom node]
    | ComparP((*Psem,*)prep,Case case) -> Tensor[Atom "comparp"; (*Atom "sem";*) Atom prep; Atom case; Atom cat; role; Atom node]
    | ComparP((*Psem,*)prep,CaseUndef) -> Tensor[Atom "comparp"; (*Atom "sem";*) Atom prep; Top; Atom cat; role; Atom node]
    (* | ComparP(Pnosem,prep,Case case) -> Tensor[Atom "comparp"; Atom "nosem"; Atom prep; Atom case; Atom cat; role; Atom node] *)
    (* | ComparPP(_,prep) -> Tensor[Atom "comparpp"; Atom prep; Atom cat; role; Atom node] *)
    (* | IP -> Tensor[Atom "ip";Top;Top;Top; Atom cat; role; Atom node] *)
    | NumP AllAgr -> Tensor[Atom "nump"; AVar "number"; AVar "case"; AVar "gender"; Top; Atom cat; role; Atom node]
    | NumP(Case case) -> Tensor[Atom "nump"; Top; Atom case; Top; Top; Atom cat; role; Atom node]
    | CP (ctype,Comp comp) -> Tensor[Atom "cp"; arg_of_ctype ctype; Atom comp; Atom cat; role; Atom node]
    (*    | CP (ctype,CompUndef) -> Tensor[Atom "cp"; arg_of_ctype ctype; Top; Atom cat; role; Atom node]*)
    | NCP(Case case,ctype,Comp comp) -> Tensor[Atom "ncp"; Top; Atom case; Top; Top; arg_of_ctype ctype; Atom comp; Atom cat; role; Atom node]
    | NCP(Case case,CompTypeUndef,CompUndef) -> Tensor[Atom "ncp"; Top; Atom case; Top; Top; Top; Top; Atom cat; role; Atom node]
    | NCP(NomAgr,ctype,Comp comp) -> Tensor[Atom "ncp"; AVar "number"; Atom "nom"; AVar "gender"; AVar "person"; arg_of_ctype ctype; Atom comp; Atom cat; role; Atom node]
    | NCP(NomAgr,CompTypeUndef,CompUndef) -> Tensor[Atom "ncp"; AVar "number"; Atom "nom"; AVar "gender"; AVar "person"; Top; Top; Atom cat; role; Atom node]
    | NCP(VocAgr,ctype,Comp comp) -> Tensor[Atom "ncp"; AVar "number"; Atom "voc"; AVar "gender"; AVar "person"; arg_of_ctype ctype; Atom comp; Atom cat; role; Atom node]
    | NCP(VocAgr,CompTypeUndef,CompUndef) -> Tensor[Atom "ncp"; AVar "number"; Atom "voc"; AVar "gender"; AVar "person"; Top; Top; Atom cat; role; Atom node]
    | PrepNCP((*Psem,*)prep,Case case,ctype,Comp comp) -> Tensor[Atom "prepncp"; (*Atom "sem";*) Atom prep; Atom case; arg_of_ctype ctype; Atom comp; Atom cat; role; Atom node]
    | PrepNCP((*Psem,*)prep,Case case,CompTypeUndef,CompUndef) -> Tensor[Atom "prepncp"; (*Atom "sem";*) Atom prep; Atom case; Top; Top; Atom cat; role; Atom node]
    (* | PrepNCP(Pnosem,prep,Case case,ctype,Comp comp) -> Tensor[Atom "prepncp"; Atom "nosem"; Atom prep; Atom case; arg_of_ctype ctype; Atom comp; Atom cat; role; Atom node]
    | PrepNCP(Pnosem,prep,Case case,CompTypeUndef,CompUndef) -> Tensor[Atom "prepncp"; Atom "nosem"; Atom prep; Atom case; Top; Top; Atom cat; role; Atom node] *)
    | InfP(Aspect aspect) -> Tensor[Atom "infp"; Atom aspect; Atom cat; role; Atom node]
    | InfP AspectUndef -> Tensor[Atom "infp"; Top; Atom cat; role; Atom node]
    | PadvP -> Tensor[Atom "padvp"; Atom cat; role; Atom node]
    | AdvP "misc" -> Tensor[Atom "advp"; Top; Atom cat; role; Atom node] (* FIXME: a może Atom "mod" zamiast Top *)
    | AdvP "" -> Tensor[Atom "advp"; Top; Atom cat; role; Atom node] (* FIXME: a może Atom "mod" zamiast Top *)
    | AdvP "pos" -> Tensor[Atom "advp"; Atom "pos"; Atom cat; role; Atom node] (* FIXME: a może Atom "mod" zamiast Top *)
    | AdvP "com" -> Tensor[Atom "advp"; Atom "com"; Atom cat; role; Atom node] (* FIXME: a może Atom "mod" zamiast Top *)
    | AdvP "sup" -> Tensor[Atom "advp"; Atom "sup"; Atom cat; role; Atom node] (* FIXME: a może Atom "mod" zamiast Top *)
    | AdvP mode -> Tensor[Atom "advp"; Top; Atom cat; role; Atom node]
    | ColonP -> Tensor[Atom "colonp"; Atom cat; role; Atom node]
    | FixedP "" -> Tensor[Atom "fixed"; Top; Atom cat; role; Atom node]
    | FixedP lex -> Tensor[Atom "fixed"; Atom lex; Atom cat; role; Atom node]
    | XP -> Tensor[Atom "xp"; Atom cat; role; Atom node]
    | IP -> Tensor[Atom "ip"; Top; Top; Top; Atom cat; role; Atom node]
    | SymbolP -> Tensor[Atom "symbol"; Atom cat; role; Atom node]
    | Inclusion -> Tensor[Atom "inclusion"; Atom cat; role; Atom node]
    (* | PrepP -> Tensor[Atom "prepp";Top]
    | Prep("",CaseAgr) -> Tensor[Atom "prep"; Top; AVar "case"]
    | Prep("",CaseUAgr) -> Tensor[Atom "prep"; Top; AVar "ucase"]
    | Num(AllAgr,Acm acm) -> Tensor[Atom "num"; AVar "number"; AVar "case"; AVar "gender"; AVar "person"; Atom acm]
    | Measure(AllUAgr) -> Tensor[Atom "measure"; AVar "unumber"; AVar "ucase"; AVar "ugender"; AVar "uperson"] *)
    | RP -> Tensor[Atom "rp"; Atom cat; role; Atom node]
    | Or -> Tensor[Atom "or"; Atom cat; role; Atom node]
    | Qub -> Tensor[Atom "qub"; Atom cat; role; Atom node]
    | AdMod(GradAgr) -> Tensor[Atom "admod"; AVar "grad"; Atom cat; role; Atom node]
    | AdMod(Grad grad) -> Tensor[Atom "admod"; Atom grad; Atom cat; role; Atom node]
    | AdMod(GradUndef) -> Tensor[Atom "admod"; Top; Atom cat; role; Atom node]
    (* | Inclusion -> Tensor[Atom "inclusion"]
    | Adja -> Tensor[Atom "adja"]
    | Aglt -> Tensor[Atom "aglt"; AVar "number"; AVar "person"]
    | AuxPast -> Tensor[Atom "aux-past"; AVar "number"; AVar "gender"; AVar "person"]
    | AuxFut -> Tensor[Atom "aux-fut"; AVar "number"; AVar "gender"; AVar "person"]
    | AuxImp -> Tensor[Atom "aux-imp"]
    | Pro -> One
        | ProNG -> One *)
    | E Or -> Tensor[Atom "or"; Atom cat; role; Atom node]
    | E (CP(CompTypeUndef,CompUndef)) -> Tensor[Atom "cp"; Top; Top; Atom cat; role; Atom node]
    | E (NCP(NomAgr,CompTypeUndef,CompUndef)) -> Tensor[Atom "ncp"; AVar "number"; Atom "nom"; AVar "gender"; AVar "person"; Top; Top; Atom cat; role; Atom node]
    | E (NCP(VocAgr,CompTypeUndef,CompUndef)) -> Tensor[Atom "ncp"; AVar "number"; Atom "voc"; AVar "gender"; AVar "person"; Top; Top; Atom cat; role; Atom node]
    | E (NP(NomAgr)) -> Tensor[Atom "np"; AVar "number"; Atom "nom"; AVar "gender"; AVar "person"; Atom cat; role; Atom node]
    | E (NP(VocAgr)) -> Tensor[Atom "np"; AVar "number"; Atom "Voc"; AVar "gender"; AVar "person"; Atom cat; role; Atom node]
    | E (PrepNP((*Psem,*)prep,Case case)) -> Tensor[Atom "prepnp"; (*Atom "sem";*) Atom prep; Atom case; Atom cat; role; Atom node]
    (* | E (PrepNP(Pnosem,prep,Case case)) -> Tensor[Atom "prepnp"; Atom "nosem"; Atom prep; Atom case; Atom cat; role; Atom node] *)
    | E (NP(Case case)) -> Tensor[Atom "np"; Top; Atom case; Top; Top; Atom cat; role; Atom node]
    | E (NCP(Case case,CompTypeUndef,CompUndef)) -> Tensor[Atom "ncp"; Top; Atom case; Top; Top; Top; Top; Atom cat; role; Atom node]
    | E (PrepNCP((*Psem,*)prep,Case case,CompTypeUndef,CompUndef)) -> Tensor[Atom "prepncp"; (*Atom "sem";*) Atom prep; Atom case; Top; Top; Atom cat; role; Atom node]
    (* | E (PrepNCP(Pnosem,prep,Case case,CompTypeUndef,CompUndef)) -> Tensor[Atom "prepncp"; Atom "nosem"; Atom prep; Atom case; Top; Top; Atom cat; role; Atom node] *)
    | phrase -> failwith ("render_phrase_cat: " ^ WalStringOf.phrase phrase)
(**
let render_morf = function
    | Null -> One
    | Pro -> One (* FIXME *)
    | ProNG -> One (* FIXME *)
    (* | X -> Tensor[Atom "X"]
       | Lex lex -> Tensor[Atom lex] *)
    | LexArg(id,lex,pos) -> Tensor([Atom "lex";Atom (string_of_int id);Atom lex] @ render_pos pos)
    | SimpleLexArg(lex,pos) -> Tensor([Atom "lex";Atom lex] @ render_pos pos)
    | phrase -> render_phrase phrase
**)
let render_morf_cat lemma pos cats role node = function
    | Null -> [One]
(*    | Pro -> [One]
    | ProNG -> [One]*)
    | Pro -> Xlist.map cats (fun cat -> Tensor[Atom "pro"; Atom cat; role; Atom node])
    | ProNG -> Xlist.map cats (fun cat -> Tensor[Atom "pro"; AVar "number"; AVar "gender"; AVar "person"; Atom cat; role; Atom node])
    (* | Or -> [Tensor[Atom "or"]]
    | E Or -> [Tensor[Atom "or"]] *)
    (* | X -> Tensor[Atom "X"]
       | Lex lex -> Tensor[Atom lex] *)
    | LexArg(id,lex,pos) -> [Tensor([Atom "lex";Atom (string_of_int id);Atom lex] @ render_pos pos)]
    | SimpleLexArg(lex,pos) -> [Tensor([Atom "lex";Atom lex] @ render_pos pos @ [role; Atom node])]
    | phrase -> Xlist.map cats (fun cat -> render_phrase_cat lemma pos cat role node phrase)

(* let extract_sel_prefs sel_prefs =
  Xlist.map sel_prefs (function
      SynsetName s -> s
    | _ -> failwith "extract_sel_prefs") *)
(**
let render_schema schema =
  Xlist.map schema (fun p ->
      match Xlist.map p.morfs render_morf with
        [] -> failwith "render_schema"
      | [s] -> Both,s
      | l -> Both,Plus l)
**)
let translate_dir = function
    Both_ -> Both
  | Forward_ -> Forward
  | Backward_ -> Backward

let render_schema_cat lemma pos schema =
  Xlist.map schema (fun p ->
      let role = if p.role = "ADJUNCT" then Top else Atom p.role in
      match List.flatten (Xlist.map p.morfs (render_morf_cat lemma pos p.cat_prefs role p.node)) with
        [] -> failwith "render_schema"
      | [s] -> translate_dir p.dir,if p.is_necessary = Multi then Maybe s else s
      | l -> translate_dir p.dir,if p.is_necessary = Multi then Maybe(Plus l) else Plus l)
(**
let render_simple_schema schema =
  Xlist.map schema (fun morfs ->
      Both,Plus(One :: Xlist.map morfs render_morf))

let render_connected_schema schema =
  Xlist.map schema (fun p ->
      {p with morfs=Xlist.map p.morfs (fun morf -> LCG (render_morf morf))})
**)
let render_connected_schema_cat lemma pos schema =
  Xlist.map schema (fun p ->
      let role = if p.role = "ADJUNCT" then Top else Atom p.role in
      {p with
        morfs=Xlist.map (List.flatten (Xlist.map p.morfs (render_morf_cat lemma pos p.cat_prefs role p.node))) (fun morf -> LCG morf)})
(**
(* FIXME: tu trzeba by dodać zwykłe reguły dla czasowników dotyczące ich negacji, aglutynatu itp. *)
let render_lex_entry = function
    SimpleLexEntry(lemma,pos) -> Tensor([Atom "lex";Atom lemma] @ render_pos_entry pos)
  | LexEntry(id,lemma,pos,NoRestr,schema) ->
    ImpSet(Tensor([Atom "lex";Atom (string_of_int id);Atom lemma] @ render_pos_entry pos),render_schema schema)
    (*Xlist.map (transform_entry pos lemma NegationUndef PredFalse AspectUndef schema) (fun (sel,schema) ->
        sel,LexEntry(id,lemma,pos,NoRestr,schema))*)
  | ComprepNPEntry(prep,NoRestr,schema) -> ImpSet(Tensor[Atom "comprepnp"; Atom prep],render_schema schema)
    (*Xlist.map (transform_entry "comprep" s NegationUndef PredFalse AspectUndef schema) (fun (sel,schema) ->
        sel,ComprepNPEntry(s,NoRestr,schema))*)
  | LexEntry(id,lemma,pos,_,[]) (*as entry*) ->
    ImpSet(Tensor([Atom "lex";Atom (string_of_int id);Atom lemma] @ render_pos_entry pos),[Both,Tensor[AVar "schema"]])
  | entry -> failwith ("render_entry:" ^ WalStringOf.lex_entry entry)
**)
(* let schemata,entries = Valence.prepare_all_valence WalParser.phrases WalParser.schemata WalParser.entries *)

(* let _ =
  (* Entries.map schemata (fun pos lemma (selectors,schema) ->
      (* Printf.printf "%s %s %s\n" pos lemma (WalStringOf.schema schema); *)
      render_schema schema) *)
    Entries.map entries (fun pos lemma (selectors,entry) ->
        (* Printf.printf "%s %s %s\n" pos lemma (WalStringOf.schema schema); *)
        selectors,render_lex_entry entry) *)
(**
let adjunct morfs = {empty_position with gf=ADJUNCT; is_necessary=Opt; morfs=Xlist.map morfs (fun morf -> LCG morf)}
let adjunct_multi dir  morfs = {empty_position with gf=ADJUNCT; is_necessary=Multi; dir=dir; morfs=Xlist.map morfs (fun morf -> LCG morf)}
let adjunct_dir dir morfs = {empty_position with gf=ADJUNCT; is_necessary=Opt; dir=dir; morfs=Xlist.map morfs (fun morf -> LCG morf)}
let adjunct_ce ce morfs = {empty_position with gf=ADJUNCT; ce=[ce]; is_necessary=Opt; morfs=Xlist.map morfs (fun morf -> LCG morf)}

let render_comprep prep = Both,Plus[One;Tensor[Atom "comprepnp"; Atom prep; Top; Top; Top]]

let render_connected_comprep prep = adjunct [Tensor[Atom "comprepnp"; Atom prep; Top; Top; Top]]

let render_prepnp prep cases =
  Both,Plus(One :: List.flatten (Xlist.map cases (fun case ->
      [Tensor[Atom "prepnp"; Atom prep; Atom case; Top; Top; Top];
       Tensor[Atom "prepncp"; Atom prep; Atom case; Top; Top; Top; Top; Top]])))

let render_connected_prepnp prep cases =
  adjunct (List.flatten (Xlist.map cases (fun case ->
      [Tensor[Atom "prepnp"; Atom prep; Atom case; Top; Top; Top];
       Tensor[Atom "prepncp"; Atom prep; Atom case; Top; Top; Top; Top; Top]])))

let render_prepadjp prep cases =
  let postp = if prep = "z" || prep = "po" || prep = "na" then [Tensor[Atom "prepadjp"; Atom prep; Atom "postp"; Top; Top; Top]] else [] in
  Both,Plus(One :: postp @ (Xlist.map cases (fun case ->
      Tensor[Atom "prepadjp"; Atom prep; Atom case; Top; Top; Top])))

let render_connected_prepadjp prep cases =
  let postp = if prep = "z" || prep = "po" || prep = "na" then [Tensor[Atom "prepadjp"; Atom prep; Atom "postp"; Top; Top; Top]] else [] in
  adjunct (postp @ (Xlist.map cases (fun case ->
      Tensor[Atom "prepadjp"; Atom prep; Atom case; Top; Top; Top])))

let render_compar prep = Both,Plus[One;Tensor[Atom "comparp"; Atom prep; Top; Top; Top; Top]]

let render_connected_compar prep = adjunct [Tensor[Atom "comparp"; Atom prep; Top; Top; Top; Top]]

let verb_adjuncts_simp = [
  Both, Plus[One;Tensor[Atom "advp"; Atom "pron"; Top; Top; Top]];
  Both, Plus[One;Tensor[Atom "advp"; Atom "locat"; Top; Top; Top]];
  Both, Plus[One;Tensor[Atom "advp"; Atom "abl"; Top; Top; Top]];
  Both, Plus[One;Tensor[Atom "advp"; Atom "adl"; Top; Top; Top]];
  Both, Plus[One;Tensor[Atom "advp"; Atom "perl"; Top; Top; Top]];
  Both, Plus[One;Tensor[Atom "advp"; Atom "temp"; Top; Top; Top]];
  Both, Plus[One;Tensor[Atom "advp"; Atom "dur"; Top; Top; Top]];
  Both, Plus[One;Tensor[Atom "advp"; Atom "mod"; Top; Top; Top]];
  Both, Plus[One;Tensor[Atom "np";Top;Atom "dat"; Top; Top; Top; Top; Top];Tensor[Atom "ncp"; Top; Atom "dat"; Top; Top; Top; Top; Top; Top; Top]];
  Both, Plus[One;Tensor[Atom "np";Top;Atom "inst"; Top; Top; Top; Top; Top];Tensor[Atom "ncp"; Top; Atom "inst"; Top; Top; Top; Top; Top; Top; Top]];
  Both, Plus[One;Tensor[Atom "date"];Tensor[Atom "day-lex"];Tensor[Atom "day-month"];Tensor[Atom "day"]];
  Forward, Plus[One;Tensor[Atom "cp";Top; Top; Top; Top; Top]]; (* FIXME: to powinno być jako ostatnia lista argumentów *)
  Both, Plus[One;Tensor[Atom "or"; Top; Top; Top]];
  Both, Plus[One;Tensor[Atom "lex";Atom "się";Atom "qub"]];
  Both, Plus[One;Tensor[Atom "padvp"; Top; Top; Top]];
]

let verb_connected_adjuncts_simp = [
  adjunct [Tensor[Atom "advp"; Atom "pron"; Top; Top; Top]];
  adjunct [Tensor[Atom "advp"; Atom "locat"; Top; Top; Top]];
  adjunct [Tensor[Atom "advp"; Atom "abl"; Top; Top; Top]];
  adjunct [Tensor[Atom "advp"; Atom "adl"; Top; Top; Top]];
  adjunct [Tensor[Atom "advp"; Atom "perl"; Top; Top; Top]];
  adjunct [Tensor[Atom "advp"; Atom "temp"; Top; Top; Top]];
  adjunct [Tensor[Atom "advp"; Atom "dur"; Top; Top; Top]];
  adjunct [Tensor[Atom "advp"; Atom "mod"; Top; Top; Top]];
  adjunct [Tensor[Atom "np";Top;Atom "dat"; Top; Top];Tensor[Atom "ncp"; Top; Atom "dat"; Top; Top; Top; Top; Top; Top; Top]];
  adjunct [Tensor[Atom "np";Top;Atom "inst"; Top; Top];Tensor[Atom "ncp"; Top; Atom "inst"; Top; Top; Top; Top; Top; Top; Top]];
  adjunct [Tensor[Atom "date"];Tensor[Atom "day-lex"];Tensor[Atom "day-month"];Tensor[Atom "day"]];
  adjunct_dir Forward_ [Tensor[Atom "cp";Top; Top; Top; Top; Top]];
  adjunct [Tensor[Atom "or"; Top; Top; Top]];
  adjunct [Tensor[Atom "lex";Atom "się";Atom "qub"]];
  adjunct_ce "3" [Tensor[Atom "padvp"; Top; Top; Top]];
]

let proper_noun_adjuncts_simp = [
  Both, Plus[One;Tensor[Atom "np";Top;Atom "gen"; Top; Top; Top; Top; Top];Tensor[Atom "ncp"; Top; Atom "gen"; Top; Top; Top; Top; Top; Top; Top]];
  Forward, Plus[One;Tensor[Atom "np";Top;Atom "nom"; Top; Top; Top; Top; Top];Tensor[Atom "np";Top;AVar "case"; Top; Top; Top; Top; Top]];
  Backward, Maybe(Tensor[Atom "adjp"; AVar "number"; AVar "case"; AVar "gender"; Top; Top; Top]);
  Forward, Plus[One;Tensor[Atom "adjp"; AVar "number"; AVar "case"; AVar "gender"; Top; Top; Top]];
]

let proper_noun_connected_adjuncts_simp = [
  adjunct [Tensor[Atom "np";Top;Atom "gen"; Top; Top; Top; Top; Top];Tensor[Atom "ncp"; Top; Atom "gen"; Top; Top; Top; Top; Top; Top; Top]];
  adjunct_dir Forward_ [Tensor[Atom "np";Top;Atom "nom"; Top; Top; Top; Top; Top];Tensor[Atom "np";Top;AVar "case"; Top; Top; Top; Top; Top]];
  adjunct_multi Backward_ [Tensor[Atom "adjp"; AVar "number"; AVar "case"; AVar "gender"; Top; Top; Top]];
  adjunct_dir Forward_ [Tensor[Atom "adjp"; AVar "number"; AVar "case"; AVar "gender"; Top; Top; Top]];
]

let common_noun_adjuncts_simp = [
  Both, Plus[One;Tensor[Atom "np";Top;Atom "gen"; Top; Top; Top; Top; Top];Tensor[Atom "ncp"; Top; Atom "gen"; Top; Top; Top; Top; Top; Top; Top]];
  Forward, Plus[One;Tensor[Atom "np";Top;Atom "nom"; Top; Top; Top; Top; Top];Tensor[Atom "np";Top;AVar "case"; Top; Top; Top; Top; Top]];
  Backward, Maybe(Tensor[Atom "adjp"; AVar "number"; AVar "case"; AVar "gender"; Top; Top; Top]);
  Forward, Plus[One;Tensor[Atom "adjp"; AVar "number"; AVar "case"; AVar "gender"; Top; Top; Top]];
]

let common_noun_connected_adjuncts_simp = [
  adjunct [Tensor[Atom "np";Top;Atom "gen"; Top; Top; Top; Top; Top];Tensor[Atom "ncp"; Top; Atom "gen"; Top; Top; Top; Top; Top; Top; Top]];
  adjunct_dir Forward_ [Tensor[Atom "np";Top;Atom "nom"; Top; Top; Top; Top; Top];Tensor[Atom "np";Top;AVar "case"; Top; Top; Top; Top; Top]];
  adjunct_multi Backward_ [Tensor[Atom "adjp"; AVar "number"; AVar "case"; AVar "gender"; Top; Top; Top]];
  adjunct_dir Forward_ [Tensor[Atom "adjp"; AVar "number"; AVar "case"; AVar "gender"; Top; Top; Top]];
]

let measure_noun_adjuncts_simp = [
  Backward, Maybe(Tensor[Atom "adjp"; AVar "number"; AVar "case"; AVar "gender"; Top; Top; Top]);
  Forward, Plus[One;Tensor[Atom "adjp"; AVar "number"; AVar "case"; AVar "gender"; Top; Top; Top]];
]

let measure_noun_connected_adjuncts_simp = [
  adjunct_multi Backward_ [Tensor[Atom "adjp"; AVar "number"; AVar "case"; AVar "gender"; Top; Top; Top]];
  adjunct_dir Forward_ [Tensor[Atom "adjp"; AVar "number"; AVar "case"; AVar "gender"; Top; Top; Top]];
]

let adj_adjuncts_simp = [
  Both, Plus[One;Tensor[Atom "advp"; Top; Top; Top; Top]];
]

let adj_connected_adjuncts_simp = [
  adjunct [Tensor[Atom "advp"; Top; Top; Top; Top]];
]

let adv_adjuncts_simp = [
   Both, Plus[One;Tensor[Atom "advp"; Top; Top; Top; Top]];
 ]

let adv_connected_adjuncts_simp = [
   adjunct [Tensor[Atom "advp"; Top; Top; Top; Top]];
 ]

let assing_prep_morfs = function
    "po","postp" -> [
        LCG(Tensor[Atom "adjp"; Atom "sg"; Atom "dat"; Atom "m1"]);
        LCG(Tensor[Atom "adjp"; Top; Atom "postp"; Top])]
  | "z","postp" -> [LCG(Tensor[Atom "adjp"; Atom "sg"; Atom "nom"; Atom "f"])]
  | "na","postp" -> [LCG(Tensor[Atom "advp"; Top])]
  | _,case -> [
        LCG(Tensor[Atom "np"; Top; Atom case; Top; Top]);
        LCG(Tensor[Atom "adjp"; Top; Atom case; Top])]

let prep_morfs = [
  LCG(Tensor[Atom "np"; Top; Atom "case"; Top; Top]);
  LCG(Tensor[Atom "adjp"; Top; Atom "case"; Top]);
  LCG(Tensor[Atom "adjp"; Atom "sg"; Atom "dat"; Atom "m1"]);
  LCG(Tensor[Atom "adjp"; Atom "sg"; Atom "nom"; Atom "f"]);
  LCG(Tensor[Atom "advp"; Top]);
  LCG(Tensor[Atom "year"]);
  LCG(Tensor[Atom "hour-minute"]);
  LCG(Tensor[Atom "day-month"]);
  LCG(Tensor[Atom "hour"]);
  LCG(Tensor[Atom "day"]);
  LCG(Tensor[Atom "date"]);
  ]

let compar_morfs = [
  LCG(Tensor[Atom "np"; Top; Atom "case"; Top; Top]);
  LCG(Tensor[Atom "adjp"; Top; Atom "case"; Top]);
  LCG(Tensor[Atom "prepnp"; Top; Top]);
  LCG(Tensor[Atom "prepadjp"; Top; Top]);
  ]
**)

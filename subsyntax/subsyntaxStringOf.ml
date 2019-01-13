(*
 *  ENIAMsubsyntax: tokenization, lemmatization, MWE and sentence detecion for Polish
 *  Copyright (C) 2016-2018 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
 *  Copyright (C) 2016-2018 Institute of Computer Science Polish Academy of Sciences
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

open SubsyntaxTypes
open Printf

let rec string_of_token = function
    SmallLetter(uc,lc) -> sprintf "SmallLetter(%s,%s)" uc lc
  | CapLetter(uc,lc) -> sprintf "CapLetter(%s,%s)" uc lc
  | AllSmall(uc,fc,lc) -> sprintf "AllSmall(%s,%s,%s)" uc fc lc
  | AllCap(uc,fc,lc) -> sprintf "AllCap(%s,%s,%s)" uc fc lc
  | FirstCap(uc,fc,lc) -> sprintf "FirstCap(%s,%s,%s)" uc fc lc
  | SomeCap(uc,orth,lc) -> sprintf "SomeCap(%s,%s,%s)" uc orth lc
  | Ideogram(v,t) -> sprintf "Ideogram(%s,%s)" v t
  | Interp orth -> sprintf "Interp(%s)" orth
  | Symbol orth  -> sprintf "Symbol(%s)" orth
  | Other orth  -> sprintf "Other(%s)" orth
  | Lemma(lemma,pos,interps,cat) -> sprintf "Lemma(%s,%s,%s,%s)" lemma pos (Tagset.render interps) cat
  | Tokens(cat,l) -> sprintf "Tokens(%s,%s)" cat (String.concat ";" (Xlist.map l string_of_int))

let rec string_of_token_simple = function
    SmallLetter _ -> "SmallLetter"
  | CapLetter _ -> "CapLetter"
  | AllSmall _ -> "AllSmall"
  | AllCap _ -> "AllCap"
  | FirstCap _ -> "FirstCap"
  | SomeCap _ -> "SomeCap"
  | Ideogram(v,t) -> "Ideogram"
  | Interp orth -> sprintf "Interp(%s)" orth
  | Symbol orth  -> sprintf "Symbol(%s)" orth
  | Other orth  -> sprintf "Other(%s)" orth
  | Lemma(lemma,pos,interp,cat) -> "Lemma"
  | Tokens _ -> sprintf "Tokens"

let string_of_attr = function
    FC -> "first capital"
  | CS -> "cs"
  | MaybeCS -> "maybe cs"
  | HasAglSuffix -> "has aglutinate suffix"
  | AglSuffix -> "aglutinate suffix"
  | MWE -> "mwe"
  | LemmNotVal -> "lemma not validated"
  | TokNotFound -> "token not found"
  | NotValProper -> "notvalidated proper"
  | LemmLowercase -> "lemmatized as lowercase"
  | Roman -> "roman"
  | SentBeg -> "NKJP sentence begin"
  | SentEnd -> "NKJP sentence end"
  | SentBegEnd -> "NKJP sentence begin-end"
  | BrevLemma s -> "NKJP brev lemma: " ^ s
  | Disamb(lemma,cat,interp) -> "NKJP disamb: " ^ lemma ^ ":" ^ cat ^ ":" ^ String.concat ":" (Xlist.map interp (String.concat "."))
  | Capitalics -> "capitalics"

let rec string_of_token_env t =
  sprintf "{orth=%s;beg=%d;len=%d;next=%d;token=%s;args=[%s];weight=%.2f;lemma_frequency=%.2f;morf_frequency=%.2f;attrs=[%s];tagger_output=[%s]}" t.orth t.beg t.len t.next (string_of_token t.token) 
    (String.concat ";" (Xlist.map t.args string_of_token_env))
    t.weight t.lemma_frequency t.morf_frequency
    (String.concat ";" (Xlist.map t.attrs string_of_attr))
    (String.concat ";" (Xlist.map t.tagger_output (fun (interp,prob,eos,disamb) ->
      Printf.sprintf "%s[%.4f%s%s]" interp prob (if eos then ",eos" else "") (if disamb then ",disamb" else ""))))

let rec spaces i =
  if i = 0 then "" else "  " ^ spaces (i-1)

let rec string_of_tokens i = function
    Token t -> sprintf "%s%s" (spaces i) (string_of_token_env t)
  | Variant l -> sprintf "%sVariant[\n%s]" (spaces i) (String.concat ";\n" (Xlist.map l (string_of_tokens (i+1))))
  | Seq l -> sprintf "%sSeq[\n%s]" (spaces i) (String.concat ";\n" (Xlist.map l (string_of_tokens (i+1))))

let rec string_of_tokens_simple = function
    Token t -> string_of_token_simple t.token
  | Variant l -> sprintf "Variant[%s]" (String.concat ";" (Xlist.map l string_of_tokens_simple))
  | Seq l -> sprintf "Seq[%s]" (String.concat ";" (Xlist.map l string_of_tokens_simple))

let mode = function
    Raw -> "Raw"
  | Struct -> "Struct"
  | CONLL -> "CONLL"
  | ENIAM -> "ENIAM"
  | Mate -> "Mate"
  | Swigra -> "Swigra"
  | POLFIE -> "POLFIE"
  | Error -> "Error"
  | Name -> "Name"
  | Identifier -> "Id"

let token_extarray t =
  String.concat "\n" (List.rev (Int.fold 0 (ExtArray.size t - 1) [] (fun l id ->
    let t2 = ExtArray.get t id in
    (Printf.sprintf "%3d %s" id (string_of_token_env t2)) :: l)))

let token_list paths (*last*) =
  String.concat "\n" (Xlist.map paths (fun t -> string_of_token_env t))
  (* ^ (if last < 0 then "" else Printf.sprintf "\nlast=%d" last) *)

let string_of_token_env_conll n t =
  Printf.sprintf "%d\t%s\t%s\t%s\t%s" n t.SubsyntaxTypes.orth
    (Tokenizer.get_lemma t.SubsyntaxTypes.token)
    (Tokenizer.get_pos t.SubsyntaxTypes.token)
    (Tokenizer.get_interp t.SubsyntaxTypes.token)

let token_list_conll paths (*last*) =
  let l,_,_ = Xlist.fold paths ([],0,0) (fun (paths,beg,n) t ->
    if t.SubsyntaxTypes.beg = beg then (string_of_token_env_conll n t) :: paths, t.SubsyntaxTypes.beg, n
    else (string_of_token_env_conll (n+1) t) :: paths, t.SubsyntaxTypes.beg, n+1) in
  String.concat "\n" (List.rev l)
  (* ^ (if last < 0 then "" else Printf.sprintf "\nlast=%d" last) *)

let struct_sentence spaces t paths last =
  spaces ^ " id lnode rnode cat orth token\n" ^
  String.concat "\n" (Xlist.map (List.sort compare paths) (fun (id,lnode,rnode) ->
    let t2 = ExtArray.get t id in
    sprintf "%s%3d %5d %5d %s %s %s" spaces
      id lnode rnode (Tokenizer.get_cat t2.SubsyntaxTypes.token) t2.SubsyntaxTypes.orth (string_of_token t2.SubsyntaxTypes.token))) ^
  sprintf "\n%s last=%d" spaces last

let dep_sentence spaces t paths =
  spaces ^ " id conll_id super label orth token \n" ^
  String.concat "\n" (List.rev (Int.fold 0 (Array.length paths - 1) [] (fun l conll_id ->
    let id,sl,sem = paths.(conll_id) in
    let sl = String.concat "|" (Xlist.map sl (fun (super,label) -> string_of_int super ^ ":" ^ label)) in
    let t2 = ExtArray.get t id in
    (sprintf "%s%3d %8d %s %s %s %s" spaces
      id conll_id sl sem t2.SubsyntaxTypes.orth (string_of_token t2.SubsyntaxTypes.token)) :: l)))

let rec sentence spaces t = function
    RawSentence s -> spaces ^ "RawSentence: " ^ s
  | StructSentence(paths,last) -> spaces ^ "StructSentence:\n" ^ struct_sentence "        " t paths last
  | DepSentence paths -> spaces ^ "DepSentence:\n" ^ String.concat "\n" (Xlist.map paths (dep_sentence "        " t))
  | QuotedSentences sentences ->
      spaces ^ "QuotedSentences:\n" ^ String.concat "\n" (Xlist.map sentences (fun p ->
        sprintf "      id=%s beg=%d len=%d next=%d\n%s" p.sid p.sbeg p.slen p.snext (sentence "      " t p.sentence)))
  | AltSentence l ->
     String.concat "\n" (Xlist.map l (fun (m,s) ->
       sprintf "%sAltSentence mode=%s %s" spaces (mode m) (sentence "" t s)))
  | ErrorSentence s -> spaces ^ "ErrorSentence: " ^ s

let rec paragraph spaces t = function
    RawParagraph s -> spaces ^ "RawParagraph: " ^ s
  | StructParagraph sentences ->
      spaces ^ "StructParagraph:\n" ^ String.concat "\n" (Xlist.map sentences (fun p ->
        sprintf "    id=%s beg=%d len=%d next=%d\n%s" p.sid p.sbeg p.slen p.snext (sentence "    " t p.sentence)))
  | AltParagraph l ->
     String.concat "\n" (Xlist.map l (fun (m,p) ->
       sprintf "%sAltParagraph mode=%s %s" spaces (mode m) (paragraph "" t p)))
  | ErrorParagraph s -> spaces ^ "ErrorParagraph: " ^ s

let rec text spaces t = function
    RawText s -> spaces ^ "RawText: " ^ s
  | StructText paragraphs ->
      spaces ^ "StructText:\n" ^ String.concat "\n" (Xlist.map paragraphs (paragraph "  " t))
  | AltText l ->
     String.concat "\n" (Xlist.map l (fun (m,te) ->
       sprintf "%sAltText mode=%s %s" spaces (mode m) (text "" t te)))
  | ErrorText s -> spaces ^ "ErrorText: " ^ s

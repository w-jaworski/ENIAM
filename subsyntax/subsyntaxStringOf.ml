(*
 *  ENIAMsubsyntax: MWE, abbreviation and sentence detecion for Polish
 *  Copyright (C) 2016 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
 *  Copyright (C) 2016 Institute of Computer Science Polish Academy of Sciences
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
    (Printf.sprintf "%3d %s" id (Tokens.string_of_token_env t2)) :: l)))

let token_list paths (*last*) =
  String.concat "\n" (Xlist.map paths (fun t -> Tokens.string_of_token_env t))
  (* ^ (if last < 0 then "" else Printf.sprintf "\nlast=%d" last) *)

let string_of_token_env_conll n t =
  Printf.sprintf "%d\t%s\t%s\t%s\t%s" n t.TokenizerTypes.orth
    (Tokens.get_lemma t.TokenizerTypes.token)
    (Tokens.get_pos t.TokenizerTypes.token)
    (Tokens.get_interp t.TokenizerTypes.token)

let token_list_conll paths (*last*) =
  let l,_,_ = Xlist.fold paths ([],0,0) (fun (paths,beg,n) t ->
    if t.TokenizerTypes.beg = beg then (string_of_token_env_conll n t) :: paths, t.TokenizerTypes.beg, n
    else (string_of_token_env_conll (n+1) t) :: paths, t.TokenizerTypes.beg, n+1) in
  String.concat "\n" (List.rev l)
  (* ^ (if last < 0 then "" else Printf.sprintf "\nlast=%d" last) *)

let struct_sentence spaces t paths last =
  spaces ^ " id lnode rnode cat orth token\n" ^
  String.concat "\n" (Xlist.map (List.sort compare paths) (fun (id,lnode,rnode) ->
    let t2 = ExtArray.get t id in
    sprintf "%s%3d %5d %5d %s %s %s" spaces
      id lnode rnode (Tokens.get_cat t2.TokenizerTypes.token) t2.TokenizerTypes.orth (Tokens.string_of_token t2.TokenizerTypes.token))) ^
  sprintf "\n%s last=%d" spaces last

let dep_sentence spaces t paths =
  spaces ^ " id conll_id super label orth token \n" ^
  String.concat "\n" (List.rev (Int.fold 0 (Array.length paths - 1) [] (fun l conll_id ->
    let id,sl,sem = paths.(conll_id) in
    let sl = String.concat "|" (Xlist.map sl (fun (super,label) -> string_of_int super ^ ":" ^ label)) in
    let t2 = ExtArray.get t id in
    (sprintf "%s%3d %8d %s %s %s %s" spaces
      id conll_id sl sem t2.TokenizerTypes.orth (Tokens.string_of_token t2.TokenizerTypes.token)) :: l)))

let rec sentence spaces t = function
    RawSentence s -> spaces ^ "RawSentence: " ^ s
  | StructSentence(paths,last) -> spaces ^ "StructSentence:\n" ^ struct_sentence "        " t paths last
  | DepSentence paths -> spaces ^ "DepSentence:\n" ^ String.concat "\n" (Xlist.map paths (dep_sentence "        " t))
  | QuotedSentences sentences ->
      spaces ^ "QuotedSentences:\n" ^ String.concat "\n" (Xlist.map sentences (fun p ->
        sprintf "      id=%s beg=%d len=%d next=%d\n%s" p.id p.beg p.len p.next (sentence "      " t p.sentence)))
  | AltSentence l ->
     String.concat "\n" (Xlist.map l (fun (m,s) ->
       sprintf "%sAltSentence mode=%s %s" spaces (mode m) (sentence "" t s)))
  | ErrorSentence s -> spaces ^ "ErrorSentence: " ^ s

let rec paragraph spaces t = function
    RawParagraph s -> spaces ^ "RawParagraph: " ^ s
  | StructParagraph sentences ->
      spaces ^ "StructParagraph:\n" ^ String.concat "\n" (Xlist.map sentences (fun p ->
        sprintf "    id=%s beg=%d len=%d next=%d\n%s" p.id p.beg p.len p.next (sentence "    " t p.sentence)))
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

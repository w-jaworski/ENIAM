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

let html_header =
"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">
<html>
  <head>
	<META HTTP-EQUIV=\"CONTENT-TYPE\" CONTENT=\"text/html; charset=utf8\">
	<TITLE>ENIAM: Kategorialny Parser Składniowo-Semantyczny</TITLE>
	<META HTTP-EQUIV=\"Content-Language\" CONTENT=\"pl\">
  </head>

  <body>
 <center>"

let html_trailer =
"</center>
  </body>
</html>"

let escape_html s =
  Int.fold 0 (String.length s - 1) "" (fun t i ->
    match String.sub s i 1 with
       "<" -> t ^ "&lt;"
     | ">" -> t ^ "&gt;"
     | "&" -> t ^ "&amp;"
     | c -> t ^ c)

let html_of_struct_sentence tokens paths last =
  "<table><tr><td><b>orth</b></td><td><b>token</b></td><td><b>id</b></td><td><b>lnode</b></td><td><b>rnode</b></td></tr>" ^
  String.concat "\n" (Xlist.map (List.sort compare paths) (fun (id,lnode,rnode) ->
    let t = ExtArray.get tokens id in
    sprintf "<tr><td>%s</td><td>%s</td><td>%d</td><td>%d</td><td>%d</td></tr>"
      t.TokenizerTypes.orth (escape_html (Tokens.string_of_token t.TokenizerTypes.token)) id lnode rnode)) ^
  sprintf "<tr><td></td><td></td><td></td><td>%d</td><td></td></tr>" last ^
  "</table>"

let html_of_dep_sentence tokens paths =
  "<table><tr><td><b>orth</b></td><td><b>token</b></td><td><b>id</b></td><td><b>conll_id</b></td><td><b>super</b></td><td><b>label</b></td></tr>" ^
  String.concat "\n" (List.rev (Int.fold 0 (Array.length paths - 1) [] (fun l conll_id ->
    let id,sl,sem = paths.(conll_id) in
    let sl = String.concat "|" (Xlist.map sl (fun (super,label) -> string_of_int super ^ ":" ^ label)) in
    let t = ExtArray.get tokens id in
    (sprintf "<tr><td>%s</td><td>%s</td><td>%d</td><td>%d</td><td>%s</td><td>%s</td></tr>"
      t.TokenizerTypes.orth (escape_html (Tokens.string_of_token t.TokenizerTypes.token)) id conll_id sl sem) :: l))) ^
  "</table>"

let html_of_token_extarray tokens =
  "<table><tr><td><b>id</b></td><td><b>orth</b></td><td><b>beg</b></td><td><b>len</b></td><td><b>next</b></td><td><b>token</b></td></td><td><b>attrs</b></td></tr>" ^
  String.concat "\n" (List.rev (Int.fold 0 (ExtArray.size tokens - 1) [] (fun l id ->
    let t = ExtArray.get tokens id in
    (sprintf "<tr><td>%d</td><td>%s</td><td>%d</td><td>%d</td><td>%d</td><td>%s</td><td>%s</td></tr>"
      id t.TokenizerTypes.orth t.TokenizerTypes.beg t.TokenizerTypes.len t.TokenizerTypes.next (escape_html (Tokens.string_of_token t.TokenizerTypes.token))
      (String.concat "; " (Xlist.map t.TokenizerTypes.attrs Tokens.string_of_attr))) :: l))) ^
  "</table>"

let html_of_token_list tokens =
  "<table><tr><td><b>orth</b></td><td><b>beg</b></td><td><b>len</b></td><td><b>next</b></td><td><b>token</b></td></td><td><b>attrs</b></td></tr>" ^
  String.concat "\n" (List.rev (Xlist.rev_map tokens (fun t ->
      sprintf "<tr><td>%s</td><td>%d</td><td>%d</td><td>%d</td><td>%s</td><td>%s</td></tr>"
         t.TokenizerTypes.orth t.TokenizerTypes.beg t.TokenizerTypes.len t.TokenizerTypes.next (escape_html (Tokens.string_of_token t.TokenizerTypes.token))
         (String.concat "; " (Xlist.map t.TokenizerTypes.attrs Tokens.string_of_attr))))) ^
  "</table>"

let rec html_of_sentence tokens = function
    RawSentence s -> s
  | StructSentence(paths,last) -> html_of_struct_sentence tokens paths last
  | DepSentence paths -> String.concat "<BR>\n" (Xlist.map paths (html_of_dep_sentence tokens))
  | QuotedSentences sentences ->
      String.concat "<BR>\n" (Xlist.map sentences (fun p ->
        sprintf "id=%s beg=%d len=%d next=%d<BR>%s" p.id p.beg p.len p.next (html_of_sentence tokens p.sentence)))
  | AltSentence l -> (*print_endline "AltSentence";*)
     "<table border=1>" ^
     String.concat "\n" (Xlist.map l (fun (mode,sentence) ->
       sprintf "<tr><td>%s</td><td>%s</td></tr>" (SubsyntaxStringOf.mode mode) (html_of_sentence tokens sentence))) ^
     "</table>"
  | ErrorSentence s -> (*print_endline "ErrorParagraph";*) s
  (* | _ -> failwith "html_of_sentence: ni" *)

let rec html_of_paragraph tokens = function
    RawParagraph s -> (*print_endline "RawParagraph";*) s
  | StructParagraph sentences -> (*print_endline "StructParagraph";*)
      String.concat "<BR>\n" (Xlist.map sentences (fun p ->
        sprintf "id=%s beg=%d len=%d next=%d<BR>%s" p.id p.beg p.len p.next (html_of_sentence tokens p.sentence)))
  | AltParagraph l -> (*print_endline "AltParagraph";*)
     "<table border=2>" ^
     String.concat "\n" (Xlist.map l (fun (mode,paragraph) ->
       sprintf "<tr><td>%s</td><td>%s</td></tr>" (SubsyntaxStringOf.mode mode) (html_of_paragraph tokens paragraph))) ^
     "</table>"
  | ErrorParagraph s -> (*print_endline "ErrorParagraph";*) s

let rec html_of_text tokens = function
    RawText s -> s
  | StructText paragraphs ->
      String.concat "<BR>\n" (Xlist.map paragraphs (html_of_paragraph tokens))
  | AltText l ->
     "<table border=3>" ^
     String.concat "\n" (Xlist.map l (fun (mode,text) ->
       sprintf "<tr><td>%s</td><td>%s</td></tr>" (SubsyntaxStringOf.mode mode) (html_of_text tokens text))) ^
     "</table>"
  | ErrorText s -> (*print_endline "ErrorParagraph";*) s

let text_and_tokens text tokens msg =
  if msg = "" then sprintf "%s\n%s<BR>\n%s<BR>\n%s\n" html_header (html_of_text tokens text) (html_of_token_extarray tokens) html_trailer
  else sprintf "%s\n%s\n%s\n" html_header msg html_trailer

let token_list tokens msg =
  if msg = "" then sprintf "%s\n%s\n%s\n" html_header (html_of_token_list tokens) html_trailer
  else sprintf "%s\n%s\n%s\n" html_header msg html_trailer

let print_text_and_tokens path name text tokens msg =
  File.file_out (path ^ name ^ ".html") (fun file ->
      output_string file (text_and_tokens text tokens msg ))

let print_token_list path name tokens msg =
  File.file_out (path ^ name ^ ".html") (fun file ->
      output_string file (token_list tokens msg))

let print_dep_sentence path name tokens paths =
  File.file_out (path ^ name ^ ".html") (fun file ->
    output_string file (html_header ^"\n"^ html_of_dep_sentence tokens paths ^"\n"^ html_trailer))

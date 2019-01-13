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

let token_list tokens =
  "digraph G {\n" ^
  String.concat "\n" (List.rev (List.flatten (Xlist.rev_map tokens (fun t ->
      let lemma = Tokenizer.get_lemma t.token in
      if lemma = "" then [] else
        [sprintf "  %d -> %d [label=\"%s\\n%s\\n%s\"]" t.beg t.next t.orth lemma (Tokenizer.get_pos t.token)]))))
  ^ "}"

let print_token_list path name tokens =
  File.file_out (path ^ name ^ ".gv") (fun file ->
      fprintf file "%s\n" (token_list tokens));
  Sys.chdir path;
  ignore (Sys.command ("dot -Tpng " ^ name ^ ".gv -o " ^ name ^ ".png"));
  String.iter (function '/' -> Sys.chdir ".." | _ -> ()) path

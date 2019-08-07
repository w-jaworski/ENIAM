(*
 *  ENIAMlexcreator: tool for phrase selection from corpus
 *  Copyright (C) 2010, 2019 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
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

open Types
open Xstd
open SubsyntaxTypes

let rec remove_suffix_sp s =
  if Xstring.check_sufix " " s then remove_suffix_sp (Xstring.cut_sufix " " s)
  else s
  
let subsyntax_host = ref "localhost"
let subsyntax_port = ref 5937

let get_sock_addr host_name port =
  let he = Unix.gethostbyname host_name in
  let addr = he.Unix.h_addr_list in
  Unix.ADDR_INET(addr.(0),port)

let _ =
  MarkedHTMLof.initialize ();
  let sub_in,sub_out =
    Unix.open_connection (get_sock_addr !subsyntax_host !subsyntax_port) in
  let f = ref true in
  Marshal.to_channel stdout Ready_to_work [Marshal.No_sharing];
  flush stdout;
(*   prerr_endline "end init"; *)
  while !f do 
    match Marshal.from_channel stdin with 
      Work_with(id,s) -> 
(*         prerr_endline (" work with " ^ id ^ " " ^ s); *)
        let s = if s = "" then " " else s in
        let text,tokens =
          try
(*             Printf.fprintf stderr "%s\n%!" s; *)
            Printf.fprintf sub_out "%s\n\n%!" s;
            (Marshal.from_channel sub_in : SubsyntaxTypes.text * SubsyntaxTypes.token_env ExtArray.t)
          with e -> (
(*            prerr_endline ("|'" ^ s ^ "'|=" ^ string_of_int (Xstring.size s));
            prerr_endline ("subsyntax_error: " ^ Printexc.to_string e);
            exit 1;*)
            AltText[Raw,RawText s;Error,ErrorText ("subsyntax_error: " ^ Printexc.to_string e)], ExtArray.make 0 SubsyntaxTypes.empty_token_env) in
        let text = match MarkedHTMLof.cat_tokens_sequence_text 1 tokens text with
            [name,text] -> text
          | _ -> failwith ("verse_worker: " ^ id ^ " " ^ s) in
        let text = Xlist.map text (fun (cat,orth) -> 
(*           Printf.fprintf stderr "cat=%s orth=%s\n%!" cat orth; *)
          if StringSet.mem MarkedHTMLof.omited cat then remove_suffix_sp orth,orth else cat,orth) in
        Marshal.to_channel stdout (Work_done(id,text)) [Marshal.No_sharing];
        flush stdout
    | Kill_yourself -> 
(*         prerr_endline "kill youself"; *)
        f := false
  done;
  prerr_endline "work finished"

  
(* uruchamianie subsyntax:
   subsyntax -m -p 5937 -a --def-cat *)

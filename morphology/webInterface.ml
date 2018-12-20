(*
 *  ENIAMmorphology, a morphological analyser and a guesser for Polish
 *  Copyright (C) 2016-2018 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
 *  Copyright (C) 2016-2018 Institute of Computer Science Polish Academy of Sciences
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Printf

let logfile = open_out_gen [Open_wronly; Open_append; Open_creat] ((6*8+4)*8+4) "results/logi/morphology_analyze.log"

let log_query query =
(*   let addr = try Sys.getenv "REMOTE_ADDR" with Not_found -> "" in *)
  let tm = Unix.gmtime (Unix.time ()) in
(*   Printf.fprintf logfile "host=%s time=%d.%02d.%02d %02d:%02d:%02d query=%s\n" addr (tm.Unix.tm_year+1900) (tm.Unix.tm_mon+1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec query *)
  Printf.fprintf logfile "time=%d.%02d.%02d %02d:%02d:%02d query=%s\n" (tm.Unix.tm_year+1900) (tm.Unix.tm_mon+1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec query

let get_sock_addr host_name port =
  let he =
    try Unix.gethostbyname host_name
    with Not_found -> failwith ("get_sock_addr: host " ^ host_name ^ " not found") in
  let addr = he.Unix.h_addr_list in
  Unix.ADDR_INET(addr.(0),port)

let process_query query =
  let sock = get_sock_addr (*"wloczykij"*)"localhost" 5736 in
  let ic,oc =
    try Unix.open_connection sock
    with e -> failwith ("server connection error: " ^ Printexc.to_string e) in
  Printf.fprintf oc "%s\n\n%!" query;
  let result,msg = (Marshal.from_channel ic : Inflexion.t list * string) in
  Printf.fprintf oc "\n%!";
  let _ = Unix.shutdown_connection ic in
  Inflexion.html_of_interpretations (Inflexion.sort_results result) msg

let get_input () =
  let r = ref [] in
  (try
    while true do
      r := (input_line stdin) :: (!r)
    done;
    !r
  with End_of_file -> !r)

let rec translate_input_rec buf i size query =
  if i >= size then Buffer.contents buf else (
  let c,i =
    if String.get query i = '%' then
      Scanf.sscanf (String.sub query (i+1) 2) "%x" (fun a -> Char.chr a), i+3 else
    if String.get query i = '+' then ' ', i+1 else
    String.get query i, i+1 in
  Buffer.add_char buf c;
  translate_input_rec buf i size query)

let translate_input query =
  match query with
    [query] ->
      if String.sub query 0 6 = "text0=" then
        let buf = Buffer.create (String.length query) in
        translate_input_rec buf 6 (String.length query) query
      else failwith "translate_input 1"
  | _ -> failwith "translate_input 2"

(* let get_query_id () =
  let filename = Filename.temp_file ~temp_dir:"results/web/" "page_" "" in
(*   print_endline filename; *)
  let n = String.length "results/web/" + String.length "page_" in
  let id = String.sub filename n (String.length filename - n) in
(*   print_endline id; *)
  id *)

let generate_header () =
  Printf.printf "Content-type: text/html\n";
  Printf.printf "\n"

let generate_trailer () =
  (*Printf.printf "</BODY>\n</HTML>\n"*)()

let page_header path =
"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">
<html>
  <head>
	<META HTTP-EQUIV=\"CONTENT-TYPE\" CONTENT=\"text/html; charset=utf8\">
	<TITLE>ENIAMmorphology</TITLE>
	<META HTTP-EQUIV=\"Content-Language\" CONTENT=\"pl\">
  </head>

  <body>
 <center>
   <h1>ENIAMmorphology: analizator morfologiczny i guessser dla języka polskiego</h1>
    <h3>Podaj słowo:</h3>
    <form method=POST action=\"" ^ path ^ "morphology.cgi\">
      <p><input type=\"text\" name=\"text0\" value=\"\" size=\"100\"></p>
      <p><input type=\"submit\" value=\"Analizuj\" size=\"60\"></p>
   </form>"

let generate_webpage query content =
  print_endline (page_header "");
  printf "    <h3>%s</h3>
    %s
</center>
  </body>
</html>" query content


let generate_error_message e =
  Printf.printf
"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">
<html>
  <head>
	<META HTTP-EQUIV=\"CONTENT-TYPE\" CONTENT=\"text/html; charset=utf8\">
	<TITLE>ENIAMmorphology</TITLE>
	<META HTTP-EQUIV=\"Content-Language\" CONTENT=\"pl\">
  </head>

  <body>
 <center>
   <h1>ENIAMmorphology</h1>
    <h3>%s</h3>
</center>
  </body>
</html>" e

let _ =
  generate_header ();
  (try
    let query = get_input () in
    let query = translate_input query in
    log_query query;
    (* let id = get_query_id () in *)
    let content = process_query query in
    generate_webpage query content
  with
    Failure e -> generate_error_message e
  | e -> generate_error_message (Printexc.to_string e));
  generate_trailer ()

(* uruchamianie serwera:
./morphology -m -p 5736
*)

(* testowanie z linii poleceń:
echo "text0=profesory" | ./morphology.cgi
echo "text0=miauczy" | ./morphology.cgi
*)

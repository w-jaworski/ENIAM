(*
 *  ENIAMfuzzyAnalyzer is a library that corrects spelling errors.
 *  Copyright (C) 2017-2018 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
 *  Copyright (C) 2017-2018 Institute of Informatics, University of Warsaw
 *  Copyright (C) 2017-2018 SELIDOR - T. Puza, Ł. Wasilewski Sp.J.
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

let comm_stdio = ref true
let port = ref 5439

let spec_list = [
  "-i", Arg.Unit (fun () -> comm_stdio:=true), "Communication using stdio (default)";
  "-p", Arg.Int (fun p -> comm_stdio:=false; port:=p), "<port> Communication using sockets on given port number";
  ]

let usage_msg =
  "Usage: corrector <options>\nInput is a sequence of lines. Empty line ends the sequence and invoke parsing. Double empty line shutdown parser.\nOptions are:"

let message = "ENIAMfuzzyAnalyzer is a library that corrects spelling errors.\n\
Copyright (C) 2017-2018 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>\n\
Copyright (C) 2017-2018 SELIDOR - T. Puza, Ł. Wasilewski Sp.J."

let anon_fun s = raise (Arg.Bad ("invalid argument: " ^ s))

let input_text channel =
  try input_line channel with End_of_file -> ""

let rec main_loop in_chan out_chan =
  let text = input_text in_chan in
  if text = "" then () else (
    (* print_endline "input text begin";
    print_endline text;
    print_endline "input text end"; *)
    let corrected = ENIAMfuzzyDetector.correct text in
    output_string out_chan (corrected ^ "\n");
    flush out_chan;
    main_loop in_chan out_chan)

let _ =
  prerr_endline message;
  Arg.parse spec_list anon_fun usage_msg;
  ENIAMfuzzyDetector.initialize ();
  Gc.compact ();
  prerr_endline "Ready!";
  if !comm_stdio then main_loop stdin stdout
  else
    let sockaddr = Unix.ADDR_INET(Unix.inet_addr_any,!port) in
    Unix.establish_server main_loop sockaddr

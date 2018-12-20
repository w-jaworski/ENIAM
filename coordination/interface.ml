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

let port = ref 5449

let spec_list = [
  "-p", Arg.Int (fun p -> port:=p), "<port> Communication using sockets on given port number (default 5449)";
  ]

let usage_msg =
  "Usage: coordination <options>\nOptions are:"

let message = "ENIAMcoordination: coordination disambiguation for Polish"(*\n\ 
Copyright (C) 2018\n\
Copyright (C) 2018*)

let anon_fun s = raise (Arg.Bad ("invalid argument: " ^ s))

let rec main_loop in_chan out_chan =
  let tokens = (Marshal.from_channel in_chan: TokenizerTypes.token_env list) in
  let tokens,msg = Coordination.catch_disambiguate tokens in
  Marshal.to_channel out_chan (tokens,msg) []; 
  flush out_chan;
  main_loop in_chan out_chan

let _ =
  prerr_endline message;
  Arg.parse spec_list anon_fun usage_msg;
  Coordination.initialize ();
  Gc.compact ();
  prerr_endline "Ready!";
  let sockaddr = Unix.ADDR_INET(Unix.inet_addr_any,!port) in
  Unix.establish_server main_loop sockaddr

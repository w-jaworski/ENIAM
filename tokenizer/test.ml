(*
 *  ENIAMtokenizer, a tokenizer for Polish
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

let theories_paths = [
  "/home/yacheu/Dokumenty/ENIAM/theories/numbers";
  "/home/yacheu/Dokumenty/ENIAM/theories/persons";
  (* "theories/numbers"; *)
  ]

let test_strings = [
  ]

let _ =
  TokenizerTypes.theories_paths := theories_paths;
  Tokenizer.initialize ();
  print_endline "Testy wbudowane";
  Xlist.iter test_strings (fun s ->
    print_endline ("\nTEST: " ^ s);
    let tokens = Tokenizer.parse s in
    (* print_endline (Tokenizer.xml_of tokens); *)
    Xlist.iter tokens (fun token -> print_endline (Tokenizer.string_of 0 token)));
  print_endline "Testy użytkownika.";
  print_endline "Wpisz tekst i naciśnij ENTER, pusty tekst kończy.";
  let s = ref (read_line ()) in
  while !s <> "" do
    let tokens = Tokenizer.parse !s in
    (* print_endline (Tokenizer.xml_of tokens); *)
    Xlist.iter tokens (fun token -> print_endline (Tokenizer.string_of 0 token));
    print_endline "Wpisz tekst i naciśnij ENTER, pusty tekst kończy.";
    s := read_line ()
  done;
  ()

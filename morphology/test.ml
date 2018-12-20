(*
 *  ENIAMmorphology, a morphological analyser and a guesser for Polish
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

let test_strings = [
  "Ale";
  "ale";
  "miał";
  "kota";
  "Kot";
  "Szpak";
  "sepulkujemy";
  "najsepulkowniejszy";
  ]

let string_of_token (lemma,interp,quantity,attrs) =
  Printf.sprintf "%s\t%s\t%d\t%s" lemma interp quantity (String.concat ", " attrs)

(*let _ =
  inflexion.initialize ();
  print_endline "Testy wbudowane";
  Xlist.iter test_strings (fun s ->
    print_endline ("\nTEST: " ^ s);
    let tokens = inflexion.get_interpretations s in
    (* print_endline (tokenizer.xml_of tokens); *)
    Xlist.iter tokens (fun token -> print_endline (string_of_token token)));
  print_endline "Testy użytkownika.";
  print_endline "Wpisz tekst i naciśnij ENTER, pusty tekst kończy.";
  let s = ref (read_line ()) in
  while !s <> "" do
    let tokens = inflexion.get_interpretations !s in
    (* print_endline (tokenizer.xml_of tokens); *)
    Xlist.iter tokens (fun token -> print_endline (string_of_token token));
    print_endline "Wpisz tekst i naciśnij ENTER, pusty tekst kończy.";
    s := read_line ()
  done;
  ()*)

open MorphologyTypes
  
let test_strings2 = [
 (* "być","praet:_:_:_",[],[];
  "być","fin:_:_:_",[],[];
  "być","ppas:_:_:_:_:_",[],[];
  "być","pant:_",[],[];
  "mieć","ger:_:_:_:_:_",[],[];
  "mieć","pant:_",[],[];
  "kot","subst:_:_:_",[],[];
  "kość","subst:_:_:_",[],[];
  "kość","subst:_:_:f",[],[];
  "zielony","adj:_:_:_:_",[],[];
  "zielony","adj:sg:gen:m1:_",[],[];
  "wesoły","adj:_:_:_:_",[],[];
  "mało","adv:_",[],[];
  "ślisko","adv:_",[],[];
  "cienko","adv:_",[],[];
  "pracować","praet:_:_:_",[],[];
  "pracować","fin:_:_:_",[],[];
  "pracować","ppas:_:_:_:_:neg",[],[Dial];
  "przepracować","ppas:_:_:_:_:_",[],[];
  "Leśniowski","subst:_:_:m1",[],[Acro;Aux;Aux2;Ndm];*)
(*  "Richelieu","subst:_:_:_",[],[];
  "VAT","subst:_:_:_",[],[];*)
  "NFZ","subst:_:_:_",[Acro],[]; (* FIXME: Zot *)
(*  "paść","praet:_:_:_",[],[];
  "paść:v1","praet:_:_:_",[],[];
  "paść:v2","praet:_:_:_",[],[];
  "Jacques","subst:_:_:m1",[],[];
  "Tacques","subst:_:_:m1",[Aux;Aux2],[Dial;Ndm];*)
  "Cauchy","subst:_:_:m1",[],[]; (* FIXME *)
(*  "blablarować","fin:_:_:_",[],[Dial];
  "blablarzyć","fin:_:_:_",[],[Dial];
  "blablarzeć","fin:_:_:_",[],[Dial];
  "blablarzuć","fin:_:_:_",[],[];
  "blablarzać","fin:_:_:_",[],[Dial];
  "blablarzić","fin:_:_:_",[],[Dial];
  "blablarzoć","fin:_:_:_",[],[];*)
(*  "elemelenikt","subst:_:_:m1",[],[Acro;Aux;Aux2;Ndm;Dial];
  "elemelenikt","subst:_:_:m2",[],[Acro;Aux;Aux2;Ndm;Dial];
  "elemelenikt","subst:_:_:m3",[],[Acro;Aux;Aux2;Ndm;Dial];
  "elemelenikt","subst:_:_:f",[],[Acro;Aux;Aux2;Ndm;Dial];
  "xligand","subst:sg:gen:_",[],[Acro;Aux;Aux2;Ndm;Dial];
  "pupcia","subst:_:_:m1",[],[Acro;Aux;Aux2;Ndm;Dial];
  "dupa","subst:_:_:m1",[],[Acro;Aux;Aux2;Ndm;Dial];*)
  "pipipipi","adj:_:_:_:pos",[],[]; (* FIXME: pojawia się j *)
  "ddddana","subst:_:_:f",[],[Acro;Aux;Aux2;Ndm;Dial]; (* FIXME: aen *)
  "lamiwudyna","",[],[Acro;Aux;Aux2;Ndm;Dial];
  "lamiwudyna","subst:_:_:_",[],[Acro;Aux;Aux2;Ndm;Dial];
  "lamiwudyna","subst:_:_:f",[],[Acro;Aux;Aux2;Ndm;Dial];
  ]
  
let _ =
  Inflexion.initialize ();
  print_endline "Testy wbudowane";
  Xlist.iter test_strings2 (fun (lemma,interp,feat,excl) ->
    print_endline ("\nTEST: " ^ lemma ^ " " ^ interp ^ 
      " [" ^ String.concat ";" (Xlist.map feat MorphologyRules.string_of_star) ^ "] [" ^ 
      String.concat ";" (Xlist.map excl MorphologyRules.string_of_star) ^ "]");
    let result = Inflexion.disambiguate feat excl (Inflexion.synthetize lemma interp) in
    (* print_endline (tokenizer.xml_of tokens); *)
    print_endline (Inflexion.string_of_interpretations result ^ "\n\n"));
(*  print_endline "Testy użytkownika.";
  print_endline "Wpisz tekst i naciśnij ENTER, pusty tekst kończy.";
  let s = ref (read_line ()) in
  while !s <> "" do
    let tokens = Inflexion.get_interpretations !s in
    (* print_endline (tokenizer.xml_of tokens); *)
    Xlist.iter tokens (fun token -> print_endline (string_of_token token));
    print_endline "Wpisz tekst i naciśnij ENTER, pusty tekst kończy.";
    s := read_line ()
  done;*)
  ()
  
(* 
zieloni i zieleni - obie poprawne
wesoli i weseli - jedna poprawna

Jacques - poprawnie lematyzowany
Tacques - z treści lematu nie wynika, że słowo ma obcą ortografie (i którą obcą ortografię), trzeba dostarczyć dodatkowych informacji
*)
  

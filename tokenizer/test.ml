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
(*  "a gdybym miałem";
  "A Gdy Miałem";
  "GDY MIAŁEM";
  "I II III IV V VI VII VIII IX X MCXIV MXC";
  "Kiedy Piotr Prabucki, przewodniczący Komisji Budżetu PeKaO";
  "25 idzie 20.";*)
  "Kot. Kot. kot.";
  "25.";
  "25.888.231";
  "Ala 25.888.231.111 ma.";
  "Ala 25.888.031,011.";
  "Ala -25.888.031,011.";
  "Ala -25 .";
  "Ala -1° C  3° ciepła 20—30°C od 180° do 260°C  około 6° poniżej horyzontu.";
  "Ala 22-25 .";
  "Ala 22.5.2000-25.5.2001 .";
  "Szpak frunie.";
  (* "Kot miauczy."; *)
(*  "Np. Ala.";*)
  (* "w. dom.";
  "tzn.";
  "c.d.n."; *)
(*  "Arabia Saudyjska biegnie.";*)
  "Cauchy'ego ONZ-owska biegnie.";
  "TE-cie E-e.";
  "MS-DOS-owska CI-cie KRRi-cie UJ-ocie UJ-OCIE.";
  "rock'n'rollowy d’Alembertowi staro-cerkiewno-słowiańskimi";
  "ping-ponga";
(*  "Tom idzie.";*)
  (* "Miałem miał."; *)
(*  "Szpak śpiewa.";
  "Ala ma kota.";
  "Ale mają kota:"*)
(*  "Matura.";
  "matura";
  "„Matura.”";
  "„Matura”.";
  "„matura”";
  "- matura";
  "- Matura";
  "2 jabłka";
  "- 2 jabłka";*)
  (* "drukowanym w „Dialogu”";
  "drukowanym w „Dialogu”."; *)
  (* "\"Throw out\" znaczy \"wyrzucić\".";
  "- Votare! ( Głosujmy !)";
  "( Głosujmy !)";
  "„Dialog”"; *)
  "x br.";
  "ponad 388 tys. ludzi";
  "ponad 388 tys. km.2";
  "(PTTK Żyrardów, tel. 0-46 855-45-26)";
  "40-045 Katowice, ul. Astrów 7, tel. (032) 51 30 86, tel. i faks 51 86 28, 517 193, 518 609";
  (* "przeciętnie 7,5 tys. kibiców";
  "0,4mln";
  "8,8665tys.";
  "70-75 tys.";
  "70-75tys."; *)
  (* "myjni \"A-Car Auto\" Myjnia"; *)
  (* "nowaka@lp2.pl"; *)
  (* "poufale :P) Im"; *)
  (*"Piłsudskiego 12 A Konstancin-Jeziorna";
  "Mary Mary";
  "TTTTTTTK TTTTTTTK"; *)
  (* "Marcin Jagodziński czytanie: http://marcin.webcorp.pl pisanie: marcinj@webcorp.pl";
  "Możemy nauczyć się kung-fu z waszego filmu."; *)
  (* "Jak znam Pola, będzie teraz rozmawiał przynajmniej z profesorem Żołądziem.";
  "Przeniosłam wzrok na jego dłoń, a potem wróciłam do badania jej faktury opuszkiem palca."; *)
  (* "On-line	Komunikat dotyczy kilku zagadnień, w tym przyjęcia przez państwa członkowskie systemów ratingowych PEGI i PEGI On-line."; *)
  (* "Moim zdaniem UMŚ stanowi"; *)
  (* "że 50-tką nie da rady"; *)
  (* "mgr inż Jan ppm hhhtcem"; *)
  (* "stosując metodę 9.3."; *)
(*   "medyczny m.in. ultrasonograf"; *)
(*   "oddechowych z kaszlemchgrypką i bezgłosem"; *)
(*   "środa"; *)
(*   "środy"; *)
(*  "Aleja Ks. Jerzego";
  "Aleja ks. Jerzego";*)
(*   "Mw"; *)
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

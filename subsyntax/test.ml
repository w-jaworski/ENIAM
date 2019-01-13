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

let test_strings = [
  (* "Szpak frunie.";
  "Kot np. miauczy.";
  "Ala ma kota.";
  "Ale mają kota:" *)
  (* "W 1984-89 uczęszczał do VII Liceum Ogólnokształcącego im. K.K. Baczyńskiego w Szczecinie."; *)
  (* "W 2003 obronił doktorat nauk technicznych w zakresie architektury i urbanistyki na Politechnice Krakowskiej i został adiunktem w Zakładzie Teorii Architektury, Historii i Konserwacji Zabytków IAiPP."; *)
  (* "Trzy lata później założył pracownię architektoniczną Atelier Bizio + Ligierko, zajmującą się adaptacjami budynków historycznych."; *)
  (* "Festiwalu Polskich Sztuk Współczesnych R@Port"; *)
  (* "Przeglądu Teatrów Małych Form „Kontrapunkt”"; *)
  (* "Dyplom uzyskał w 1994."; *)
  (* "dyplom uzyskał w 1994";
  "o trąbach powietrznych";
  "trąba powietrzny";  *)
  (* "ul. III Poprzecznej"; *)
  (* "ul. Stefana Banacha";
  "Chłopcy mają ulicę kwiatami."; *)
  (* "„Dialog”"; *)
  (* "( Głosujmy !)"; *)
  "Jakie są ceny w obu firmach za a) wymianę płyty głównej; b) wymianę portu HDMI"
]

let test_strings2 = [
  (* "Szpak frunie. Kot miauczy.";
  "Szpak powiedział: „Frunę. Śpiewam.”"; *)
  (* "Istniejący od XI w. Czersk uzyskał prawa miejskie w 1350 r. Mazowsze było wtedy samodzielnym księstwem."; *)
  (* "Dyplom uzyskał w 1994.";
  "dyplom uzyskał w 1994"; *)
  (* "Chłopcy mają ulicę kwiatami."; *)
  (* "\"Throw out\" znaczy \"wyrzucić\".";
  "„Dialog”";
  "„Dialog”:"; *)
  (* "- Votare! ( Głosujmy !)";
  "( Głosujmy !)"; *)
  (* "À propos"; *)
  ]

let _ =
  Subsyntax.initialize ();
  let test_num = ref 1 in
  print_endline "Testy wbudowane";
  Xlist.iter test_strings (fun s ->
    print_endline ("\nTEST: " ^ s);
    let tokens = Subsyntax.parse s in
    print_endline (SubsyntaxStringOf.token_list tokens);
    File.file_out ("results/test" ^ string_of_int !test_num ^ ".xml") (fun file ->
        output_string file (Xml.to_string_fmt (SubsyntaxXMLof.token_list tokens "")));
    SubsyntaxHTMLof.print_token_list "results/" ("test" ^ string_of_int !test_num) tokens "";
    SubsyntaxGraphOf.print_token_list "results/" ("test" ^ string_of_int !test_num) tokens;
    incr test_num);
  print_endline "Testy wbudowane 2";
  Xlist.iter test_strings2 (fun s ->
    print_endline ("\nTEST: " ^ s);
    let text,tokens = Subsyntax.parse_text s in
    print_endline (SubsyntaxStringOf.token_extarray tokens);
    print_endline "";
    print_endline (SubsyntaxStringOf.text "" tokens text);
    File.file_out ("results/test" ^ string_of_int !test_num ^ ".xml") (fun file ->
        output_string file (Xml.to_string_fmt (SubsyntaxXMLof.text_and_tokens text tokens "")));
    SubsyntaxHTMLof.print_text_and_tokens "results/" ("test" ^ string_of_int !test_num) text tokens "";
    incr test_num);
  ()

(*
    Text -> output_string out_chan (SubsyntaxStringOf.text "" tokens text ^ "\n" ^ SubsyntaxStringOf.token_extarray tokens ^ "\n\n")
| Xml -> output_string out_chan (Xml.to_string (SubsyntaxXMLof.text_and_tokens text tokens) ^ "\n\n")
| Html -> output_string out_chan (SubsyntaxHTMLof.text_and_tokens text tokens ^ "\n\n")
| Marsh -> Marshal.to_channel out_chan (text,tokens) []
| Graphviz -> failwith "main_loop: ni")
else
  let tokens = Subsyntax.parse text in
  (match !output with
   | Html -> output_string out_chan (SubsyntaxHTMLof.token_list tokens ^ "\n\n")
   | Marsh -> Marshal.to_channel out_chan tokens []
   | Graphviz -> output_string out_chan (SubsyntaxGraphOf.token_list tokens ^ "\n\n")));

*)

(*
 *  ENIAMgenerator: generator of inflected phrases for Polish
 *  Copyright (C) 2020 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
 *  Copyright (C) 2020 Institute of Computer Science Polish Academy of Sciences
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

(*let _ =
  CanonicalParser.initialize ();
  let phrases = File.load_lines Sys.argv.(1) in
  Xlist.iter phrases (fun phrase -> 
(*     print_endline phrase; *)
    try
      let _ = CanonicalParser.parse_np_nom true phrase in
      ()
    with 
      CanonicalParser.Strange -> print_endline (phrase ^ " STRANGE")
    | CanonicalParser.PatternNotFound ->print_endline (phrase ^ " PATTERN NOT FOUND"));
  ()*)

let _ =
  CanonicalParser.initialize ();
  let phrases = File.load_lines Sys.argv.(1) in
  let l = List.rev (Xlist.fold phrases [] (fun l phrase -> 
    try
      (CanonicalParser.parse_np_nom (*true*)false phrase) @ l
    with 
      CanonicalParser.Strange -> l
    | CanonicalParser.PatternNotFound -> l)) in
  Xlist.iter l (fun phrase ->
    Printf.printf "\n%s\n" 
      (String.concat " " (Xlist.map phrase (fun (lemma,pos,tags) -> 
        CanonicalParser.canonical_string lemma pos tags)));
    Xlist.iter Generator.cases (fun case ->
      Xlist.iter Generator.numbers (fun number ->
        let phrases = Generator.generate_np_number_case number case phrase in
        Xlist.iter phrases (fun phrase ->
          print_endline (number ^ ":" ^ case ^ " " ^ String.concat "" (Xlist.map phrase (fun i -> i.Inflexion.lemma)))))));    
  ()

(*let _ =
  CanonicalParser.initialize ();
  let phrases = File.load_lines Sys.argv.(1) in
  let l = List.rev (Xlist.fold phrases [] (fun l phrase -> 
    try
      (CanonicalParser.parse_np_nom true phrase) @ l
    with 
      CanonicalParser.Strange -> l
    | CanonicalParser.PatternNotFound -> l)) in
  let grouped_phrases = Generator.generate_case_grouped_np l in
  Xlist.iter grouped_phrases (fun (s,l) ->
    print_endline ("\n" ^ s ^ ":");
    Xlist.iter l print_endline)*)
    
(*let _ =
  CanonicalParser.initialize ();
  let phrases = File.load_lines Sys.argv.(1) in
  Xlist.iter phrases (fun phrase -> 
    print_endline phrase;
    try
      let l = CanonicalParser.parse_infp phrase in
      Xlist.iter l (fun phrase ->
        Printf.printf "%s\n" 
          (String.concat " " (Xlist.map phrase (fun (lemma,pos,tags) -> 
            CanonicalParser.canonical_string lemma pos tags))));
     ()
    with 
      CanonicalParser.Strange -> print_endline (phrase ^ " STRANGE")
    | CanonicalParser.PatternNotFound ->print_endline (phrase ^ " PATTERN NOT FOUND"));
  ()*)

(*let _ =
  CanonicalParser.initialize ();
  let phrases = File.load_lines Sys.argv.(1) in
  let l = List.rev (Xlist.fold phrases [] (fun l phrase -> 
    try
      (CanonicalParser.parse_infp phrase) @ l
    with 
      CanonicalParser.Strange -> l
    | CanonicalParser.PatternNotFound -> l)) in
  Xlist.iter l (fun phrase ->
    Printf.printf "\n%s\n" 
      (String.concat " " (Xlist.map phrase (fun (lemma,pos,tags) -> 
        CanonicalParser.canonical_string lemma pos tags)));
    Xlist.iter ["fin";"impt"] (fun pos ->
      Xlist.iter ["pri";"sec"] (fun person ->
        let phrases = Generator.generate_ip pos "sg" person phrase in
        Xlist.iter phrases (fun phrase ->
          print_endline (pos ^ ":" ^ person ^ " " ^ String.concat "" (Xlist.map phrase (fun i -> i.Inflexion.lemma)))))));    
  ()*)
  
(*let _ =
  CanonicalParser.initialize ();
  let phrases = File.load_lines Sys.argv.(1) in
  Xlist.iter phrases (fun phrase -> 
    print_endline phrase;
    try
      let l = CanonicalParser.parse_adjp_sg_nom_m phrase in
      Xlist.iter l (fun phrase ->
        Printf.printf "%s\n" 
          (String.concat " " (Xlist.map phrase (fun (lemma,pos,tags) -> 
            CanonicalParser.canonical_string lemma pos tags))));
     ()
    with 
      CanonicalParser.Strange -> print_endline (phrase ^ " STRANGE")
    | CanonicalParser.PatternNotFound ->print_endline (phrase ^ " PATTERN NOT FOUND"));
  ()*)
  
(*let _ =
  CanonicalParser.initialize ();
  let phrases = File.load_lines Sys.argv.(1) in
  let l = List.rev (Xlist.fold phrases [] (fun l phrase -> 
    try
      (CanonicalParser.parse_adjp_sg_nom_m phrase) @ l
    with 
      CanonicalParser.Strange -> l
    | CanonicalParser.PatternNotFound -> l)) in
  Xlist.iter l (fun phrase ->
    Printf.printf "\n%s\n" 
      (String.concat " " (Xlist.map phrase (fun (lemma,pos,tags) -> 
        CanonicalParser.canonical_string lemma pos tags)));
    Xlist.iter Generator.cases (fun case ->
      Xlist.iter Generator.numbers (fun number ->
        Xlist.iter Generator.genders (fun gender ->
        let phrases = Generator.generate_adjp_number_case_gender number case gender phrase in
        Xlist.iter phrases (fun phrase ->
          print_endline (number ^ ":" ^ case ^ " " ^ String.concat "" (Xlist.map phrase (fun i -> i.Inflexion.lemma))))))));    
  ()*)
  
(*let _ =
  CanonicalParser.initialize ();
  let phrases = File.load_lines Sys.argv.(1) in
  let l = List.rev (Xlist.fold phrases [] (fun l phrase -> 
    try
      (CanonicalParser.parse_adjp_sg_nom_m phrase) @ l
    with 
      CanonicalParser.Strange -> l
    | CanonicalParser.PatternNotFound -> l)) in
  let grouped_phrases = Generator.generate_case_grouped_adjp l in
  Xlist.iter grouped_phrases (fun (s,l) ->
    print_endline ("\n" ^ s ^ ":");
    Xlist.iter l print_endline)*)

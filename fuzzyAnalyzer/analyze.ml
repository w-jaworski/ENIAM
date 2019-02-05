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

open Xstd
open ENIAMfuzzyDetector
open Xunicode

let string_to_list s =
  utf8_chars_of_utf8_string (lowercase_utf8_string s)

let string_to_array s =
  Array.of_list (Sign "" :: classified_chars_of_utf8_string (lowercase_utf8_string s))

let string_to_array2 s =
  Array.of_list (Small "**" :: Sign "" :: classified_chars_of_utf8_string (lowercase_utf8_string s))

let string_to_array3 s =
  Array.of_list ((Small "**" :: Sign "" :: classified_chars_of_utf8_string (lowercase_utf8_string s)) @ [Sign "##"])

let correct = "Proponuję termin w kancelarii X, poniedziałek 24.04 10:00, ul. Nowy Świat 7, Warszawa."
let observed = "Propnouje termin w Kancelarii X, poniedzialek 24.04 10:00, ul. Noywy Świat 7, Warszwa."
let correct1 = "Proponuję"
(* let observed = "Propnouje" *)
let observed1 = "Proponuje"
let correct2 = "Proponuję termin"

(* let _ =
  let l = count_differences_simple (string_to_array observed) (string_to_array correct) in
  Printf.printf "%s\n" (String.concat " " (Xlist.map l string_of_edit)) *)

let rec allign_lines verbose filename time rev = function
    [],[] -> List.rev rev
  | (name,t) :: l, [] ->
        if verbose then Printf.printf "%s: Del1 [%s] %s: %s\n" filename time name t;
        allign_lines verbose filename time rev (l,[])
  | [], (name,t) :: l ->
        if verbose then Printf.printf "%s: Ins  [%s] %s: %s\n" filename time name t;
        allign_lines verbose filename time rev (l,[])
  | (oname,ot) :: ol, (cname,ct) :: cl ->
        if oname = cname then
          let l = count_differences_simple (string_to_array ot) (string_to_array ct) in
          let n = Xlist.fold l 0 (fun n -> function
              Accept _ -> n
            | _ -> n+1) in
          if n <= 20 then allign_lines verbose filename time ((ot,ct) :: rev) (ol,cl) else (
          if verbose then Printf.printf "%s: Del2 [%s] %s: %s\n" filename time oname ot;
          if verbose then Printf.printf "%s: Del- [%s] %s: %s\n" filename time oname ct;
          if verbose then Printf.printf "%s\n" (String.concat " " (Xlist.map l string_of_edit));
          allign_lines verbose filename time rev (ol,(cname,ct) :: cl))
        else (
          if verbose then Printf.printf "%s: Del3 [%s] %s: %s\n" filename time oname ot;
          allign_lines verbose filename time rev (ol,(cname,ct) :: cl))

let group_time observed_lines correct_lines =
  let map = Xlist.fold observed_lines StringMap.empty (fun map (time,name,t,f) ->
    StringMap.add_inc map time ([name,t],[]) (fun (ol,cl) -> (name,t) :: ol, cl)) in
  let map = Xlist.fold correct_lines map (fun map (time,name,t,f) ->
    StringMap.add_inc map time ([],[name,t]) (fun (ol,cl) -> ol, (name,t) :: cl)) in
  List.rev (StringMap.fold map [] (fun l time (ol,cl) -> (time,List.rev ol,List.rev cl) :: l))

let chat_filenames = File.load_lines "/home/yacheu/Dropbox/Selidor/chaty/chaty.txt"
let chat2_filenames = File.load_lines "/home/yacheu/Dropbox/Selidor/chaty/chaty2.txt"

let parse_chats result =
  (* let chat_filenames = [List.hd chat_filenames] in *)
  Xlist.fold chat_filenames result (fun result filename ->
    (* print_endline filename; *)
    let correct_chat = ChatDriver.load_chat "/home/yacheu/Dropbox/Selidor/chaty/chaty1-JP-SR-WJ-2/" filename in
    let observed_chat = ChatDriver.load_chat "/home/yacheu/Dropbox/Selidor/chaty/chaty1-WJ/" filename in
    let filename = if Xstring.check_sufix ".rsv" filename then Xstring.cut_sufix ".rsv" filename else failwith "parse_chats" in
    let filename,etap,names,correct_lines = ChatDriver.parse_metadata filename correct_chat in
    let filename,etap,names,observed_lines = ChatDriver.parse_metadata filename observed_chat in
    let lines = group_time observed_lines correct_lines in
    let lines = Xlist.fold lines [] (fun l (time,ol,cl) ->
      (allign_lines false filename time [] (ol,cl)) @ l) in
    lines @ result)

let parse_chats2 result =
  Xlist.fold chat2_filenames result (fun result filename ->
    (* print_endline filename; *)
    let correct_chat = ChatDriver2.load_chat "/home/yacheu/Dropbox/Selidor/chaty/chaty2-JP-SR-2/" filename in
    let observed_chat = ChatDriver2.load_chat "/home/yacheu/Dropbox/Selidor/chaty/chaty2/" filename in
    let filename = if Xstring.check_sufix ".txt" filename then Xstring.cut_sufix ".txt" filename else failwith "parse_chats2" in
    let correct_lines = ChatDriver2.exclude_comments correct_chat in
    let observed_lines = ChatDriver2.exclude_comments observed_chat in
    let lines = group_time observed_lines correct_lines in
    let lines = Xlist.fold lines [] (fun l (time,ol,cl) ->
      (allign_lines false filename time [] (ol,cl)) @ l) in
    lines @ result)

let print_stringqmap qmap =
  let sum = StringQMap.fold qmap 0 (fun sum _ v -> sum + v) in
  StringQMap.iter qmap (fun k v ->
    Printf.printf "%8d %0.6f %s\n" v (float v /. float sum) k);
  Printf.printf "%8d\n" sum

let ocaml_print_stringqmap qmap =
  let sum = StringQMap.fold qmap 0 (fun sum _ v -> sum + v) in
  StringQMap.iter qmap (fun k v ->
    Printf.printf "  \"%s\",%0.6f;" k (float v /. float sum));
  print_endline ""

let count_differences operators (qmap,qmapo,qmapc) lines =
    Xlist.fold lines (qmap,qmapo,qmapc) (fun (qmap,qmapo,qmapc) (observed, correct) ->
      let l = count_differences operators (string_to_array2 observed) (string_to_array2 correct) in
      Xlist.fold l qmap EditQMap.add,
      Xlist.fold (string_to_list observed) qmapo StringQMap.add,
      Xlist.fold (string_to_list correct) qmapc StringQMap.add)
      (* if l <> [] then Printf.printf "%s\n" (String.concat " " (Xlist.map l string_of_edit))); *)

let special_letters = StringSet.of_list ["a";"ą";"c";"ć";"e";"ę";"l";"ł";"n";"ń";"o";"ó";"s";"ś";"z";"ź";"ż"]

let special_letter_pairs_list = ["a","ą";"c","ć";"e","ę";"l","ł";"n","ń";"o","ó";"s","ś";"z","ź";"z","ż"]

let special_letter_pairs =
  let map = Xlist.fold special_letter_pairs_list StringMap.empty (fun map (k,v) ->
    StringMap.add_inc map k [v] (fun l -> v :: l)) in
  let map = Xlist.fold special_letter_pairs_list map (fun map (v,k) ->
    StringMap.add_inc map k [v] (fun l -> v :: l)) in
  map

let group_sign_types = function
    Transpose(Small _,Small _) -> "Transpose(L)"
  | Substitute(Small _,Small _) -> "Substitute(L)"
  | Substitute(Sign _,Sign _) -> "Substitute(S)"
  | Insert(Digit _,Digit _) -> "Insert(D)"
  | Insert(Sign _,Sign _) -> "Insert(S)"
  | Insert(Small _,Small _) -> "Insert(L)"
  | Insert(Small _,Sign _) -> "Insert(L->S)"
  | Insert(Digit _,Sign _) -> "Insert(D->S)"
  | Insert(Digit _,Small _) -> "Insert(D->L)"
  | Insert(Sign _,Small _) -> "Insert(S->L)"
  | Insert(Sign _,Digit _) -> "Insert(S->D)"
  | Delete(Digit _,Digit _) -> "Delete(D)"
  | Delete(Sign _,Sign _) -> "Delete(S)"
  | Delete(Small _,Small _) -> "Delete(L)"
  | Delete(Small _,Sign _) -> "Delete(L->S)"
  | Delete(Digit _,Sign _) -> "Delete(D->S)"
  | Delete(Digit _,Small _) -> "Delete(D->L)"
  | Delete(Sign _,Small _) -> "Delete(S->L)"
  | Delete(Sign _,Digit _) -> "Delete(S->D)"
  | Accept(Digit _) -> "Accept(D)"
  | Accept(Sign _) -> "Accept(S)"
  | Accept(Small _) -> "Accept(L)"
  | Accept(Emoticon _) -> "Accept(S)"
  | Accept(ForeignSmall _) -> "Accept(L)"
  | Accept(Other _) -> "Accept(S)"
  | s -> string_of_edit s

let group_sign_type = function
    Small s -> if StringSet.mem special_letters s then Small "y" else Small "x"
  | Digit _ -> Small "d"
  | Sign " " -> Small " "
  | Sign s -> Small "s"
  | ForeignSmall _ -> Small "x"
  | t -> t

let group_sign_types2 = function
    Transpose(s,t) -> Transpose(group_sign_type s,group_sign_type t)
  | Substitute(s,t) -> Substitute(group_sign_type s,group_sign_type t)
  | Insert(s,t) -> Insert(group_sign_type s,group_sign_type t)
  | Delete(s,t) -> Delete(group_sign_type s,group_sign_type t)
  | Accept(s) -> Accept(group_sign_type s)

let group_operator_types = function
    Transpose(s,t) -> "Transpose"
  | Substitute(s,t) -> "Substitute"
  | Insert(s,_) -> "Insert"
  | Delete(s,_) -> "Delete"
  | Accept(s) -> "Accept"

let group_typical_letters = function
  | Transpose(Small s,Small _) ->
      if StringSet.mem special_letters s then Transpose(Small s,Small "x") else Transpose(Small "x",Small "x")
  | Transpose _ as t -> t
  | Substitute(Small s,Small t) ->
      if StringSet.mem special_letters s then
        if Xlist.mem (StringMap.find special_letter_pairs s) t then
         Substitute(Small s,Small t)
        else Substitute(Small s,Small "x")
      else Substitute(Small "x",Small "x")
  | Substitute _ as t -> t
  | Insert(Small s, _) ->
      if StringSet.mem special_letters s then Insert(Small s,Small "x") else Insert(Small "x",Small "x")
  | Insert _ as t -> t
  | Delete(Small s, _) ->
      if StringSet.mem special_letters s then Delete(Small s,Small "x") else Delete(Small "x",Small "x")
  | Delete _ as t -> t
  | Accept(Small s) ->
      if StringSet.mem special_letters s then Accept(Small s) else Accept(Small "x")
  | Accept _ as t -> t

let group_typical_letters2 = function
  | Transpose(Small _,Small t) ->
      if StringSet.mem special_letters t then Transpose(Small "x",Small t) else Transpose(Small "x",Small "x")
  | Transpose _ as t -> t
  | Substitute(Small s,Small t) ->
      if StringSet.mem special_letters t then
        if Xlist.mem (StringMap.find special_letter_pairs t) s then
         Substitute(Small s,Small t)
        else Substitute(Small "x",Small t)
      else Substitute(Small "x",Small "x")
  | Substitute _ as t -> t
  | Insert(_, Small t) ->
      if StringSet.mem special_letters t then Insert(Small "x",Small t) else Insert(Small "x",Small "x")
  | Insert _ as t -> t
  | Delete(_, Small t) ->
      if StringSet.mem special_letters t then Delete(Small "x",Small t) else Delete(Small "x",Small "x")
  | Delete _ as t -> t
  | Accept(Small s) ->
      if StringSet.mem special_letters s then Accept(Small s) else Accept(Small "x")
  | Accept _ as t -> t

let match_sign pat = function
    Transpose(s,t) -> s = pat
  | Substitute(s,t) -> s = pat
  | Insert(s,t) -> s = pat
  | Delete(s,t) -> s = pat
  | Accept(s) -> s = pat

let match_sign2 pat = function
    Transpose(s,t) -> t = pat
  | Substitute(s,t) -> t = pat
  | Insert(s,t) -> t = pat
  | Delete(s,t) -> t = pat
  | Accept(s) -> s = pat

let match_sign_type_small = function
    Transpose(Small _,t) -> true
  | Substitute(Small _,t) -> true
  | Insert(Small _,t) -> true
  | Delete(Small _,t) -> true
  | Accept(Small _) -> true
  | _ -> false

let match_sign_type_small2 = function
    Transpose(t,Small _) -> true
  | Substitute(t,Small _) -> true
  | Insert(t,Small _) -> true
  | Delete(t,Small _) -> true
  | Accept(Small _) -> true
  | _ -> false

let match_sign_type_small_unpaired = function
    Transpose(Small s,t) -> not (StringSet.mem special_letters s)
  | Substitute(Small s,t) -> not (StringSet.mem special_letters s)
  | Insert(Small s,t) -> not (StringSet.mem special_letters s)
  | Delete(Small s,t) -> not (StringSet.mem special_letters s)
  | Accept(Small s) -> not (StringSet.mem special_letters s)
  | _ -> false

let match_sign_type_small_unpaired2 = function
    Transpose(Small s,Small t) -> not (StringSet.mem special_letters s) && not (StringSet.mem special_letters t)
  | Substitute(Small s,Small t) -> not (StringSet.mem special_letters s) && not (StringSet.mem special_letters t)
  | Insert(Small s,Small t) -> not (StringSet.mem special_letters s) && not (StringSet.mem special_letters t)
  | Delete(Small s,Small t) -> not (StringSet.mem special_letters s) && not (StringSet.mem special_letters t)
  | Accept(Small s) -> not (StringSet.mem special_letters s)
  | _ -> false


let group group_fun qmap =
  EditQMap.fold qmap StringQMap.empty (fun qmap k v ->
    StringQMap.add_val qmap (group_fun k) v)

let group2 group_fun qmap =
  EditQMap.fold qmap EditQMap.empty (fun qmap k v ->
    EditQMap.add_val qmap (group_fun k) v)

let select_subset select_fun qmap =
  EditQMap.fold qmap EditQMap.empty (fun qmap k v ->
    if select_fun k then
      EditQMap.add_val qmap k v else qmap)

let stringqmap_find qmap v =
  try StringQMap.find qmap v with Not_found -> 0

let editqmap_find qmap v =
  try EditQMap.find qmap v with Not_found -> 0

let print_pair_stats (qmap,qmapo,qmapc) a b =
  let a' = to_string a in
  let b' = to_string b in
  let a'' = char_of_classified_char a in
  let b'' = char_of_classified_char b in
  Printf.printf "'%s'o=%d '%s'c=%d '%s'o=%d '%s'c=%d\n"
    a' (stringqmap_find qmapo a'') a' (stringqmap_find qmapc a'')
    b' (stringqmap_find qmapo b'') b' (stringqmap_find qmapc b'');
  Printf.printf "Substitute(%s,%s)=%d Substitute(%s,%s)=%d Accept(%s)=%d Accept(%s)=%d\n"
    a' b' (editqmap_find qmap (Substitute(a,b)))
    b' a' (editqmap_find qmap (Substitute(b,a)))
    a' (editqmap_find qmap (Accept a))
    b' (editqmap_find qmap (Accept b));
  let prob_b = float (stringqmap_find qmapc b'') /. (float (stringqmap_find qmapc a'') +. float (stringqmap_find qmapc b'')) in
  let prob_subs_a_b = float (editqmap_find qmap (Substitute(a,b))) /. float (stringqmap_find qmapo a'') in
  Printf.printf "s2=%f\n" (prob_subs_a_b /. prob_b);
  ()


(* Eksperyment 1:
   obserwacja jakie są częstości przejścia pomiędzy poszczególnymi typami znaków
   przy standardowej odległości edycyjnej *)
(*let _ =
  let lines = parse_chats [] in
  let lines = parse_chats2 lines in
  let qmap,qmapo,qmapc = count_differences std_edit_distance (EditQMap.empty,StringQMap.empty,StringQMap.empty) lines in
  print_endline "pełne rozróżnianie";
  let qmap2 = group string_of_edit qmap in
  print_stringqmap qmap2;
  print_endline "rozróżnianie typów operatorów i typów znaków";
  let qmap2 = group group_sign_types qmap in
  print_stringqmap qmap2;
  print_endline "rozróżnianie typów operatorów";
  let qmap = group group_operator_types qmap in
  print_stringqmap qmap;
  ()*)

(* Wnioski:
   Obserwowane wyniki wskazują na to, że transpozycję i substytucję należy ograniczyć
   do sytuacji gdy obserwowany i poprawny znak są literami.
   [Można by pozostawić substytucję ciapków]
   Z kolei substytucja pomięczy literami wykazuje nadreprezentację zastąpień polskich znaków
   i ich wersji bezogonkowych (w obu kierunkach) *)

(* Eksperyment 2:
   obserwacja jakie są częstości przejścia pomiędzy poszczególnymi typami znaków,
   gdy obserwowana jest litera, przy ograniczeniu trazspozycji i substytucji *)
(* let _ =
  let lines = parse_chats [] in
  let lines = parse_chats2 lines in
  let qmap,qmapo,qmapc = count_differences restricted_edit_distance (EditQMap.empty,StringQMap.empty,StringQMap.empty) lines in
  let qmap = select_subset match_sign_type_small qmap in
  let qmap = group2 group_typical_letters qmap in
  StringSet.iter (StringSet.add special_letters "x") (fun s ->
    print_stringqmap (group string_of_edit (select_subset (match_sign (Small s)) qmap)));
  () *)
(* Wnioski:
   Prawdopodobieństwa transpozycji, insercji i delecji nie zależą od obserwowanych liter
   Prawdopodobieństwo substytucji gdy, nie litery nie są sparowane nie zależy od obserwowanych liter

   Całość to suma dwu rozkładów: losowej transpozycji, insercji i delecji oraz zamiany sparowanych liter

   Trzeba wyliczyć prawdopodobieństwo transpozycji pod warunkiem, że obserwujemy literę *)

(* Eksperyment 3:
   obserwacja jakie są częstości przejścia pomiędzy poszczególnymi typami znaków,
   gdy obserwowana jest litera, przy ograniczeniu trazspozycji i substytucji *)
(* let _ =
  let lines = parse_chats [] in
  let lines = parse_chats2 lines in
  let qmap,qmapo,qmapc = count_differences restricted_edit_distance (EditQMap.empty,StringQMap.empty,StringQMap.empty) lines in
  let qmap = select_subset match_sign_type_small2 qmap in
  let qmap = group2 group_typical_letters2 qmap in
  StringSet.iter (StringSet.add special_letters "x") (fun s ->
    print_stringqmap (group string_of_edit (select_subset (match_sign2 (Small s)) qmap))) *)
(* Wnioski:
   Prawdopodobieństwa transpozycji, insercji i delecji nie zależą od obserwowanych liter
   Prawdopodobieństwo substytucji gdy, nie litery nie są sparowane nie zależy od obserwowanych liter

   Zakładam następujący model
   P(I(o,c))=i*P(o)*P(c)
   P(D(o,c))=d*P(o)*P(c)
   P(T(o,c))=t*P(o)*P(c)
   P(S1(o,c))=s1*P(o)*P(c)
   S1 to substytucja niesparowanych liter. *)

(* Eksperyment 4:
   wyliczenie i,d,t oraz s1 przy założeniu, że o i c są literami *)
(* let _ =
  let lines = parse_chats [] in
  let lines = parse_chats2 lines in
  let qmap,qmapo,qmapc = count_differences restricted_edit_distance (EditQMap.empty,StringQMap.empty,StringQMap.empty) lines in
  print_endline "rozkład dla wszystkich liter";
  let qmap = select_subset match_sign_type_small qmap in
  let qmap = select_subset match_sign_type_small2 qmap in
  let qmap2 = group group_operator_types qmap in
  print_stringqmap qmap2;
  print_endline "rozkład dla liter niesparowanych";
  let qmap = select_subset match_sign_type_small_unpaired qmap in
  let qmap2 = group group_operator_types qmap in
  print_stringqmap qmap2;
  print_endline "rozkład dla liter niesparowanych po obu stronach";
  let qmap = select_subset match_sign_type_small_unpaired2 qmap in
  let qmap2 = group group_operator_types qmap in
  print_stringqmap qmap2;
  () *)
(* Wnioski:
   przyjmuję następujące wartości:
   i=0.000524, d=0.000167, t=0.000091, s1=0.000278
   prawdopodobieństwo akceptacji dla liter niesparowanych a=0,99894 *)

(* Eksperyment 5:
   badanie substytucji dla sparowanych liter *)
(* let _ =
  let lines = parse_chats [] in
  let lines = parse_chats2 lines in
  let qmap,qmapo,qmapc = count_differences restricted_edit_distance (EditQMap.empty,StringQMap.empty,StringQMap.empty) lines in
  StringMap.iter special_letter_pairs (fun a l ->
    Xlist.iter l (fun b ->
      print_pair_stats (qmap,qmapo,qmapc) (Small a) (Small b))) *)
(* Wnioski:
   litery a, c, e, l, n, o, s, z zastępują swoje ogonkowe wersje
   zjawisko to nie zachodzi w drugą stronę
   zastępowanie można przybliżyć rozkładem:
   P(S2(o,c'))=s2*P(o)*P(c')/(P(c)+P(c'))
   o i c to ten sam znak tylko raz obserwowany, a drugi raz poprawny
   c' to ogonkowa wersja znaku. *)

(* Eksperyment 5:
   obliczenie wartości s2 *)
(*let _ =
  let lines = parse_chats [] in
  let lines = parse_chats2 lines in
  let qmap,qmapo,qmapc = count_differences restricted_edit_distance (EditQMap.empty,StringQMap.empty,StringQMap.empty) lines in
  let substs = Xlist.fold special_letter_pairs_list 0. (fun sum (a,b) ->
    float (editqmap_find qmap (Substitute(Small a, Small b))) +. sum) in
  let means = Xlist.fold special_letter_pairs_list 0. (fun sum (a,b) ->
    (float (stringqmap_find qmapo a) *. float (stringqmap_find qmapc b) /. (float (stringqmap_find qmapc a) +. float (stringqmap_find qmapc b))) +. sum) in
  Printf.printf "s2=%f\n%!" (substs /. means);
  Xlist.iter special_letter_pairs_list (fun (a,b) ->
    Printf.printf "#%s/(#%s+#%s)=%f\n%!" b a b (float (stringqmap_find qmapc b) /. (float (stringqmap_find qmapc a) +. float (stringqmap_find qmapc b))))*)
(* Wnioski:
   s2=0.033395
   #ą/(#a+#ą)=0.066465
   #ć/(#c+#ć)=0.148455
   #ę/(#e+#ę)=0.142284
   #ł/(#l+#ł)=0.463547
   #ń/(#n+#ń)=0.023364
   #ó/(#o+#ó)=0.103772
   #ś/(#s+#ś)=0.098700
   #ź/(#z+#ź)=0.006494
   #ż/(#z+#ż)=0.094648
   *)

(* Eksperyment 6:
   obliczenie prawdopodobieństw poszczególnych symboli w rozkładzie poprawnym *)
(* let _ =
  let lines = parse_chats [] in
  let lines = parse_chats2 lines in
  let qmap,qmapo,qmapc = count_differences restricted_edit_distance (EditQMap.empty,StringQMap.empty,StringQMap.empty) lines in
  print_stringqmap qmapc;
  ocaml_print_stringqmap qmapc *)
(* Wnioski:
   uzyskany rozkład
   66017 0.118815
      58 0.000104 !
     597 0.001074 "  "
       4 0.000007 $
       2 0.000004 %
       7 0.000013 &
      15 0.000027 '
     100 0.000180 (
     168 0.000302 )
       4 0.000007 *
      14 0.000025 +
    7727 0.013907 ,
     709 0.001276 -
    5623 0.010120 .
    1005 0.001809 /
    3557 0.006402 0
    3492 0.006285 1
    1700 0.003060 2
    1159 0.002086 3
    1336 0.002404 4
    1832 0.003297 5
     616 0.001109 6
     798 0.001436 7
     733 0.001319 8
     436 0.000785 9
    5458 0.009823 :
      29 0.000052 ;
       1 0.000002 <
       1 0.000002 >
    3620 0.006515 ?
     166 0.000299 @
       9 0.000016 [
       9 0.000016 ]
       3 0.000005 _
   44426 0.079956 a
    5633 0.010138 b
   20122 0.036215 c
   15549 0.027985 d
   36332 0.065389 e
    1149 0.002068 f
    6231 0.011214 g
    4378 0.007879 h
   34093 0.061359 i
   10935 0.019680 j
   14920 0.026852 k
    8771 0.015786 l
   14577 0.026235 m
   27087 0.048750 n
   29675 0.053408 o
   14350 0.025827 p
       7 0.000013 q
   23323 0.041976 r
   19268 0.034678 s
   17530 0.031550 t
   13522 0.024336 u
      74 0.000133 v
   15236 0.027421 w
      69 0.000124 x
   17351 0.031228 y
   24784 0.044605 z
    3436 0.006184 ó
    3163 0.005693 ą
    3508 0.006314 ć
    6027 0.010847 ę
    7579 0.013640 ł
     648 0.001166 ń
    2110 0.003798 ś
     162 0.000292 ź
    2591 0.004663 ż
       1 0.000002 ”
       1 0.000002 „
       1 0.000002 …
       1 0.000002 ☺
       1 0.000002 子
       1 0.000002 椅
       1 0.000002 🙂
  555628 *)

(* Eksperyment 7:
   obserwacja jakie są częstości przejścia pomiędzy poszczególnymi typami znaków,
   gdy znaki poszczególnych typów są scalone, przy ograniczeniu trazspozycji i substytucji *)
(* let _ =
  let lines = parse_chats [] in
  let lines = parse_chats2 lines in
  let qmap,qmapo,qmapc = count_differences restricted_edit_distance (EditQMap.empty,StringQMap.empty,StringQMap.empty) lines in
  let qmap = group2 group_sign_types2 qmap in
  Xlist.iter ["x";"y";"d";"s";" "] (fun s ->
    print_stringqmap (group string_of_edit (select_subset (match_sign (Small s)) qmap)));
  print_endline "";
  Xlist.iter ["x";"y";"d";"s";" "] (fun s ->
    print_stringqmap (group string_of_edit (select_subset (match_sign2 (Small s)) qmap))) *)
(* Wnioski:
   uznaję, że rozkład jest taki jak dla liter
   z uwagi na to, że symbole inne niż litery będą poddawane działaniu algorytmu
   tylko wtedy, gdy znajdą się w leksykonie.

   Wyjątkiem jest spacja, która ma znaczenie przy określaniu końców słów. *)

let _ =
  (*let l = ENIAMfuzzyDetector.count_differences probabilistic_edit_distance (string_to_array2 observed) (string_to_array2 correct) in
  Printf.printf "%s\n" (String.concat " " (Xlist.map l string_of_edit));
  let l = ENIAMfuzzyDetector.count_differences2 probabilistic_edit_distance (string_to_array2 observed) (string_to_array2 correct) in
  Printf.printf "%s\n" (String.concat " " (Xlist.map l string_of_edit));
  let l = ENIAMfuzzyDetector.count_differences3 probabilistic_edit_distance (string_to_array2 observed) (Array.to_list (string_to_array2 correct)) in
  Printf.printf "%s\n" (String.concat " " (Xlist.map l string_of_edit));
  let l = ENIAMfuzzyDetector.count_differences4 probabilistic_edit_distance (string_to_array2 observed) (Array.to_list (string_to_array2 correct)) in
  Printf.printf "%s\n" (String.concat " " (Xlist.map l string_of_edit));
  print_endline "count_differences5 3";
  let l = ENIAMfuzzyDetector.count_differences5 3 probabilistic_edit_distance (string_to_array2 observed) (Array.to_list (string_to_array2 correct)) in
  Printf.printf "%s\n" (String.concat " " (Xlist.map l string_of_edit));
  print_endline "count_differences5 2";
  let l = ENIAMfuzzyDetector.count_differences5 2 probabilistic_edit_distance (string_to_array2 observed) (Array.to_list (string_to_array2 correct)) in
  Printf.printf "%s\n" (String.concat " " (Xlist.map l string_of_edit));
  print_endline "count_differences5 1";
  let l = ENIAMfuzzyDetector.count_differences5 1 probabilistic_edit_distance (string_to_array2 observed) (Array.to_list (string_to_array2 correct)) in
  Printf.printf "%s\n" (String.concat " " (Xlist.map l string_of_edit));
  print_endline "count_differences5 0";
  let l = ENIAMfuzzyDetector.count_differences5 0 probabilistic_edit_distance (string_to_array2 observed) (Array.to_list (string_to_array2 correct)) in
  Printf.printf "%s\n" (String.concat " " (Xlist.map l string_of_edit));
  print_endline "count_differences6 3";
  let l = ENIAMfuzzyDetector.count_differences6 3 probabilistic_edit_distance (string_to_array2 observed) (Array.to_list (string_to_array2 correct)) in
  Printf.printf "%s\n" (String.concat " " (Xlist.map l string_of_edit));
  print_endline "count_differences6 2";
  let l = ENIAMfuzzyDetector.count_differences6 2 probabilistic_edit_distance (string_to_array2 observed) (Array.to_list (string_to_array2 correct)) in
  Printf.printf "%s\n" (String.concat " " (Xlist.map l string_of_edit));
  print_endline "count_differences6 1";
  let l = ENIAMfuzzyDetector.count_differences6 1 probabilistic_edit_distance (string_to_array2 observed) (Array.to_list (string_to_array2 correct)) in
  Printf.printf "%s\n" (String.concat " " (Xlist.map l string_of_edit));
  print_endline "count_differences6 0";
  let l = ENIAMfuzzyDetector.count_differences6 0 probabilistic_edit_distance (string_to_array2 observed) (Array.to_list (string_to_array2 correct)) in
  Printf.printf "%s\n" (String.concat " " (Xlist.map l string_of_edit));*)
  (* let dict = ENIAMfuzzyDetector.prepare_dict "data/slownik1.tab" in
  let l = ENIAMfuzzyDetector.count_differences7 3 probabilistic_edit_distance (string_to_array2 observed1) dict in
  Xlist.iter l (fun (cost,l,s) -> Printf.printf "%s %f %s\n" s cost (String.concat " " (Xlist.map l string_of_edit)));
  print_endline "";
  let dict = ENIAMfuzzyDetector.prepare_dict "data/slownik1.tab" in
  let l = ENIAMfuzzyDetector.count_differences8 3 40 5. probabilistic_edit_distance probabilistic_prev_cost (string_to_array2 observed1) dict in
  Xlist.iter l (fun (cost,l,s) -> Printf.printf "%s %f %s\n" s cost (String.concat " " (Xlist.map l string_of_edit))); *)
  (*let dict = ENIAMfuzzyDetector.prepare_dict "data/slownik2.tab" in
  let l = ENIAMfuzzyDetector.count_differences9 3 40 500. probabilistic_edit_distance probabilistic_prev_cost (string_to_array3 observed) dict in
  Xlist.iter l (fun (cost,l,s) -> Printf.printf "%s %f %s\n" (String.concat "" s) cost (String.concat " " (Xlist.map l string_of_edit)));
  print_endline "";
  let word_dict,sign_dict = ENIAMfuzzyDetector.prepare_dicts "data/words_simple.tab" "data/signs_simple.tab" in
  let l = ENIAMfuzzyDetector.count_differences10 3 40 500. probabilistic_edit_distance probabilistic_prev_cost (string_to_array3 observed) word_dict sign_dict in
  Xlist.iter l (fun (cost,l,s) -> Printf.printf "%s %f %s\n" (String.concat "" s) cost (String.concat " " (Xlist.map l string_of_edit)));*)
  ()

(* let sgjp_filename = "../../NLP resources/SGJP/sgjp-20170730.tab.gz" *)
let sgjp_filename = "/home/yacheu/Dokumenty/NLP resources/SGJP/sgjp-20170730.tab.gz"

(* Przygotowanie listy słów words.tab *)
(*let _ =
  let lines = parse_chats [] in
  let lines = parse_chats2 lines in
  let set = Xlist.fold lines StringSet.empty (fun set (observed,correct) ->
    let l = Xstring.split "[ -@]\\|[\\-`]\\[{-~]\\|„\\|”\\|¶" (lowercase_utf8_string correct) in (* FIXME: nawiasy kwadratowe *)
    Xlist.fold l set StringSet.add) in
  let set = File.fold_tab sgjp_filename set (fun set -> function
      [] -> set
    | s :: _ -> StringSet.add set (lowercase_utf8_string s)) in
  StringSet.iter set print_endline;
  ()*)

(* Test algorytmu *)
let _ =
  print_endline "Loading chats";
  let lines = parse_chats [] in
  let lines = parse_chats2 lines in
  print_endline "Loading dictionary";
  ENIAMfuzzyDetector.initialize ();
  Xlist.iter lines (fun (observed,correct) ->
    let observed = lowercase_utf8_string observed in
    let correct = lowercase_utf8_string correct in
    if observed = correct then () else (
    print_endline observed;
    print_endline correct;
    let corrected = ENIAMfuzzyDetector.correct observed in
    if correct = corrected then print_endline "Corrected!" else print_endline corrected));
  ()

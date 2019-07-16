(*
 *  ENIAMsubsyntax: tokenization, lemmatization, MWE and sentence detecion for Polish
 *  Copyright (C) 2018 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
 *  Copyright (C) 2018 LekSeek Sp. z o.o. sp. k.
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
open Printf
open SubsyntaxTypes

let html_header =
"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">
<html>
  <head>
	<META HTTP-EQUIV=\"CONTENT-TYPE\" CONTENT=\"text/html; charset=utf8\">
	<TITLE>ENIAM: Kategorialny Parser Składniowo-Semantyczny</TITLE>
	<META HTTP-EQUIV=\"Content-Language\" CONTENT=\"pl\">
  </head>

  <body>
 <center>"

let html_header_title title =
"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">
<html>
  <head>
	<META HTTP-EQUIV=\"CONTENT-TYPE\" CONTENT=\"text/html; charset=utf8\">
	<TITLE>" ^ title ^ "</TITLE>
	<META HTTP-EQUIV=\"Content-Language\" CONTENT=\"pl\">
  </head>

  <body>
 <center>"

let html_trailer =
"</center>
  </body>
</html>"

let escape_html s =
(*   try *)
  let t = Buffer.create (Xstring.size s) in
  Int.iter 0 (String.length s - 1) (fun i ->
    match String.get s i with
       '<' -> Buffer.add_string t "&lt;"
     | '>' -> Buffer.add_string t "&gt;"
     | '&' -> Buffer.add_string t "&amp;"
     | '\n' -> Buffer.add_string t "<BR>"
     | c -> Buffer.add_char t c);
  Buffer.contents t
(*   with e -> failwith ("escape_html: '" ^ s ^ "' " ^ Printexc.to_string e) *)

let get_text_fragment par_string node_mapping node1 node2 =
  let beg = try IntMap.find node_mapping node1 with Not_found -> failwith "get_text_fragment beg" in
  let next = try IntMap.find node_mapping node2 with Not_found -> failwith "get_text_fragment next" in
  if beg >= next then "" else (* FIXME: to się nie powinno przydarzać, prawdopodobnie przyczyną jest <query> i </query> *)
  try String.sub par_string beg (next-beg) with _ -> failwith ("get_text_fragment sub beg=" ^ string_of_int beg ^ " next=" ^ string_of_int next)
  
type marked =
    Chart of (string * string * string list) list
  | Message of string


let excluded_cats = (*StringSet.union Visualization.omited*) (StringSet.of_list [ (* FIXME *)
  "0";"Prep";"Compar";"Comp";"Aglt";"s";"BracketSet";"<root>";"by";"nie";"się";"jak";"int";"hyphen";"qub";"X";"Unknown";"rparen";
  "wieś";"ulica";"osada leśna";"część miejscowości";"astr.";"przysiółek";"nazwisko";"część miasta";
  "imię";"geograficzna";"pseudonim";"gmina wiejska";"osada";"firma";"język programowania";"kolonia";
  "instytucja";"gmina miejska";"miasto";"pora roku";"miesiąc";"krój pisma";"gmina miejsko-wiejska";
  "obszar wiejski";"powiat";"organizacja";"dzielnica";"własna";"marka";"przydomek";"hour-minute";
  "inicjał";"rondo";"tytuł"; ])

let load_colours_of_cats filename map =
  File.fold_tab filename map (fun map -> function
    [cat; colour] -> StringMap.add map cat colour
  | line -> failwith ("load_colours_of_cats: " ^ String.concat "\t" line))

let colours_of_cats = ref StringMap.empty

let initialize () =
  let map = load_colours_of_cats colours_filename StringMap.empty in
  let map = 
    Xlist.fold !SubsyntaxTypes.theories map (fun map theory ->
      File.catch_no_file (load_colours_of_cats (SubsyntaxTypes.theories_path ^ theory ^ "/colours.tab")) map) in
  let map = 
    Xlist.fold !SubsyntaxTypes.user_theories map (fun map theory ->
      File.catch_no_file (load_colours_of_cats (SubsyntaxTypes.user_theories_path ^ theory ^ "/colours.tab")) map) in
  colours_of_cats := map

let rec merge_cat_chart rev = function
    (i,j,s,x) :: (m,n,t,y) :: l ->
      (* printf "i=%d j=%d s=%s m=%d n=%d t=%s\n%!" i j s m n t; *)
      if j=m && s=t then merge_cat_chart rev ((i,n,t,y) :: l)
      else merge_cat_chart ((i,j,s,x) :: rev) ((m,n,t,y) :: l)
  | l -> List.rev (l @ rev)

(**let cat_chart text_fragments g =
  (* print_endline "cat_chart 1"; *)
  let l,last = LCGchart.fold g ([],0) (fun (l,last) (symbol,node1,node2,cost,sem,layer) ->
    (* printf "node1=%d node2=%d symbol=%s\n" node1 node2 (LCGstringOf.grammar_symbol 0 symbol); *)
    (node1,node2,Visualization.extract_pos_cat [] symbol) :: l, max node2 last) in
  let a = Array.make (Array.length g) StringSet.empty in
  Xlist.iter l (fun (node1,node2,cat) ->
    if StringSet.mem excluded_cats cat then () else
    Int.iter node1 (node2 - 1) (fun i ->
      a.(i) <- StringSet.add a.(i) cat));
  let l = List.rev (Int.fold 0 (Array.length g - 1) [] (fun l i ->
    if i >= last then l else
    let cats = List.sort compare (StringSet.to_list a.(i)) in
    (i,i+1,String.concat "|" cats, cats ) :: l)) in
  let l = merge_cat_chart [] l in
  (* print_endline "cat_chart 2"; *)
  List.rev (Xlist.fold l [] (fun l (node1,node2,key,cats) ->
    let t = Visualization.get_text_fragment text_fragments node1 node2 in
    (* if t = "???" then printf "node1=%d node2=%d key=%s cats=[%s]\n%!" node1 node2 key (String.concat ";" cats); *)
    if node1 = node2 then l else
    (t,key,cats) :: l))**)

let cat_chart2 par_string node_mapping tokens paths last =
  (* print_endline "cat_chart 1"; *)
  let l = Xlist.fold paths [] (fun l (id,node1,node2) ->
    let t = ExtArray.get tokens id in
    (node1,node2,Tokenizer.get_cat t.token) :: l) in
  let a = Array.make (last+1) StringSet.empty in
  Xlist.iter l (fun (node1,node2,cat) ->
    (*if StringSet.mem excluded_cats cat then () else*)
    Int.iter node1 (node2 - 1) (fun i ->
      a.(i) <- StringSet.add a.(i) cat));
  let l = List.rev (Int.fold 0 last [] (fun l i ->
    if i >= last then l else
    let cats = List.sort compare (StringSet.to_list a.(i)) in
    (i,i+1,String.concat "|" cats, cats ) :: l)) in
  let l = merge_cat_chart [] l in
  (* print_endline "cat_chart 2"; *)
  List.rev (Xlist.fold l [] (fun l (node1,node2,key,cats) ->
    let t = get_text_fragment par_string node_mapping node1 node2 in
    (* if t = "???" then printf "node1=%d node2=%d key=%s cats=[%s]\n%!" node1 node2 key (String.concat ";" cats); *)
    if node1 = node2 then l else
    (t,key,cats) :: l))

(**let chem_cats cats =
  Xlist.fold cats true (fun b cat ->
(*     print_endline cat; *)
    if cat = "ChemAttr" || cat = "ChemCompound" || cat = "ChemFunGroup" || cat = "ChemFunGroup2" || cat = "ChemMod" || 
      cat = "ChemLatin" || cat = "Extract" || cat = "ExtractAttr"  || cat = "Herb"  || cat = "HerbAdj" || 
      cat = "HerbComponent" || cat = "HerbComponentAttr"  || cat = "HerbLatin" then ((*print_endline "b";*) b) else ((*print_endline "false";*) false))
    
let rec merge_semics rev = function
    [] -> rev
  | ("; ",key1,cats1) :: ("; ",key2,cats2) :: l -> print_endline "cat_chart3 b";  merge_semics rev ((";",key2,cats2) :: l)
  | x :: l -> merge_semics (x :: rev) l
    
let cat_chart3 text_fragments tokens paths last =
  (* print_endline "cat_chart 1"; *)
  let l = Xlist.fold paths [] (fun l (id,node1,node2) ->
    let t = ExtArray.get tokens id in
    (node1,node2,Tokenizer.get_cat t.token) :: l) in
  let a = Array.make (last+1) StringSet.empty in
  Xlist.iter l (fun (node1,node2,cat) ->
    (*if StringSet.mem excluded_cats cat then () else*)
    Int.iter node1 (node2 - 1) (fun i ->
      a.(i) <- StringSet.add a.(i) cat));
  let l = List.rev (Int.fold 0 last [] (fun l i ->
    if i >= last then l else
    let cats = List.sort compare (StringSet.to_list a.(i)) in
    (i,i+1,String.concat "|" cats, cats ) :: l)) in
  let l = merge_cat_chart [] l in
  (* print_endline "cat_chart 2"; *)
  let l = Xlist.fold l [] (fun l (node1,node2,key,cats) ->
    let t = Visualization.get_text_fragment text_fragments node1 node2 in
    (* if t = "???" then printf "node1=%d node2=%d key=%s cats=[%s]\n%!" node1 node2 key (String.concat ";" cats); *)
    if node1 = node2 then l else
    if chem_cats cats then (t,key,cats) :: l
    else ("; ","X",["X"]) :: l) in
  merge_semics [] l**)

let create_styles ll =
  let key_map = Xlist.fold ll StringMap.empty (fun key_map -> function
      (_,_,Message l) -> key_map
    | (_,_,Chart l) ->
        Xlist.fold l key_map (fun key_map (_,key,cats) ->
          let key_map = StringMap.add key_map key cats in
          Xlist.fold cats key_map (fun key_map cat -> 
            StringMap.add key_map cat [cat]))) in
  let key_map = StringMap.remove key_map "" in
  fst (StringMap.fold key_map (StringMap.empty,1) (fun (map,n) key cats ->
    let cats = if cats = ["X"] then cats else Xlist.remove_all cats "X" in
    let colours = List.rev (Xlist.rev_map cats (fun cat ->
      try StringMap.find !colours_of_cats cat with Not_found -> 
(*         print_endline ("create_styles: unknown cat " ^ cat);  *)
        colours_of_cats := StringMap.add !colours_of_cats cat "#cccccc";
        "#cccccc")) in
    let colours,_ = Xlist.fold colours ([],0) (fun (colours,i) colour ->
      (Printf.sprintf "%s %dpx,%s %dpx" colour (i*7) colour ((i+1)*7)) :: colours, i+1) in
    StringMap.add map key ("B" ^ string_of_int n,List.rev colours),n+1))
(*  fst (Xlist.fold ll (StringMap.empty,1) (fun (map,n) -> function
  | (_,_,Message l) -> map,n
  | (_,_,Chart l) ->
    Xlist.fold l (map,n) (fun (map,n) (_,key,cats) ->
      if StringMap.mem map key || key = "" then map,n else
      let colours = List.rev (Xlist.rev_map cats (fun cat ->
        try StringMap.find !colours_of_cats cat with Not_found -> 
          print_endline ("create_styles: unknown cat " ^ cat); 
          colours_of_cats := StringMap.add !colours_of_cats cat "#cccccc";
          "#cccccc")) in
      let colours,_ = Xlist.fold colours ([],0) (fun (colours,i) colour ->
        (Printf.sprintf "%s %dpx,%s %dpx" colour (i*7) colour ((i+1)*7)) :: colours, i+1) in
      StringMap.add map key ("B" ^ string_of_int n,List.rev colours),n+1)))*)

let render_styles styles =
  "<style type=\"text/css\">\n  " ^
  String.concat "\n  " (List.rev (StringMap.fold styles [] (fun l _ (name,colours) ->
    (sprintf ".%s { background-image:repeating-linear-gradient(-45deg,%s); }" name
      (String.concat "," colours)) :: l))) ^
  "</style>"

let assign_style styles (t,key,_) =
(*   print_endline "assign_style"; *)
  if key = "" || t = "" then t else
  let id,_ = try StringMap.find styles key with Not_found -> failwith ("assign_style: " ^ key) in
  if String.get t (Xstring.size t - 1) = ' ' then
    sprintf "<span class=\"%s\">%s</span> " id (escape_html (String.sub t 0 (Xstring.size t - 1)))
  else sprintf "<span class=\"%s\">%s</span>" id (escape_html t)

let rec render_colours_rec = function
    a1 :: a2 :: a3 :: a4 :: a5 :: l -> (String.concat " " [a1;a2;a3;a4;a5] ^ "<br>") :: render_colours_rec l
  | [] -> []
  | l -> [String.concat " " l ^ "<br>"]

let render_colours styles =
  let l = List.rev (StringMap.fold !colours_of_cats [] (fun l cat _ ->
    try
      let id,_ = StringMap.find styles cat in
      (sprintf "<span class=\"%s\">%s</span>" id cat) :: l
    with Not_found -> l)) in
  let l = render_colours_rec l in
    (* (sprintf "<FONT COLOR=\"%s\">%s</FONT><BR>" v cat) :: l) in *)
  String.concat "\n" l

(**
let rec to_string2_paragraph verbosity tokens = function
    RawParagraph s -> []
  | StructParagraph sentences ->
       let l = List.flatten (Xlist.map sentences (fun p -> Visualization.to_string_sentence verbosity tokens p.sentence)) in
       List.rev (Xlist.rev_map l (fun t -> "","",Message t))
  | AltParagraph((Name,RawParagraph name) :: l) ->
       let l = List.flatten (Xlist.map l (fun (mode,paragraph) -> to_string2_paragraph verbosity tokens paragraph)) in
       List.rev (Xlist.rev_map l (fun (_,s,t) -> name,s,t))
  | AltParagraph l -> List.flatten (Xlist.map l (fun (mode,paragraph) -> to_string2_paragraph verbosity tokens paragraph))
  | ErrorParagraph s -> ["","",Message "SubsyntaxError"]

let rec to_string2_text verbosity tokens = function
    RawText s -> []
  | StructText paragraphs -> List.flatten (Xlist.map paragraphs (to_string2_paragraph verbosity tokens))
  | JSONtext s -> []
  | AltText l -> List.flatten (Xlist.map l (fun (mode,text) -> to_string2_text verbosity tokens text))
  | ErrorText s -> ["","",Message "ErrorText"]**)

let rec skip_tag = function
    ">" :: l -> l
  | s :: l -> skip_tag l
  | [] -> []

let rec check_name_length_rec n rev = function
    "<" :: l -> check_name_length_rec n rev (skip_tag l)
  | [s] -> String.concat "" (List.rev (s :: rev))
  | [] -> String.concat "" (List.rev rev)
  | s :: l ->
      if n > 1 then check_name_length_rec (n-1) (s :: rev) l
      else String.concat "" (List.rev ("…" :: rev))

let check_name_length n s =
  let l = Xunicode.utf8_chars_of_utf8_string s in
  check_name_length_rec n [] l
  (* if String.length s > n then
    String.sub s 0 (n-1) ^ "…"
  else s *)

(**let to_string2_simplify name_length= function
    name,_,Message s ->
      if name_length <= 0 then s
      else (check_name_length name_length name) ^ "\t" ^ s
  | _ -> failwith "to_string2_simplify"


let marked_string_of_eniam_sentence verbosity tokens (result : eniam_parse_result) =
  let status_string = Visualization.string_of_status result.status in
  if result.status = NotParsed then
    [status_string, Chart(cat_chart result.text_fragments result.chart1)]
  else [status_string,Message result.msg]**)

let retranslate a i =
  let n = (i-factor) / factor in
(*   Printf.printf "retranslate: |a|=%d i=%d n=%d\n%!" (Array.length a) i n; *)
  if i mod factor < factor / 2 then a.(n)
  else a.(n+1)
  
let create_node_mapping par_string tokens paths = 
(*   print_endline "create_node_mapping 1"; *)
  let l = Xunicode.utf8_chars_of_utf8_string par_string in
  let a = Array.make (Xlist.size l + 1) 0 in
  let _ = Xlist.fold l (0,0) (fun (i,len) c ->
    a.(i+1) <- len + Xstring.size c;
    i+1, len + Xstring.size c) in
(*   print_endline "create_node_mapping 2"; *)
  let node_mapping = Xlist.fold paths IntMap.empty (fun node_mapping (id,lnode,rnode) ->
    let t = ExtArray.get tokens id in
(*     print_endline (SubsyntaxStringOf.string_of_token t.token); *)
    let tbeg,tnext = match t.token with
        Lemma("<query>","interp",_,_) -> t.next,t.next
      | Lemma("</query>","interp",_,_) -> t.beg,t.beg
      | _ -> t.beg,t.next in
(*     print_endline "create_node_mapping 2a"; *)
    let node_mapping = IntMap.add node_mapping lnode (retranslate a tbeg) in
(*     print_endline "create_node_mapping 2b"; *)
    IntMap.add node_mapping rnode (retranslate a tnext)) in
(*   print_endline "create_node_mapping 3"; *)
  node_mapping
  
  
let marked_string_of_struct_sentence par_string tokens paths last =
(*   print_endline "marked_string_of_struct_sentence 1"; *)
  let node_mapping = create_node_mapping par_string tokens paths in
(*   print_endline "marked_string_of_struct_sentence 2"; *)
  let l = ["NotParsed",Chart(cat_chart2 par_string node_mapping tokens paths last)] in
(*   let l = ["NotParsed",Chart(cat_chart3 text_fragments tokens paths last)] in *)
(*   print_endline "marked_string_of_struct_sentence 3"; *)
  l
  
let rec marked_string_of_sentence verbosity par_string tokens = function
    RawSentence s -> (*print_endline s;*) []
  | StructSentence(paths,last) -> marked_string_of_struct_sentence par_string tokens paths last
  | DepSentence paths -> []
(*   | ENIAMSentence result -> marked_string_of_eniam_sentence verbosity tokens result *)
  | QuotedSentences sentences -> List.flatten (Xlist.map sentences (fun p -> marked_string_of_sentence verbosity par_string tokens p.sentence))
  | AltSentence l -> List.flatten (Xlist.map l (fun (mode,sentence) -> marked_string_of_sentence verbosity par_string tokens sentence))
  | ErrorSentence s -> ["SubsyntaxError",Message s]

let rec marked_string_of_paragraph verbosity par_string tokens = function
    RawParagraph s -> []
  | StructParagraph(stats,sentences) ->
       let l = List.flatten (Xlist.map sentences (fun p -> marked_string_of_sentence verbosity par_string tokens p.sentence)) in
       List.rev (Xlist.rev_map l (fun (s,t) -> "",s,t))
  | AltParagraph((Name,RawParagraph name) :: l) ->
        print_endline name; 
       let l = List.flatten (Xlist.map l (fun (mode,paragraph) -> marked_string_of_paragraph verbosity par_string tokens paragraph)) in
       List.rev (Xlist.rev_map l (fun (_,s,t) -> name,s,t))
  | AltParagraph l -> List.flatten (Xlist.map l (fun (mode,paragraph) -> marked_string_of_paragraph verbosity par_string tokens paragraph))
  | ErrorParagraph s -> ["","SubsyntaxError",Message s]

let rec find_paragraph_string mode = function
    RawParagraph s -> if mode = Raw then [s] else []
  | StructParagraph(_,sentences) -> []
  | AltParagraph l -> List.flatten (Xlist.map l (fun (mode,paragraph) -> find_paragraph_string mode paragraph))
  | ErrorParagraph s -> []
  
let rec marked_string_of_text verbosity tokens = function
    RawText s -> []
  | StructText paragraphs -> (*print_endline "marked_string_of_text 1";*) List.flatten (Xlist.map paragraphs (fun paragraph ->
      let par_string = 
        match find_paragraph_string Struct paragraph with
          [par_string] -> par_string 
		| _ -> failwith "marked_string_of_text" in
      marked_string_of_paragraph verbosity par_string tokens paragraph))
(*   | JSONtext s -> [] *)
  | AltText l -> (*print_endline "marked_string_of_text 2";*) List.flatten (Xlist.map l (fun (mode,text) -> marked_string_of_text verbosity tokens text))
  | ErrorText s -> ["","ErrorText",Message s]

let print_html_marked_simple_text path name name_length l =
  File.file_out (path ^ name ^ ".html") (fun file ->
    fprintf file "%s\n" (html_header_title "Marked Text");
(*     print_endline "print_html_marked_text 1"; *)
    (* print_endline "print_html_marked_text 2"; *)
    let styles = create_styles l in
    (* print_endline "print_html_marked_text 3"; *)
    fprintf file "%s\n" (render_styles styles);
    (* print_endline "print_html_marked_text 4"; *)
    fprintf file "%s\n" (render_colours styles);
    (* print_endline "print_html_marked_text 5"; *)
    if name_length <= 0 then
      Xlist.iter l (function
          name, "NotParsed", Chart t -> fprintf file "%s<BR>\n" (String.concat "" (List.rev (Xlist.rev_map t (assign_style styles))));
        | name, status, Chart t -> fprintf file "%s: %s<BR>\n" status (String.concat "" (List.rev (Xlist.rev_map t (assign_style styles))));
        | name, status, Message t -> fprintf file "%s: %s<BR>\n" status (escape_html t))
    else (
      fprintf file "<TABLE border=1>\n";
      Xlist.iter l (function
          name, "NotParsed", Chart t -> fprintf file "<TR><TD>%s</TD><TD>%s</TD><TR>\n" (check_name_length name_length name) (String.concat "" (List.rev (Xlist.rev_map t (assign_style styles))));
        | name, status, Chart t -> fprintf file "<TR><TD>%s</TD><TD>%s: %s</TD><TR>\n" (check_name_length name_length name) status (String.concat "" (List.rev (Xlist.rev_map t (assign_style styles))));
        | name, status, Message t -> fprintf file "<TR><TD>%s</TD><TD>%s: %s</TD><TR>\n" (check_name_length name_length name) status (escape_html t));
      fprintf file "</TABLE>\n");
(*     print_endline "print_html_marked_text 6"; *)
    fprintf file "%s\n" html_trailer)
    
let compare_lines (s1,l1,_,_,_) (s2,l2,_,_,_) =
  match compare s1 s2 with
    0 -> compare l1 l2
  | x -> x
  
let sort_texts ll =
  let ll = Xlist.fold ll [] (fun ll -> function
      name, status, Chart c -> 
        let l = fst (Xlist.fold c ([],[]) (fun (l,rev) (t,key,cats) ->
          let name = if t = "" then escape_html "<empty> | " ^ name else escape_html t ^ " | " ^ name in
          if key = "X" then ((Xunicode.lowercase_utf8_string t),rev,name,status,Chart c) :: l, (Xunicode.lowercase_utf8_string t) :: rev 
          else l, (Xunicode.lowercase_utf8_string t) :: rev)) in
        if l = [] then ("~~~~~",[],name,status,Chart c) :: ll else
        (List.hd (Xlist.sort l compare_lines)) :: ll
    | name, status, Message t -> ("",[],name,status,Message t) :: ll) in
  let ll = Xlist.sort ll compare_lines in
  List.rev (Xlist.rev_map ll (fun (_,_,name,status,t) -> 
    let word,name = match Xstring.split " | " name with
        word :: l -> word, String.concat " | " l
      | _ -> "", name in
    name,word,status,t))
    
let print_html_marked_sorted_text path name name_length l =
  File.file_out (path ^ name ^ ".html") (fun file ->
    fprintf file "%s\n" (html_header_title "Marked Text");
(*      print_endline "print_html_marked_text 1";  *)
    (* print_endline "print_html_marked_text 2"; *)
    let styles = create_styles l in
(*      print_endline "print_html_marked_text 3";  *)
    fprintf file "%s\n" (render_styles styles);
(*      print_endline "print_html_marked_text 4";  *)
    fprintf file "%s\n" (render_colours styles);
(*      print_endline "print_html_marked_text 5";  *)
    let l = sort_texts l in
    if name_length <= 0 then
      Xlist.iter l (function
          name, word, "NotParsed", Chart t -> fprintf file "%s<BR>\n" (String.concat "" (List.rev (Xlist.rev_map t (assign_style styles))));
        | name, word, status, Chart t -> fprintf file "%s: %s<BR>\n" status (String.concat "" (List.rev (Xlist.rev_map t (assign_style styles))));
        | name, word, status, Message t -> fprintf file "%s: %s<BR>\n" status (escape_html t))
    else (
      fprintf file "<TABLE border=1>\n";
      Xlist.iter l (function
          name, word, "NotParsed", Chart t -> 
(*             print_endline name; *)
            fprintf file "<TR><TD>%s</TD><TD>%s</TD><TD>%s</TD><TR>\n" (check_name_length name_length name) word
              (String.concat "" (List.rev (Xlist.rev_map t (assign_style styles))));
        | name, word, status, Chart t -> 
(*             print_endline name; *)
            fprintf file "<TR><TD>%s</TD><TD>%s</TD><TD>%s: %s</TD><TR>\n" (check_name_length name_length name) word
              status (String.concat "" (List.rev (Xlist.rev_map t (assign_style styles))));
        | name, word, status, Message t -> 
(*             print_endline name; *)
            fprintf file "<TR><TD>%s</TD><TD>%s</TD><TD>%s: %s</TD><TR>\n" (check_name_length name_length name) word
              status (escape_html t));
      fprintf file "</TABLE>\n");
(*      print_endline "print_html_marked_text 6";  *)
    fprintf file "%s\n" html_trailer)

(**let rec split_cat_chart_rec rev = function
    [],chart -> rev,chart
  | _,[] -> raise Not_found
  | p :: pat, (a,b,l) :: chart ->
      if p = a || Xlist.mem l p then split_cat_chart_rec ((a,b,l) :: rev) (pat,chart)
      else raise Not_found

let rec split_cat_chart pat found rev chart =
  if chart = [] then if rev = [] then found else (List.rev rev) :: found else
  try
    let matched,rest = split_cat_chart_rec [] (pat,chart) in
    if rev = [] then split_cat_chart pat found matched rest
    else split_cat_chart pat ((List.rev rev) :: found) matched rest
  with Not_found -> split_cat_chart pat found (List.hd chart :: rev) (List.tl chart)

let compare_charts = function
    (a1,"",_), (a2,"",_) -> compare a1 a2
  | (_,b1,_), (_,b2,_) -> compare b1 b2

let rec compare_charts_list = function
    c1 :: l1, c2 :: l2 ->
      let x = compare_charts (c1,c2) in
      if x = 0 then compare_charts_list (l1,l2) else x
  | c1, c2 -> compare c1 c2

let split_and_sort pat l =
  let l = Xlist.fold l [] (fun l -> function
      name,status,Chart chart -> Xlist.fold (split_cat_chart pat [] [] chart) l (fun l chart -> (name,status,Chart chart) :: l)
    | name,status,t -> (name,status,t) :: l) in
  Xlist.sort l (fun (_,_,chart1) (_,_,chart2) ->
    match chart1,chart2 with
      Chart c1, Chart c2 -> compare_charts_list (c1,c2)
    | _ -> compare chart1 chart2)
**)
let omited = StringSet.of_list ["X ";"MWEcomponent "]

let extract_pos_cat tokens id =
  match (ExtArray.get tokens id).token with
    Lemma(lemma,pos,interp,cat) -> cat
  | t -> failwith ("extract_pos_cat: " ^ SubsyntaxStringOf.string_of_token t)

let cat_tokens_sequence par_string tokens paths last =
  if paths = [] then "" else
  let node_mapping = create_node_mapping par_string tokens paths in
  let _,beg,_ = List.hd paths in
  let _,_,t,l = Xlist.fold paths (beg,beg,(-1,beg,beg),[]) (fun (m,n,last,l) (id,node1,node2) ->
    if (ExtArray.get tokens id).orth = "" then m,n,last,l else
    if m > node1 then failwith "cat_tokens_sequence 1" else
    if m = node1 then 
      if n > node2 then m,n,last,l else 
      if n = node2 && StringSet.mem omited (extract_pos_cat tokens id ^ " ") then m,n,last,l else
      node1,node2,(id,node1,node2),l else
    if n > node1 then m,n,last,l else 
    (*if n < node1 then failwith "cat_tokens_sequence 2" else*)
    node1,node2,(id,node1,node2),last :: l) in
  let l = List.tl (List.rev (t :: l)) in
  let l = List.rev (Xlist.rev_map l (fun (id,node1,node2) ->
    let cat = extract_pos_cat tokens id ^ " " in
    if StringSet.mem omited cat then get_text_fragment par_string node_mapping node1 node2
    else cat)) in
  String.concat "" l
    
(*    node1,node2,
    (if m < node1 then
      if n < node1 then [n, node1, get_text_fragment par_string node_mapping n node1, "null"]
      else if n = node1 then []
      else [node1, n, get_text_fragment par_string node_mapping node1 n, "overlap"]
    else if m = node1 then
      if n < node2 then [m, n, get_text_fragment par_string node_mapping m n, "overlap"]
      else if n = node2 then []
      else [node1, node2, get_text_fragment par_string node_mapping node1 node2, "overlap"]
    else failwith "cat_tokens_sequence") @
    [node1, node2, get_text_fragment par_string node_mapping node1 node2, extract_pos_cat tokens id] @ l) in
  let map = Xlist.fold l IntMap.empty (fun map (m,n,text,symbol) ->
    IntMap.add_inc map (1000000*m+n) [text,symbol] (fun l -> (text,symbol) :: l)) in
  let map = IntMap.map map (fun l ->
    let t,ov,set = Xlist.fold l ("",false,StringSet.empty) (fun (t,ov,set) (text,symbol) ->
      if symbol = "null" then text,ov,set
      else if symbol = "overlap" then t,true,set
      else if StringSet.mem omited symbol then text,ov,set
      else t,ov,StringSet.add set symbol) in
    let l = if StringSet.is_empty set then [t] else StringSet.to_list set in
    if ov then "OVERLAP{" ^ String.concat " " l ^ "}" else
    match l with
      [t] -> t
    | _ -> "{" ^ String.concat " " l ^ "}") in
  let l = List.sort compare (IntMap.fold map [] (fun l k texts -> (k,texts) :: l)) in
(*  let l = Xlist.sort l (fun (m1,n1,text1,symbol1) (m2,n2,text2,symbol2) ->
    if m1 <> m2 then compare m1 m2 else
    if n1 <> n2 then compare n1 n2 else
    compare symbol1 symbol2) in
  let l = if l = [] then l else
    Xlist.fold (List.tl l) [List.hd l] (fun l a ->
      match l with
        [] -> failwith "cat_tokens_sequence"
      | b :: l -> if a = b then b :: l else a :: b :: l) in*)
  String.concat "" (Xlist.map l (fun (n,texts) -> texts))*)

let rec cat_tokens_sequence_sentence verbosity par_string tokens = function
    RawSentence s -> (*print_endline s;*) []
  | StructSentence(paths,last) -> [cat_tokens_sequence par_string tokens paths last]
  | DepSentence paths -> []
(*   | ENIAMSentence result -> cat_tokens_sequence_eniam_sentence verbosity tokens result *)
  | QuotedSentences sentences -> List.flatten (Xlist.map sentences (fun p -> cat_tokens_sequence_sentence verbosity par_string tokens p.sentence))
  | AltSentence l -> List.flatten (Xlist.map l (fun (mode,sentence) -> cat_tokens_sequence_sentence verbosity par_string tokens sentence))
  | ErrorSentence s -> ["SubsyntaxError " ^ s]

let rec cat_tokens_sequence_paragraph verbosity par_string tokens = function
    RawParagraph s -> []
  | StructParagraph(stats,sentences) ->
       let l = List.flatten (Xlist.map sentences (fun p -> cat_tokens_sequence_sentence verbosity par_string tokens p.sentence)) in
       List.rev (Xlist.rev_map l (fun s -> "",s))
  | AltParagraph((Name,RawParagraph name) :: l) ->
       print_endline name; 
       let l = List.flatten (Xlist.map l (fun (mode,paragraph) -> cat_tokens_sequence_paragraph verbosity par_string tokens paragraph)) in
       List.rev (Xlist.rev_map l (fun (_,s) -> name,s))
  | AltParagraph l -> List.flatten (Xlist.map l (fun (mode,paragraph) -> cat_tokens_sequence_paragraph verbosity par_string tokens paragraph))
  | ErrorParagraph s -> ["","SubsyntaxError " ^ s]

let rec cat_tokens_sequence_text verbosity tokens = function
    RawText s -> []
  | StructText paragraphs -> (*print_endline "cat_tokens_sequence_text 1";*) List.flatten (Xlist.map paragraphs (fun paragraph ->
      let par_string = 
        match find_paragraph_string Struct paragraph with
          [par_string] -> par_string 
		| _ -> failwith "cat_tokens_sequence_text" in
      cat_tokens_sequence_paragraph verbosity par_string tokens paragraph))
(*   | JSONtext s -> [] *)
  | AltText l -> (*print_endline "cat_tokens_sequence_text 2";*) List.flatten (Xlist.map l (fun (mode,text) -> cat_tokens_sequence_text verbosity tokens text))
  | ErrorText s -> ["","ErrorText " ^ s]
  
let print_cat_tokens_sequence path name name_length sort_sentences_flag par_names_flag l =
  let l = if sort_sentences_flag then Xlist.sort l (fun x y -> compare (snd x) (snd y)) else l in
  let l = List.rev (if par_names_flag then Xlist.rev_map l (fun (a,b) -> a ^ "\t" ^ b) else Xlist.rev_map l snd) in
  File.file_out (path ^ name ^ ".tab") (fun file ->
    Xlist.iter l (Printf.fprintf file "%s\n"))

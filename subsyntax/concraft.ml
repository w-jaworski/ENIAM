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

open Printf
open SubsyntaxTypes
open Xstd

let get_sock_addr host_name port =
  let he =
    try Unix.gethostbyname host_name
    with Not_found -> failwith ("get_sock_addr: host " ^ host_name ^ " not found") in
  let addr = he.Unix.h_addr_list in
  Unix.ADDR_INET(addr.(0),port)

let get_header chan =
  let r = ref [] in
  let f = ref true in
  (try
    while !f do
      let s = input_line chan in
      if s = "\r" then f := false else
      r := s :: (!r)
    done
  with End_of_file -> failwith "get_header");
  List.rev !r

let get_trailer chan =
  let r = ref [] in
  (try
    while true do
      r := (input_line chan) :: (!r)
    done;
    !r
  with End_of_file -> List.rev !r)

let rec get_message rev chan =
  let s = input_line chan in
  let len = Scanf.sscanf s "%x\r" (fun x -> x) in
  if len = 0 then String.concat "" (List.rev rev) else
  (* printf "len=%d\n" len; *)
  let s = really_input_string chan len in
  let n = input_line chan in
  if n <> "\r" then failwith "get_message" else
  get_message (s :: rev) chan

let render l =
  String.concat "\n" (List.rev (Xlist.rev_map l (fun (i,j,orth,lemma,interp,cat,cat2,prob,x1,x2) ->
    String.concat "\t" [i;j;orth;lemma;interp;cat;cat2;prob;x1;x2])))

let encode s =
  String.concat "" (List.rev (Xlist.rev_map (Xunicode.utf8_chars_of_utf8_string s) (function
      "\r" -> "\\r"
    | "\t" -> "\\t"
    | "\n" -> "\\n"
    | "\\" -> "\\\\"
    | "\"" -> "\\\""
    | c ->
      if String.length c = 1 then c else
      (match Xunicode.unicode_chars_of_utf8_string c with
        [c] -> sprintf "\\u%04x" c
      | _ -> failwith "encode"))))

let send_message file host port msg =
  fprintf file "POST /parse HTTP/1.1\r\n";
  fprintf file "Host: %s:%d\r\n" host port;
  fprintf file "Content-Length: %d\r\n" (String.length msg);
  fprintf file "User-Agent: python-requests/1.9.1\r\n";
  fprintf file "Connection: keep-alive\r\n";
  fprintf file "Accept: */*\r\n";
  fprintf file "Accept-Encoding: gzip, deflate\r\n";
  fprintf file "\r\n";
  fprintf file "%s\n" msg;
  flush file

let analyse_header = function
    "HTTP/1.1 200 OK\r" :: header ->
      let header = Xlist.rev_map header (fun s ->
        if Xstring.check_sufix "\r" s then Xstring.cut_sufix "\r" s else failwith ("analyse_header 3: " ^ s)) in
      let header = Xlist.rev_map header (fun s ->
        match Xstring.split ": " s with
          [k;v] -> k,v
        | _ -> failwith ("analyse_header 2: " ^ s)) in
      Xlist.iter header (function
          "Transfer-Encoding","chunked" -> ()
        | "Date",_ -> ()
        | "Server","Warp/3.2.11.2" -> ()
        | "Content-Type","application/json; charset=utf-8" -> ()
        | k,v -> failwith ("analyse_header 4: " ^ k ^ ": " ^ v))
  | header -> failwith ("analyse_header 1: " ^ String.concat "\n" header)

let analyse_trailer = function
    ["\r"] -> ()
  | l ->
    Xlist.iter l print_endline;
    failwith "analyse_trailer"
    (* failwith ("analyse_trailer: " ^ String.concat "\n" l) *)

let analyse_message s =
  let s = if Xstring.check_prefix "{\"dag\":\"" s then Xstring.cut_prefix "{\"dag\":\"" s else failwith ("analyse_message 1: " ^ s) in
  let s = if Xstring.check_sufix "\\n\\n\"}" s then Xstring.cut_sufix "\\n\\n\"}" s else failwith ("analyse_message 2: " ^ s) in
  List.rev (Xlist.rev_map (Xstring.split "\\\\n" s) (fun line ->
   (* print_endline line; *)
   match Xstring.split "\\\\t" line with
      [i;j;orth;lemma;interp;id;"";prob;"";""] -> int_of_string id, interp, float_of_string prob, false, false
    | [i;j;orth;lemma;interp;id;"";prob;"";"eos"] -> int_of_string id, interp, float_of_string prob, true, false
    | [i;j;orth;lemma;interp;id;"";prob;"";"";"disamb"] -> int_of_string id, interp, float_of_string prob, false, true
    | [i;j;orth;lemma;interp;id;"";prob;"";"eos";"disamb"] -> int_of_string id, interp, float_of_string prob, true, true
    | _ -> failwith ("analyse_message: " ^ line)))

let process_query host_name port query =
  let sock = get_sock_addr host_name port in
  let ic,oc =
    try Unix.open_connection sock
    with e -> failwith ("server connection error: " ^ Printexc.to_string e) in
  let query = "{\"dag\": \"" ^ encode (render query) ^ "\\n\\n\"}" in
  send_message oc host_name port query;
  analyse_header (get_header ic);
  let message = get_message [] ic in
  (* let answer = get_answer ic in
  Xlist.iter answer print_endline; *)
  analyse_trailer (get_trailer ic);
  (*Printf.fprintf oc "\n%!";*)
  let _ = Unix.shutdown_connection ic in
  analyse_message message

let std_pos = StringSet.of_list
        ["subst";"depr";"ppron12";"ppron3";"siebie";"prep";"num";"numcomp";"adj";"adjc";"adjp";"adja";
          "adv";"ger";"pact";"ppas";"fin";"bedzie";"praet";"winien";"impt";
          "imps";"pred";"aglt";"inf";"pcon";"pant";"pacta";"qub";"comp";"conj";"interj";
          "burk";"interp"]

let pos_map = Xlist.fold [ (* FIXME !!! *)
  "fixed","brev:npun" ;
  "initial","brev:npun" ;
  "intnum","dig";
  "realnum","dig";
  "intnum-interval","dig";
  "realnum-interval","dig";
  "symbol","dig";
  "ordnum","dig";
  "date","dig";
  "date-interval","dig";
  "hour-minute","dig";
  "hour","dig";
  "hour-minute-interval","dig";
  "hour-interval","dig";
  "year","dig";
  "year-interval","dig";
  "day","dig";
  "day-interval","dig";
  "day-month","dig";
  "day-month-interval","dig";
  "month-interval","dig";
  "roman","romandig";
  "roman-interval","romandig";
  "roman-ordnum","romandig";
  "match-result","dig";
  "url","dig";
  "email","dig";
  "phone-number","dig";
  "postal-code","dig";
  "obj-id","dig";
  "building-number","dig";
  "list-item","dig";
  "sinterj","interp";
  "xxx","brev:npun" ;
  "unk","brev:npun" ;
  "html-tag","interp";
  "apron","adj";
  "compar","prep";
  "x","brev:npun" ;
  "other","brev:npun" ;
  "part","qub"
  ] StringMap.empty (fun map (k,v) -> StringMap.add map k v)

let process_paths paths =
  let paths,next_id = Xlist.fold paths ([],0) (fun (paths,next_id) t -> (next_id,t) :: paths, next_id+1) in
  let msg_struct = Xlist.fold paths [] (fun msg_struct (id,t) ->
    let l = match t.token with
        Lemma(lemma,pos,interp,_) ->
          let pos =
            if StringSet.mem std_pos pos then pos else
            try StringMap.find pos_map pos with Not_found -> failwith "process_paths" in
          Xlist.rev_map (Tagset.expand interp) (function
                [] -> (lemma,pos)
              | tags -> (lemma,pos ^ ":" ^ String.concat ":" tags))
      | Interp lemma -> [lemma,"interp"]
      | Other lemma  -> [lemma,"brev:npun"]
      | t -> failwith ("process_paths: " ^ SubsyntaxStringOf.string_of_token t) in
    Xlist.fold l msg_struct (fun msg_struct (lemma,interp) ->
      (string_of_int t.beg, string_of_int t.next, t.orth, lemma, interp, string_of_int id, "", "0.000",	 "",	 "") :: msg_struct)) in
  let result = process_query !SubsyntaxTypes.concraft_host_name !SubsyntaxTypes.concraft_port msg_struct in
  let a = Array.make next_id empty_token_env in
  Xlist.iter paths (fun (id,t) -> a.(id) <- t);
  Xlist.iter result (fun (id,interp,prob,eos,disamb) ->
    a.(id) <- {a.(id) with tagger_output=(interp,prob,eos,disamb) :: a.(id).tagger_output});
  Array.to_list a

let rec disambiguate_rec rev prev_beg beg paths last =
  if beg = last then rev else
  if paths = [] then failwith "disambiguate_rec 1" else
  let t = List.hd paths in
  let l = Xlist.fold t.tagger_output [] (fun l (interp,prob,eos,disamb) ->
    if disamb then (interp,prob,eos,disamb) :: l else l) in
  match l with
    [] -> disambiguate_rec rev prev_beg beg (List.tl paths) last
  | [_] ->
      if t.beg = beg then
        disambiguate_rec ({t with tagger_output=l} :: rev) beg t.next (List.tl paths) last
      else if t.beg = prev_beg && t.next = beg then
        disambiguate_rec ({t with tagger_output=l} :: rev) prev_beg beg (List.tl paths) last
      else failwith "disambiguate_rec 2"
  | _ -> failwith "disambiguate_rec 3"

(* To korzystamy z tego, że w pos_map są tylko kategorie bez tagów fleksyjnych *)
let disambiguate paths last =
  let paths = disambiguate_rec [] (-1) 0 paths last in
  Xlist.rev_map paths (fun t ->
    match t.token with
      Lemma(lemma,pos,_,cat) ->
        let interp,_,_,_ = List.hd t.tagger_output in
        let interp = match Tagset.parse interp with [_,interp] -> interp | _ -> failwith "disambiguate" in
        {t with token=Lemma(lemma,pos,[interp],cat)}
    | Interp _ -> t
    | Other _ -> t
    | _ -> failwith "disambiguate")


(* uruchamianie serwera
./concraft-dag2-pl server --port=4322 -i ./concraft-pl-model-180317.gz +RTS -N4
*)

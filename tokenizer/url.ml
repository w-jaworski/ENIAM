(*
 *  Copyright (C) 2018 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
 *)

open Xunicode
open Xstd

type t =
    D of unicode list
  | S of string * unicode
  | L of string * unicode
  | O of unicode
  | TopLevelDomain of t list
  | Host of unicode list
  | IP4 of unicode list
  | Email of unicode list

let letters = StringSet.of_list [
  "a"; "b"; "c"; "d"; "e"; "f";
  "g"; "h"; "i"; "j"; "k"; "l";
  "m"; "n"; "o"; "p"; "q"; "r";
  "s"; "t"; "u"; "v"; "w"; "x";
  "y"; "z"]

let load_top_level_domains () =
  StringSet.of_list (File.catch_no_file (fun _ -> File.load_lines TokenizerTypes.top_level_domains_filename) [])

let top_level_domains = ref StringSet.empty

let rec to_string = function
    D d -> Printf.sprintf "D[%s]" (String.concat ";" (Xlist.map d Xunicode.to_string))
  | S(s,a) -> Printf.sprintf "S(%s,%s)" s (Xunicode.to_string a)
  | L(s,a) -> Printf.sprintf "L(%s,%s)" s (Xunicode.to_string a)
  | O a -> Printf.sprintf "O(%s)" (Xunicode.to_string a)
  | IP4 x -> Printf.sprintf "IP4[%s]" (String.concat ";" (Xlist.map x Xunicode.to_string))
  | TopLevelDomain l -> Printf.sprintf "TopLevelDomain[%s]" (String.concat ";" (Xlist.map l to_string))
  | Host x -> Printf.sprintf "Host[%s]" (String.concat ";" (Xlist.map x Xunicode.to_string))
  | Email x -> Printf.sprintf "Email[%s]" (String.concat ";" (Xlist.map x Xunicode.to_string))

let print_list s l =
  Printf.printf "%s: %s\n%!" s (String.concat " " (Xlist.map l to_string))

let escape = function
    Digit s as a -> D[a]
  | Sign s as a -> S(s,a)
  | Capital(uc,lc) as a -> if StringSet.mem letters lc then L(lc,a) else O a
  | ForeignCapital(uc,lc) as a -> if StringSet.mem letters lc then L(lc,a) else O a
  | Small(uc,lc) as a -> if StringSet.mem letters lc then L(lc,a) else O a
  | ForeignSmall(uc,lc) as a -> if StringSet.mem letters lc then L(lc,a) else O a
  | Emoticon s as a -> O a
  | Other(s,x) as a -> O a

let extract = function
    Digit s -> s
  | Sign s -> s
  | Capital(uc,lc) -> uc
  | ForeignCapital(uc,lc) -> uc
  | Small(uc,lc) -> lc
  | ForeignSmall(uc,lc) -> lc
  | Emoticon s -> s
  | Other(s,x) -> s

let de_escape = function
    D d -> d
  | S(_,a) -> [a]
  | L(_,a) -> [a]
  | O a -> [a]
  | IP4 x -> x
  | TopLevelDomain _ -> failwith "de_escape"
  | Host x -> [Other("url",Xlist.size x);Sign(String.concat "" (List.rev (Xlist.rev_map x extract)))]
  | Email x -> [Other("email",Xlist.size x);Sign(String.concat "" (List.rev (Xlist.rev_map x extract)))]

let de_escape_list l =
  List.flatten (List.rev (Xlist.rev_map l de_escape))

let rec merge_digits rev cur_rev = function
    D d :: l -> merge_digits rev (d @ cur_rev) l
  | a :: l ->
      if cur_rev = [] then merge_digits (a :: rev) [] l else
      merge_digits (a :: D(List.rev cur_rev) :: rev) [] l
  | [] -> if cur_rev = [] then List.rev rev else
      List.rev (D(List.rev cur_rev) :: rev)

let rec find_top_level_domains rev cur cur_rev = function
    S(".",a) :: l ->
      (* Printf.printf "find_top_level_domains S.: rev=[%s] cur=%s cur_rev=[%s]\n%!" (String.concat ";" (Xlist.map rev to_string)) cur (String.concat ";" (Xlist.map cur_rev to_string)); *)
      let rev = if StringSet.mem !top_level_domains cur then TopLevelDomain(List.rev cur_rev) :: rev else cur_rev @ rev in
      find_top_level_domains rev "." [S(".",a)] l
  | S(s,a) :: l ->
      (* Printf.printf "find_top_level_domains S: %s rev=[%s] cur=%s cur_rev=[%s]\n%!" s (String.concat ";" (Xlist.map rev to_string)) cur (String.concat ";" (Xlist.map cur_rev to_string)); *)
      let rev = if StringSet.mem !top_level_domains cur then TopLevelDomain(List.rev cur_rev) :: rev else cur_rev @ rev in
      find_top_level_domains ((S(s,a)) :: rev) "" [] l
  | L(s,a) :: l ->
      (* Printf.printf "find_top_level_domains L: %s rev=[%s] cur=%s cur_rev=[%s]\n%!" s (String.concat ";" (Xlist.map rev to_string)) cur (String.concat ";" (Xlist.map cur_rev to_string)); *)
      if cur = "" then find_top_level_domains ((L(s,a)) :: rev) cur cur_rev l else
      find_top_level_domains rev (cur ^ s) ((L(s,a)) :: cur_rev) l
  | D d :: l ->
      (* Printf.printf "find_top_level_domains D: rev=[%s] cur=%s cur_rev=[%s]\n%!" (String.concat ";" (Xlist.map rev to_string)) cur (String.concat ";" (Xlist.map cur_rev to_string)); *)
      find_top_level_domains ((D d) :: cur_rev @ rev) "" [] l
  | O a :: l ->
      (* Printf.printf "find_top_level_domains O: rev=[%s] cur=%s cur_rev=[%s]\n%!" (String.concat ";" (Xlist.map rev to_string)) cur (String.concat ";" (Xlist.map cur_rev to_string)); *)
      find_top_level_domains ((O a) :: cur_rev @ rev) "" [] l
  | [] ->
      (* Printf.printf "find_top_level_domains e: rev=[%s] cur=%s cur_rev=[%s]\n%!" (String.concat ";" (Xlist.map rev to_string)) cur (String.concat ";" (Xlist.map cur_rev to_string)); *)
      let rev = if StringSet.mem !top_level_domains cur then TopLevelDomain(List.rev cur_rev) :: rev else cur_rev @ rev in
      List.rev rev
  | _ -> failwith "find_top_level_domains"

let rec find_host_rev x = function
    S("-",a) :: l -> find_host_rev (a :: x) l
  | L(s,a) :: l -> find_host_rev (a :: x) l
  | D d :: l -> find_host_rev (d @ x) l
  | Host y :: l -> find_host_rev (y @ x) l
  | S(".",a) :: S("-",b) :: l -> find_host_rev (b :: a :: x) l
  | S(".",a) :: L(s,b) :: l -> find_host_rev (b :: a :: x) l
  | S(".",a) :: D d :: l -> find_host_rev (d @ a :: x) l
  | S(".",a) :: Host y :: l -> find_host_rev (y @ a :: x) l
  | l -> Host x :: l

let rec find_host rev = function
    TopLevelDomain x :: l -> find_host (match rev with
            S("-",a) :: _ -> find_host_rev (de_escape_list x) rev
          | L(s,a) :: _ -> find_host_rev (de_escape_list x) rev
          | D d :: _ -> find_host_rev (de_escape_list x) rev
          | Host y :: rev2 -> Host(y @ (de_escape_list x)) :: rev2
          | rev -> List.rev x @ rev) l
  | a :: l -> find_host (a :: rev) l
  | [] -> List.rev rev

let rec check_host rev = function
    Host x :: l ->
      (* print_endline "check_host 1"; *)
      let hostname = String.concat "" (List.rev (Xlist.rev_map x extract)) in
      (* print_endline hostname; *)
      if hostname = "m.in" || hostname = "m.st" || hostname = "b.zm" then
        check_host (Xlist.rev_map x escape @ rev) l
      else check_host (Host x :: rev) l
  | a :: l -> (*print_endline "check_host 2";*) check_host (a :: rev) l
  | [] -> List.rev rev

let rec find_ip4 rev = function
    D d1 :: S(".",a1) :: D d2 :: S(".",a2) :: D d3 :: S(".",a3) :: D d4 :: l ->
      find_ip4 (IP4(d1 @ [a1] @ d2 @ [a2] @ d3 @ [a3] @ d4) :: rev) l
  | a :: l -> find_ip4 (a :: rev) l
  | [] -> List.rev rev

let rec find_email_rev x = function
    D d :: l -> find_email_rev (d @ x) l
  | L(_,a) :: l -> find_email_rev (a :: x) l
  | S(".",a) :: l -> find_email_rev (a :: x) l
  | S("-",a) :: l -> find_email_rev (a :: x) l
  | S("_",a) :: l -> find_email_rev (a :: x) l
  | S("$",a) :: l -> find_email_rev (a :: x) l
  | l -> Email x :: l

let rec find_email rev = function
    S("@",a) :: Host x :: l -> find_email (find_email_rev (a :: x) rev) l
  | S("@",a) :: IP4 x :: l -> find_email (find_email_rev (a :: x) rev) l
  | a :: l -> find_email (a :: rev) l
  | [] -> List.rev rev

let rec find_port rev = function
    Host x :: S(":",a) :: D d :: l -> find_port (Host(x @ [a] @ d) :: rev) l
  | IP4 x :: S(":",a) :: D d :: l -> find_port (Host(x @ [a] @ d) :: rev) l
  | a :: l -> find_port (a :: rev) l
  | [] -> List.rev rev

let rec find_url2 rev = function
    D d :: l -> find_url2 ((List.rev d) @ rev) l
  | L(_,a) :: l -> find_url2 (a :: rev) l
  | S(".",a) :: l -> find_url2 (a :: rev) l
  | S("-",a) :: l -> find_url2 (a :: rev) l
  | S("/",a) :: l -> find_url2 (a :: rev) l
  | S(":",a) :: l -> find_url2 (a :: rev) l
  | S("%",a) :: l -> find_url2 (a :: rev) l
  | S("#",a) :: l -> find_url2 (a :: rev) l
  | S("?",a) :: l -> find_url2 (a :: rev) l
  | S("=",a) :: l -> find_url2 (a :: rev) l
  | S(",",a) :: l -> find_url2 (a :: rev) l
  | S("~",a) :: l -> find_url2 (a :: rev) l
  | S("_",a) :: l -> find_url2 (a :: rev) l
  | l -> List.rev rev, l

let rec find_url rev = function
    Host x :: S("/",a) :: l -> let x,l = find_url2 (List.rev x) (S("/",a) :: l) in find_url (Host x :: rev) l
  | IP4 x :: S("/",a) :: l -> let x,l = find_url2 (List.rev x) (S("/",a) :: l) in find_url (Host x :: rev) l
  | a :: l -> find_url (a :: rev) l
  | [] -> List.rev rev

let rec find_scheme rev = function
    L("h",a1) :: L("t",a2) :: L("t",a3) :: L("p",a4) :: S(":",a5) :: S("/",a6) :: S("/",a7) :: Host x :: l ->
      find_scheme (Host([a1;a2;a3;a4;a5;a6;a7] @ x) :: rev) l
  | L("h",a1) :: L("t",a2) :: L("t",a3) :: L("p",a4) :: S(":",a5) :: S("/",a6) :: S("/",a7) :: IP4 x :: l ->
      find_scheme (Host([a1;a2;a3;a4;a5;a6;a7] @ x) :: rev) l
  | L("h",a1) :: L("t",a2) :: L("t",a3) :: L("p",a4) :: L("s",a5) :: S(":",a6) :: S("/",a7) :: S("/",a8) :: Host x :: l ->
      find_scheme (Host([a1;a2;a3;a4;a5;a6;a7;a8] @ x) :: rev) l
  | L("h",a1) :: L("t",a2) :: L("t",a3) :: L("p",a4) :: L("s",a5) :: S(":",a6) :: S("/",a7) :: S("/",a8) :: IP4 x :: l ->
      find_scheme (Host([a1;a2;a3;a4;a5;a6;a7;a8] @ x) :: rev) l
  | a :: l -> find_scheme (a :: rev) l
  | [] -> List.rev rev

let rec find l =
  let l = List.rev (Xlist.rev_map l escape) in
  (* print_list "f1" l; *)
  let l = merge_digits [] [] l in
  (* print_list "f2" l; *)
  let l = find_top_level_domains [] "" [] l in
  (* print_list "f3a" l; *)
  let l = find_host [] l in
  (* print_list "f3b" l; *)
  let l = check_host [] l in
  (* print_list "f4" l; *)
  let l = find_ip4 [] l in
  (* print_list "f5" l; *)
  let l = find_email [] l in
  (* print_list "f6" l; *)
  let l = find_port [] l in
  (* print_list "f7" l; *)
  let l = find_url [] l in
  (* print_list "f8" l; *)
  let l = find_scheme [] l in
  (* print_list "f9" l; *)
  let l = de_escape_list l in
  l

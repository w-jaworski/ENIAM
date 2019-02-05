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

(* Tekst obserwowany traktujemy jako źródłowy
   Nazwy operacji to czynności, które wykonuje korektor *)

(* type edit =
    Transpose of string * string
  | Substitute of string * string
  | Insert of string
  | Delete of string
  | Accept of string *)
type edit = (* observed * correct *)
    Transpose of Xunicode.unicode * Xunicode.unicode
  | Substitute of Xunicode.unicode * Xunicode.unicode
  | Insert of Xunicode.unicode * Xunicode.unicode
  | Delete of Xunicode.unicode * Xunicode.unicode
  | Accept of Xunicode.unicode

module OrderedEdit = struct

  type t = edit

  let compare = compare

end

module EditQMap = Xmap.MakeQ(OrderedEdit)

(* let string_of_edit = function
    Transpose(s,t) -> Printf.sprintf "Transpose(%s,%s)" s t
  | Substitute(s,t) -> Printf.sprintf "Substitute(%s,%s)" s t
  | Insert(s) -> Printf.sprintf "Insert(%s)" s
  | Delete(s) -> Printf.sprintf "Delete(%s)" s
  | Accept(s) -> Printf.sprintf "Accept(%s)" s *)

let string_of_edit = function
    Transpose(s,t) -> Printf.sprintf "Transpose(%s,%s)" (Xunicode.to_string s) (Xunicode.to_string t)
  | Substitute(s,t) -> Printf.sprintf "Substitute(%s,%s)" (Xunicode.to_string s) (Xunicode.to_string t)
  | Insert(s,t) -> Printf.sprintf "Insert(%s,%s)" (Xunicode.to_string s) (Xunicode.to_string t)
  | Delete(s,t) -> Printf.sprintf "Delete(%s,%s)" (Xunicode.to_string s) (Xunicode.to_string t)
  | Accept(s) -> Printf.sprintf "Accept(%s)" (Xunicode.to_string s)

let count_differences_simple (*mapping*) observed correct =
  let m = Array.make_matrix (Array.length correct) (Array.length observed) 0 in
  let a = Array.make_matrix (Array.length correct) (Array.length observed) [] in
  Int.iter 1 (Array.length correct - 1) (fun i -> m.(i).(0) <- i);
  Int.iter 1 (Array.length observed - 1) (fun j -> m.(0).(j) <- j);
  Int.iter 1 (Array.length correct - 1) (fun i ->
    Int.iter 1 (Array.length observed - 1) (fun j ->
      m.(i).(j) <- min (m.(i-1).(j-1) + if correct.(i) = observed.(j) then 0 else 1)
        (min (m.(i-1).(j) + 1) (m.(i).(j-1) + 1));
      if i > 1 && j > 1 then
        if correct.(i-1) = observed.(j) && correct.(i) = observed.(j-1) then
          m.(i).(j) <- min m.(i).(j) (m.(i-2).(j-2) + 1);
      if i > 1 && j > 1 && m.(i).(j) = m.(i-2).(j-2) + 1 && correct.(i-1) = observed.(j) && correct.(i) = observed.(j-1) then
        a.(i).(j) <- (Transpose(observed.(j),correct.(i))) :: a.(i-2).(j-2) else
      if m.(i).(j) =  m.(i-1).(j-1) + if correct.(i) = observed.(j) then 0 else 1 then
        if correct.(i) = observed.(j) then a.(i).(j) <- (Accept(correct.(i))) :: a.(i-1).(j-1)
        else a.(i).(j) <- (Substitute(observed.(j),correct.(i))) :: a.(i-1).(j-1) else
      if m.(i).(j) = (m.(i-1).(j) + 1) then a.(i).(j) <- (Insert(observed.(j),correct.(i))) :: a.(i-1).(j) else
      a.(i).(j) <- (Delete(observed.(j),correct.(i))) :: a.(i).(j-1)));
  List.rev (a.(Array.length correct - 1).(Array.length observed - 1))

let std_edit_distance observed correct prev_observed prev_correct =
  (if observed = correct then [0.,2,-1,-1,Accept(correct)] else [1.,3,-1,-1,Substitute(observed,correct)]) @
  [1.,4,0,-1,Insert(observed,correct);1.,5,-1,0,Delete(observed,correct)] @
  (if correct = prev_observed && observed = prev_correct then [1.,1,-2,-2,Transpose(observed,correct)] else [])

let is_letter = function
    Xunicode.Small _ -> true
  | _ -> false

let restricted_edit_distance observed correct prev_observed prev_correct =
  (if observed = correct then [0.,2,-1,-1,Accept(correct)] else
   if is_letter observed && is_letter correct then [1.,3,-1,-1,Substitute(observed,correct)] else []) @
  [1.,4,0,-1,Insert(observed,correct);1.,5,-1,0,Delete(observed,correct)] @
  (if correct = prev_observed && observed = prev_correct && is_letter observed && is_letter correct then
    [1.,1,-2,-2,Transpose(observed,correct)] else [])

let p_correct = Xlist.fold [
  " ",0.118815;  "!",0.000104; "\"",0.001074;  "$",0.000007;  "%",0.000004;  "&",0.000013;  "'",0.000027;
  "(",0.000180;  ")",0.000302;  "*",0.000007;  "+",0.000025;  ",",0.013907;  "-",0.001276;  ".",0.010120;
  "/",0.001809;  "0",0.006402;  "1",0.006285;  "2",0.003060;  "3",0.002086;  "4",0.002404;  "5",0.003297;
  "6",0.001109;  "7",0.001436;  "8",0.001319;  "9",0.000785;  ":",0.009823;  ";",0.000052;  "<",0.000002;
  ">",0.000002;  "?",0.006515;  "@",0.000299;  "[",0.000016;  "]",0.000016;  "_",0.000005;  "a",0.079956;
  "b",0.010138;  "c",0.036215;  "d",0.027985;  "e",0.065389;  "f",0.002068;  "g",0.011214;  "h",0.007879;
  "i",0.061359;  "j",0.019680;  "k",0.026852;  "l",0.015786;  "m",0.026235;  "n",0.048750;  "o",0.053408;
  "p",0.025827;  "q",0.000013;  "r",0.041976;  "s",0.034678;  "t",0.031550;  "u",0.024336;  "v",0.000133;
  "w",0.027421;  "x",0.000124;  "y",0.031228;  "z",0.044605;  "ó",0.006184;  "ą",0.005693;  "ć",0.006314;
  "ę",0.010847;  "ł",0.013640;  "ń",0.001166;  "ś",0.003798;  "ź",0.000292;  "ż",0.004663;  "”",0.000002;
  "„",0.000002;  "…",0.000002
] StringMap.empty (fun map (a,p) ->
    StringMap.add_inc map a p (fun _ -> failwith "p_correct"))

let p_correct_tail = 0.000002

let p_insert=0.000524
let p_delete=0.000167
let p_transpose=0.000091
let p_std_substitute=0.000278
let p_ogon_substitute=0.033395
let p_letter_accept = 1. -. p_insert -. p_delete -. p_transpose -. p_std_substitute
let p_other_accept = 1. -. p_insert -. p_delete

let ogonki = Xlist.fold [
  "a","ą",0.066465;
  "c","ć",0.148455;
  "e","ę",0.142284;
  "l","ł",0.463547;
  "n","ń",0.023364;
  "o","ó",0.103772;
  "s","ś",0.098700;
  "z","ź",0.006494;
  "z","ż",0.094648] StringMap.empty (fun map (a,b,p) ->
    StringMap.add_inc map a [b,p] (fun l -> (b,p_ogon_substitute *. p) :: l))

let probabilistic_edit_distance observed correct prev_observed prev_correct =
  if observed = Xunicode.Sign "##" && correct = Xunicode.Sign "##" then [1.,2,-1,-1,Accept(correct)] else
  if observed = Xunicode.Sign "##" || correct = Xunicode.Sign "##" then [] else
  let p_correct = try StringMap.find p_correct (Xunicode.char_of_classified_char correct) with Not_found -> p_correct_tail in
  let l =
    [p_correct *. p_insert,4,0,-1,Insert(observed,correct);p_correct *. p_delete,5,-1,0,Delete(observed,correct)] @
    (if is_letter observed && is_letter correct then
      (if correct = prev_observed && observed = prev_correct && is_letter observed && is_letter correct then
       [p_correct *. p_transpose,1,-2,-2,Transpose(observed,correct)] else []) @
      (let l = try StringMap.find ogonki (Xunicode.char_of_classified_char observed) with Not_found -> [] in
      let p_accept = Xlist.fold l p_letter_accept (fun pa (_,p) -> pa -. p) in
      if observed = correct then [p_accept,2,-1,-1,Accept(correct)] else
      try [Xlist.assoc l (Xunicode.char_of_classified_char correct),3,-1,-1,Substitute(observed,correct)]
      with Not_found -> [p_correct *. p_std_substitute,3,-1,-1,Substitute(observed,correct)])
    else if observed = correct then [p_other_accept,2,-1,-1,Accept(correct)] else []) in
  Xlist.rev_map l (fun (p,prec,dj,di,op) -> -. log p,prec,dj,di,op)

let big_float = 1000000000.

let probabilistic_prev_cost x = x -. log p_transpose

let count_differences operators observed correct =
  let m = Array.make_matrix (Array.length correct) (Array.length observed) big_float in
  let a = Array.make_matrix (Array.length correct) (Array.length observed) [] in
  m.(0).(0) <- 0.;
  m.(1).(1) <- 0.;
  Int.iter 2 (Array.length correct - 1) (fun i ->
    Int.iter 2 (Array.length observed - 1) (fun j ->
      let ops = operators observed.(j) correct.(i) observed.(j-1) correct.(i-1) in
      let best_cost,_,best_ops = Xlist.fold ops (max_float,max_int,[]) (fun (best_cost,best_prec,best_ops) (cost,prec,dj,di,op) ->
        let c = m.(i+di).(j+dj) +. cost in
        if best_cost < c then best_cost,best_prec,best_ops else
        if best_cost > c then c,prec,[op :: a.(i+di).(j+dj)] else
        if best_prec < prec then best_cost,best_prec,best_ops else
        if best_prec > prec then c,prec,[op :: a.(i+di).(j+dj)] else
        best_cost,best_prec,(op :: a.(i+di).(j+dj)) :: best_ops) in
      let best_op = match best_ops with
          [] -> failwith "count_differences"
        | [op] -> op
        | l -> failwith ("count_differences: multiple choices: " ^ String.concat " " (Xlist.map l (fun op -> string_of_edit (List.hd op)))) in
      if best_cost >= big_float then Printf.printf "i=%d j=%d %f\n" i j best_cost;
      m.(i).(j) <- best_cost;
      a.(i).(j) <- best_op));
  List.rev (a.(Array.length correct - 1).(Array.length observed - 1))

let count_differences2 operators observed correct =
  let m = Array.make_matrix 3 (Array.length observed) big_float in
  let a = Array.make_matrix 3 (Array.length observed) [] in
  m.(0).(0) <- 0.;
  m.(1).(1) <- 0.;
  Int.iter 2 (Array.length correct - 1) (fun i ->
    Int.iter 2 (Array.length observed - 1) (fun j ->
      let ops = operators observed.(j) correct.(i) observed.(j-1) correct.(i-1) in
      let best_cost,_,best_ops = Xlist.fold ops (max_float,max_int,[]) (fun (best_cost,best_prec,best_ops) (cost,prec,dj,di,op) ->
        let c = m.((i+di) mod 3).(j+dj) +. cost in
        if best_cost < c then best_cost,best_prec,best_ops else
        if best_cost > c then c,prec,[op :: a.((i+di) mod 3).(j+dj)] else
        if best_prec < prec then best_cost,best_prec,best_ops else
        if best_prec > prec then c,prec,[op :: a.((i+di) mod 3).(j+dj)] else
        best_cost,best_prec,(op :: a.((i+di) mod 3).(j+dj)) :: best_ops) in
      let best_op = match best_ops with
          [] -> failwith "count_differences"
        | [op] -> op
        | l -> failwith ("count_differences: multiple choices: " ^ String.concat " " (Xlist.map l (fun op -> string_of_edit (List.hd op)))) in
      if best_cost >= big_float then Printf.printf "i=%d j=%d %f\n" i j best_cost;
      m.(i mod 3).(j) <- best_cost;
      a.(i mod 3).(j) <- best_op));
  List.rev (a.((Array.length correct - 1) mod 3).(Array.length observed - 1))

let count_differences3 operators observed correct =
  let m = Array.make_matrix 3 (Array.length observed) big_float in
  let a = Array.make_matrix 3 (Array.length observed) [] in
  let rcorrect = ref (List.tl correct) in
  m.(0).(0) <- 0.;
  m.(1).(1) <- 0.;
  Int.iter 2 (Xlist.size correct - 1) (fun i ->
    Int.iter 2 (Array.length observed - 1) (fun j ->
      let ops = operators observed.(j) (List.hd (List.tl !rcorrect)) observed.(j-1) (List.hd !rcorrect) in
      let best_cost,_,best_ops = Xlist.fold ops (max_float,max_int,[]) (fun (best_cost,best_prec,best_ops) (cost,prec,dj,di,op) ->
        let c = m.((i+di) mod 3).(j+dj) +. cost in
        if best_cost < c then best_cost,best_prec,best_ops else
        if best_cost > c then c,prec,[op :: a.((i+di) mod 3).(j+dj)] else
        if best_prec < prec then best_cost,best_prec,best_ops else
        if best_prec > prec then c,prec,[op :: a.((i+di) mod 3).(j+dj)] else
        best_cost,best_prec,(op :: a.((i+di) mod 3).(j+dj)) :: best_ops) in
      let best_op = match best_ops with
          [] -> failwith "count_differences"
        | [op] -> op
        | l -> failwith ("count_differences: multiple choices: " ^ String.concat " " (Xlist.map l (fun op -> string_of_edit (List.hd op)))) in
      if best_cost >= big_float then Printf.printf "i=%d j=%d %f\n" i j best_cost;
      m.(i mod 3).(j) <- best_cost;
      a.(i mod 3).(j) <- best_op);
    rcorrect := List.tl !rcorrect);
  List.rev (a.((Xlist.size correct - 1) mod 3).(Array.length observed - 1))

let rec count_differences4_rec operators observed m a i = function
    [] -> failwith "count_differences4_rec"
  | [_] -> ()
  | prev_corr :: corr :: correct ->
    Int.iter 2 (Array.length observed - 1) (fun j ->
      let ops = operators observed.(j) corr observed.(j-1) prev_corr in
      let best_cost,_,best_ops = Xlist.fold ops (max_float,max_int,[]) (fun (best_cost,best_prec,best_ops) (cost,prec,dj,di,op) ->
        let c = m.((i+di) mod 3).(j+dj) +. cost in
        if best_cost < c then best_cost,best_prec,best_ops else
        if best_cost > c then c,prec,[op :: a.((i+di) mod 3).(j+dj)] else
        if best_prec < prec then best_cost,best_prec,best_ops else
        if best_prec > prec then c,prec,[op :: a.((i+di) mod 3).(j+dj)] else
        best_cost,best_prec,(op :: a.((i+di) mod 3).(j+dj)) :: best_ops) in
      let best_op = match best_ops with
          [] -> failwith "count_differences"
        | [op] -> op
        | l -> failwith ("count_differences: multiple choices: " ^ String.concat " " (Xlist.map l (fun op -> string_of_edit (List.hd op)))) in
      if best_cost >= big_float then Printf.printf "i=%d j=%d %f\n" i j best_cost;
      m.(i mod 3).(j) <- best_cost;
      a.(i mod 3).(j) <- best_op);
    count_differences4_rec operators observed m a (i+1) (corr :: correct)

let count_differences4 operators observed correct =
  let m = Array.make_matrix 3 (Array.length observed) big_float in
  let a = Array.make_matrix 3 (Array.length observed) [] in
  m.(0).(0) <- 0.;
  m.(1).(1) <- 0.;
  count_differences4_rec operators observed m a 2 (List.tl correct);
  if m.((Xlist.size correct - 1) mod 3).(Array.length observed - 1) >= big_float then print_endline "out of beam";
  List.rev (a.((Xlist.size correct - 1) mod 3).(Array.length observed - 1))

let rec count_differences5_rec k operators observed m a i = function
    [] -> failwith "count_differences5_rec"
  | [_] -> ()
  | prev_corr :: corr :: correct ->
    Int.iter 2 (Array.length observed - 1) (fun j ->
      if abs (i - j) > k then (
        m.(i mod 3).(j) <- big_float;
        a.(i mod 3).(j) <- [])
      else (
      let ops = operators observed.(j) corr observed.(j-1) prev_corr in
      let best_cost,_,best_ops = Xlist.fold ops (max_float,max_int,[]) (fun (best_cost,best_prec,best_ops) (cost,prec,dj,di,op) ->
        let c = m.((i+di) mod 3).(j+dj) +. cost in
        if best_cost < c then best_cost,best_prec,best_ops else
        if best_cost > c then c,prec,[op :: a.((i+di) mod 3).(j+dj)] else
        if best_prec < prec then best_cost,best_prec,best_ops else
        if best_prec > prec then c,prec,[op :: a.((i+di) mod 3).(j+dj)] else
        best_cost,best_prec,(op :: a.((i+di) mod 3).(j+dj)) :: best_ops) in
      let best_op = match best_ops with
          [] -> failwith "count_differences"
        | [op] -> op
        | l -> failwith ("count_differences: multiple choices: " ^ String.concat " " (Xlist.map l (fun op -> string_of_edit (List.hd op)))) in
      m.(i mod 3).(j) <- best_cost;
      a.(i mod 3).(j) <- best_op));
    count_differences5_rec k operators observed m a (i+1) (corr :: correct)

let count_differences5 k operators observed correct =
  let m = Array.make_matrix 3 (Array.length observed) big_float in
  let a = Array.make_matrix 3 (Array.length observed) [] in
  m.(0).(0) <- 0.;
  m.(1).(1) <- 0.;
  count_differences5_rec k operators observed m a 2 (List.tl correct);
  if m.((Xlist.size correct - 1) mod 3).(Array.length observed - 1) >= big_float then print_endline "out of beam";
  List.rev (a.((Xlist.size correct - 1) mod 3).(Array.length observed - 1))

let beam_make k zero =
  Array.make_matrix 3 (2*k+1) zero,k,zero

let beam_set (m,k,zero) i j v =
  if abs (j - i) > k then m,k,zero else (
  m.(i mod 3).(j - i + k) <- v;
  m,k,zero)

let beam_get (m,k,zero) i j =
  if abs (j - i) > k then zero else
  m.(i mod 3).(j - i + k)

let beam_copy (m,k,zero) =
  let m = Array.copy m in
  Int.iter 0 (Array.length m - 1) (fun i -> m.(i) <- Array.copy m.(i));
  m,k,zero (* FIXME: to może kopiować tylko jeden poziom *)

let beam_print i (m,k,zero) (a,_,zero2) print_fun print_fun2 =
  Int.iter (-2) 0 (fun di ->
    Int.iter (-k) k (fun dj ->
      Printf.printf "i=%d j=%d %s %s\n" (i+di) (i+di+dj)
        (print_fun (beam_get (m,k,zero) (i+di) (i+di+dj)))
        (print_fun2 (beam_get (a,k,zero2) (i+di) (i+di+dj)))))

(*let beam_min (m,k,zero) i = (* FIXME: to nie działa, prawdopodobnie dlatego, że bierze pod uwagę stare wpisy *)
  let a = i mod 3 in
  let b = (i-1) mod 3 in
  Int.fold 0 (2*k) zero (fun n j ->
    min (min n (m.(a).(j))) m.(b).(j))*)

let rec count_differences6_rec k operators observed m a i = function
    [] -> failwith "count_differences6_rec"
  | [_] -> ()
  | prev_corr :: corr :: correct ->
    let m,a = Int.fold (max 2 (i-k)) (min (Array.length observed - 1) (i+k)) (m,a) (fun (m,a) j ->
      let ops = operators observed.(j) corr observed.(j-1) prev_corr in
      let best_cost,_,best_ops = Xlist.fold ops (max_float,max_int,[]) (fun (best_cost,best_prec,best_ops) (cost,prec,dj,di,op) ->
        let c = beam_get m (i+di) (j+dj) +. cost in
        if best_cost < c then best_cost,best_prec,best_ops else
        if best_cost > c then c,prec,[op :: beam_get a (i+di) (j+dj)] else
        if best_prec < prec then best_cost,best_prec,best_ops else
        if best_prec > prec then c,prec,[op :: beam_get a (i+di) (j+dj)] else
        best_cost,best_prec,(op :: beam_get a (i+di) (j+dj)) :: best_ops) in
      let best_op = match best_ops with
          [] -> failwith "count_differences"
        | [op] -> op
        | l -> failwith ("count_differences: multiple choices: " ^ String.concat " " (Xlist.map l (fun op -> string_of_edit (List.hd op)))) in
      beam_set m i j best_cost,
      beam_set a i j best_op) in
    count_differences6_rec k operators observed m a (i+1) (corr :: correct)

let count_differences6 k operators observed correct =
  let m = beam_make k big_float in
  let a = beam_make k [] in
  let m = beam_set m 0 0 0. in
  let m = beam_set m 1 1 0. in
  count_differences6_rec k operators observed m a 2 (List.tl correct);
  if beam_get m (Xlist.size correct - 1) (Array.length observed - 1) >= big_float then print_endline "out of beam";
  List.rev (beam_get a (Xlist.size correct - 1) (Array.length observed - 1))

type value_type = Full | Suf | Last

type value = int * string * value_type

type t = M of (Xunicode.unicode * t) list * value list

let rec count_differences7_rec k operators observed m a i prev_corr (M(correct_list,values)) =
  (* Printf.printf "count_differences7_rec: i=%d prev_corr=%s\n" i (Xunicode.to_string prev_corr); *)
  (* beam_print i m a (Printf.sprintf "%f") (fun l -> String.concat " " (Xlist.rev_map l string_of_edit)); *)
  Xlist.map values (fun (corr_len,value,_) ->
    beam_get m (corr_len - 2) (Array.length observed - 4),
    List.rev (beam_get a (corr_len - 2) (Array.length observed - 4)),
    value) @
  List.flatten (Xlist.rev_map correct_list (fun (corr,correct) ->
    let m = beam_copy m in
    let a = beam_copy a in
    let m,a = Int.fold (max 2 (i-k)) (min (Array.length observed - 1) (i+k)) (m,a) (fun (m,a) j ->
      let ops = operators observed.(j) corr observed.(j-1) prev_corr in
      let best_cost,_,best_ops = Xlist.fold ops (max_float,max_int,[]) (fun (best_cost,best_prec,best_ops) (cost,prec,dj,di,op) ->
        let c = beam_get m (i+di) (j+dj) +. cost in
        if best_cost < c then best_cost,best_prec,best_ops else
        if best_cost > c then c,prec,[op :: beam_get a (i+di) (j+dj)] else
        if best_prec < prec then best_cost,best_prec,best_ops else
        if best_prec > prec then c,prec,[op :: beam_get a (i+di) (j+dj)] else
        best_cost,best_prec,(op :: beam_get a (i+di) (j+dj)) :: best_ops) in
      let best_op = match best_ops with
          [] -> failwith "count_differences"
        | [op] -> op
        | l -> failwith ("count_differences: multiple choices: " ^ String.concat " " (Xlist.map l (fun op -> string_of_edit (List.hd op)))) in
      beam_set m i j best_cost,
      beam_set a i j best_op) in
    count_differences7_rec k operators observed m a (i+1) corr correct))

let count_differences7 k operators observed correct =
  let m = beam_make k big_float in
  let a = beam_make k [] in
  let m = beam_set m 0 0 0. in
  let m = beam_set m 1 1 0. in
  count_differences7_rec k operators observed m a 2 (Xunicode.Sign "") correct

module PrioQueue =
    struct
      type priority = float

      type 'a queue = Empty | Node of priority * 'a * 'a queue * 'a queue

      let empty = 0,Empty

      let is_empty (_,queue) =
        queue = Empty

      let rec add_rec prio elt = function
          Empty -> Node(prio, elt, Empty, Empty)
        | Node(p, e, left, right) ->
            if prio <= p
            then Node(prio, elt, add_rec p e right, left)
            else Node(p, e, add_rec prio elt right, left)

      let add (size,queue) prio elt =
        size+1, add_rec prio elt queue

      exception Queue_is_empty

      let rec remove_top = function
          Empty -> raise Queue_is_empty
        | Node(prio, elt, left, Empty) -> left
        | Node(prio, elt, Empty, right) -> right
        | Node(prio, elt, (Node(lprio, lelt, _, _) as left),
                          (Node(rprio, relt, _, _) as right)) ->
            if lprio <= rprio
            then Node(lprio, lelt, remove_top left, right)
            else Node(rprio, relt, left, remove_top right)

      let extract = function
          _,Empty -> raise Queue_is_empty
        | size,(Node(prio, elt, _, _) as queue) -> (prio, elt, (size-1,remove_top queue))

      let size (s,_) = s

    end

let rec count_differences8_rec found k no_candidates done_candidates max_cost operators prev_cost_mod observed i candidates new_candidates =
  (* Printf.printf "count_differences8_rec 1: |candidates|=%d |new_candidates|=%d\n" (PrioQueue.size candidates) (PrioQueue.size new_candidates); *)
  if PrioQueue.is_empty candidates || done_candidates >= no_candidates then (
    (* print_endline "count_differences8_rec: next char"; *)
    if PrioQueue.is_empty new_candidates then found else
    count_differences8_rec found k no_candidates 0 max_cost operators prev_cost_mod observed (i+1) new_candidates PrioQueue.empty) else
  let cost,(m,a,prev_cost,prev_corr,M(correct_list,values)),candidates = PrioQueue.extract candidates in
  Printf.printf "count_differences8_rec 2: i=%d prev_corr=%s cost=%f\n" i (Xunicode.to_string prev_corr) cost;
  beam_print i m a (Printf.sprintf "%f") (fun l -> String.concat " " (Xlist.rev_map l string_of_edit));
  if cost >= max_cost then count_differences8_rec found k no_candidates done_candidates max_cost operators prev_cost_mod observed i PrioQueue.empty new_candidates else
  let new_candidates = Xlist.fold correct_list new_candidates (fun new_candidates (corr,correct) ->
    let m = beam_copy m in
    let a = beam_copy a in
    let m,a,min_cost = Int.fold (max 2 (i-k)) (min (Array.length observed - 1) (i+k)) (m,a,big_float) (fun (m,a,min_cost) j ->
      let ops = operators observed.(j) corr observed.(j-1) prev_corr in
      let best_cost,_,best_ops = Xlist.fold ops (max_float,max_int,[]) (fun (best_cost,best_prec,best_ops) (cost,prec,dj,di,op) ->
        let c = beam_get m (i+di) (j+dj) +. cost in
        if best_cost < c then best_cost,best_prec,best_ops else
        if best_cost > c then c,prec,[op :: beam_get a (i+di) (j+dj)] else
        if best_prec < prec then best_cost,best_prec,best_ops else
        if best_prec > prec then c,prec,[op :: beam_get a (i+di) (j+dj)] else
        best_cost,best_prec,(op :: beam_get a (i+di) (j+dj)) :: best_ops) in
      let best_op = match best_ops with
          [] -> failwith "count_differences"
        | [op] -> op
        | l -> failwith ("count_differences: multiple choices: " ^ String.concat " " (Xlist.map l (fun op -> string_of_edit (List.hd op)))) in
      beam_set m i j best_cost,
      beam_set a i j best_op,
      min min_cost best_cost) in
    (* Printf.printf "count_differences8_rec 3: i=%d prev_corr=%s corr=%s beam_min=%f\n" i (Xunicode.to_string prev_corr) (Xunicode.to_string corr) (beam_min m i); *)
    (* beam_print i m a (Printf.sprintf "%f") (fun l -> String.concat " " (Xlist.rev_map l string_of_edit)); *)
    PrioQueue.add new_candidates (min (prev_cost_mod prev_cost) min_cost) (m,a,min_cost,corr,correct)) in
  (* Printf.printf "count_differences8_rec 4: |candidates|=%d |new_candidates|=%d\n" (PrioQueue.size candidates) (PrioQueue.size new_candidates); *)
  let found = Xlist.fold values found (fun found (corr_len,value,_) ->
    (beam_get m (corr_len - 2) (Array.length observed - 4),
    List.rev (beam_get a (corr_len - 2) (Array.length observed - 4)),
    value) :: found) in
  count_differences8_rec found k no_candidates (done_candidates + 1) max_cost operators prev_cost_mod observed i candidates new_candidates

let count_differences8 k no_candidates max_cost operators prev_cost_mod observed correct =
  let m = beam_make k big_float in
  let a = beam_make k [] in
  let m = beam_set m 0 0 0. in
  let m = beam_set m 1 1 0. in
  let candidates = PrioQueue.add PrioQueue.empty 0. (m,a,0.,Xunicode.Sign "",correct) in
  count_differences8_rec [] k no_candidates 0 max_cost operators prev_cost_mod observed 2 candidates PrioQueue.empty

let rec count_differences9_rec found k no_candidates done_candidates max_cost operators prev_cost_mod observed i full_correct candidates new_candidates =
  (* Printf.printf "count_differences9_rec 1: |candidates|=%d |new_candidates|=%d\n" (PrioQueue.size candidates) (PrioQueue.size new_candidates); *)
  if PrioQueue.is_empty candidates || done_candidates >= no_candidates then (
    (* print_endline "count_differences9_rec: next char"; *)
    if PrioQueue.is_empty new_candidates then found else
    count_differences9_rec found k no_candidates 0 max_cost operators prev_cost_mod observed (i+1) full_correct new_candidates PrioQueue.empty) else
  let cost,(prev_found,m,a,prev_cost,prev_corr_len,prev_corr,M(correct_list,values)),candidates = PrioQueue.extract candidates in
  (* Printf.printf "count_differences9_rec 2: i=%d prev_corr=%s cost=%f\n" i (Xunicode.to_string prev_corr) cost; *)
  (* beam_print i m a (Printf.sprintf "%f") (fun l -> String.concat " " (Xlist.rev_map l string_of_edit)); *)
  if cost >= max_cost then count_differences9_rec found k no_candidates done_candidates max_cost operators prev_cost_mod observed i full_correct PrioQueue.empty new_candidates else
  let new_candidates = Xlist.fold correct_list new_candidates (fun new_candidates (corr,M(correct_list2,values2)) ->
    let m = beam_copy m in
    let a = beam_copy a in
    let m,a,min_cost = Int.fold (max 2 (i-k)) (min (Array.length observed - 1) (i+k)) (m,a,big_float) (fun (m,a,min_cost) j ->
      let ops = operators observed.(j) corr observed.(j-1) prev_corr in
      let best_cost,_,best_ops = Xlist.fold ops (big_float,max_int,[]) (fun (best_cost,best_prec,best_ops) (cost,prec,dj,di,op) ->
        let c = beam_get m (i+di) (j+dj) +. cost in
        if best_cost < c then best_cost,best_prec,best_ops else
        if best_cost > c then c,prec,[op :: beam_get a (i+di) (j+dj)] else
        if best_prec < prec then best_cost,best_prec,best_ops else
        if best_prec > prec then c,prec,[op :: beam_get a (i+di) (j+dj)] else
        best_cost,best_prec,(op :: beam_get a (i+di) (j+dj)) :: best_ops) in
      let best_op = match best_ops with
          [] -> [](*failwith "count_differences"*)
        | [op] -> op
        | l -> failwith ("count_differences: multiple choices: " ^ String.concat " " (Xlist.map l (fun op -> string_of_edit (List.hd op)))) in
      beam_set m i j best_cost,
      beam_set a i j best_op,
      min min_cost best_cost) in
    (* Printf.printf "count_differences9_rec 3: i=%d prev_corr=%s corr=%s beam_min=%f\n" i (Xunicode.to_string prev_corr) (Xunicode.to_string corr) (beam_min m i); *)
    (* beam_print i m a (Printf.sprintf "%f") (fun l -> String.concat " " (Xlist.rev_map l string_of_edit)); *)
    let new_candidates = Xlist.fold values2 new_candidates (fun new_candidates (corr_len,value,_) ->
      PrioQueue.add new_candidates (min (prev_cost_mod prev_cost) min_cost) (value :: prev_found,m,a,min_cost,corr_len + prev_corr_len,Xunicode.Sign ""(*corr*),full_correct)) in
    PrioQueue.add new_candidates (min (prev_cost_mod prev_cost) min_cost) (prev_found,m,a,min_cost,prev_corr_len,corr,M(correct_list2,values2))) in
  (* Printf.printf "count_differences9_rec 4: |candidates|=%d |new_candidates|=%d\n" (PrioQueue.size candidates) (PrioQueue.size new_candidates); *)
  let found =
    if beam_get m (1 + prev_corr_len - 2) (Array.length observed - 4) >= max_cost then found else
    Xlist.fold values found (fun found -> function
        -1,"##",Last ->
          (beam_get m (1 + prev_corr_len - 2) (Array.length observed - 4),
          List.rev (beam_get a (1 + prev_corr_len - 2) (Array.length observed - 4)),
          List.rev ((*value ::*) prev_found)) :: found
      | corr_len,value,_ -> found) in
  count_differences9_rec found k no_candidates (done_candidates + 1) max_cost operators prev_cost_mod observed i full_correct candidates new_candidates

let count_differences9 k no_candidates max_cost operators prev_cost_mod observed correct =
  let m = beam_make k big_float in
  let a = beam_make k [] in
  let m = beam_set m 0 0 0. in
  let m = beam_set m 1 1 0. in
  let candidates = PrioQueue.add PrioQueue.empty 0. ([],m,a,0.,0,Xunicode.Sign "",correct) in
  count_differences9_rec [] k no_candidates 0 max_cost operators prev_cost_mod observed 2 correct candidates PrioQueue.empty

type candidate =
  string list * (float array array * int * float) *
  (edit list array array * int * edit list) * float * int *
  Xunicode.unicode * t

type params = {
  k: int; no_candidates: int; done_candidates: int; max_cost: float;
  operators: Xunicode.unicode -> Xunicode.unicode -> Xunicode.unicode  -> Xunicode.unicode ->
    (float * int * int * int * edit) list; prev_cost_mod: float -> PrioQueue.priority;
  observed: Xunicode.unicode array; i: int; full_correct: t; suf_correct: t;
  candidates: int * candidate PrioQueue.queue; new_candidates: int * candidate PrioQueue.queue}

let rec count_differences10_rec found p =
  (* Printf.printf "count_differences10_rec 1: |candidates|=%d |new_candidates|=%d\n" (PrioQueue.size p.candidates) (PrioQueue.size p.new_candidates); *)
  if PrioQueue.is_empty p.candidates || p.done_candidates >= p.no_candidates then (
    (* print_endline "count_differences10_rec: next char"; *)
    if PrioQueue.is_empty p.new_candidates then found else
    count_differences10_rec found {p with done_candidates=0; i=p.i+1; candidates=p.new_candidates; new_candidates=PrioQueue.empty}) else
  let cost,(prev_found,m,a,prev_cost,prev_corr_len,prev_corr,M(correct_list,values)),candidates = PrioQueue.extract p.candidates in
  (* Printf.printf "count_differences10_rec 2: i=%d prev_corr=%s cost=%f\n" p.i (Xunicode.to_string prev_corr) cost; *)
  (* beam_print i m a (Printf.sprintf "%f") (fun l -> String.concat " " (Xlist.rev_map l string_of_edit)); *)
  if cost >= p.max_cost then count_differences10_rec found {p with candidates=PrioQueue.empty; new_candidates=p.new_candidates} else
  let new_candidates = Xlist.fold correct_list p.new_candidates (fun new_candidates (corr,M(correct_list2,values2)) ->
    let m = beam_copy m in
    let a = beam_copy a in
    let m,a,min_cost = Int.fold (max 2 (p.i-p.k)) (min (Array.length p.observed - 1) (p.i+p.k)) (m,a,big_float) (fun (m,a,min_cost) j ->
      let ops = p.operators p.observed.(j) corr p.observed.(j-1) prev_corr in
      let best_cost,_,best_ops = Xlist.fold ops (big_float,max_int,[]) (fun (best_cost,best_prec,best_ops) (cost,prec,dj,di,op) ->
        let c = beam_get m (p.i+di) (j+dj) +. cost in
        if best_cost < c then best_cost,best_prec,best_ops else
        if best_cost > c then c,prec,[op :: beam_get a (p.i+di) (j+dj)] else
        if best_prec < prec then best_cost,best_prec,best_ops else
        if best_prec > prec then c,prec,[op :: beam_get a (p.i+di) (j+dj)] else
        best_cost,best_prec,(op :: beam_get a (p.i+di) (j+dj)) :: best_ops) in
      let best_op = match best_ops with
          [] -> [](*failwith "count_differences"*)
        | [op] -> op
        | l -> failwith ("count_differences: multiple choices: " ^ String.concat " " (Xlist.map l (fun op -> string_of_edit (List.hd op)))) in
      beam_set m p.i j best_cost,
      beam_set a p.i j best_op,
      min min_cost best_cost) in
    (* Printf.printf "count_differences10_rec 3: i=%d prev_corr=%s corr=%s beam_min=%f\n" p.i (Xunicode.to_string prev_corr) (Xunicode.to_string corr) 0.(*beam_min m p.i*); *)
    (* beam_print i m a (Printf.sprintf "%f") (fun l -> String.concat " " (Xlist.rev_map l string_of_edit)); *)
    let new_candidates = Xlist.fold values2 new_candidates (fun new_candidates (corr_len,value,typ) -> (* FIXME: dodać złączanie kandydatów, tzn [value] oraz dwie listy new_candidates do złączenia *)
      PrioQueue.add new_candidates (min (p.prev_cost_mod prev_cost) min_cost) (value :: prev_found,m,a,min_cost,corr_len + prev_corr_len,Xunicode.Sign ""(*corr*),
        match typ with Full -> p.full_correct | Suf -> p.suf_correct | Last -> M([],[]))) in
    PrioQueue.add new_candidates (min (p.prev_cost_mod prev_cost) min_cost) (prev_found,m,a,min_cost,prev_corr_len,corr,M(correct_list2,values2))) in
  (* Printf.printf "count_differences10_rec 4: |candidates|=%d |new_candidates|=%d\n" (PrioQueue.size candidates) (PrioQueue.size new_candidates); *)
  let found =
    if beam_get m (1 + prev_corr_len - 2) (Array.length p.observed - 4) >= p.max_cost then found else
    Xlist.fold values found (fun found -> function
        -1,"##",Last ->
          (beam_get m (1 + prev_corr_len - 2) (Array.length p.observed - 4),
          List.rev (beam_get a (1 + prev_corr_len - 2) (Array.length p.observed - 4)),
          List.rev ((*value ::*) prev_found)) :: found
      | corr_len,value,_ -> found) in
  (* Printf.printf "count_differences10_rec 5\n"; *)
  count_differences10_rec found {p with done_candidates=p.done_candidates + 1; candidates=candidates; new_candidates=new_candidates}

let count_differences10 k no_candidates max_cost operators prev_cost_mod observed full_correct suf_correct =
  let m = beam_make k big_float in
  let a = beam_make k [] in
  let m = beam_set m 0 0 0. in
  let m = beam_set m 1 1 0. in
  let candidates = PrioQueue.add PrioQueue.empty 0. ([],m,a,0.,0,Xunicode.Sign "",full_correct) in
  let p = {k=k; no_candidates=no_candidates; done_candidates=0; max_cost=max_cost; operators=operators; prev_cost_mod=prev_cost_mod; observed=observed;
    i=2; full_correct=full_correct; suf_correct=suf_correct; candidates=candidates; new_candidates=PrioQueue.empty} in
  count_differences10_rec [] p

type p = N of (Xunicode.unicode * p) StringMap.t * value list

let empty = N(StringMap.empty,[])
let empty2 = N(StringMap.add StringMap.empty "##" (Xunicode.Sign "##",N(StringMap.empty,[-1,"##",Last])),[])

let rec add_path value orth (N(map,values)) =
  if orth = [] then N(map,value :: values) else
  let c = Xunicode.char_of_classified_char (List.hd orth) in
  let tree = try snd (StringMap.find map c) with Not_found -> empty in
  let tree = add_path value (List.tl orth) tree in
  N(StringMap.add map c (List.hd orth,tree),values)

let add_dict_to_tree dict tree typ =
  Xlist.fold dict tree (fun tree orth ->
    let l = Xunicode.classified_chars_of_utf8_string orth in
    add_path (Xlist.size l,orth,typ) l tree)

let rec translate_dict (N(map,values)) =
  M(StringMap.fold map [] (fun l _ (k,tree) ->
    (k,translate_dict tree) :: l),values)

let prepare_dict filename =
  let dict = File.load_lines filename in
  let tree = add_dict_to_tree dict empty2 Full in
  translate_dict tree

let prepare_dicts words_filename signs_filename =
  let word_dict = File.load_lines words_filename in
  let sign_dict = File.load_lines signs_filename in
  let word_tree = add_dict_to_tree sign_dict (add_dict_to_tree word_dict empty2 Suf) Full in
  let sign_tree = add_dict_to_tree sign_dict empty2 Full in
  translate_dict word_tree, translate_dict sign_tree

let word_tree = ref (M([],[]))
let sign_tree = ref (M([],[]))

let resource_path =
  try Sys.getenv "ENIAM_RESOURCE_PATH"
  with Not_found ->
    if Sys.file_exists "/usr/share/eniam" then "/usr/share/eniam" else
    if Sys.file_exists "/usr/local/share/eniam" then "/usr/local/share/eniam" else
    if Sys.file_exists "resources" then "resources" else
    failwith "resource directory does not exists"

let words_filename = resource_path ^ "/fuzzyAnalyzer/words.tab"
let words_sgjp_filename = resource_path ^ "/fuzzyAnalyzer/words_sgjp.tab"
let signs_filename = resource_path ^ "/fuzzyAnalyzer/signs.tab"

let initialize () =
  (* let a,b = prepare_dicts words_filename signs_filename in *)
  (* let a,b = prepare_dicts "data/words_simple.tab" signs_filename in *)
  let a,b = prepare_dicts words_sgjp_filename signs_filename in
  word_tree := a;
  sign_tree := b

let correct observed =
  (* if String.length observed < 4 then observed else *)
  if String.length observed < 8 then observed else
  let observed = Array.of_list ((Xunicode.Small("**","**") :: Xunicode.Sign "" ::
    Xunicode.classified_chars_of_utf8_string (Xunicode.lowercase_utf8_string observed)) @ [Xunicode.Sign "##"]) in
  (* let l = count_differences10 3 40 500. *)
  let l = count_differences10 7 40 5000.
    probabilistic_edit_distance probabilistic_prev_cost observed !word_tree !sign_tree in
  let _,s = Xlist.fold l (big_float,[]) (fun (best_cost,best_s) (cost,l,s) ->
    if cost < best_cost then cost,s else best_cost,best_s) in
  String.concat "" s

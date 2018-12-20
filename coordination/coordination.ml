(*
dodac:

* deklaracja typów w mwe:
  dodac typ (linia 26)
* (65)load mwe dict: zamien O s na C s
* (323) match_path_rec dodaj matchowanie - C s, token (analogicznie do Os tylko
	zamiast get_orths)
| C s _ -> if .cat = s then new_t,sels :: found2
* w match_path analogiczna linia
* własne apply_rule
* zmienić load_mwe_dict zeby dict był listą
*)

open ENIAMsubsyntaxTypes
open ENIAMtokenizerTypes
open ENIAMpatterns
open ENIAMpaths
open ENIAM_MWE
open Xstd
open Xset

let coordination_filename = "../data/coordination.tab"

let coordination_load_mwe_dict filename dict =
  File.fold_tab filename dict (fun dict -> function
      [cats; interp_cat] ->
        let cats = Xstring.split " " cats in
        if cats = [] then failwith "coordination_load_mwe_dict" else
        let cats = Xlist.map cats (fun s -> C s) in
        (cats,interp_cat) :: dict
		| ["#"; comment_val] -> dict (* zezwalam na komentarze w pliku i je ignoruję *)
    | l -> failwith ("coordination_load_mwe_dict '" ^ String.concat "\t" l ^ "'"))

let rec coordination_match_path_rec map found (t:token_env) sels rev = function
    [] -> (t :: rev, sels) :: found
  | s :: l ->
     let map2 = try IntMap.find map t.next with Not_found -> IntMap.empty in
     let found2 = IntMap.fold map2 [] (fun found2 _ l ->
       TokenEnvSet.fold l found2 (fun found2 new_t ->
           match s,new_t.token with
             O s, token -> if Xlist.mem (ENIAMtokens.get_orths token) s then (new_t,sels) :: found2 else found2
					 | C s, _ -> if new_t.cat = s then (new_t,sels) :: found2 else found2
           | L(s,cat,interp), Lemma(s2,cat2,interps2) ->
               Xlist.fold interps2 found2 (fun found2 interp2 ->
                 if s=s2 && cat=cat2 && check_interp sels (interp,interp2) then
                   (new_t,get_sels sels (interp,interp2)) :: found2 else found2)
           (* | D(s,cat), Dig(s2,cat2) -> if s=s2 && cat=cat2 then (new_t,sels) :: found2 else found2 *)
           | D cat, Dig(s2,cat2) -> if cat=cat2 then (new_t,sels) :: found2 else found2
           | I s, Interp s2 -> if s=s2 then (new_t,sels) :: found2 else found2
           | SL, SmallLetter _ -> (new_t,sels) :: found2
           | SL, CapLetter _ -> (new_t,sels) :: found2
           | _ -> found2)) in
     Xlist.fold found2 found (fun found (new_t,sels) -> coordination_match_path_rec map found new_t sels (t :: rev) l)

let coordination_match_path map = function
   [] -> failwith "match_path"
 | s :: l ->
    let found = IntMap.fold map [] (fun found i map2 ->
      IntMap.fold map2 found (fun found j l ->
        TokenEnvSet.fold l found (fun found t ->
          match s,t.token with
            O s, token -> if Xlist.mem (ENIAMtokens.get_orths token) s then (t,[]) :: found else found
					| C s, _ -> if t.cat = s then (t,[]) :: found else found
          | L(s,cat,interp), Lemma(s2,cat2,interps2) ->
              Xlist.fold interps2 found (fun found interp2 ->
                if s=s2 && cat=cat2 && check_interp [] (interp,interp2) then
                  (t,get_sels [] (interp,interp2)) :: found else found)
          (* | D(s,cat), Dig(s2,cat2) -> if s=s2 && cat=cat2 then (t,[]) :: found else found *)
          | D cat , Dig(s2,cat2) -> if cat=cat2 then (t,[]) :: found else found
          | I s, Interp s2 -> if s=s2 then (t,[]) :: found else found
          | SL, SmallLetter _ -> (t,[]) :: found
          | SL, CapLetter _ -> (t,[]) :: found
          | _ -> found))) in
    Xlist.fold found [] (fun found (t,sels) -> coordination_match_path_rec map found t sels [] l)

let coordination_create_token (matching:token_env list) cat =
	let l = List.rev matching in
	let beg = (List.hd l).beg in
	let t = List.hd matching in
	[{
		empty_token_env with
		beg=beg;
		next=t.next;
		cat=cat
	}]

let coordination_apply_rule paths (match_list,interp_cat) =
  let matchings_found = coordination_match_path paths match_list in
  Xlist.fold matchings_found paths (fun paths (matching,sels) ->
    try
      (* Xlist.fold (create_token (* is_mwe = *)true matching sels lemma cat interp) paths add_token2 *)
			Xlist.fold (coordination_create_token matching interp_cat) paths add_token2
    with Not_found -> paths)

let initialize () =
  print_endline "initialize";
  ()

let disambiguate tokens =
  print_endline (ENIAMsubsyntaxStringOf.token_list tokens);
  print_endline "";
	(* Zamieniam listę tokenów na format akceptowany przez apply_rule (czyli mapę
	map TokenEnvSetów). *)
	let paths = Xstd.IntMap.empty in
	let tokens = Xlist.fold tokens paths add_token2 in
	(* Wczytuję reguły z pliku. *)
	let rules = File.catch_no_file (coordination_load_mwe_dict coordination_filename) [] in
	(* Aplikuję reguły. *)
  let tokens = Xlist.fold rules tokens coordination_apply_rule in
	let tokens = Xlist.fold rules tokens coordination_apply_rule in
	(* Konwertuję |tokens| z powrotem do listy token_env. *)
	let tokens = IntMap.fold tokens [] (fun found i map2 ->
		IntMap.fold map2 found (fun found j l ->
			TokenEnvSet.fold l found (fun found t ->
				t :: found ))) in
  tokens

let catch_disambiguate tokens =
  try
    let tokens = disambiguate tokens in tokens,"" (* ENIAM_MWE.disambiguate ? *)
  with e -> [], Printexc.to_string e

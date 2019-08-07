(*
 *  ENIAMlexcreator: tool for phrase selection from corpus
 *  Copyright (C) 2007-2009, 2019 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
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
open Types

type project = {
    mutable filename: string;
    mutable corpus_filename: string;
    mutable loaded_corpus: string StringMap.t;
    mutable showed_corpus: (string * string) list StringMap.t;
    mutable showed_corpus_model_changed: (unit -> unit) list;
    mutable subcorpora: subcorpus StringMap.t}

let set_showed_corpus project showed_corpus =
  project.showed_corpus <- showed_corpus;
  Xlist.iter project.showed_corpus_model_changed (fun model_changed -> model_changed ())

let fold_selection selection s f =
  let progress = Progress.create "Fold Selected" (StringSet.size selection) in
  let v = StringSet.fold selection s (fun s id ->
    Progress.next progress;
    f s id) in
  Progress.destroy progress;
  v

let fold_loaded project s f =
  let progress = Progress.create "Fold Loaded" (StringMap.size project.loaded_corpus) in
  let v = StringMap.fold project.loaded_corpus s (fun s id graph ->
    Progress.next progress;
    f s id graph) in
  Progress.destroy progress;
  v

let fold_showed project s f =
  let progress = Progress.create "Fold Showed" (StringMap.size project.showed_corpus) in
  let v = StringMap.fold project.showed_corpus s (fun s id graph ->
    Progress.next progress;
    f s id graph) in
  Progress.destroy progress;
  v

let execution n_workers project corpus selected =
  let n_workers = min n_workers (Xlist.size selected) in
  if n_workers = 0 then corpus else
  let r = ref (Xlist.size selected + n_workers) in 
  let work = ref selected in
  let corpus = ref corpus in
  let io_list = Int.fold 1 n_workers [] (fun io_list _ ->
    let in_chan,out_chan = Unix.open_process verse_worker_filename in
(*    Marshal.to_channel out_chan Init [Marshal.No_sharing]; 
    flush out_chan;*)
    let descr = Unix.descr_of_in_channel in_chan in
    (in_chan,out_chan,descr) :: io_list) in
  let descr_list = Xlist.map io_list (fun (_,_,descr) -> descr) in
  let progress = Progress.create "Parsing" (Xlist.size selected) in
(*   print_endline "execution: start"; *)
  while !r <> 0 do 
    let list,_,_ = Unix.select descr_list [] [] (-1.) in
    Xlist.iter list (fun descr2 ->
      decr r; 
      Xlist.iter io_list (fun (in_chan,out_chan,descr) ->
        if descr = descr2 then (
          (match Marshal.from_channel in_chan with
            Ready_to_work -> ()
          | Work_done (id,verse) -> 
              Progress.next progress;
              corpus := StringMap.add (!corpus) id verse);
          match !work with 
            id :: l -> 
              Marshal.to_channel out_chan (Work_with(id,StringMap.find project.loaded_corpus id)) [Marshal.No_sharing]; 
              flush out_chan;
              work := l
          | [] -> ())))
  done;
(*   print_endline "execution: end1"; *)
  Xlist.iter io_list (fun (in_chan,out_chan,descr) -> 
    Marshal.to_channel out_chan Kill_yourself [Marshal.No_sharing];
    flush out_chan);
(*   print_endline "execution: end2"; *)
(*   print_endline (id ^ " exit"); *)
  Progress.destroy progress;
  !corpus

let update_corpus project = 
(*   print_endline "update_corpus 1"; *)
  let selected = StringMap.fold project.subcorpora StringSet.empty (fun set _ subcorpus ->
    if subcorpus.status = Showed then 
      StringSet.union set subcorpus.ids 
    else set) in
(*   Printf.printf "update_corpus 2: |selected|=%d\n%!" (StringSet.size selected); *)
  let corpus, selected = StringSet.fold selected (StringMap.empty, []) (fun (corpus,selected) id ->
    try
      StringMap.add corpus id (StringMap.find project.showed_corpus id), selected
    with Not_found ->
      corpus, id :: selected) in
(*   Printf.printf "update_corpus 3: |loaded|=%d\n%!" (StringMap.size project.loaded_corpus); *)
  let corpus = execution !no_workers project corpus selected in
  set_showed_corpus project corpus

(********************************************************************)

let empty _ = 
  {filename=""; corpus_filename = ""; 
   loaded_corpus = StringMap.empty; showed_corpus = StringMap.empty; 
   subcorpora = StringMap.empty; 
   showed_corpus_model_changed=[]}

let xml_scan filename project = 
  match Xml.parse_file filename with
    Xml.Element("project",["corpus_filename",corpus_filename], list) ->
                             project.corpus_filename <- corpus_filename;
                             let subcorpora = Xlist.fold list StringMap.empty (fun subcorpora -> function
                                 Xml.Element("subcorpus",["name",name;"status",status], list) ->
                                   let ids = Xlist.fold list StringSet.empty (fun ids -> function
                                       Xml.Element("id",["id",id],[]) -> StringSet.add ids id
                                     | _ -> failwith "xml_scan 3") in
                                   StringMap.add subcorpora name {status=subcstatus_of_string status; ids=ids}
                               | _ -> failwith "xml_scan 2") in
                             project.subcorpora <- subcorpora
  | _ -> failwith "xml_scan 1"

let set_corpus project corpus_filename = 
  project.corpus_filename <- corpus_filename;
  project.loaded_corpus <- StringMap.empty;
  set_showed_corpus project StringMap.empty;
  let ids = 
      if Sys.file_exists project.corpus_filename then 
        let corpus = File.load_lines project.corpus_filename in
        let ids,_ = Xlist.fold corpus (StringSet.empty,0) (fun (ids,n) line ->
          StringSet.add ids (string_of_int n), n+1) in
        ids
      else StringSet.empty in
  project.subcorpora <- 
    StringMap.add StringMap.empty "default" {status=Loaded; ids=ids}
    
let load_corpus project =
  let corpus =
      if Sys.file_exists project.corpus_filename then 
        let corpus = File.load_lines project.corpus_filename in
        let corpus,_ = Xlist.fold corpus (StringMap.empty,0) (fun (corpus,n) line ->
          StringMap.add corpus (string_of_int n) line, n+1) in
        corpus
      else StringMap.empty in
  project.loaded_corpus <- corpus

let new_project update project () = 
  project.filename <- "";
  set_corpus project "";
  update ()

let open_project (update, project, filename) =  
  project.filename <- filename;
  xml_scan filename project;
  project.loaded_corpus <- StringMap.empty;
  set_showed_corpus project StringMap.empty;
  load_corpus project;
  update_corpus project;
  update ()

let open_corpus (update, project, corpus_filename) = 
  set_corpus project corpus_filename; 
  load_corpus project;
  update_corpus project;
  update ()

let save_project project () =
  let xml = 
    Xml.Element("project",["corpus_filename",project.corpus_filename],
                StringMap.fold project.subcorpora [] (fun list name subcorpus -> 
                  Xml.Element("subcorpus",["name",name;"status",string_of_subcstatus subcorpus.status],
                              StringSet.fold subcorpus.ids [] (fun list id -> Xml.Element("id",["id",id],[]) :: list)) :: list)) in
  File.file_out project.filename (fun file -> output_string file (Xml.to_string_fmt xml))

(********************************************************************)

let move project dest_name ids = 
  let dest_subcorpus = StringMap.find project.subcorpora dest_name in
  let dest_subcorpus = {status=dest_subcorpus.status; ids=StringSet.union dest_subcorpus.ids ids} in
  let subcorpora = StringMap.remove project.subcorpora dest_name in
  let subcorpora = StringMap.map subcorpora (fun subcorpus ->
    {status=subcorpus.status; ids=StringSet.difference subcorpus.ids ids}) in
  project.subcorpora <- StringMap.add subcorpora dest_name dest_subcorpus;
  update_corpus project

let change_subcorpus_status (project, name, status) =
  let subcorpus = StringMap.find project.subcorpora name  in
  project.subcorpora <- StringMap.add project.subcorpora name {status=status; ids=subcorpus.ids};
  update_corpus project

let select project symbol subcorpus status_after =
  match subcorpus.status with
  | Loaded ->
      fold_loaded project StringSet.empty (fun ids id text ->
        if StringSet.mem subcorpus.ids id then 
          let text = Xstring.split " " text in
          let b = Xlist.fold text false (fun b s -> if s = symbol then true else b) in
          if b then (
            StringSet.add ids id 
           ) else ids
        else ids)
  | Showed ->
      fold_showed project StringSet.empty (fun ids id text ->
        if StringSet.mem subcorpus.ids id then 
          let b = Xlist.fold text false (fun b (s,_) -> if s = symbol then true else b) in
          if b then (
            StringSet.add ids id 
           ) else ids
        else ids)


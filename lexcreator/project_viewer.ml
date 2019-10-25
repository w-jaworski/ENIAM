(*
 *  ENIAMlexcreator: tool for phrase selection from corpus
 *  Copyright (C) 2009, 2019 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
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
open Project
open Types

type 'a project_viewer = {
    project: project;
    main_vbox: GPack.box;
    filename_label: GMisc.label;
    corpus_filename_label: GMisc.label;
    mutable map: 'a StringMap.t;
    subcorpus_vbox: GPack.box;
    move_from_combo_box: GEdit.combo;
    move_to_combo_box: GEdit.combo;
    delete_subcorpus_combo_box: GEdit.combo;
    }

let select_file title filename ok_fun =
  let filew = GWindow.file_selection ~modal:true ~title ~filename () in
  let _ = filew#ok_button#connect#clicked ~callback: (fun () -> 
    ok_fun filew#filename;
    filew#destroy ()) in 
  let _ = filew#cancel_button#connect#clicked ~callback:filew#destroy in
  filew#show ()

let overwrite_dialog label old_filename new_filename load_fun =
  if Sys.file_exists new_filename then (
    let dialog = GWindow.dialog ~title:"File exists!" ~modal:true ~position:`CENTER_ALWAYS () in
    let _ = GMisc.label ~text:"Do you wish to overwrite it, open it or cancel operation?" ~packing:dialog#vbox#add () in
    let overwrite_button = GButton.button ~label:"Overwrite" ~packing:dialog#action_area#add () in
    let open_button = GButton.button ~label:"Open" ~packing:dialog#action_area#add () in
    let cancel_button = GButton.button ~label:"Cancel" ~packing:dialog#action_area#add () in
    ignore(overwrite_button#connect#clicked ~callback:(fun () -> 
      label#set_text new_filename; 
      dialog#destroy ()));
    ignore(open_button#connect#clicked ~callback:(fun () -> 
      label#set_text new_filename; 
      load_fun new_filename;
      dialog#destroy ()));
    ignore(cancel_button#connect#clicked ~callback:dialog#destroy);
    dialog#show ())
  else label#set_text new_filename

let fill_words_clist (project, words_clist, words_label) =
  let selected = StringMap.fold project.subcorpora StringSet.empty (fun set _ subcorpus ->
    if subcorpus.status = Showed then 
      StringSet.union set subcorpus.ids 
    else set) in
  let qmap = StringSet.fold selected StringQMap.empty (fun qmap id ->
    let verse = try StringMap.find project.showed_corpus id with Not_found -> failwith "make_words_freq_list" in
    Xlist.fold verse qmap (fun qmap (cat,orth) -> StringQMap.add qmap cat)) in
  let l = StringQMap.fold qmap [] (fun l k v -> (v,k) :: l) in
  let l = List.sort (fun (v1,_) (v2,_) -> compare v1 v2) l in     
  words_label#set_text (string_of_int (Xlist.size l));
  words_clist#freeze ();
  words_clist#clear ();
  Xlist.iter l (fun (v,word) ->
    let _ = words_clist#append [Printf.sprintf "%7d" v; word] in ());
  words_clist#columns_autosize ();
  words_clist#thaw ()

let set_combo map combo =
  let s = combo#entry#text in
  let list = StringMap.fold map [] (fun list name _ -> name :: list) in
  combo#set_popdown_strings list;
  if list = [] then combo#entry#set_text "" else combo#entry#set_text s

let set_combo_zero map combo =
  let list = StringMap.fold map [] (fun list name subcorpus -> 
    if StringSet.size subcorpus.ids = 0 then name :: list else list) in
  combo#set_popdown_strings list;
  if list = [] then combo#entry#set_text ""

let change_subcorpus_status project name status =
  ignore(Thread.create change_subcorpus_status (project, name, status))

(* FIXME: co sie dzieje gdy jeden update jest w trakcie drugiego i kiedy to sie przydarza (ladowanie projektu) *)

let update project_viewer verse_viewer () = 
  project_viewer.filename_label#set_text project_viewer.project.filename;
  project_viewer.corpus_filename_label#set_text project_viewer.project.corpus_filename;
  let old_map = StringMap.fold project_viewer.map StringMap.empty (fun old_map name scv ->
    if StringMap.mem project_viewer.project.subcorpora name then StringMap.add old_map name scv else (
    Subcorpus_viewer.destroy scv;
    old_map)) in
  project_viewer.map <- StringMap.fold project_viewer.project.subcorpora StringMap.empty (fun map name subcorpus ->
    try 
      let scv = StringMap.find old_map name in
      Subcorpus_viewer.set_model scv subcorpus;
      StringMap.add map name scv
    with Not_found ->
      let scv = Subcorpus_viewer.create name subcorpus (change_subcorpus_status project_viewer.project name) in
      Subcorpus_viewer.set_model scv subcorpus;
      project_viewer.subcorpus_vbox#pack (Subcorpus_viewer.coerce scv);
      StringMap.add map name scv);
  set_combo project_viewer.map project_viewer.move_from_combo_box;
  set_combo project_viewer.map project_viewer.move_to_combo_box;
  set_combo_zero project_viewer.project.subcorpora project_viewer.delete_subcorpus_combo_box;
  set_combo project_viewer.map verse_viewer.Verse_viewer.exclude_combo_box

let move (n, project_viewer, verse_viewer) =
  let from_name = project_viewer.move_from_combo_box#entry#text in
  let to_name = project_viewer.move_to_combo_box#entry#text in
  let from_subcorpus = StringMap.find project_viewer.project.subcorpora from_name in
print_endline ("move2 " ^ (String.concat " " (StringSet.to_list from_subcorpus.ids)));
  let to_subcorpus = StringMap.find project_viewer.project.subcorpora to_name in
  let from_set,to_set,_ = StringSet.fold from_subcorpus.ids (StringSet.empty,to_subcorpus.ids,n) (fun (from_set,to_set,n) id ->
    if n > 0 then from_set,StringSet.add to_set id,n-1
    else StringSet.add from_set id, to_set, n-1) in
  project_viewer.project.subcorpora <-
    StringMap.add (StringMap.add project_viewer.project.subcorpora from_name {status=from_subcorpus.status; ids=from_set}) 
      to_name {status=to_subcorpus.status; ids=to_set};
  update_corpus project_viewer.project;
  update project_viewer verse_viewer ()

let move_selected (symbol, project_viewer, verse_viewer) =
  let from_name = project_viewer.move_from_combo_box#entry#text in
  let to_name = project_viewer.move_to_combo_box#entry#text in
  let from_subcorpus = StringMap.find project_viewer.project.subcorpora from_name in
  let to_subcorpus = StringMap.find project_viewer.project.subcorpora to_name in
  let ids = select project_viewer.project symbol from_subcorpus to_subcorpus.status in
  let from_set,to_set = StringSet.fold from_subcorpus.ids (StringSet.empty,to_subcorpus.ids) (fun (from_set,to_set) id ->
    if StringSet.mem ids id then from_set,StringSet.add to_set id
    else StringSet.add from_set id, to_set) in
  project_viewer.project.subcorpora <-
    StringMap.add (StringMap.add project_viewer.project.subcorpora from_name {status=from_subcorpus.status; ids=from_set}) 
      to_name {status=to_subcorpus.status; ids=to_set};
  update_corpus project_viewer.project;
  update project_viewer verse_viewer ()

let create project verse_viewer =
  let main_hbox = GPack.hbox () in
  let main_vbox = GPack.vbox ~packing:main_hbox#pack () in
  let filename_hbox = GPack.hbox ~packing:main_vbox#pack () in
  let _ = GMisc.label ~text:"Select Project Filename" ~packing:filename_hbox#pack () in
  let filename_button = GButton.button ~label:"Select" ~packing:filename_hbox#pack () in 
  let filename_label = GMisc.label ~packing:filename_hbox#pack () in
  let corpus_filename_hbox = GPack.hbox ~packing:main_vbox#pack () in
  let _ = GMisc.label ~text:"Select Corpus Filename" ~packing:corpus_filename_hbox#pack () in
  let corpus_filename_button = GButton.button ~label:"Select" ~packing:corpus_filename_hbox#pack () in 
  let corpus_filename_label = GMisc.label ~packing:corpus_filename_hbox#pack () in
  let move_hbox = GPack.hbox ~packing:main_vbox#pack () in
  let move_hbox2 = if !low_res_flag then GPack.hbox ~packing:main_vbox#pack () else move_hbox in
  let move_from_combo_box = GEdit.combo ~packing:move_hbox#pack () in
  let _ = GMisc.label ~text:"move to" ~packing:move_hbox#pack () in
  let move_to_combo_box = GEdit.combo ~packing:move_hbox#pack () in
  let _ = GMisc.label ~text:"quantity" ~packing:move_hbox2#pack () in
  let move_1_button = GButton.button ~label:"1" ~packing:move_hbox2#pack () in 
  let move_10_button = GButton.button ~label:"10" ~packing:move_hbox2#pack () in 
  let move_100_button = GButton.button ~label:"100" ~packing:move_hbox2#pack () in 
  let move_1000_button = GButton.button ~label:"1000" ~packing:move_hbox2#pack () in 
  let move_10000_button = GButton.button ~label:"10000" ~packing:move_hbox2#pack () in 
  let move_all_button = GButton.button ~label:"All" ~packing:move_hbox2#pack () in 
  let move_selected_button = GButton.button ~label:"Selected" ~packing:move_hbox2#pack () in 
  let move_selected_entry = GEdit.entry ~packing:move_hbox2#pack ~text:"" () in
  let delete_subcorpus_hbox = GPack.hbox ~packing:main_vbox#pack () in
  let delete_subcorpus_combo_box = GEdit.combo ~packing:delete_subcorpus_hbox#pack () in
  let delete_subcorpus_button = GButton.button ~label:"Delete" ~packing:delete_subcorpus_hbox#pack () in 
  let add_subcorpus_hbox = GPack.hbox ~packing:main_vbox#pack () in
  let add_subcorpus_entry = GEdit.entry ~packing:add_subcorpus_hbox#pack () in
  let add_subcorpus_button = GButton.button ~label:"Add" ~packing:add_subcorpus_hbox#pack () in 
  let count_concepts_hbox = GPack.hbox ~packing:main_vbox#pack () in
  let count_concepts_button = GButton.button ~label:"Count Words" ~packing:count_concepts_hbox#pack () in 
  let words_clist_vbox = GPack.vbox ~packing:(main_hbox#pack ~expand:true) () in
  let vadjustment = GData.adjustment () in
  let words_clist = GList.clist ~titles:["Quantity";"Word"] 
      ~height:(if !low_res_flag then 250 else 500) ~vadjustment ~selection_mode:`SINGLE ~packing:(words_clist_vbox#pack ~expand:true) () in
  let _ = GRange.scrollbar `VERTICAL ~adjustment:vadjustment ~packing:main_hbox#pack () in
  words_clist#set_column ~justification:`RIGHT ~resizeable:true 0; 
  words_clist#set_column ~resizeable:true 1; 
  let words_clist_hbox2 = GPack.hbox ~packing:words_clist_vbox#pack () in
  let _ = GMisc.label ~text:"Number of entries: " ~packing:words_clist_hbox2#pack () in
  let words_label = GMisc.label ~text:"        " ~packing:words_clist_hbox2#pack () in
  let subcorpus_scrolled_window = GBin.scrolled_window ~vpolicy:`AUTOMATIC ~hpolicy:`NEVER 
      ~packing:(main_vbox#pack ~expand:true) () in
  let subcorpus_vbox = GPack.vbox ~packing:subcorpus_scrolled_window#add_with_viewport () in
  let project_viewer = {main_vbox=main_hbox; filename_label=filename_label;
			corpus_filename_label=corpus_filename_label;
			map=StringMap.empty; subcorpus_vbox=subcorpus_vbox; move_from_combo_box=move_from_combo_box;
			move_to_combo_box=move_to_combo_box; delete_subcorpus_combo_box=delete_subcorpus_combo_box;
			project=project} in
  ignore(filename_button#connect#clicked ~callback:(fun () ->
    select_file "Select Project" project.filename (fun filename ->
      project.filename <- filename;
      overwrite_dialog filename_label project.filename filename (fun filename ->
	ignore(Thread.create Project.open_project (update project_viewer verse_viewer, project, filename))))));
  ignore(corpus_filename_button#connect#clicked ~callback:(fun () ->
    select_file "Select Corpus" project.corpus_filename (fun filename ->
      project.corpus_filename <- filename;
      overwrite_dialog corpus_filename_label project.corpus_filename filename (fun corpus_filename ->
	ignore(Thread.create Project.open_corpus (update project_viewer verse_viewer, project, corpus_filename))))));
  ignore(move_1_button#connect#clicked ~callback:(fun () ->
    ignore(Thread.create move (1, project_viewer, verse_viewer))));
  ignore(move_10_button#connect#clicked ~callback:(fun () ->
    ignore(Thread.create move (10, project_viewer, verse_viewer))));
  ignore(move_100_button#connect#clicked ~callback:(fun () ->
    ignore(Thread.create move (100, project_viewer, verse_viewer))));
  ignore(move_1000_button#connect#clicked ~callback:(fun () ->
    ignore(Thread.create move (1000, project_viewer, verse_viewer))));
  ignore(move_10000_button#connect#clicked ~callback:(fun () ->
    ignore(Thread.create move (10000, project_viewer, verse_viewer))));
  ignore(move_all_button#connect#clicked ~callback:(fun () ->
    ignore(Thread.create move (max_int, project_viewer, verse_viewer))));
  ignore(move_selected_button#connect#clicked ~callback:(fun () ->
    ignore(Thread.create move_selected (move_selected_entry#text, project_viewer, verse_viewer))));
  ignore(delete_subcorpus_button#connect#clicked ~callback:(fun () ->
    let name = delete_subcorpus_combo_box#entry#text in
    try 
      let subcorpus = StringMap.find project.subcorpora name in
      if StringSet.size subcorpus.ids = 0 then (
	project.subcorpora <- StringMap.remove project.subcorpora name;
	update project_viewer verse_viewer ()) (* brak uaktualnienie verse_vever - excluced ??? *)
    with Not_found -> ()));
  ignore(add_subcorpus_button#connect#clicked ~callback:(fun () ->
    let name = add_subcorpus_entry#text in
    add_subcorpus_entry#set_text ""; (* brak uaktualnienie verse_vever - excluced ??? *)
    if name <> "" && not (StringMap.mem project.subcorpora name) then 
      let subcorpus = {status=Loaded; ids=StringSet.empty} in
      project.subcorpora <- StringMap.add project.subcorpora name subcorpus;
      update project_viewer verse_viewer ()));
  ignore (count_concepts_button#connect#clicked ~callback:(fun () ->
    ignore(Thread.create fill_words_clist (project, words_clist, words_label))));
  ignore (words_clist#connect#click_column ~callback: (fun col ->
    words_clist#set_sort ~column:col ~dir:`ASCENDING ();
    words_clist#sort ()));
  project_viewer

let coerce project_viewer =
  project_viewer.main_vbox#coerce

let visible project_viewer = (* FIXME: dlaczego to jest niepotrzebne? *)
  ()


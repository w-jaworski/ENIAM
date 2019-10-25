(*
 *  ENIAMlexcreator: tool for phrase selection from corpus
 *  Copyright (C) 2007, 2019 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
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

open Project
open Xstd
open Types

type verse_viewer = {
    line_interface: GPack.box;
    project: project;
    mutable verses: ((string * string) list * (string * string) list * 
		       (string * string) list * (string * int * int) list) Xstd.StringMap.t list;
    mutable column_state: int;
    line_clist: string GList.clist;
    adjustment: GData.adjustment;
    size_label: GMisc.label;
    exclude_combo_box: GEdit.combo;
    mutable project_viewer_update_fun: unit -> unit}

let save_file = open_out "results/selected_phrases.tab"

(*let set_selected clist selection =
  print_endline "set_selected";
  Int.iter 0 (clist#rows - 1)  (fun set -> fun i ->
    let k = clist#cell_text i 0 in
    if StringSet.mem selection k then 
      clist#select i (i+1)
    else clist#unselect i (i+1))*)
(*    if StringSet.mem selection k then 
      clist#set_row_state i `SELECTED
    else clist#set_row_state i `NORMAL)*)
      
let set_selected clist selection = 
(*   print_endline "set_selected A"; *)
  Int.iter 0 (clist#rows - 1) (fun i ->
    if StringSet.mem selection (clist#cell_text i 0) then 
      clist#select i 1
    else clist#unselect i 1)

let get_selected clist =
  Int.fold 0 (clist#rows - 1) StringSet.empty (fun set -> fun i ->
    if clist#get_row_state i = `SELECTED then 
      StringSet.add set (clist#cell_text i 0) else set)

let get_all_selected clist =
  Int.fold 0 (clist#rows - 1) StringSet.empty (fun set -> fun i ->
    StringSet.add set (clist#cell_text i 0))

let reset_column_names (clist : string GList.clist) i =
  clist#set_column ~title:"No" 1;
  clist#set_column ~title:"Pre" 2;
  clist#set_column ~title:"Vis" 3;
  clist#set_column ~title:"Post" 4;
  match i with
    1 -> clist#set_column ~title:"No v" 1 
  | 2 -> clist#set_column ~title:"Pre v" 2 
  | 3 -> clist#set_column ~title:"Vis v" 3  
  | 4 -> clist#set_column ~title:"Post v" 4 
  | -1 -> clist#set_column ~title:"No ^" 1 
  | 5 -> clist#set_column ~title:"Pre ^" 2 
  | 6 -> clist#set_column ~title:"Vis ^" 3 
  | 7 -> clist#set_column ~title:"Post ^" 4 
  | _ -> failwith "reset_column_names"

let sort_clist verse_viewer col =
  if col = 1 then
    if verse_viewer.column_state = 1 then (
      verse_viewer.column_state <- -1;
      verse_viewer.line_clist#set_sort ~column:col ~dir:`DESCENDING ()) 
    else (
      verse_viewer.column_state <- 1;
      verse_viewer.line_clist#set_sort ~column:col ~dir:`ASCENDING ()) 
  else (
    if verse_viewer.column_state = col then verse_viewer.column_state <- col + 3
    else verse_viewer.column_state <- col;
    verse_viewer.line_clist#set_sort ~column:verse_viewer.column_state ~dir:`ASCENDING ());
  reset_column_names verse_viewer.line_clist verse_viewer.column_state;
  verse_viewer.line_clist#sort ()

let make_line_clist verse_viewer =
  let v = verse_viewer.adjustment#value in
  verse_viewer.line_clist#freeze ();
  verse_viewer.line_clist#clear ();
  StringMap.iter (List.hd verse_viewer.verses) (fun k -> fun (pre_vis,vis,post_vis,lines) ->
    let _ = verse_viewer.line_clist#append 
	[k;(Printf.sprintf "%5d" (Xlist.size lines));
	 (Verse.string_of_verse pre_vis);(Verse.string_of_verse vis);Verse.string_of_verse post_vis;
	 Verse.string_of_verse (List.rev pre_vis);Verse.string_of_verse (List.rev vis);
	 Verse.string_of_verse (List.rev post_vis)] in ());
  verse_viewer.size_label#set_text (string_of_int (StringMap.size (List.hd verse_viewer.verses)));
  let x = if verse_viewer.column_state = 0 then 3 else verse_viewer.column_state in
  verse_viewer.column_state <- 0;
  sort_clist verse_viewer x;
  verse_viewer.line_clist#columns_autosize ();
  verse_viewer.line_clist#thaw ();
  verse_viewer.adjustment#set_value v

let update verse_viewer () =
(*   print_endline "Verse_viewer.update"; *)
  verse_viewer.verses <- [Verse.join verse_viewer.project.showed_corpus];
  verse_viewer.column_state <- 0;
  make_line_clist verse_viewer;
  let list = StringMap.fold verse_viewer.project.subcorpora [] (fun list name subcorpus ->
    if subcorpus.status = Loaded then name :: list else list) in
  verse_viewer.exclude_combo_box#set_popdown_strings list;
  if list = [] then verse_viewer.exclude_combo_box#entry#set_text ""

let move_lines verse_viewer f =
  let selection = get_selected verse_viewer.line_clist in
  verse_viewer.verses <- (f (List.hd verse_viewer.verses)) :: verse_viewer.verses;
  make_line_clist verse_viewer;
  set_selected verse_viewer.line_clist selection

let move_lines_selection verse_viewer f =
  let selection = get_selected verse_viewer.line_clist in
  verse_viewer.verses <- (f selection (List.hd verse_viewer.verses)) :: verse_viewer.verses;
  make_line_clist verse_viewer;
  set_selected verse_viewer.line_clist selection

let rec find_symbol left symbol = function 
    [] -> failwith "find_symbol"
  | s :: list -> 
      if s = symbol then List.rev left, list else find_symbol (s :: left) symbol list

let create_prod edges symbol =
  let left,right = find_symbol [] symbol edges in
  symbol ^ (String.concat "" (Xlist.map left (fun x -> "\\" ^ x))) ^ 
      (String.concat "" (Xlist.map right (fun x -> "#" ^ x))) 

let create project = 
  let line_interface = GPack.vbox () in
  let hbox = GPack.hbox ~packing:(line_interface#pack ~expand:false) () in
  let hbox2 = GPack.hbox ~packing:(line_interface#pack ~expand:false) () in
  let hbox3 = if !low_res_flag then GPack.hbox ~packing:(line_interface#pack ~expand:false) () else hbox2 in
  let undo_button = GButton.button ~label:"Undo" ~packing:(hbox#pack ~expand:false) () in
  let exclude_combo_box = GEdit.combo ~packing:hbox#pack () in
  let exclude_button = GButton.button ~label:"Exclude" ~packing:(hbox#pack ~expand:false) () in
  let move_left_button = GButton.button ~label:"Move left" ~packing:(hbox#pack ~expand:false) () in
  let entry = GEdit.entry ~text:"" ~packing:hbox#pack () in
  let move_right_button = GButton.button ~label:"Move right" ~packing:(hbox#pack ~expand:false) () in
  let move_to_pre_vis_button = GButton.button ~label:"Pre <- Vis" ~packing:(hbox2#pack ~expand:false) () in
  let move_to_pre_vis_button2 = GButton.button ~label:"Pre <-- Vis" ~packing:(hbox2#pack ~expand:false) () in
  let move_to_pre_vis_button3 = GButton.button ~label:"Pre <--- Vis" ~packing:(hbox2#pack ~expand:false) () in
  let move_to_pre_vis_all_button = GButton.button ~label:"Pre <<- Vis" ~packing:(hbox2#pack ~expand:false) () in
  let move_from_pre_vis_all_button = GButton.button ~label:"Pre ->> Vis" ~packing:(hbox2#pack ~expand:false) () in
  let move_from_pre_vis_button3 = GButton.button ~label:"Pre ---> Vis" ~packing:(hbox2#pack ~expand:false) () in
  let move_from_pre_vis_button2 = GButton.button ~label:"Pre --> Vis" ~packing:(hbox2#pack ~expand:false) () in
  let move_from_pre_vis_button = GButton.button ~label:"Pre -> Vis" ~packing:(hbox2#pack ~expand:false) () in
  let move_from_post_vis_button = GButton.button ~label:"Vis <- Post" ~packing:(hbox3#pack ~expand:false) () in
  let move_from_post_vis_button2 = GButton.button ~label:"Vis <-- Post" ~packing:(hbox3#pack ~expand:false) () in
  let move_from_post_vis_button3 = GButton.button ~label:"Vis <--- Post" ~packing:(hbox3#pack ~expand:false) () in
  let move_from_post_vis_all_button = GButton.button ~label:"Vis <<- Post" ~packing:(hbox3#pack ~expand:false) () in
  let move_to_post_vis_all_button = GButton.button ~label:"Vis ->> Post" ~packing:(hbox3#pack ~expand:false) () in
  let move_to_post_vis_button3 = GButton.button ~label:"Vis ---> Post" ~packing:(hbox3#pack ~expand:false) () in
  let move_to_post_vis_button2 = GButton.button ~label:"Vis --> Post" ~packing:(hbox3#pack ~expand:false) () in
  let move_to_post_vis_button = GButton.button ~label:"Vis -> Post" ~packing:(hbox3#pack ~expand:false) () in
  let save_verses_button = GButton.button ~label:"Save verses" ~packing:(hbox#pack ~expand:false) () in
  let view_visible_graph_button = GButton.button ~label:"View original text" ~packing:(hbox#pack ~expand:false) () in
  let delete_selected_button = GButton.button ~label:"Delete selected" ~packing:(hbox2#pack ~expand:false) () in
  let size_label = GMisc.label ~packing:(hbox2#pack ~from:`END) () in
  let vadjustment = GData.adjustment () in
  let hadjustment = GData.adjustment () in
  let clist_hbox = GPack.hbox ~packing:(line_interface#pack ~expand:true) () in
  let line_clist = GList.clist ~titles:["Key";"No";"Pre";"Vis";"Post";"Rev Pre";"Rev Vis";"Rev Post"] 
      ~height:(if !low_res_flag then 250 else 500) ~width:(if !low_res_flag then 500 else 1000) 
      ~vadjustment ~hadjustment ~selection_mode:`MULTIPLE ~packing:(clist_hbox#pack ~expand:true) () in
  let _ = GRange.scrollbar `VERTICAL ~adjustment:vadjustment ~packing:clist_hbox#pack () in
  let _ = GRange.scrollbar `HORIZONTAL ~adjustment:hadjustment ~packing:line_interface#pack () in
  line_clist#set_column ~visibility:false 0; 
  line_clist#set_column ~justification:`RIGHT ~resizeable:true 1; 
  line_clist#set_column ~justification:`RIGHT ~resizeable:true 2; 
  line_clist#set_column ~resizeable:true 3; 
  line_clist#set_column ~resizeable:true 4; 
  line_clist#set_column ~visibility:false 5; 
  line_clist#set_column ~visibility:false 6; 
  line_clist#set_column ~visibility:false 7; 
  let verse_viewer = {
    line_interface=line_interface; project=project;
    verses=[StringMap.empty]; column_state=0;
    line_clist=line_clist; adjustment=vadjustment; size_label=size_label; exclude_combo_box=exclude_combo_box;
    project_viewer_update_fun=(fun () -> ())} in
  ignore (line_clist#connect#click_column ~callback: (sort_clist verse_viewer));
  ignore (undo_button#connect#clicked ~callback: (fun () ->
    let selection = get_selected line_clist in
    verse_viewer.verses <- (match verse_viewer.verses with [] -> [] | [x] -> [x] | _ :: x :: l -> x :: l);
    make_line_clist verse_viewer;
    set_selected line_clist selection));
  ignore (exclude_button#connect#clicked ~callback: (fun () ->
    let selection = get_selected line_clist in
    let id_map = Verse.get_verses (Verse.select selection (List.hd verse_viewer.verses)) in
    verse_viewer.verses <- (Verse.remove selection (List.hd verse_viewer.verses)) :: verse_viewer.verses;
    make_line_clist verse_viewer; (* brakuje odswierzania project viewer ??? *)
    Project.move project exclude_combo_box#entry#text (StringMap.fold id_map StringSet.empty (fun l id _ -> StringSet.add l id));
    verse_viewer.project_viewer_update_fun ()));
  ignore (move_left_button#connect#clicked ~callback: (fun () ->
    move_lines verse_viewer (Verse.move_term_to_pre_vis entry#text)));
  ignore (move_right_button#connect#clicked ~callback: (fun () -> 
    move_lines verse_viewer (Verse.move_term_to_post_vis entry#text)));
  ignore (delete_selected_button#connect#clicked ~callback: (fun () ->
    let selection = get_selected line_clist in
    verse_viewer.verses <- (Verse.remove selection (List.hd verse_viewer.verses)) :: verse_viewer.verses;
    make_line_clist verse_viewer));
  ignore (move_to_pre_vis_button#connect#clicked ~callback: (fun () ->
    move_lines_selection verse_viewer (Verse.move_term_to_pre_visx 1)));
  ignore (move_to_pre_vis_button2#connect#clicked ~callback: (fun () ->
    move_lines_selection verse_viewer (Verse.move_term_to_pre_visx 2)));
  ignore (move_to_pre_vis_button3#connect#clicked ~callback: (fun () ->
    move_lines_selection verse_viewer (Verse.move_term_to_pre_visx 3)));
  ignore (move_to_pre_vis_all_button#connect#clicked ~callback: (fun () ->
    move_lines_selection verse_viewer Verse.move_term_to_pre_vis_allx));
  ignore (move_from_pre_vis_button#connect#clicked ~callback: (fun () ->
    move_lines_selection verse_viewer (Verse.move_term_from_pre_visx 1)));
  ignore (move_from_pre_vis_button2#connect#clicked ~callback: (fun () ->
    move_lines_selection verse_viewer (Verse.move_term_from_pre_visx 2)));
  ignore (move_from_pre_vis_button3#connect#clicked ~callback: (fun () ->
    move_lines_selection verse_viewer (Verse.move_term_from_pre_visx 3)));
  ignore (move_from_pre_vis_all_button#connect#clicked ~callback: (fun () ->
    move_lines_selection verse_viewer Verse.move_term_from_pre_vis_allx));
  ignore (move_to_post_vis_button#connect#clicked ~callback: (fun () ->
    move_lines_selection verse_viewer (Verse.move_term_to_post_visx 1)));
  ignore (move_to_post_vis_button2#connect#clicked ~callback: (fun () ->
    move_lines_selection verse_viewer (Verse.move_term_to_post_visx 2)));
  ignore (move_to_post_vis_button3#connect#clicked ~callback: (fun () ->
    move_lines_selection verse_viewer (Verse.move_term_to_post_visx 3)));
  ignore (move_to_post_vis_all_button#connect#clicked ~callback: (fun () ->
    move_lines_selection verse_viewer Verse.move_term_to_post_vis_allx));
  ignore (move_from_post_vis_button#connect#clicked ~callback: (fun () ->
    move_lines_selection verse_viewer (Verse.move_term_from_post_visx 1)));
  ignore (move_from_post_vis_button2#connect#clicked ~callback: (fun () ->
    move_lines_selection verse_viewer (Verse.move_term_from_post_visx 2)));
  ignore (move_from_post_vis_button3#connect#clicked ~callback: (fun () ->
    move_lines_selection verse_viewer (Verse.move_term_from_post_visx 3)));
  ignore (move_from_post_vis_all_button#connect#clicked ~callback: (fun () ->
    move_lines_selection verse_viewer Verse.move_term_from_post_vis_allx));
  ignore (view_visible_graph_button#connect#clicked ~callback: (fun () ->  
    let window = GWindow.window ~title:"Verses" (*~width:500*) () in 
    let main_vbox = GPack.vbox ~packing:window#add () in
    let vadjustment = GData.adjustment () in
    let hadjustment = GData.adjustment () in
    let clist_hbox = GPack.hbox ~packing:(main_vbox#pack ~expand:true) () in
    let verse_clist = GList.clist ~titles:["Pre";"Vis";"Post"] 
      (*~height:500 ~width:1000*) ~vadjustment ~hadjustment ~packing:(clist_hbox#pack ~expand:true) () in
    let _ = GRange.scrollbar `VERTICAL ~adjustment:vadjustment ~packing:clist_hbox#pack () in
    let _ = GRange.scrollbar `HORIZONTAL ~adjustment:hadjustment ~packing:window#add () in
    verse_clist#set_column ~justification:`RIGHT ~resizeable:true 0; 
    verse_clist#set_column ~resizeable:true 1; 
    verse_clist#set_column ~resizeable:true 2; 
    let selection = get_selected line_clist in
    let verses = Verse.select selection (List.hd verse_viewer.verses) in
    StringMap.iter verses (fun k -> fun (pre_vis,vis,post_vis,lines) ->
      ignore(verse_clist#append 
        [Verse.string_of_verse pre_vis; Verse.string_of_verse vis; Verse.string_of_verse post_vis]);  
      ignore(verse_clist#append [String.concat "" (Xlist.map pre_vis snd);
        String.concat "" (Xlist.map vis snd); String.concat "" (Xlist.map post_vis snd)]));
    verse_clist#columns_autosize ();
    verse_clist#thaw ();
    window#show ()));  
  ignore (save_verses_button#connect#clicked ~callback: (fun () ->
    let selection = get_selected line_clist in
    let verses = Verse.select selection (List.hd verse_viewer.verses) in
    StringMap.iter verses (fun k -> fun (pre_vis,vis,post_vis,lines) ->
(*       Printf.printf "%s\n%!" (String.concat "" (Xlist.map vis snd)); *)
      Printf.fprintf save_file "%s\n" (String.concat "" (Xlist.map vis snd)));
    flush save_file));
   verse_viewer

let coerce verse_viewer =
  verse_viewer.line_interface#coerce

let visible verse_viewer =
  ()

let model_changed verse_viewer () =
  update verse_viewer ()

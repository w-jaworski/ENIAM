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

let lex_creator_name = "LexCreator v1.0"

let about_command () =
  let window = GWindow.dialog ~title:"About LexCreator" () in 
  ignore(GMisc.label ~text:lex_creator_name ~packing:window#vbox#pack ());
  ignore(GMisc.label ~text:"Copyright (c) 2009,2019 by Wojciech Jaworski" ~packing:window#vbox#pack ());
  let ok_button = GButton.button ~label:"Close" ~packing:window#action_area#add () in 
  ignore(ok_button#connect#clicked ~callback:window#destroy);
  window#show ()

let open_project update project () =
  Project_viewer.select_file "Open project" "data" (fun filename -> 
    ignore(Thread.create Project.open_project (update, project, filename)))

let help_entries = [
  `I ("About", about_command); 
]

let quit_command () =
  close_out Verse_viewer.save_file;
  GMain.Main.quit ()

let delete_event ev =
  quit_command (); 
  false

let create_menu label menubar entries = 
  let item = GMenu.menu_item ~label ~packing:menubar#append () in
  let menu = GMenu.menu ~packing:item#set_submenu () in
  GToolbox.build_menu menu ~entries:entries

let main () =
(**  Sys.chdir ((try String.sub Sys.argv.(0) 0 (String.rindex Sys.argv.(0) '/' + 1) with Not_found -> "") ^ "data/");**)
  let window = GWindow.window ~title:lex_creator_name ~border_width:0 () in
  let _ = window#event#connect#delete ~callback:delete_event in 
  let main_vbox = GPack.vbox ~packing:window#add () in
  let menubar = GMenu.menu_bar ~packing:(main_vbox#pack ~expand:false) () in
  let project = Project.empty () in
  let verse_view = Verse_viewer.create project in
  let project_view = Project_viewer.create project in
  let notebook = GPack.notebook ~tab_pos:`TOP ~packing:(main_vbox#pack ~expand:true) () in
  ignore(notebook#insert_page ~pos:0 ~tab_label:(GMisc.label ~text:"Project" ())#coerce (Project_viewer.coerce project_view));
  ignore(notebook#insert_page ~pos:1 ~tab_label:(GMisc.label ~text:"Verse" ())#coerce (Verse_viewer.coerce verse_view));
  ignore(notebook#connect#switch_page ~callback:(function 
(*       0 -> Project_viewer.visible project_view *)
    | 1 -> Verse_viewer.visible verse_view
    | _ -> ()));    (* Odswierzanie combo w Verse_viewer nie dziala gdy zmieniam status subcorpusu ??? *)
  project.Project.showed_corpus_model_changed <- [Verse_viewer.model_changed verse_view];
  let project_entries = [
    `I ("New", Project.new_project (Project_viewer.update project_view) project);
    `I ("Open", open_project (Project_viewer.update project_view) project);
    `I ("Save", Project.save_project project);
    `S;
    `I ("Quit", quit_command) 
  ] in
  create_menu "Project" menubar project_entries;
  create_menu "Help" menubar help_entries;
  Project.new_project (Project_viewer.update project_view) project ();
  window#show ();
  ignore(Glib.Timeout.add ~ms:(1000 * 60 * 15) ~callback:(fun () -> Gc.compact () ; true));
  GtkThread.thread_main ()

let _ = main ()

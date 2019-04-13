(*
 *  ENIAMexec implements ENIAM processing stream
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


open ExecTypes

type output = Html | JSON

let output = ref Html
let verbosity = ref 0
let img = ref 1
let output_dir = ref "results/"
let no_workers = ref 4
let worker_command = ref ""(*"domparser -w --port 1234 --port2 1235 --internet-mode -v 0"*)
let input_filename = ref ""
let select_parsed_flag = ref false
let spec_list = [
  "-h", Arg.Unit (fun () -> output:=Html), "Output as HTML (default)";
  "-j", Arg.Unit (fun () -> output:=JSON), "Output as JSON";
  "-v", Arg.Int (fun v -> verbosity:=v), "<val> Sets verbosity level of parser\n     0 - print only status information (default)\n     1 - print data relevant to the status of a given sentence\n     2 - print all data structures";
  "--img", Arg.Int (fun v -> img:=v), "<val> Selects which images are included in output html page \n     0 - no images included\n     1 - simple dependency trees included (default)\n     2 - dependency trees included";
  "--output", Arg.String (fun s -> output_dir:=s), "<dir> Sets output directory (by default results/)";
  "-n", Arg.Int (fun v -> no_workers:=v), "<val> Sets the number of workers (default 4)";
  "-e", Arg.String (fun v -> worker_command:=v), "<cmd> Command for worker invocation";
  "--in-file", Arg.String (fun v -> input_filename:=v), "<filename> Input filename";
  "--select-parsed", Arg.Unit (fun () -> select_parsed_flag:=true), "Print only parsed teksts";
  ]
  
let usage_msg =
  "Usage: distriparser <options>\nOptions are:"

let message = "ENIAM_LCGparser, semantic parser for Logical Categorial Grammar formalism\n\
Copyright (C) 2017 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>\n\
Copyright (C) 2017 Institute of Computer Science Polish Academy of Sciences"

let anon_fun s = raise (Arg.Bad ("invalid argument: " ^ s))

(*let get_sock_addr host_name port =
  let he =
    try Unix.gethostbyname host_name
    with Not_found -> failwith ("get_sock_addr: host " ^ host_name ^ " not found") in
  let addr = he.Unix.h_addr_list in
  Unix.ADDR_INET(addr.(0),port)*)

let aggregate_stats stats (text,status,tokens,lex_sems) =
  DomExec.add_stats stats (DomExec.aggregate_stats_text text)
  
let print_stats (stats,_) =
  print_endline (Visualization.string_of_stats stats)
  
let print_results_header file =
  match !output with
  | Html -> Printf.fprintf file "%s\n" Visualization.html_header
  | JSON -> Printf.fprintf file "[\n"
  
let print_results_trailer file =
  match !output with
  | Html -> Printf.fprintf file "%s\n" Visualization.html_trailer
  | JSON -> Printf.fprintf file "]\n"
  
let print_result file (text,status,tokens,lex_sems) is_last =
  if !select_parsed_flag && not status then () else
  match !output with
  | Html -> 
      if text <> ExecTypes.AltText [] then
      Printf.fprintf file "%s<BR>\n%!" (Visualization.html_of_text_as_paragraph !output_dir ExecTypes.Struct !img !verbosity tokens text)
  | JSON ->
      let s = match text with
          ExecTypes.JSONtext s -> s
		| _ -> failwith "print_results: json" in
	  if is_last then Printf.fprintf file "%s\n" s
	  else Printf.fprintf file "%s,\n" s

let print_killed_result file s is_last =
  if !select_parsed_flag then () else
  match !output with
  | Html -> 
      Printf.fprintf file "%s<BR>\n%!" (Visualization.html_of_text_as_paragraph !output_dir ExecTypes.Struct !img !verbosity 
        (ExtArray.make 0 SubsyntaxTypes.empty_token_env) (ExecTypes.AltText [Raw,RawText s; Error,ErrorText "process killed"]))
  | JSON -> 
      let s = Json.to_string "" (Json.JObject["text", Json.JString s;"error", Json.JString "process killed"]) in
	  if is_last then Printf.fprintf file "%s\n" s
	  else Printf.fprintf file "%s,\n" s

(*let print_results results =
  match !output with
  | Html -> 
          File.file_out (!output_dir ^ "parsed_text.html") (fun file ->
            Printf.fprintf file "%s\n" Visualization.html_header;
            Xlist.iter results (fun (text,tokens,lex_sems) ->
              if text <> ExecTypes.AltText [] then
              Printf.fprintf file "%s<BR>\n%!" (Visualization.html_of_text_as_paragraph !output_dir ExecTypes.Struct !img !verbosity tokens text));
            Printf.fprintf file "%s\n" Visualization.html_trailer)
  | JSON ->
          File.file_out (!output_dir ^ "parsed_text.json") (fun file ->
            let results = Xlist.rev_map results (fun (text,_,_) ->
                match text with
                  ExecTypes.JSONtext s -> s
				| _ -> failwith "print_results: json") in
            if results = [] then Printf.fprintf file "[]\n" else
            Printf.fprintf file "[\n";
            Printf.fprintf file "%s" (List.hd results);
            Xlist.iter (List.tl results) (Printf.fprintf file ",\n%s");
            Printf.fprintf file "\n]\n")*)
(*   | _ -> failwith "print_results: ni" *)

let rec update_pid descr2 pid2 = function
    [] -> failwith "updade_pid"
  | (in_chan,out_chan,descr,pid) :: l ->
      if descr = descr2 then (in_chan,out_chan,descr,pid2) :: l else
      (in_chan,out_chan,descr,pid) :: (update_pid descr2 pid2 l)

let rec remove_pid pid2 = function
    [] -> failwith "remove_pid"
  | (in_chan,out_chan,descr,pid) :: l ->
      if pid = pid2 then l else
      (in_chan,out_chan,descr,pid) :: (remove_pid pid2 l)

let rec find_scheduled pid2 = function
    [] -> "???","???"
  | (pid,id,params) :: l ->
      if pid = pid2 then id,params else
      find_scheduled pid2 l
      
let create_worker id worker_command = 
    print_endline (id ^ " create_worker");
(*    let in_chan,out_chan =
      try Unix.open_connection sock
      with e -> failwith ("server connection error: " ^ Printexc.to_string e) in*)
    let in_chan,out_chan = Unix.open_process worker_command in
    let descr = Unix.descr_of_in_channel in_chan in
    in_chan,out_chan,descr,""
    
let execution file n_workers work worker_command =
  let size = Xlist.size work in
  let r = ref (size + n_workers) in
  let pending_work = ref size in
  let size = string_of_int size in
  let work = ref work in
  let scheduled = ref [] in
  let stats = ref (DomExec.zero_stats,0) in
(*   let results = ref [] in *)
(*   let sum_result = ref Exec.empty_sum_result in *)
  let id = string_of_int (Unix.getpid ()) in
(*   let sock = get_sock_addr "localhost" 1236 in *)
  let io_list = ref (Int.fold 1 n_workers [] (fun io_list _ ->
    (create_worker id worker_command ) :: io_list)) in
  let descr_list = ref (Xlist.map !io_list (fun (_,_,descr,_) -> descr)) in
  while !r <> 0 do
    print_endline (id ^ " Unix.select");
    let selected,_,_ = Unix.select !descr_list [] [] (-1.) in
    print_endline (id ^ " selected " ^ (string_of_int (Xlist.size selected)));
    Xlist.iter selected (fun descr2 ->
      decr r;
      Xlist.iter !io_list (fun (in_chan,out_chan,descr,pid) ->
        if descr = descr2 then (
         try
          let idw = match Marshal.from_channel in_chan with
            Ready_to_work idw ->
              print_endline (idw ^ " ready");
              io_list := update_pid descr idw !io_list;
              idw
          | Work_done (idw,s) ->
              print_endline (idw ^ " work done");
              decr pending_work;
              stats := aggregate_stats !stats s;
              print_stats !stats;
              print_result file s (!pending_work=0);
(*               results := s :: (!results); *)
              idw in
          match !work with
            (idt,params) :: l ->
              Marshal.to_channel out_chan (Work_with (idt,params)) [Marshal.No_sharing];
              flush out_chan;
              scheduled := (idw,idt,params) :: !scheduled;
              print_endline (idw ^ " scheduled " ^ idt ^ " of " ^ size);
              work := l
          | [] ->
              Marshal.to_channel out_chan Kill_yourself [Marshal.No_sharing];
              print_endline (idw ^ " finished")
         with End_of_file -> (
           print_endline (pid ^ " killed");
           decr pending_work;
           let idt,params = find_scheduled pid !scheduled in
           print_endline (pid ^ " task numer " ^ idt ^ ": " ^ params);
           print_killed_result file params (!pending_work=0);
           io_list := remove_pid pid !io_list;
           io_list := (create_worker id worker_command) :: !io_list;
           descr_list := Xlist.map !io_list (fun (_,_,descr,_) -> descr)))))
  done;
  print_endline (id ^ " exit");
  print_stats !stats;
(*   !results *)
  ()

let output_filename () = 
  match !output with
  | Html -> !output_dir ^ "parsed_text.html"
  | JSON -> !output_dir ^ "parsed_text.json"
  
let _ =
  Arg.parse spec_list anon_fun usage_msg;
  if !worker_command = "" then failwith "no worker_command provided";
  let data = File.load_lines !input_filename in
  let data = List.rev (fst (Xlist.fold data ([],1) (fun (data,n) s -> (string_of_int n,s) :: data, n+1))) in
  File.file_out (output_filename ()) (fun file ->
    print_results_header file;
    execution file !no_workers data !worker_command;
    print_results_trailer file);
  Printf.printf "Done!\n"

  

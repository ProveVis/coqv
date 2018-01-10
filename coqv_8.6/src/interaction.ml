open Printf
open Runtime
open Types
open Lexing
open Interface
open Proof_model
open History
open Feedback
open Util
open Stateid
open CSig
open Callbacks
open Coqv_utils

let batch_commands = ref []

type request_mode = 
      Request_add of (Stateid.t * string)    
    | Request_edit_at of Stateid.t
    | Request_query     
    | Request_goals
    | Request_evars         | Request_hints         | Request_status    | Request_search 
    | Request_getoptions    | Request_setoptions    | Request_mkcases   | Request_quit
    | Request_about         | Request_init          | Request_interp    | Request_stopworker 
    | Request_printast      | Request_annotate

let request_mode = ref Request_about 

(*************************************************************************************)
(**sending xml characters to coqtop**)

let rec request_coq_info cout = 
    request_mode := Request_about;
    let about = Xmlprotocol.About () in
    let xml_about = Xmlprotocol.of_call about in
    Xml_printer.print (Xml_printer.TChannel cout) xml_about

and response_coq_info fb_val = 
    (match fb_val with
    | Good fb -> 
            Runtime.coqtop_info := fb;
            printf "Coqtop Info: \n\tversion %s, \n\tprotocol version %s, \n\trelease date %s, \n\tand compile date %s\n" 
                fb.coqtop_version fb.protocol_version fb.release_date fb.compile_date
    | _ -> printf "parsing coq info message fails\n");
    flush stdout

and request_init filename = 
    let cout = Runtime.coq_channels.cout in
    request_mode := Request_init;
    let init = Xmlprotocol.init filename in
    let xml_init = Xmlprotocol.of_call init in
    Xml_printer.print (Xml_printer.TChannel cout) xml_init;
    log_coqtop true (Xml_printer.to_string xml_init)

and response_init msg = 
    match msg with
    | Good stateid -> 
        Doc_model.current_stateid := stateid;
        Doc_model.init_doc ()
    | _ -> printf "unknown from response init\n"; flush stdout    

and request_quit () = 
    let cout = Runtime.coq_channels.cout in
    request_mode := Request_quit;
    let quit = Xmlprotocol.quit () in
    let xml_quit = Xmlprotocol.of_call quit in
    Xml_printer.print (Xml_printer.TChannel cout) xml_quit;
    log_coqtop true (Xml_printer.to_string xml_quit)

and response_quit msg = 
    match msg with
    | Good _ -> 
        print_string ("now quit! \n"); 
        Runtime.running := false
        (* exit 0 *)
    | _ -> printf "unknown from response quit\n"; flush stdout  

and request_goals () =
    let cout = Runtime.coq_channels.cout in 
    request_mode := Request_goals;
    let goals = Xmlprotocol.goals () in
    let xml_goals = Xmlprotocol.of_call goals in
    Xml_printer.print (Xml_printer.TChannel cout) xml_goals;
    log_coqtop true (Xml_printer.to_string xml_goals)

and response_goals msg =
    begin
    match msg with
    | Good None -> 
        begin
            match !Coqv_utils.current_cmd_type with
            | Qed -> 
                Proof_model.change_current_proof_state Defined;
                current_session_id := "";
                History.record_step !Doc_model.current_stateid Dummy
            | Admitted ->
                let chosen_node = select_chosen_node () in
                begin match chosen_node with
                | None -> ()
                | Some cnode -> Callbacks.on_change_node_state cnode Admitted
                end;
                print_endline "current proof tree is admitted";
                Proof_model.change_current_proof_state Declared;
                current_session_id := ""
            | _ -> ()
        end;
        Doc_model.commit ()
    | Good (Some goals) -> 
        on_receive_goals !Coqv_utils.current_cmd_type goals;
        Doc_model.commit ()
    | Fail (id,loc, msg) -> 
        print_endline "fail to get goals";
        printf "Fail at stateid %d: %s\n" id (richpp_to_string msg);
        flush stdout;
        request_edit_at (Doc_model.latest_committed_stateid ())
    end;
    Doc_model.goal_responsed := true

and request_add cmd editid stateid verbose = 
    Doc_model.goal_responsed := false;
    let ecmd = cmd in
    let cout = Runtime.coq_channels.cout in
    request_mode := Request_add (stateid, cmd);
    let add = Xmlprotocol.add ((ecmd, editid), (stateid, verbose)) in
    let xml_add = Xmlprotocol.of_call add in
    Xml_printer.print (Xml_printer.TChannel cout) xml_add;
    log_coqtop true (Xml_printer.to_string xml_add)
    (* Doc_model.try_add cmd *)

and response_add msg old_stateid cmd =
    let add_success = ref false in
    begin
        match msg with
        | Good (stateid, (CSig.Inl (), content)) ->
            add_success := true;
            if String.trim content <> "" then 
                printf "new state id: %d, message content: %s\n" stateid content;
            Doc_model.current_stateid := stateid;
            (* Doc_model.finish_add stateid; *)
            Doc_model.add stateid cmd;
            flush stdout
        | Good (stateid, (CSig.Inr next_stateid, content)) ->
            add_success := true;
            if String.trim content <> "" then 
                printf "finished current proof, move to state id: %d, message content: %s\n" next_stateid content;
            Doc_model.current_stateid := next_stateid;
            (* Doc_model.finish_add stateid; *)
            Doc_model.add stateid cmd;
            flush stdout
        | Fail (stateid, _, xml_content) -> 
            printf "error add in state id %d, message content: " stateid;
            print_xml stdout xml_content;
            print_endline "";
            flush stdout
    end;
    if (!add_success) then
        request_goals ()
    else
        Doc_model.goal_responsed := true;
    flush coq_channels.cout

and request_edit_at stateid = 
    let cout = Runtime.coq_channels.cout in
    request_mode := Request_edit_at stateid;
    let editat = Xmlprotocol.edit_at stateid in
    let xml_editat = Xmlprotocol.of_call editat in
    Xml_printer.print (Xml_printer.TChannel cout) xml_editat;
    log_coqtop true (Xml_printer.to_string xml_editat)

and response_edit_at msg stateid =
    begin
        match msg with
        | Good (CSig.Inl ()) ->
            printf "simple backtract;\n";
            flush stdout;
            on_edit_at stateid
        | Good (CSig.Inr (focusedStateId, (focusedQedStateId, oldFocusedStateId))) ->
            printf "focusedStateId: %d, focusedQedStateId: %d, oldFocusedStateId: %d\n" focusedStateId focusedQedStateId oldFocusedStateId;
            flush stdout;
            on_edit_at stateid
        | Fail (errorFreeStateId, loc, xml_content) ->
            printf "errorFreeStateId: %d, message content: " errorFreeStateId;
            print_xml stdout xml_content;
            print_endline "";
            flush stdout;
            request_edit_at errorFreeStateId
    end;
    (*request_goals (); (*fetch goals after edit at some new stateid*)*)
    flush coq_channels.cout

and request_query query stateid = 
    let cout = Runtime.coq_channels.cout in
    request_mode := Request_query;
    let query = Xmlprotocol.query query in
    let xml_query = Xmlprotocol.of_call query in
    Xml_printer.print (Xml_printer.TChannel cout) xml_query;
    log_coqtop true (Xml_printer.to_string xml_query)

and response_query msg = 
    match msg with
    | Good query -> print_endline query
    | _ -> printf "unknown from response query\n"; flush stdout

and request_evars () = 
    let cout = Runtime.coq_channels.cout in
    request_mode := Request_evars;
    let evars = Xmlprotocol.evars () in
    let xml_evars = Xmlprotocol.of_call evars in
    Xml_printer.print (Xml_printer.TChannel cout) xml_evars;
    log_coqtop true (Xml_printer.to_string xml_evars)

and response_evars msg =
    print_endline "response evars, not finished."

and request_hints () = 
    let cout = Runtime.coq_channels.cout in
    request_mode := Request_hints;
    let hints = Xmlprotocol.hints () in
    let xml_hints = Xmlprotocol.of_call hints in
    Xml_printer.print (Xml_printer.TChannel cout) xml_hints;
    log_coqtop true (Xml_printer.to_string xml_hints)

and response_hints msg =
    print_endline "response hints, not finished."

and request_status force = 
    let cout = Runtime.coq_channels.cout in
    request_mode := Request_status;
    let status = Xmlprotocol.status force in
    let xml_status = Xmlprotocol.of_call status in
    Xml_printer.print (Xml_printer.TChannel cout) xml_status;
    log_coqtop true (Xml_printer.to_string xml_status)

and response_status msg =
    print_endline "response status, not finished"

and request_search search_list = 
    let cout = Runtime.coq_channels.cout in
    request_mode := Request_search;
    let search = Xmlprotocol.search search_list in
    let xml_search = Xmlprotocol.of_call search in
    Xml_printer.print (Xml_printer.TChannel cout) xml_search;
    log_coqtop true (Xml_printer.to_string xml_search)

and response_search msg = 
    print_endline "response search, not finished"

and request_getoptions () = 
    let cout = Runtime.coq_channels.cout in
    request_mode := Request_getoptions;
    let getoptions = Xmlprotocol.get_options () in
    let xml_getoptions = Xmlprotocol.of_call getoptions in
    Xml_printer.print (Xml_printer.TChannel cout) xml_getoptions;
    log_coqtop true (Xml_printer.to_string xml_getoptions)

and response_getoptions msg = 
    print_endline "response getoptions, not finished"

and request_setoptions options = 
    let cout = Runtime.coq_channels.cout in
    request_mode := Request_setoptions;
    let setoptions = Xmlprotocol.set_options options in
    let xml_setoptions = Xmlprotocol.of_call setoptions in
    Xml_printer.print (Xml_printer.TChannel cout) xml_setoptions;
    log_coqtop true (Xml_printer.to_string xml_setoptions)

and response_setoptions msg = 
    print_endline "response setoptions, not finished"

and request_mkcases str = 
    let cout = Runtime.coq_channels.cout in
    request_mode := Request_mkcases;
    let mkcases = Xmlprotocol.mkcases str in
    let xml_mkcases = Xmlprotocol.of_call mkcases in
    Xml_printer.print (Xml_printer.TChannel cout) xml_mkcases;
    log_coqtop true (Xml_printer.to_string xml_mkcases)

and response_mkcases msg = 
    print_endline "response mkcases, not finished"

and request_interp interp = 
    let cout = Runtime.coq_channels.cout in
    request_mode := Request_interp;
    let interp = Xmlprotocol.interp interp in
    let xml_interp = Xmlprotocol.of_call interp in
    Xml_printer.print (Xml_printer.TChannel cout) xml_interp;
    log_coqtop true (Xml_printer.to_string xml_interp)

and response_interp msg = 
    print_endline "response interp, not finished"

and request_stopworker str = 
    let cout = Runtime.coq_channels.cout in
    request_mode := Request_stopworker;
    let stopworker = Xmlprotocol.stop_worker str in
    let xml_stopworker = Xmlprotocol.of_call stopworker in
    Xml_printer.print (Xml_printer.TChannel cout) xml_stopworker;
    log_coqtop true (Xml_printer.to_string xml_stopworker)

and response_stopworker msg = 
    print_endline "response stopworker, not finished"

and request_printast stateid = 
    let cout = Runtime.coq_channels.cout in
    request_mode := Request_printast;
    let printast = Xmlprotocol.print_ast stateid in
    let xml_printast = Xmlprotocol.of_call printast in
    Xml_printer.print (Xml_printer.TChannel cout) xml_printast;
    log_coqtop true (Xml_printer.to_string xml_printast)

and response_printast msg = 
    print_endline "response printast, not finished"

and request_annotate str = 
    let cout = Runtime.coq_channels.cout in
    request_mode := Request_annotate;
    let annotate = Xmlprotocol.annotate str in
    let xml_annotate = Xmlprotocol.of_call annotate in
    Xml_printer.print (Xml_printer.TChannel cout) xml_annotate;
    log_coqtop true (Xml_printer.to_string xml_annotate)

and response_annotate msg = 
    print_endline "response annotate, not finished"

(*************************************************************************************)

let interpret_feedback xml_fb = 
    let fb = Xmlprotocol.to_feedback xml_fb in
    begin
        match fb.contents, fb.id with 
        | Processed, State sid -> ()(*printf "Processed stateid %d\n" sid*); Doc_model.processed_stateid sid
        | Processed, Edit eid -> ()(*printf "Processed editid %d\n" eid*)
        | Incomplete, State sid -> ()(*printf "Incomplete stateid %d" sid*)
        | Incomplete, Edit eid -> ()(*printf "Incomplete editid %d" eid*)
        | Complete, State sid -> ()(*printf "Complete stateid %d" sid*)
        | Complete, Edit eid -> ()(*printf "Complete editid %d" eid*)
        | ProcessingIn worker_name, State sid -> ()(*printf "ProcessingIn worker %s, stateid %d\n" worker_name sid*); Doc_model.processing_stateid sid
        | ProcessingIn worker_name, Edit eid -> ()(*printf "ProcessingIn worker %s, editid %d\n" worker_name eid*)
        | InProgress i, State sid -> ()(*printf "InProgress %d, stateid %d" i sid*)
        | InProgress i, Edit eid -> ()(*printf "InProgress %d, editid %d" i eid *)
        | WorkerStatus (worker_name, status), State sid -> ()(*printf "WorkerStatus: %s --> %s, stateid %d" worker_name status sid*)
        | WorkerStatus (worker_name, status), Edit eid -> ()(*printf "WorkerStatus: %s --> %s, editid %d" worker_name status eid*)
        | Goals (loc, str), State sid -> ()(*printf "Goals: %s, stateid %d" str sid*)
        | Goals (loc, str), Edit eid -> ()(*printf "Goals: %s, editid %d" str eid *)
        | AddedAxiom, State sid -> ()(*printf "AddedAxiom stateid %d" sid*)
        | AddedAxiom, Edit eid -> ()(*printf "AddedAxiom editid %d" eid*)
        | GlobRef _, State sid -> ()(*printf "GlobRef ... stateid %d" sid*)
        | GlobRef _, Edit eid -> ()(*printf "GlobRef ... editid %d" eid*)
        | GlobDef _, State sid -> ()(*printf "GlobDef ... stateid %d" sid*)
        | GlobDef _, Edit eid -> ()(*printf "GlobDef ... editid %d" eid*)
        | FileDependency _, State sid -> ()(*printf "FileDependency ... stateid %d" sid*)
        | FileDependency _, Edit eid -> ()(*printf "FileDependency ... editid %d" eid*)
        | FileLoaded (module_name, vofile_name), State sid -> ()(*printf "FileLoaded: %s from %s, stateid %d" module_name vofile_name sid*)
        | FileLoaded (module_name, vofile_name), Edit eid -> ()(*printf "FileLoaded: %s from %s, editid %d" module_name vofile_name eid*)
        | Custom _, State sid -> ()(*printf "Custom ... stateid %d" sid*)
        | Custom _, Edit eid -> ()(*printf "Custom ... editid %d" eid*)
        | Message (levl, loc, xml_content), State sid -> 
            (* if levl = Feedback.Error then
                Doc_model.coqtop_processed sid; *)
            ()(*printf "Message %s, stateid %d" (str_feedback_level levl) sid; print_xml stdout xml_content*)
        | Message (levl, loc, xml_content), Edit eid -> ()(*printf "Message %s, editid %d" (str_feedback_level levl) eid; print_xml stdout xml_content*)
    end;
    (* printf "\n"; *)
    flush stdout



let handle_input input_str = 
    (* printf "cmd: %s, type: %s\n" input_str (str_cmd_type (get_cmd_type input_str)); *)
    Coqv_utils.current_cmd_type := get_cmd_type input_str;
    Flags.running_coqv := false;
    request_add (input_str) (-1) !Doc_model.current_stateid true;
    log_coqtop true input_str;
    flush stdout

let other_xml_str xml_str tag = 
    let xml_str_list = Str.split (Str.regexp tag) xml_str in
    let prefix_length = String.length (List.nth xml_str_list 0) + String.length tag in
    let other_str = String.trim (String.sub xml_str prefix_length (String.length xml_str - prefix_length)) in
    other_str

let handle_answer received_str = 
    (* let fb_str = Str.global_replace (ignored_re ()) "" received_str in *)
    let fb_str = received_str in
    (* print_endline fb_str; *)
    log_coqtop false fb_str;
    let handle str = 
        try
        let xparser = Xml_parser.make (Xml_parser.SString str) in
        let xml_str = Xml_parser.parse xparser in
        match Xmlprotocol.is_message xml_str with
        | Some (level, loc, content) ->
            printf "%s: " (str_feedback_level level);
            print_xml stdout content;
            print_endline "";
            flush stdout;
            other_xml_str str "</message>"            
        | None -> 
            if Xmlprotocol.is_feedback xml_str then begin
                interpret_feedback xml_str;
                other_xml_str str "</feedback>"    
            end else begin
                begin
                    match !request_mode with
                    | Request_about ->      response_coq_info (Xmlprotocol.to_answer (Xmlprotocol.About ()) xml_str)
                    | Request_init ->       
                        response_init (Xmlprotocol.to_answer (Xmlprotocol.init None) xml_str)
                    | Request_edit_at stateid -> response_edit_at (Xmlprotocol.to_answer (Xmlprotocol.edit_at 0) xml_str) stateid
                    | Request_query ->      response_query (Xmlprotocol.to_answer (Xmlprotocol.query ("", 0)) xml_str)
                    | Request_goals ->      
                        response_goals (Xmlprotocol.to_answer (Xmlprotocol.goals ()) xml_str)
                    | Request_evars ->      response_evars (Xmlprotocol.to_answer (Xmlprotocol.evars ()) xml_str)
                    | Request_hints ->      response_hints (Xmlprotocol.to_answer (Xmlprotocol.hints ()) xml_str)
                    | Request_status ->     response_status (Xmlprotocol.to_answer (Xmlprotocol.status true) xml_str)
                    | Request_search ->     response_search (Xmlprotocol.to_answer (Xmlprotocol.search []) xml_str)
                    | Request_getoptions -> response_getoptions (Xmlprotocol.to_answer (Xmlprotocol.get_options ()) xml_str)
                    | Request_setoptions -> response_setoptions (Xmlprotocol.to_answer (Xmlprotocol.set_options []) xml_str)
                    | Request_mkcases ->    response_mkcases (Xmlprotocol.to_answer (Xmlprotocol.mkcases "") xml_str)
                    | Request_quit ->       response_quit (Xmlprotocol.to_answer (Xmlprotocol.quit ()) xml_str)
                    | Request_add (stateid, cmd) ->        
                        response_add (Xmlprotocol.to_answer (Xmlprotocol.add (("",0),(0,true))) xml_str) stateid cmd
                    | Request_interp ->     response_interp (Xmlprotocol.to_answer (Xmlprotocol.interp ((true, true),"")) xml_str)
                    | Request_stopworker -> response_stopworker (Xmlprotocol.to_answer (Xmlprotocol.stop_worker "") xml_str)
                    | Request_printast ->   response_printast (Xmlprotocol.to_answer (Xmlprotocol.print_ast 0) xml_str)
                    | Request_annotate ->   response_annotate (Xmlprotocol.to_answer (Xmlprotocol.annotate "") xml_str)
                end;
                other_xml_str str "</value>"
            end 
        with Xml_parser.Error e -> print_endline ("Error when handling "^str); exit 1
        in
    let to_be_handled = ref fb_str in
    while !to_be_handled <> "" do
        to_be_handled := handle !to_be_handled
    done
    

let interpret_cmd cmd_str_list = 
    begin
        match cmd_str_list with
        | [] -> ()
        | cmd :: options -> 
        begin
            match cmd with
            | "status" -> print_endline (Status.str_status ())
            | "history" -> print_endline (History.str_history ())
            | "proof" -> 
                if options = [] then
                    begin
                        match !Proof_model.current_session_id with
                        | "" -> print_endline "not in proof mode"
                        | sname -> print_endline (Status.str_proof_tree sname)    
                    end
                else 
                    List.iter (fun a -> print_endline (Status.str_proof_tree a)) options
            | "undo_to" ->
                let new_stateid = int_of_string (List.hd options) in
                request_edit_at new_stateid
            | "quit" -> 
                Flags.running_coqv := false;
                request_quit ()
                (* exit 0 *)
            | "export" ->
                let eout = open_out (List.hd options) in
                let cmd_list = Doc_model.get_committed_commands () in
                List.iter (fun cmd -> output_string eout cmd; output_string eout "\n"; if cmd="Qed." then output_string eout "\n") cmd_list;
                flush eout;
                close_out eout
            | "import" ->
                Flags.batch_mode := true; (*we are in batch mode now*)
                let inpt = open_in (List.hd options) in
                let cmd_strs = ref [] in
                let lineno = ref 1 in
                let chars_to_string chars = 
                    String.trim (String.init (List.length chars) (fun i -> List.nth chars i)) in
                let inpt_buffer = ref [] in begin
                    try
                        while true do
                            let c = input_char inpt in
                            match c with
                            | '\n' -> 
                                incr lineno; 
                                if List.length !inpt_buffer > 0 && List.nth !inpt_buffer (List.length !inpt_buffer - 1) <> ' ' then  
                                    inpt_buffer := !inpt_buffer @ [' ']
                            | '.' -> let cmd_str = chars_to_string (!inpt_buffer @ ['.']) in cmd_strs := !cmd_strs @ [cmd_str]; inpt_buffer := []
                            | _ -> inpt_buffer := !inpt_buffer @ [c]
                        done
                    with
                        End_of_file ->
                            if List.length !cmd_strs = 0 then begin
                                printf "The content of %s is empty\n" (List.hd options);
                                flush stdout
                            end else if (chars_to_string !inpt_buffer = "") then begin
                                let cmdhd, cmdtl = List.hd !cmd_strs, List.tl !cmd_strs in
                                batch_commands := cmdtl;
                                (* printf "number of commands wait to send to coqtop: %d\n" (List.length !batch_commands); *)
                                handle_input cmdhd;
                                (* List.iter (fun cmd_str -> handle_input cmd_str; Thread.delay 1.0; printf "Sent: %s\n" cmd_str; incr current_line) !cmd_strs; *)
                                printf "Successfully read from file %s\n" (List.hd options);
                                flush stdout
                            end else begin
                                printf "read file %s error: line %d\n" (List.hd options) !lineno;
                                flush stdout
                            end
                end
            | "help" -> begin
                    match options with
                    | [] -> List.iter (fun (cmd, args, usage) -> printf "%s %s: %s\n" cmd args usage; flush stdout) Coqv_doc.commands 
                    | _ -> List.iter (fun (cmd, args, usage) -> if List.mem cmd options then begin printf "%s %s: %s\n" cmd args usage; flush stdout end) Coqv_doc.commands 
                end
            | _ -> print_endline "command not interpreted."
        end
    end
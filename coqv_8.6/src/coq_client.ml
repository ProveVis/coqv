open Interface
open Printf
open Runtime
open Types
open Proof_model
open Vmdv_client
open Coqv_utils
open Feedback

(*****************************************callbacks***************************************************************)

let in_focus_mode = ref false 
let leaf_nids = ref []
type pending_task = Focus of string | TryedFocus of string | No_task
let pending_task = ref No_task

let on_new_session (session: session) = 
    current_session_id := session.name;
    (* printf "current session id: %s\n" session.name; *)
    flush stdout;
    assert(List.length !moduls > 0);
    add_session_to_modul (List.hd !moduls) session;
    (*printf "%d moduls at the moment\n" (List.length !moduls);*)
    let node = session.proof_tree.root in
    (* History.record_step !Doc_model.current_stateid (Add_node node.id); *)
    begin
        match !vagent with
        | None -> (*print_endline "no vmdv agent currently"*)()
        | Some vagt -> 
            Vmdv_client.create_session vagt session;
            Vmdv_client.add_node vagt session.name session.name node
    end

let rec change_one_node_state (node:node) state = 
    if node.state <> state then begin
        (* print_endline ("change state of ndoe "^node.id^" to "^(str_node_state state)); *)
        node.state <- state;
        Options.action (fun vagt -> 
            let sid = !Proof_model.current_session_id in
            if sid <> "" then change_node_state vagt sid node.id state
            ) !vagent;
        (* print_endline ("changed state of ndoe "^node.id); *)
        true
    end else
        false
and update_parent_node_state (node:node) = 
    let parent = node.parent in
    let prooftree = current_proof_tree () in
    (* print_endline ("updating parent of "^node.id^": "^parent.id); *)
    if parent.id <> node.id then begin
        
        if is_children_proved prooftree parent.id then begin
            if change_one_node_state parent Proved then
                update_parent_node_state parent
        end else if is_children_admitted prooftree parent.id then begin
            if change_one_node_state parent Admitted then
                update_parent_node_state parent
        end else begin
            if change_one_node_state parent Not_proved then
                update_parent_node_state parent
        end
    end 

let on_change_node_state nid state = 
    let node = get_node nid in
    if change_one_node_state node state then begin
        if node.state = Chosen then
            node.stateid <- !Doc_model.current_stateid;
        update_parent_node_state node
    end

let on_change_node_label (node:node) new_label tactic = 
    set_new_label node.id new_label tactic

let on_change_tactic nid tactic = 
    Options.action (fun vagt -> 
        let sid = !Proof_model.current_session_id in
        if sid <> "" then set_proof_rule vagt sid (sid^"+"^nid) tactic
        ) !vagent

let on_add_node node_from node_to state = 
    let label = Doc_model.uncommitted_command () in
    Proof_model.add_edge node_from node_to [label];
    (* History.record_step !Doc_model.current_stateid (Add_node node_to.id); *)
    node_to.state <- state;
    if state = Chosen then
            node_to.stateid <- !Doc_model.current_stateid;
    begin
        match !vagent with
        | None -> (*print_endline "no vmdv agent currently"*)()
        | Some vagt ->
            let sid = !Proof_model.current_session_id in
            if sid <> "" then begin
                Vmdv_client.add_node vagt sid sid node_to;
                Vmdv_client.add_edge vagt sid (sid^"+"^node_from.id) (sid^"+"^node_to.id) label    
            end
    end

let on_remove_node nid =
    if Proof_model.remove_node nid then begin
        Options.action (fun vagt ->
            let sid = !Proof_model.current_session_id in
            if sid <> "" then Vmdv_client.remove_node vagt sid nid
        ) !vagent
    end

let on_change_proof_state pstate = 
    if Proof_model.change_current_proof_state pstate then begin
        Options.action (fun vagt -> 
            let sid = !Proof_model.current_session_id in
            if sid <> "" then Vmdv_client.change_proof_state vagt sid pstate
            ) !vagent
        (* ;
        print_endline ("changed the state of the proof of "^(!current_session_id)^" to "^(str_proof_state pstate)) *)
    end

let add_new_goals cmd goals =     
    let fg_goals = goals.fg_goals in
    let chosen_node = select_chosen_node () in
    (match chosen_node with
    | None -> print_endline "No focus node, maybe the proof tree is complete"
    | Some cnode -> 
        let new_nodes : node list = List.map (fun g -> 
        {
            id = g.goal_id;
            label = Coqv_utils.goal_to_label g;
            state = To_be_chosen;
            parent = cnode;
            stateid = (*!Doc_model.current_stateid*)-1;
        }) fg_goals in
        if List.length new_nodes = 0 then begin
            (* print_endline "No more goals."; *)
            if cmd = ["Admit"] then
                on_change_node_state cnode.id Admitted
            else
                on_change_node_state cnode.id Proved;
            add_tactic cnode.id (Doc_model.uncommitted_command ());
            let tactic = get_tactic cnode.id in
            on_change_tactic cnode.id tactic
        end else begin
            begin match cmd with
            (* | Focus _ ->
                on_change_node_state cnode.id To_be_chosen;
                add_tactic cnode.id (Doc_model.uncommitted_command ()) *)
            | ["Admit"] -> 
                on_change_node_state cnode.id Admitted;
                add_tactic cnode.id (Doc_model.uncommitted_command ())
            | _ -> 
                let has_new = List.fold_left (fun b (n:node) -> 
                if b then 
                    b 
                else
                    not (node_exists n.id) 
                ) false new_nodes in
                if (not has_new) && (not (List.mem cnode.id (List.map (fun (n:node) -> n.id) new_nodes))) then begin
                    on_change_node_state cnode.id Proved;
                    add_tactic cnode.id (Doc_model.uncommitted_command ());
                    let tactic = get_tactic cnode.id in
                    on_change_tactic cnode.id tactic
                end
            (* | _ -> () *)
            end;
            List.iter (fun (n:node) ->
                if node_exists n.id then
                    on_change_node_state (n.id) To_be_chosen
                else begin
                    on_add_node cnode n To_be_chosen;
                    on_change_node_state cnode.id Not_proved
                end         
            ) (List.tl new_nodes);
            let new_focused_node = List.hd new_nodes in
            if node_exists new_focused_node.id then begin
                on_change_node_state new_focused_node.id Chosen;
                print_endline "Current Goal:\n";
                print_endline (Status.str_node new_focused_node.id);
            end else begin
                on_add_node cnode new_focused_node Chosen;
                on_change_node_state cnode.id Not_proved;
                print_endline "Current Goal:\n";
                print_endline (Status.str_node new_focused_node.id);
            end
        end)               

let handle_proof cmd goals = 
    match cmd with
    | ["Proof"] -> ()
    | ["Qed"] -> 
        on_change_proof_state Defined;
        current_session_id := ""
    | ["Admitted"] -> 
        (* print_endline "current proof tree is admitted"; *)
        on_change_proof_state Declared;
        current_session_id := ""
    | "Abort" :: _ -> 
        (* print_endline "current proof tree is aborted"; *)
        on_change_proof_state Aborted;
        current_session_id := ""
    | "Undo" :: _ | "Restart" :: _ | ["Editat"] -> 
        let fg_goals = goals.fg_goals in
        let nids = List.map (fun g -> g.goal_id) fg_goals in
        let removed_ids = ref [] in
        List.iter (fun nid -> removed_ids := (snd (children (nid) (current_proof_tree ()))) @ !removed_ids) nids;
        (* print_endline ("Preparing to remove "^(string_of_int (List.length !removed_ids))^" nodes"); *)
        List.iter (fun nid -> on_remove_node nid) !removed_ids;
        if nids <> [] then begin
            on_change_node_state (List.hd nids) Chosen;
            List.iter (fun nid -> on_change_node_state nid To_be_chosen) (List.tl nids)
        end
    | "Focus" :: _ | "Unfocus" :: _ -> 
        let fg_goals = goals.fg_goals in
        let nids = List.map (fun g -> g.goal_id) fg_goals in begin
        (* leaf_nids := nids; *)
        match select_chosen_node () with
        | None -> print_endline ("No focused node right now")
        | Some cnode -> 
            if nids <> [] then begin
                on_change_node_state cnode.id To_be_chosen;
                on_change_node_state (List.hd nids) Chosen;
                List.iter (fun nid -> on_change_node_state nid To_be_chosen) (List.tl nids)
            end
        end
    | _ -> 
        print_string "Unimplemented proof handling command: "; 
        List.iter (fun c -> print_string (c^" ")) cmd; 
        print_endline ""
    

let on_receive_goals cmd_type goals = 
    (*printf "focused goals number: %d. \n" (List.length (goals.fg_goals));*)
    leaf_nids := List.map (fun g -> g.goal_id) goals.fg_goals;
    begin
        match cmd_type with
        | Module modul_name -> moduls := (create_module modul_name) :: !moduls
        | End modul_name -> closing_modul modul_name
        | Proposition (thm_name, kind) -> 
            assert(List.length (goals.fg_goals) <> 0);
            let goal = List.hd (goals.fg_goals) in
            let label = goal_to_label goal in
            let rec node : node = {
                id = goal.goal_id;
                label = label;
                state = Chosen;
                parent = node;
                stateid = !Doc_model.current_stateid;
            } in
            let proof_tree = new_proof_tree node in
            let session = new_session thm_name kind Proving proof_tree in
            on_new_session session;
            print_endline "Current Goal:\n";
            print_endline (Status.str_node goal.goal_id);
            (* ;
            print_endline ("created new session "^thm_name) *)
        | ProofHandling cmd -> handle_proof cmd goals
        | Tactic cmd -> add_new_goals cmd goals
        | Require -> ()
        | Other cmd -> 
            print_endline ("Unknown type of command: "^(str_cmd_type cmd_type))
    end
            
let on_finishing_proof cmd_type = 
    match cmd_type with
    | ProofHandling ["Qed"] -> 
        on_change_proof_state Defined;
        current_session_id := ""
        (* History.record_step !Doc_model.current_stateid Dummy *)
    | ProofHandling ["Admitted"] ->
        (* let chosen_node = select_chosen_node () in
        begin match chosen_node with
        | None -> ()
        | Some cnode -> Callbacks.on_change_node_state cnode.id Admitted
        end; *)
        print_endline "current proof tree is admitted";
        on_change_proof_state Declared;
        current_session_id := ""
    | cmd -> () 
        (* print_endline ("Don't know what to do when receiving \"Good None\" in respose_goals with cmd type: "^(str_cmd_type cmd)) *)



let on_edit_at stateid = 
    Doc_model.current_stateid := stateid;
    Doc_model.reset_process_stateid ();
    Doc_model.discard_uncommitted ();
    Options.action (fun vagt -> 
        let sid = !Proof_model.current_session_id in
        let prooftree = Proof_model.current_proof_tree () in
        let nid = ref "" in
        Hashtbl.iter (fun tmp_nid node -> if node.stateid = stateid then nid := tmp_nid) prooftree.nodes;
        if !nid <> "" && sid <> "" then 
            remove_subproof vagt sid !nid
        ) !vagent;
    print_endline ("now edit at stateid "^(string_of_int stateid))
    (* Doc_model.move_focus_to stateid;
    Doc_model.current_stateid := stateid;
    History.undo_upto stateid *)
(******************************************end callbacks********************************************************************)




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

and response_goals msg goal_callbacks =
    let on_receive_goals, on_finishing_proof = goal_callbacks in 
    begin
    match msg with
    | Good None -> 
        on_finishing_proof !Coqv_utils.current_cmd_type;
        Doc_model.commit ()
    | Good (Some goals) -> 
        (* print_endline ("have received some goals after "^(str_cmd_type !Coqv_utils.current_cmd_type)); *)
        on_receive_goals !Coqv_utils.current_cmd_type goals;
        Doc_model.commit ()
    | Fail (id,loc, msg) -> 
        print_endline "fail to get goals";
        printf "Fail at stateid %d: %s\n" id (Coqv_utils.richpp_to_string msg);
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
            Coqv_utils.print_xml stdout xml_content;
            print_endline "";
            flush stdout
    end;
    if (!add_success) then
        request_goals ()
    else
        Doc_model.goal_responsed := true;
    flush coq_channels.cout

and request_edit_at stateid = 
    Coqv_utils.current_cmd_type := ProofHandling ["Editat"];
    Doc_model.goal_responsed := false;
    let cout = Runtime.coq_channels.cout in
    request_mode := Request_edit_at stateid;
    let editat = Xmlprotocol.edit_at stateid in
    let xml_editat = Xmlprotocol.of_call editat in
    Xml_printer.print (Xml_printer.TChannel cout) xml_editat;
    log_coqtop true (Xml_printer.to_string xml_editat)

and response_edit_at msg stateid =
    let editat_success = ref false in
    begin
        match msg with
        | Good (CSig.Inl ()) ->
            printf "simple backtract;\n";
            flush stdout;
            on_edit_at stateid;
            editat_success := true
        | Good (CSig.Inr (focusedStateId, (focusedQedStateId, oldFocusedStateId))) ->
            printf "focusedStateId: %d, focusedQedStateId: %d, oldFocusedStateId: %d\n" focusedStateId focusedQedStateId oldFocusedStateId;
            flush stdout;
            on_edit_at stateid;
            editat_success := true
        | Fail (errorFreeStateId, loc, xml_content) ->
            printf "errorFreeStateId: %d, message content: " errorFreeStateId;
            print_xml stdout xml_content;
            print_endline "";
            flush stdout;
            request_edit_at errorFreeStateId
    end;
    (*request_goals (); (*fetch goals after edit at some new stateid*)*)
    if !editat_success then
        request_goals ()
    else
        Doc_model.goal_responsed := true;
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
            (print_xml stdout xml_content; print_endline "")(*printf "Message %s, stateid %d" (str_feedback_level levl) sid; print_xml stdout xml_content*)
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

let handle_answer received_str goal_callbacks = 
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
                begin match !request_mode with
                    | Request_about ->      response_coq_info (Xmlprotocol.to_answer (Xmlprotocol.About ()) xml_str)
                    | Request_init ->       
                        response_init (Xmlprotocol.to_answer (Xmlprotocol.init None) xml_str)
                    | Request_edit_at stateid -> response_edit_at (Xmlprotocol.to_answer (Xmlprotocol.edit_at 0) xml_str) stateid
                    | Request_query ->      response_query (Xmlprotocol.to_answer (Xmlprotocol.query ("", 0)) xml_str)
                    | Request_goals ->      
                        response_goals (Xmlprotocol.to_answer (Xmlprotocol.goals ()) xml_str) goal_callbacks
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

let read_write_condition = Condition.create ()
let read_write_mutex = Mutex.create ()

let worker (cin, goal_callbacks) =     
    let buffer = Bytes.create !Flags.xml_bufsize in
    while !Runtime.running do
        (* print_endline "wait for coqtop"; *)
        let len = input cin buffer 0 !Flags.xml_bufsize in
        (* print_endline ("coqtop responsed with length "^(string_of_int len)); *)
        if len = 0 then
            running := false
        else begin
            let output_str = Bytes.sub_string buffer 0 len in
            (*printf "%s" (Str.global_replace (ignored_re ()) "" output_str);
            flush stdout*)
            if(len <> 0) then
                handle_answer output_str goal_callbacks;
            flush stdout
        end;
        (* print_endline ("there are "^(string_of_int (List.length !batch_commands))^" commands wait to send to coqtop"); *)
        if Doc_model.is_processed () && !Doc_model.goal_responsed then begin
            (* print_endline ("coqtop processed "); *)
            match !pending_task with
            | Focus nid -> 
                let pos = Lists.find_pos nid !leaf_nids in
                if pos >= 0 then begin
                    let cmd_str = "Focus "^(string_of_int (pos+1))^"." in
                    handle_input cmd_str;
                    pending_task := No_task
                end else begin
                    handle_input "Unfocus.";
                    pending_task := TryedFocus nid
                end
            | TryedFocus nid -> 
                let pos = Lists.find_pos nid !leaf_nids in
                if pos >= 0 then begin
                    let cmd_str = "Focus "^(string_of_int (pos+1))^"." in
                    handle_input cmd_str
                end else begin
                    print_endline ("Cannot chose node "^nid)
                end;
                pending_task := No_task
            | No_task ->
                if !Flags.batch_mode = false || (!batch_commands = []) then begin
                    Flags.batch_mode := false;
                    Flags.running_coqv := true;
                    Condition.signal read_write_condition
                end else begin
                    let bch, bct = List.hd !batch_commands, List.tl !batch_commands in
                    handle_input bch;
                    batch_commands := bct
                end
        end
    done;
    print_endline "worker quit"



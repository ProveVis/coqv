open Printf
open Runtime
open Proof_model
open Coqv_utils
open Types
open Interface
open History
(* 
let focused_goalid = ref 0
let last_goalid_list = ref [] *)

let in_focus_mode = ref false 

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
        match !Communicate.vagent with
        | None -> (*print_endline "no vmdv agent currently"*)()
        | Some vagt -> 
            Communicate.create_session vagt session;
            Communicate.add_node vagt session.name node
    end

let rec change_one_node_state (node:node) state = 
    if node.state <> state then begin
        print_endline ("change state of ndoe "^node.id^" to "^(str_node_state state));
        node.state <- state;
        Options.action (fun vagt -> 
            let sid = !Proof_model.current_session_id in
            if sid <> "" then Communicate.change_node_state vagt sid node.id state
            ) !Communicate.vagent;
        print_endline ("changed state of ndoe "^node.id);
        true
    end else
        false
and update_parent_node_state (node:node) = 
    let parent = node.parent in
    let prooftree = current_proof_tree () in
    print_endline ("updating parent of "^node.id^": "^parent.id);
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

let on_add_node node_from node_to state = 
    let label = Doc_model.uncommitted_command () in
    add_edge node_from node_to [label];
    (* History.record_step !Doc_model.current_stateid (Add_node node_to.id); *)
    node_to.state <- state;
    if state = Chosen then
            node_to.stateid <- !Doc_model.current_stateid;
    begin
        match !Communicate.vagent with
        | None -> (*print_endline "no vmdv agent currently"*)()
        | Some vagt ->
            let sid = !Proof_model.current_session_id in
            if sid <> "" then begin
                Communicate.add_node vagt sid node_to;
                Communicate.add_edge vagt sid node_from.id node_to.id label    
            end
    end

let on_remove_node nid =
    if remove_node nid then begin
        Options.action (fun vagt ->
            let sid = !Proof_model.current_session_id in
            if sid <> "" then Communicate.remove_node vagt sid nid
        ) !Communicate.vagent
    end

let on_change_proof_state pstate = 
    if Proof_model.change_current_proof_state pstate then begin
        Options.action (fun vagt -> 
            let sid = !Proof_model.current_session_id in
            if sid <> "" then Communicate.change_proof_state vagt sid pstate
            ) !Communicate.vagent;
        print_endline ("changed the state of the proof of "^(!current_session_id)^" to "^(str_proof_state pstate))
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
            label = goal_to_label g;
            state = To_be_chosen;
            parent = cnode;
            stateid = (*!Doc_model.current_stateid*)-1;
        }) fg_goals in
        if List.length new_nodes = 0 then begin
            print_endline "No more goals.";
            if cmd = ["Admit"] then
                on_change_node_state cnode.id Admitted
            else
                on_change_node_state cnode.id Proved;
            add_tactic cnode.id (Doc_model.uncommitted_command ())
        end else begin
            begin match cmd with
            (* | Focus _ ->
                on_change_node_state cnode.id To_be_chosen;
                add_tactic cnode.id (Doc_model.uncommitted_command ()) *)
            | ["Admit"] -> 
                on_change_node_state cnode.id Admitted;
                add_tactic cnode.id (Doc_model.uncommitted_command ())
            | _ -> 
                let has_new = List.fold_left (fun b n -> 
                if b then 
                    b 
                else
                    not (node_exists n.id) 
                ) false new_nodes in
                if (not has_new) && (not (List.mem cnode.id (List.map (fun n -> n.id) new_nodes))) then begin
                    on_change_node_state cnode.id Proved;
                    add_tactic cnode.id (Doc_model.uncommitted_command ())
                end
            (* | _ -> () *)
            end;
            List.iter (fun n ->
                if node_exists n.id then
                    on_change_node_state (n.id) To_be_chosen
                else begin
                    on_add_node cnode n To_be_chosen;
                    on_change_node_state cnode.id Not_proved
                end         
            ) (List.tl new_nodes);
            let new_focused_node = List.hd new_nodes in
            if node_exists new_focused_node.id then
                on_change_node_state new_focused_node.id Chosen
            else begin
                on_add_node cnode new_focused_node Chosen;
                on_change_node_state cnode.id Not_proved
            end
        end)               

let handle_proof cmd goals = 
    match cmd with
    | ["Proof"] -> ()
    | ["Qed"] -> 
        on_change_proof_state Defined;
        current_session_id := ""
    | ["Admitted"] -> 
        print_endline "current proof tree is admitted";
        on_change_proof_state Declared;
        current_session_id := ""
    | "Abort" :: _ -> 
        print_endline "current proof tree is aborted";
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
            print_endline ("created new session "^thm_name)
        | ProofHandling cmd -> handle_proof cmd goals
        | Tactic cmd -> add_new_goals cmd goals
        | Require -> ()
        | Other cmd -> 
            print_endline ("Unknown type of command: "^(str_cmd_type cmd_type))
    end
            

let on_edit_at stateid = 
    Doc_model.current_stateid := stateid;
    Doc_model.reset_process_stateid ();
    Doc_model.discard_uncommitted ();
    print_endline ("now edit at stateid "^(string_of_int stateid))
    (* Doc_model.move_focus_to stateid;
    Doc_model.current_stateid := stateid;
    History.undo_upto stateid *)
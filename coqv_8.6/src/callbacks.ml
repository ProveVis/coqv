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

let on_new_session (session: session) = 
    current_session_id := session.name;
    (* printf "current session id: %s\n" session.name; *)
    flush stdout;
    assert(List.length !moduls > 0);
    add_session_to_modul (List.hd !moduls) session;
    (*printf "%d moduls at the moment\n" (List.length !moduls);*)
    let node = session.proof_tree.root in
    History.record_step !Doc_model.current_stateid (Add_node node.id);
    begin
        match !Communicate.vagent with
        | None -> (*print_endline "no vmdv agent currently"*)()
        | Some vagt -> 
            Communicate.create_session vagt session;
            Communicate.add_node vagt session.name node
    end

let on_change_node_state (node:node) state = 
    if node.state <> state then begin
        History.record_step !Doc_model.current_stateid (Change_state (node.id, node.state));
        change_node_state node.id state
    end

let on_change_node_label (node:node) new_label tactic = 
    set_new_label node.id new_label tactic

let on_add_node node_from node_to state = 
    let label = Doc_model.uncommitted_command () in
    add_edge node_from node_to [label];
    History.record_step !Doc_model.current_stateid (Add_node node_to.id);
    node_to.state <- state;
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

let add_new_goals cmd_type goals =     
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
            stateid = !Doc_model.current_stateid;
        }) fg_goals in
        if List.length new_nodes = 0 then begin
            print_endline "No more goals.";
            if cmd_type = Admit then
                on_change_node_state cnode Admitted
            else
                on_change_node_state cnode Proved;
            add_tactic cnode.id (Doc_model.uncommitted_command ())
        end else begin
            begin match cmd_type with
            | Focus _ ->
                on_change_node_state cnode To_be_chosen;
                add_tactic cnode.id (Doc_model.uncommitted_command ())
            | Admit -> 
                on_change_node_state cnode Admitted;
                add_tactic cnode.id (Doc_model.uncommitted_command ())
            | Other -> 
                let has_new = List.fold_left (fun b n -> 
                if b then 
                    b 
                else
                    not (node_exists n.id) 
                ) false new_nodes in
                if not has_new then begin
                    on_change_node_state cnode Proved;
                    add_tactic cnode.id (Doc_model.uncommitted_command ())
                end
            | _ -> ()
            end;
            (* if not focus_mode then begin
                let has_new = List.fold_left (fun b n -> 
                    if b then 
                        b 
                    else
                        not (node_exists n.id) 
                    ) false new_nodes in
                if not has_new then begin
                    on_change_node_state cnode Proved;
                    add_tactic cnode.id (Doc_model.uncommitted_command ())
                end
            end else begin
                on_change_node_state cnode To_be_chosen;
                add_tactic cnode.id (Doc_model.uncommitted_command ())
            end; *)
            List.iter (fun n ->
                if node_exists n.id then
                    on_change_node_state (get_node n.id) To_be_chosen
                else begin
                    on_add_node cnode n To_be_chosen;
                    on_change_node_state cnode Not_proved
                end         
            ) new_nodes;
            on_change_node_state (List.hd new_nodes) Chosen
        end)               


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
            on_new_session session
        | Proof -> ()
        | Qed -> 
            Proof_model.change_current_proof_state Defined;
            current_session_id := "";
            History.record_step !Doc_model.current_stateid Dummy
        | Admitted ->
            print_endline "current proof tree is admitted";
            Proof_model.change_current_proof_state Declared;
            current_session_id := ""
        | Focus _  | Admit | Other -> add_new_goals cmd_type goals
        | Require -> ()
        (* | Edit_label -> begin
                let chosen_node = select_chosen_node () in
                match chosen_node with
                | None -> print_endline "error finding the current chosen node when editing the label"
                | Some cnode -> 
                    assert (List.length goals.fg_goals > 0);
                    let new_goal = List.hd (goals.fg_goals) in
                    let new_label = goal_to_label new_goal in
                    print_endline ("new label for "^(cnode.id)^": \n"^(str_label new_label));
                    on_change_node_label cnode new_label (Doc_model.uncommitted_command ())
            end *)
    end
            

let on_edit_at stateid = 
    Doc_model.current_stateid := stateid;
    Doc_model.reset_process_stateid ();
    Doc_model.discard_uncommitted ();
    print_endline ("now edit at stateid "^(string_of_int stateid))
    (* Doc_model.move_focus_to stateid;
    Doc_model.current_stateid := stateid;
    History.undo_upto stateid *)
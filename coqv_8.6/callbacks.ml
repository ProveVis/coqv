open Printf
open Runtime
open Proof_model
open Coqv_utils
open Types
open Interface
open History

let on_new_session (session: session) = 
    current_session_id := session.name;
    printf "current session id: %s\n" session.name;
    flush stdout;
    assert(List.length !moduls > 0);
    add_session_to_modul (List.hd !moduls) session;
    (*printf "%d moduls at the moment\n" (List.length !moduls);*)
    let node = session.proof_tree.root in
    History.record_step !Runtime.new_stateid (Add_node node.id);
    begin
        match !Runtime.vagent with
        | None -> print_endline "no vmdv agent currently"
        | Some vagt -> 
            Communicate.create_session vagt session;
            Communicate.add_node vagt session.name node
    end

let on_change_node_state node state = 
    History.record_step !Runtime.new_stateid (Change_state (node.id, node.state));
    change_node_state node.id state

let on_add_node node_from node_to state = 
    let label = (snd (List.hd !Doc_model.doc)) in
    add_edge node_from node_to label;
    History.record_step !Runtime.new_stateid (Add_node node_to.id);
    node_to.state <- state;
    begin
        match !Runtime.vagent with
        | None -> print_endline "no vmdv agent currently"
        | Some vagt ->
            let sid = !Proof_model.current_session_id in
            if sid <> "" then begin
                Communicate.add_node vagt sid node_to;
                Communicate.add_edge vagt sid node_from.id node_to.id label    
            end
    end


let on_receive_goals cmd_type goals = 
    (*printf "focused goals number: %d. \n" (List.length (goals.fg_goals));*)
    begin
        match cmd_type with
        | Module modul_name -> moduls := (create_module modul_name) :: !moduls
        | End modul_name -> closing_modul modul_name
        | Proof (thm_name, kind) -> 
            assert(List.length (goals.fg_goals) <> 0);
            let goal = List.hd (goals.fg_goals) in
            let label = goal_to_label goal in
            (*print_endline label.id;*)
            let rec node : node = {
                id = goal.goal_id;
                label = label;
                state = Chosen;
                parent = node;
                stateid = !Runtime.new_stateid;
            } in
            let proof_tree = new_proof_tree node in
            let session = new_session thm_name kind Processing proof_tree in
            on_new_session session
            (*print_endline "finished creating session."*)
        | Qed -> 
            current_session_id := "";
            History.record_step !Runtime.new_stateid Dummy
        | Other -> 
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
                    stateid = !Runtime.new_stateid;
                }) fg_goals in
                if List.length new_nodes = 0 then begin
                    print_endline "No more goals, shall change the focused node into proved.";
                    on_change_node_state cnode Proved
                    (*History.record_step !Runtime.new_stateid (Change_state (cnode.id, cnode.state));
                    change_node_state cnode.id Proved*)
                end else begin
                    let node, other_nodes = List.hd new_nodes, List.tl new_nodes in 
                    (*match chosen_node with
                    | None -> print_endline "No focus node, maybe the proof tree is complete"
                    | Some cnode ->*)
                        if node_exists node.id then begin
                            (*previous focused goal is proved*)
                            if node.id <> cnode.id then begin
                                (*History.record_step !Runtime.new_stateid (Change_state (cnode.id, cnode.state));
                                change_node_state cnode.id Proved;*)
                                on_change_node_state cnode Proved;
                                let node_to_chose = get_node node.id in
                                (*History.record_step !Runtime.new_stateid (Change_state (node_to_chose.id, node_to_chose.state));
                                node_to_chose.state <- Chosen *)
                                on_change_node_state node_to_chose Chosen;
                                node_to_chose.stateid <- !Runtime.new_stateid;
                                List.iter (fun n ->
                                    let node = get_node n.id in
                                    on_change_node_state node To_be_chosen;
                                    node.stateid <- !Runtime.new_stateid
                                ) other_nodes
                            end
                        end else begin
                            (*History.record_step !Runtime.new_stateid (Change_state (cnode.id, cnode.state);
                            cnode.state <- Not_proved;*)
                            on_change_node_state cnode Not_proved;
                            (*printf "changed state of node %s" cnode.id;*)
                            (* flush stdout; *)
                            (*add_edge cnode node (snd (List.hd !Doc_model.doc));
                            History.record_step !Runtime.new_stateid (Add_node node.id);
                            node.state <- Chosen;*)
                            on_add_node cnode node Chosen;
                            List.iter (fun (n:node) -> if not (node_exists n.id) then begin
                                    (*add_edge cnode n (snd (List.hd !Doc_model.doc));
                                    History.record_step !Runtime.new_stateid (Add_node n.id);
                                    n.state <- To_be_chosen*)
                                    on_add_node cnode n To_be_chosen
                                end
                            ) other_nodes
                        end
                end)
            end                


let on_edit_at stateid = 
    Doc_model.move_focus_to stateid;
    Runtime.new_stateid := stateid;
    History.undo_upto stateid
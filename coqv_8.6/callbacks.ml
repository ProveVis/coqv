open Printf
open Runtime
open Proof_model
open Coqv_utils

let on_receive_goals cmd_type goals = 
    printf "focused goals number: %d. \n" (List.length (goals));
    begin
        match cmd_type with
        | Module modul_name -> moduls := (create_module modul_name) :: !moduls
        | End modul_name -> closing_modul modul_name
        | Proof (thm_name, kind) -> 
            assert(List.length goals <> 0);
            let goal = List.hd (goals) in
            let label = goal_to_label goal in
            (*print_endline label.id;*)
            let rec node : node = {
                id = goal.goal_id;
                label = label;
                state = Chosen;
                parent = node;
            } in
            let proof_tree = new_proof_tree node in
            let session = new_session thm_name kind Processing proof_tree in
            current_session_id := Some thm_name;
            printf "current session id: %s\n" thm_name;
            flush stdout;
            assert(List.length !moduls > 0);
            add_session_to_modul (List.hd !moduls) session;
            (*printf "%d moduls at the moment\n" (List.length !moduls);*)
            History.record_step !Runtime.new_stateid (Add_node node.id)
            (*print_endline "finished creating session."*)
        | Qed -> 
            current_session_id := None;
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
                }) fg_goals in
                if List.length new_nodes = 0 then begin
                    print_endline "No more goals, shall change the focused node into proved.";
                    History.record_step !Runtime.new_stateid (Change_state (cnode.id, cnode.state));
                    change_node_state cnode.id Proved
                end else begin
                    let node, other_nodes = List.hd new_nodes, List.tl new_nodes in 
                    (*match chosen_node with
                    | None -> print_endline "No focus node, maybe the proof tree is complete"
                    | Some cnode ->*)
                        if node_exists node.id then begin
                            (*previous focused goal is proved*)
                            if node.id <> cnode.id then begin
                                History.record_step !Runtime.new_stateid (Change_state (cnode.id, cnode.state));
                                change_node_state cnode.id Proved;
                                let node_to_chose = get_node node.id in
                                History.record_step !Runtime.new_stateid (Change_state (node_to_chose.id, node_to_chose.state));
                                node_to_chose.state <- Chosen 
                            end
                        end else begin
                            History.record_step !Runtime.new_stateid (Change_state (cnode.id, cnode.state));
                            cnode.state <- Not_proved;
                            add_edge cnode node (snd (List.hd !Doc_model.doc));
                            History.record_step !Runtime.new_stateid (Add_node node.id);
                            node.state <- Chosen;
                            List.iter (fun (n:node) -> if not (node_exists n.id) then begin
                                    add_edge cnode n (snd (List.hd !Doc_model.doc));
                                    History.record_step !Runtime.new_stateid (Add_node n.id);
                                    n.state <- To_be_chosen
                                end
                            ) other_nodes
                        end
                end)
            end                
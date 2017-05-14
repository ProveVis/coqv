open Types
open Printf


let new_proof_tree node = 
    {
        root = node;
        nodes = begin 
                let ht = Hashtbl.create 1 in
                Hashtbl.add node.id node;
                ht
            end;
        edges = Hashtbl.create 1;
    }

let add_edge proof_tree from_node to_node tatic = 
    if from_node.id = to_node.id then ();
    if Hashtbl.mem proof_tree.nodes from_node.id then begin
        to_node.parent <- from_node;
        if not Hashtbl.mem proof_tree.nodes to_node.id then
            Hashtbl.add proof_tree.nodes to_node.id to_node;
        if Hashtbl.mem proof_tree.edges from_node.id then begin
            let (t, nl) = Hashtbl.find proof_tree.edges from_node.id in
            if not List.mem to_node.id nl then
                Hashtbl.replace proof_tree.edges from_node.id (t, to_node.id :: nl)
        end else begin
            Hashtbl.add proof_tree.edges from_node.id (tatic, [to_node.id])    
        end
    end else begin
        printf "cannot find where to add node %s\n" to_node.id;
        flush stdout
    end

let change_node_state node node_state = 
    node.state <- node_state

let new_session sname skind sstate proof_tree = 
    {
        name = sname;
        kind = skind;
        state = sstate;
        proof_tree = proof_tree;
    }

let add_session_to_modul modul session = 
    Hashtbl.add modul.sessions session
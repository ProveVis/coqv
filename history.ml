open Types
open Runtime



let history_list = ref [] 

let record_step tatic node sub_nodes = 
    let proof_tree = current_proof_tree () in
    List.iter (fun n -> Hashtbl.add proof_tree.nodes n.id n) sub_nodes;
    Hashtbl.add proof_tree.edges node (tatic, sub_nodes);
    history_list := (tatic, node, sub_nodes) :: !history_list


let undo_step n = 
    match !history_list with
    | [] -> ()
    | (tatic, node, sub_nodes) :: steps ->
        let proof_tree = current_proof_tree () in
        List.iter (fun n -> Hashtbl.remove proof_tree.nodes n.id) sub_nodes;
        Hashtbl.remove proof_tree.edges node;
        history_list := List.tl (!history_list);
        undo_step (n-1) 

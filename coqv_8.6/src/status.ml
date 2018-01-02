open Printf
open Runtime
open Types
open Interface

let str_status () = 
    let str_coqinfo = sprintf "coq version: %s" (!coqtop_info).coqtop_version in
    let str_stateid = sprintf "current stateid: %d" !new_stateid in
    let str_history = sprintf "history recorded: %d records" (List.length !History.history) in
    let str_session = sprintf "current session: %s" begin
            match !Proof_model.current_session_id with
            | "" -> "None"
            | sname -> sname
        end in
    sprintf "\n\t%s\n\t%s\n\t%s\n\t%s" 
        str_coqinfo 
        str_stateid 
        str_history
        str_session

let str_proof_tree sn_path = 
    let str_sp_list = String.split_on_char '.' (String.trim sn_path) in
    let sno = Proof_model.find_session str_sp_list in
    match sno with
    | None -> 
        printf "session %s not found." sn_path;
        flush stdout;
        ""
    | Some sn -> 
        let str_buf = ref "" in
        let node_queue = Queue.create () in
        let proof_tree = sn.proof_tree in
        Queue.push proof_tree.root.id node_queue;
        while not (Queue.is_empty node_queue) do
            let current_node = Queue.pop node_queue in
            str_buf := !str_buf ^ current_node;
            let node = Proof_model.select_node current_node proof_tree in
            str_buf := (!str_buf) ^" ("^(str_node_state node.state)^")\n"^ (Types.str_label node.label);
            try 
                let cmd, clist = Proof_model.children current_node proof_tree in
                str_buf := !str_buf ^ "(" ^ cmd ^ ")\t[";
                List.iter (fun c -> str_buf := !str_buf ^ c ^ " "; Queue.push c node_queue) clist;
                str_buf := !str_buf ^ "]\n\n";
            with Not_found -> begin
                str_buf := !str_buf ^ "\t[]\n"
            end
        done;
        !str_buf
           
















open Types
open Printf

let sessions = Hashtbl.create 10
let current_session_id = ref None

exception Node_not_found of string
exception Session_not_found of string
exception Not_in_session

let select_node nid = 
    match !current_session_id with
    | None -> raise Not_in_session
    | Some sid -> begin
        try
            let session = Hashtbl.find sessions sid in
            try
                Hashtbl.find session.proof_tree.nodes nid
            with 
                Not_found -> raise (Node_not_found nid);
        with Not_found -> raise (Session_not_found sid)
    end

let children node = 
    match !current_session_id with
    | None -> raise Not_in_session
    | Some sid -> 
        let id = node.id in
        let session = Hashtbl.find sessions sid in
        let ids = Hashtbl.find session.proof_tree.edges id in
        List.map (fun id -> Hashtbl.find session.proof_tree.nodes id) ids

let hide node = 
    printf "hiding node %s\n" node.id; 
    flush stdout

let show node = 
    printf "showing node %s\n" node.id;
    flush stdout

let change_state node state = 
    node.state <- state

let print_label node = 
    printf "%s\n" node.label;
    flush stdout
    
(*
let hide_subproof node_id_list = 
    match !current_session_id with
    | None -> raise No_session_id
    | Some sid -> begin
        try
            let session = Hashtbl.find sessions sid in
            let find_node_by_id nid = 
                try
                    Hashtbl.find session.proof_tree.nodes nid
                with 
                    Not_found -> raise (Node_not_found nid) in
            List.iter 
                (fun nid ->
                    let subnode_ids = Hashtbl.find session.proof_tree.edges nid in
                    List.iter 
                        (fun subnid -> 
                            let subnode = Hashtbl.find session.proof_tree.nodes subnid in
                            subnode.visible <- false
                        ) subnode_ids;
                    printf "Subnodes of %s are hidden\n" nid
                ) node_id_list;
            flush stdout
        with Not_found -> raise (Session_not_found sid)
    end

let show_children node_id_list = 
    match !current_session_id with
    | None -> raise No_session_id
    | Some sid -> begin
        try
            let session = Hashtbl.find sessions sid in
            let find_node_by_id nid = 
                try
                    Hashtbl.find session.proof_tree.nodes nid
                with 
                    Not_found -> raise (Node_not_found nid) in
            List.iter 
                (fun nid ->
                    let subnode_ids = Hashtbl.find session.proof_tree.edges nid in
                    List.iter 
                        (fun subnid -> 
                            printf "%s\n" subnid
                        ) subnode_ids
                ) node_id_list;
            flush stdout
        with Not_found -> raise (Session_not_found sid)
    end   *)





open Session
open Printf
(*
module Key = struct
    type t = node
    let compare n1 n2 = Pervasives.compare (n1.id) (n2.id)
end

module Node_set = Set.Make(Key)*)

let sessions = Hashtbl.create 10
let current_session_id = ref None

exception Node_not_found of string
exception No_session_id
exception Session_not_found of string

let select_node node_id_list = 
    match !current_session_id with
    | None -> raise No_session_id
    | Some sid -> begin
        try
            let session = Hashtbl.find sessions sid in
            let node_list = ref [] in
            node_list := List.map 
                (fun nid ->
                    try
                        Hashtbl.find session.proof_tree.nodes nid
                    with 
                        Not_found -> raise (Node_not_found nid)
                ) node_id_list;
            printf "%d nodes selected:\n----------------\n" List.length node_list;
            List.iter 
                (fun n -> 
                    printf "%s\n" (str_node n)
                ) node_list;
            flush stdout
        with Not_found -> raise (Session_not_found sid)
    end
    

let show_label node_id_list = 
    match !current_session_id with
    | None -> raise No_session_id
    | Some sid -> begin
        try
            let session = Hashtbl.find sessions sid in
            let node_list = ref [] in
            node_list := List.map 
                (fun nid ->
                    try
                        Hashtbl.find session.proof_tree.nodes nid
                    with 
                        Not_found -> raise (Node_not_found nid)
                ) node_id_list;
            printf "%d nodes selected:\n----------------\n" List.length node_list;
            List.iter 
                (fun n -> 
                    printf "Node %s: %s\n" n.id n.label
                ) node_list;
            flush stdout
        with Not_found -> raise (Session_not_found sid)
    end
    

let hide_subproof node_id_list = 
    let find_node_by_id nid = 
        try
            Hashtbl.find nid
        with 
            Not_found -> raise (Node_not_found nid) in
    List.iter 
        (fun nid ->
            Hashtbl.find 
        ) 
        node_id_list
    let node_list = ref [] in
    node_list := List.map (fun nid -> find_node_by_id nid) node_id_list;






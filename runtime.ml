open Types
open Printf

let prompt = ref "Coq < "
let ignored_re () = Str.regexp !prompt

let dummy_modul : modul = 
    {
        name = "";
        sessions = Hashtbl.create 1;
        modul_tbl = Hashtbl.create 1;
    }

let current_session_id = ref None
let moduls = ref [dummy_modul]


exception Node_not_found of string
exception Session_not_found of string
exception Not_in_session
exception Closing_wrong_module of string * string

let closing_modul name = 
    let top_modul = List.hd !moduls in
    let snd_top_modul = List.hd (List.tl !moduls) in
    if name = top_modul.name then begin
        Hashtbl.add snd_top_modul.modul_tbl name top_modul;
        moduls := List.tl !moduls
    end else begin
        raise (Closing_wrong_module (top_modul.name, name))  
    end

let current_proof_tree () = 
    match !current_session_id with
    | None -> raise Not_in_session
    | Some sid -> 
        try
            let session = Hashtbl.find ((List.hd !moduls).sessions) sid in
            session.proof_tree
        with Not_found -> raise (Session_not_found sid)

let select_node nid = 
    match !current_session_id with
    | None -> raise Not_in_session
    | Some sid -> begin
        try
            let session = Hashtbl.find ((List.hd !moduls).sessions) sid in
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
        let session = Hashtbl.find ((List.hd !moduls).sessions) sid in
        Hashtbl.find session.proof_tree.edges node

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





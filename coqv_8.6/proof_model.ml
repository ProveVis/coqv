open Types
open Printf

let create_module modul_name = 
    {
        name = modul_name;
        sessions = Hashtbl.create 1;
        modul_tbl = Hashtbl.create 1;
    }


let current_session_id = ref None
let top_module = create_module "Unnamed_module"
let moduls = ref [top_module]

(*let coq_state_id = ref 0*)

let new_proof_tree node = 
    {
        root = node;
        nodes = begin 
                let ht = Hashtbl.create 1 in
                Hashtbl.add ht node.id node;
                ht
            end;
        edges = Hashtbl.create 1;
    }


exception Node_not_found of string
exception Session_not_found of string
exception Not_in_session
exception Closing_wrong_module of string * string

let closing_modul name = 
    if name = top_module.name then ();
    let fst_modul = List.hd !moduls in
    let snd_top_modul = List.hd (List.tl !moduls) in
    if name = fst_modul.name then begin
        Hashtbl.add snd_top_modul.modul_tbl name fst_modul;
        moduls := List.tl !moduls
    end else begin
        raise (Closing_wrong_module (fst_modul.name, name))  
    end

let current_proof_tree () = 
    match !current_session_id with
    | None -> raise Not_in_session
    | Some sid -> 
        try
            let session = Hashtbl.find ((List.hd !moduls).sessions) sid in
            session.proof_tree
        with Not_found -> raise (Session_not_found sid)

let node_exists nid = 
    let proof_tree = current_proof_tree () in
    Hashtbl.mem proof_tree.nodes nid

let select_node nid proof_tree = 
    try
        Hashtbl.find proof_tree.nodes nid
    with 
        Not_found -> raise (Node_not_found nid)

let select_chosen_node () = 
    let focused = ref None in
    let proof_tree = current_proof_tree () in
    let tmp_node_queue = Queue.create () in
    Queue.push proof_tree.root tmp_node_queue;
    let flag = ref true in
    while !flag && not (Queue.is_empty tmp_node_queue) do
        let node = Queue.pop tmp_node_queue in
        if node.state = Types.Chosen then begin
            focused := Some node;
            flag := false    
        end else try
            let _, children_id = Hashtbl.find proof_tree.edges node.id in
            List.iter (fun cid -> Queue.push (Hashtbl.find proof_tree.nodes cid) tmp_node_queue) children_id
        with Not_found -> ()
    done;
    !focused

let add_node node nodeid proof_tree = 
    let parent = select_node nodeid proof_tree in
    node.parent <- parent;
    (*let proof_tree = current_proof_tree () in*)
    Hashtbl.add proof_tree.nodes node.id node



let children node proof_tree = 
    Hashtbl.find proof_tree.edges node

let hide node = 
    printf "hiding node %s\n" node.id; 
    flush stdout

let show node = 
    printf "showing node %s\n" node.id;
    flush stdout

let is_children_complete proof_tree nodeid = 
    let _, children_id = Hashtbl.find proof_tree.edges nodeid in
    let flag = ref true in
    List.iter (fun cid -> 
        match (Hashtbl.find proof_tree.nodes cid).state with
        | Proved | Assumed -> ()
        | _ -> flag := false
    ) children_id;
    !flag

let change_node_state nid state = 
    let proof_tree = current_proof_tree () in
    let tmp_node_queue = Queue.create () in
    Queue.push proof_tree.root tmp_node_queue;
    let flag = ref true in
    while !flag && not (Queue.is_empty tmp_node_queue) do
        let node = Queue.pop tmp_node_queue in
        if node.id = nid then begin
            node.state <- state;
            let rec change_others other_node = 
                if is_children_complete proof_tree other_node.id then
                    other_node.state <- Proved
                else 
                    other_node.state <- Not_proved;
                if other_node.id <> other_node.parent.id then
                    change_others other_node.parent in
            change_others node.parent;
            flag := false
        end else begin try
            let _, children_id = Hashtbl.find proof_tree.edges nid in
            List.iter (fun cid -> Queue.push (Hashtbl.find proof_tree.nodes cid) tmp_node_queue) children_id
        with Not_found -> ()
        end 
    done

let remove_node nid = 
    let proof_tree = current_proof_tree () in
    assert (nid <> proof_tree.root.id);
    let tmp_node_queue = Queue.create () in
    Queue.push proof_tree.root tmp_node_queue;
    while not (Queue.is_empty tmp_node_queue) do
        let node = Queue.pop tmp_node_queue in
        Hashtbl.remove proof_tree.nodes node.id;
        let _, children_id = Hashtbl.find proof_tree.edges nid in
        List.iter (fun cid -> Queue.push (Hashtbl.find proof_tree.nodes cid) tmp_node_queue) children_id;
        Hashtbl.remove proof_tree.edges node.id
    done

let print_label node = 
    printf "%s\n" (str_label node.label);
    flush stdout


let add_edge from_node to_node tatic = 
    let proof_tree = current_proof_tree () in
    if from_node.id = to_node.id then ();
    if Hashtbl.mem proof_tree.nodes from_node.id then begin
        to_node.parent <- from_node;
        if not (Hashtbl.mem proof_tree.nodes to_node.id) then
            Hashtbl.add proof_tree.nodes to_node.id to_node;
        if Hashtbl.mem proof_tree.edges from_node.id then begin
            let (t, nl) = Hashtbl.find proof_tree.edges from_node.id in
            if not (List.mem to_node.id nl) then
                Hashtbl.replace proof_tree.edges from_node.id (t, to_node.id :: nl)
        end else begin
            Hashtbl.add proof_tree.edges from_node.id (tatic, [to_node.id])    
        end
    end else begin
        printf "cannot find where to add node %s\n" to_node.id;
        flush stdout
    end

let new_session sname skind sstate proof_tree = 
    {
        name = sname;
        kind = skind;
        state = sstate;
        proof_tree = proof_tree;
    }

let add_session_to_modul modul (session : session) = 
    Hashtbl.add modul.sessions session.name session

let find_session sn_path = 
    let rec find_in_moduls sp modul = 
        match sp with
        | [] -> None
        | [sname] -> 
            (try
                Some (Hashtbl.find modul.sessions sname) 
            with Not_found -> None)
        | mname :: sp' -> 
            (try
                let next_modul = Hashtbl.find modul.modul_tbl mname in
                find_in_moduls sp' next_modul
            with Not_found -> None) in
    find_in_moduls sn_path (List.hd !moduls)
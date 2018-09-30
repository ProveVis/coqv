open Printf
open Vmdv_protocol
open Vmdv_client
open Coq_client
open Runtime
open Types
open Proof_model

let parse_vmsv_msg vagent msg = 
    match msg with
    | Highlight_node (sid, nid) -> 
        printf "Highlight node %s in session %s\n" nid sid;
        flush stdout;
        feedback_ok vagent sid
    | Unhighlight_node (sid, nid) ->
        printf "Unhighlight node %s in session %s\n" nid sid;
        flush stdout;
        feedback_ok vagent sid
    | Remove_subproof (sid, nid) ->
        let node = Proof_model.get_node nid in
        let new_stateid = node.stateid in
        if new_stateid <> -1 then
            Coq_client.request_edit_at new_stateid
    | Expand_cut (sid, nid, cutname) ->
        begin match Proof_model.find_session [cutname] with
        | None -> Printf.printf "cannot find the proof of %s\n" cutname
        | Some sesion ->
            Options.action (fun vagt -> 
                (* Vmdv_client.create_session vagt sesion; *)
                (* printf "created session %s, which is %s\n" sid sesion.name; *)
                let prooftree = sesion.proof_tree in
                let nodeq = Queue.create () in
                Queue.push prooftree.root.id nodeq;
                Vmdv_client.add_node vagt sid prooftree.root;
                let current_prooftree = Proof_model.current_proof_tree () in
                let tactics = fst (Hashtbl.find current_prooftree.edges nid) in
                Vmdv_client.add_edge vagt sid nid prooftree.root.id (Status.str_tactics tactics);
                Thread.delay (2.0);
                let tactics,cnids = Hashtbl.find prooftree.edges prooftree.root.id in
                Vmdv_client.set_proof_rule vagt sid prooftree.root.id (Status.str_tactics tactics);
                while not (Queue.is_empty nodeq) do
                    let pnid = Queue.pop nodeq in
                    if Hashtbl.mem prooftree.edges pnid then begin
                        let tactics, cnids = (Hashtbl.find prooftree.edges pnid) in
                        List.iter (fun cnid -> 
                            let cnode = Hashtbl.find prooftree.nodes cnid in
                            Vmdv_client.add_node vagt sid cnode;
                            (* printf "adding node %s\n" cnid; *)
                            Vmdv_client.add_edge vagt sid pnid cnid (Status.str_tactics tactics);
                            (* printf "adding edge %s-->%s\n" pnid cnid; *)
                            let tactics = fst (Hashtbl.find prooftree.edges cnid) in
                            Vmdv_client.set_proof_rule vagt sid cnid (Status.str_tactics tactics);
                            Queue.push cnid nodeq
                        ) cnids
                    end else 
                        printf "node %s has no successors\n" pnid
                done
            ) !Runtime.vagent
        end
    | Feedback_ok sid ->
        printf "Feedback OK received from %s\n" sid;
        flush stdout
    | Feedback_fail (sid, error_msg) ->
        printf "Feedback Fail received from %s: %s\n" sid error_msg;
        flush stdout
    | Clear_color sid -> ()
    | _ -> 
        printf "Not supposed to recieve this message: \n%s\n" (Yojson.Basic.to_string (json_of_msg msg));
        flush stdout

let customized_funcs = Hashtbl.create 1

let add_coqv_cmd cmd docstr func = 
    if Hashtbl.mem customized_funcs cmd then
        printf "coqv command %s is already defined, cannot define twice" cmd
    else
        Hashtbl.add customized_funcs cmd (docstr, func)

let invoke cmd args = 
    if Hashtbl.mem customized_funcs cmd then
        let docstr_func = Hashtbl.find customized_funcs cmd in
        (snd docstr_func) args
    else
        printf "coqv command %s is not defined\n" cmd

let coqv_visualize options = 
    match (List.hd options) with
    | "on" ->
        let s = List.hd (List.tl options) in
        begin try
            vagent := Some (get_visualize_agent s parse_vmsv_msg);
            print_endline "connected to vmdv."
        with _ -> print_endline ("connect to vmdv in "^s^" failed.")
        end
    | "off" -> 
        close_current_visualize_agent ();
        (* Communicate.vagent := None; *)
        print_endline "now close the connect to vmdv"
    | ":current" ->
        let sid = !Proof_model.current_session_id in
        begin match Proof_model.find_session [sid] with 
        | None -> printf "cannot find the proof of %s\n" sid
        | Some sesion -> 
            Options.action (fun vagt -> 
                Vmdv_client.create_session vagt sesion;
                printf "created session %s, which is %s\n" sid sesion.name;
                let prooftree = sesion.proof_tree in
                let nodeq = Queue.create () in
                Queue.push prooftree.root.id nodeq;
                Vmdv_client.add_node vagt sid prooftree.root;
                Thread.delay (2.0);
                let tactics,cnids = Hashtbl.find prooftree.edges prooftree.root.id in
                Vmdv_client.set_proof_rule vagt sid prooftree.root.id (Status.str_tactics tactics);
                while not (Queue.is_empty nodeq) do
                    let pnid = Queue.pop nodeq in
                    if Hashtbl.mem prooftree.edges pnid then begin
                        let tactics, cnids = (Hashtbl.find prooftree.edges pnid) in
                        List.iter (fun cnid -> 
                            let cnode = Hashtbl.find prooftree.nodes cnid in
                            Vmdv_client.add_node vagt sid cnode;
                            (* printf "adding node %s\n" cnid; *)
                            Vmdv_client.add_edge vagt sid pnid cnid (Status.str_tactics tactics);
                            (* printf "adding edge %s-->%s\n" pnid cnid; *)
                            let tactics = fst (Hashtbl.find prooftree.edges cnid) in
                            Vmdv_client.set_proof_rule vagt sid cnid (Status.str_tactics tactics);
                            Queue.push cnid nodeq
                        ) cnids
                    end else 
                        printf "node %s has no successors\n" pnid
                done
            ) !vagent
        end

    | sid -> 
        begin match Proof_model.find_session [sid] with 
        | None -> printf "cannot find the proof of %s\n" sid
        | Some sesion -> 
            Options.action (fun vagt -> 
                Vmdv_client.create_session vagt sesion;
                printf "created session %s, which is %s\n" sid sesion.name;
                let prooftree = sesion.proof_tree in
                let nodeq = Queue.create () in
                Queue.push prooftree.root.id nodeq;
                Vmdv_client.add_node vagt sid prooftree.root;
                Thread.delay (2.0);
                let tactics,cnids = Hashtbl.find prooftree.edges prooftree.root.id in
                Vmdv_client.set_proof_rule vagt sid prooftree.root.id (Status.str_tactics tactics);
                while not (Queue.is_empty nodeq) do
                    let pnid = Queue.pop nodeq in
                    if Hashtbl.mem prooftree.edges pnid then begin
                        let tactics, cnids = (Hashtbl.find prooftree.edges pnid) in
                        List.iter (fun cnid -> 
                            let cnode = Hashtbl.find prooftree.nodes cnid in
                            Vmdv_client.add_node vagt sid cnode;
                            (* printf "adding node %s\n" cnid; *)
                            Vmdv_client.add_edge vagt sid pnid cnid (Status.str_tactics tactics);
                            (* printf "adding edge %s-->%s\n" pnid cnid; *)
                            let tactics = fst (Hashtbl.find prooftree.edges cnid) in
                            Vmdv_client.set_proof_rule vagt sid cnid (Status.str_tactics tactics);
                            Queue.push cnid nodeq
                        ) cnids
                    end else 
                        printf "node %s has no successors\n" pnid
                done
            ) !vagent
            
        end
        (* print_endline "invalid command"  *)

let coqv_show options = 
    match (List.hd options) with
    | "module" -> List.iter (fun (m:Types.modul) -> print_endline m.name) !Proof_model.moduls
    | "proofs" -> 
        let m = List.hd !Proof_model.moduls in
        let ss = m.sessions in
        Hashtbl.iter (fun a b -> print_endline a) ss
    | _ -> ()

let coqv_prove options = 
    let nid = List.hd options in
    if Proof_model.node_exists nid then
        let node = Proof_model.get_node nid in
        let chosen_stateid = node.stateid in
        if chosen_stateid <> (-1) then
            request_edit_at chosen_stateid
        else begin
            let pos = Lists.find_pos nid !leaf_nids in
            if pos >= 0 then begin
                handle_input ("Focus "^(string_of_int (pos+1))^".");
                pending_task := No_task
            end else begin
                handle_input "Unfocus.";
                pending_task := TryedFocus nid
            end
        end
    else begin
        print_endline ("Cannot find node "^nid)
    end

let coqv_stateid options = 
    print_endline ("current stateid: "^(string_of_int (!Doc_model.current_stateid)))

let coqv_node options = 
    try
        print_endline (str_node (Proof_model.get_node (List.hd options)))
    with
        Not_in_session -> print_endline ("node not found!")

let coqv_status options = 
    print_endline (Status.str_status ())

let coqv_proof options = 
    if options = [] then begin
        match !Proof_model.current_session_id with
        | "" -> print_endline "not in proof mode"
        | sname -> print_endline (Status.str_proof_tree sname)    
    end else 
        List.iter (fun a -> print_endline (Status.str_proof_tree a)) options

let coqv_undo_to options = 
    let new_stateid = int_of_string (List.hd options) in
    if new_stateid <> -1 then 
        request_edit_at new_stateid

let coqv_quit options = 
    Flags.running_coqv := false;
    request_quit ()

let coqv_export options = 
    let eout = open_out (List.hd options) in
    let cmd_list = Doc_model.get_committed_commands () in
    List.iter (fun cmd -> output_string eout cmd; output_string eout "\n"; if cmd="Qed." then output_string eout "\n") cmd_list;
    flush eout;
    close_out eout

let coqv_import options = 
    Flags.batch_mode := true; (*we are in batch mode now*)
    let inpt = open_in (List.hd options) in
    let cmd_strs = ref [] in
    let lineno = ref 1 in
    let chars_to_string chars = 
        String.trim (String.init (List.length chars) (fun i -> List.nth chars i)) in
    let inpt_buffer = ref [] in 
    let comment_level = ref [] in
    begin
        try
            while true do
                let c = input_char inpt in
                match c with
                | '\n' -> 
                    incr lineno; 
                    if !comment_level = [] && List.length !inpt_buffer > 0 && List.nth !inpt_buffer (List.length !inpt_buffer - 1) <> ' ' then  
                        inpt_buffer := !inpt_buffer @ ['\n']
                | '.' -> 
                    if !comment_level = [] then begin
                        let c1 = input_char inpt in 
                        begin match c1 with
                        | ' ' | '\t' | '\n' -> 
                            let cmd_str = String.trim (chars_to_string (!inpt_buffer @ ['.'])) in 
                            if cmd_str <> "" && cmd_str <> "." then
                                cmd_strs := !cmd_strs @ [cmd_str]; 
                            inpt_buffer := []
                        | _ -> inpt_buffer := !inpt_buffer @ [c;c1]
                        end
                    end
                | '(' ->
                    let c1 = input_char inpt in
                    if c1 = '*' then
                        comment_level := "*"::!comment_level
                    else if !comment_level = [] then
                        inpt_buffer := !inpt_buffer @ [c;c1]
                | '*' ->
                    let c1 = input_char inpt in
                    if c1 = ')' then
                        comment_level := List.tl !comment_level
                    else if !comment_level = [] then
                        inpt_buffer := !inpt_buffer @ [c;c1]

                | _ -> 
                    if !comment_level = [] then
                        inpt_buffer := !inpt_buffer @ [c]
            done
        with End_of_file ->
            if List.length !cmd_strs = 0 then begin
                printf "The content of %s is empty\n" (List.hd options);
                flush stdout
            end else if (String.trim(chars_to_string !inpt_buffer) = "") then begin
                (* print_endline "command read:";
                List.iter (fun cmd -> print_endline cmd) !cmd_strs; *)
                let cmdhd, cmdtl = List.hd !cmd_strs, List.tl !cmd_strs in
                batch_commands := cmdtl;
                (* printf "number of commands wait to send to coqtop: %d\n" (List.length !batch_commands); *)
                handle_input cmdhd;
                (* List.iter (fun cmd_str -> handle_input cmd_str; Thread.delay 1.0; printf "Sent: %s\n" cmd_str; incr current_line) !cmd_strs; *)
                printf "Successfully read from file %s\n" (List.hd options);
                flush stdout
            end else begin
                print_endline "command read:";
                List.iter (fun cmd -> print_endline (cmd^"--\n")) !cmd_strs;
                printf "read file %s error: line %d\n" (List.hd options) !lineno;
                flush stdout
            end
    end


let coqv_help options = 
    Hashtbl.iter (fun cmd ((para, usage), _) -> 
        printf "%s %s:\t\t\t\t%s\n" cmd para usage
    ) customized_funcs 
(* 
    match options with
    | [] -> List.iter (fun (cmd, args, usage) -> printf "%s %s: %s\n" cmd args usage; flush stdout) Coqv_doc.commands 
    | _ -> List.iter (fun (cmd, args, usage) -> if List.mem cmd options then begin printf "%s %s: %s\n" cmd args usage; flush stdout end) Coqv_doc.commands  *)

let interpret_cmd cmd_str_list = 
    match cmd_str_list with
    | [] -> ()
    | cmd :: options -> invoke cmd options





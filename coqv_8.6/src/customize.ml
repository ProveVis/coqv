
let interpret_cmd cmd_str_list = 
    begin
        match cmd_str_list with
        | [] -> ()
        | cmd :: options -> 
        begin match cmd with
        | "visualize" ->
            begin
            match (List.hd options) with
            | "on" ->
                let s = List.hd (List.tl options) in
                begin try
                    Communicate.vagent := Some (Communicate.get_visualize_agent s);
                    print_endline "connected to vmdv."
                with _ -> print_endline ("connect to vmdv in "^s^" failed.")
                end
            | "off" -> 
                Communicate.close_current_visualize_agent ();
                (* Communicate.vagent := None; *)
                print_endline "now close the connect to vmdv"
            | _ -> print_endline "invalid command" 
            end
        | "show" ->
            begin match (List.hd options) with
            | "module" -> List.iter (fun (m:Types.modul) -> print_endline m.name) !Proof_model.moduls
            | "proofs" -> 
                let m = List.hd !Proof_model.moduls in
                let ss = m.sessions in
                Hashtbl.iter (fun a b -> print_endline a) ss
            | _ -> ()
            end
        | "prove" ->
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
        | "stateid" -> print_endline ("current stateid: "^(string_of_int (!Doc_model.current_stateid)))
        | "node" -> 
            begin try
                print_endline (str_node (Proof_model.get_node (List.hd options)))
            with
                Not_in_session -> print_endline ("node not found!")
            end
        | "status" -> print_endline (Status.str_status ())
            (* | "history" -> print_endline (History.str_history ()) *)
        | "proof" -> 
            if options = [] then
                begin
                    match !Proof_model.current_session_id with
                    | "" -> print_endline "not in proof mode"
                    | sname -> print_endline (Status.str_proof_tree sname)    
                end
            else 
                List.iter (fun a -> print_endline (Status.str_proof_tree a)) options
        | "undo_to" ->
            let new_stateid = int_of_string (List.hd options) in
            if new_stateid <> -1 then 
                request_edit_at new_stateid
        | "quit" -> 
            Flags.running_coqv := false;
            request_quit ()
            (* exit 0 *)
        | "export" ->
            let eout = open_out (List.hd options) in
            let cmd_list = Doc_model.get_committed_commands () in
            List.iter (fun cmd -> output_string eout cmd; output_string eout "\n"; if cmd="Qed." then output_string eout "\n") cmd_list;
            flush eout;
            close_out eout
        | "import" ->
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
                with
                    End_of_file ->
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
        | "help" -> begin
                match options with
                | [] -> List.iter (fun (cmd, args, usage) -> printf "%s %s: %s\n" cmd args usage; flush stdout) Coqv_doc.commands 
                | _ -> List.iter (fun (cmd, args, usage) -> if List.mem cmd options then begin printf "%s %s: %s\n" cmd args usage; flush stdout end) Coqv_doc.commands 
            end
        | _ -> print_endline "command not interpreted."
        end
    end
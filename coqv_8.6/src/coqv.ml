open Printf
open Str
open Runtime
open Interaction
open Interface
open Callbacks

let command_re = Str.regexp ":[-_A-Za-z0-9]+"

let read_write_condition = Condition.create ()
let read_write_mutex = Mutex.create ()

let worker cin =     
    let buffer = Bytes.create !Flags.xml_bufsize in
    while !Runtime.running do
        (* print_endline "wait for coqtop"; *)
        let len = input cin buffer 0 !Flags.xml_bufsize in
        (* print_endline ("coqtop responsed with length "^(string_of_int len)); *)
        if len = 0 then
            running := false
        else begin
            let output_str = Bytes.sub_string buffer 0 len in
            (*printf "%s" (Str.global_replace (ignored_re ()) "" output_str);
            flush stdout*)
            if(len <> 0) then
                handle_answer output_str;
            flush stdout
        end;
        (* print_endline ("there are "^(string_of_int (List.length !batch_commands))^" commands wait to send to coqtop"); *)
        if Doc_model.is_processed () && !Doc_model.goal_responsed then begin
            (* print_endline ("coqtop processed "); *)
            match !Callbacks.pending_task with
            | Focus nid -> 
                let pos = Lists.find_pos nid !Callbacks.leaf_nids in
                if pos >= 0 then begin
                    let cmd_str = "Focus "^(string_of_int (pos+1))^"." in
                    handle_input cmd_str;
                    Callbacks.pending_task := No_task
                end else begin
                    handle_input "Unfocus.";
                    Callbacks.pending_task := TryedFocus nid
                end
            | TryedFocus nid -> 
                let pos = Lists.find_pos nid !Callbacks.leaf_nids in
                if pos >= 0 then begin
                    let cmd_str = "Focus "^(string_of_int (pos+1))^"." in
                    handle_input cmd_str
                end else begin
                    print_endline ("Cannot chose node "^nid)
                end;
                Callbacks.pending_task := No_task
            | No_task ->
                if !Flags.batch_mode = false || (!batch_commands = []) then begin
                    Flags.batch_mode := false;
                    Flags.running_coqv := true;
                    Condition.signal read_write_condition
                end else begin
                    let bch, bct = List.hd !batch_commands, List.tl !batch_commands in
                    handle_input bch;
                    batch_commands := bct
                end
        end
    done;
    print_endline "worker quit"

let rec loop args = 
    let master2slave_in, master2slave_out = Unix.pipe () 
    and slave2master_in, slave2master_out = Unix.pipe () in
    Unix.set_close_on_exec master2slave_out;
    Unix.set_close_on_exec slave2master_in;
    (*let pid = Unix.create_process "/usr/bin/coqtop" (Array.of_list args)  master2slave_in slave2master_out slave2master_out in*)
    let pid = Unix.create_process "coqtop" (Array.of_list args)  master2slave_in slave2master_out slave2master_out in
    if pid = 0 then 
        printf "create process error"
    else begin
        (*printf "created process %d\n" pid;*)
        let cin, cout = Unix.in_channel_of_descr slave2master_in, Unix.out_channel_of_descr master2slave_out in
        Unix.close master2slave_in;
        Unix.close slave2master_out;
        Runtime.coq_channels.cin <- cin;
        Runtime.coq_channels.cout <- cout;
        begin
            Interaction.request_coq_info cout;
            Thread.delay 0.1;
            let buffer = Bytes.create !Flags.xml_bufsize in
            let len = input cin buffer 0 !Flags.xml_bufsize in
            let header = Bytes.sub_string buffer 0 len in
            let xparser = Xml_parser.make (Xml_parser.SString header) in
            let xml_fb = Xml_parser.parse xparser in
            let fb_val2 = Xmlprotocol.to_answer (Xmlprotocol.About ()) xml_fb in
            (match fb_val2 with
            | Good fb -> begin
                    Runtime.coqtop_info := fb;
                    printf "\t\tcoqv version %s [coqtop version %s (%s)]\n\n" Flags.version_no fb.coqtop_version fb.release_date;
                    flush stdout
                end
            | _ -> printf "parsing message fails");
            Flags.running_coqv := true
        end;
        ignore(Thread.create worker cin);
        request_init None;
        while !running do
            if not !Flags.running_coqv then begin
                Mutex.lock read_write_mutex;
                Condition.wait read_write_condition read_write_mutex;
                Mutex.unlock read_write_mutex
            end; 
            if !running then begin
                print_string "coqv> ";
                let input_str = String.trim (read_line ()) in
                if (String.length input_str > 0) then begin
                    if String.sub input_str 0 1 = ":" then begin
                        let cmd = (String.sub input_str 1 (String.length input_str - 1)) in
                        let cmd_str_list = Str.split (Str.regexp "[ \t]+") (String.trim cmd) in
                        interpret_cmd cmd_str_list
                    end else begin
                        let str_buffer = ref input_str in
                        while (String.sub !str_buffer (String.length !str_buffer - 1) 1 <> ".") do
                            print_string "    > ";
                            str_buffer := !str_buffer ^ (read_line ())
                        done;
                        handle_input !str_buffer
                    end
                end else 
                    Flags.running_coqv := true
            end
        done;
        print_endline "main thread quit"
    end

let _ = 
    Arg.parse 
        [
            "-ip", Arg.String (fun s -> 
                    try
                        Communicate.vagent := Some (Communicate.get_visualize_agent s)
                    with _ -> print_endline ("connect to vmdv in "^s^" failed.")
                ), "\tIP address of the VMDV.";
            "-debug", Arg.Unit (fun () -> 
                    if not (Sys.file_exists "log") then begin
                        let log_dir_created = Sys.command "mkdir log" in
                        if log_dir_created <> 0 then begin
                            print_endline "Warning: cannot create directory log, now running in non-debug mode";
                        end else begin
                            Flags.debug := true
                        end
                    end else begin
                        Flags.debug := true
                    end;
                    if !Flags.debug = true then begin
                        Runtime.logs := Some {
                            coqtop_log = open_out ("./log/"^(!Flags.xml_log_file)); 
                            vmdv_log = open_out ("./log/"^(!Flags.json_log_file))}
                    end
                ), "\tUsing debug mode or not."
        ]
        (fun s -> print_endline ("unknown option"^s))
        "Usage: coqv [-ip <ip_address>] [-debug]";
    loop ["-xml";"-ideslave"; "-main-channel"; "stdfds"; "-async-proofs"; "off"]
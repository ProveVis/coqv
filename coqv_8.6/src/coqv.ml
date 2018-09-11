open Printf
open Str
open Runtime
open Interface
open Coq_client
open Vmdv_client
open Customize

(* let command_re = Str.regexp ":[-_A-Za-z0-9]+" *)
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
            Coq_client.request_coq_info cout;
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
        ignore(Thread.create Coq_client.worker (cin, (Coq_client.on_receive_goals, Coq_client.on_finishing_proof)));
        request_init None;

        add_coqv_cmd "visualize" ("on <IP>/off","Turn on/off vmdv client") coqv_visualize;
        add_coqv_cmd "show" ("module/proofs","Show the names of defined modules or proofs") coqv_show;
        add_coqv_cmd "prove" ("<nid>","Jump between nodes to be proved") coqv_prove;
        add_coqv_cmd "stateid" ("","Show the current stateid") coqv_stateid;
        add_coqv_cmd "node" ("<nid>","Show the information of node with ID nid") coqv_node;
        add_coqv_cmd "status" ("","Show the current status of coqv") coqv_status;
        add_coqv_cmd "proof" (" /<pname>", "Show the current proof tree, or the proof tree with name pname") coqv_proof;
        add_coqv_cmd "undo_to" ("<stateid>", "Undo the proof until the given stateid") coqv_undo_to;
        add_coqv_cmd "quit" ("","Quit the system") coqv_quit;
        add_coqv_cmd "export" ("<fname>", "Export the coq script into a file") coqv_export;
        add_coqv_cmd "import" ("<fname>", "Import coq script from a file") coqv_import;


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
                        vagent := Some (get_visualize_agent s parse_vmsv_msg)
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
open Printf
open Str
open Runtime
open Interaction
open Interface

let command_re = Str.regexp ":[-_A-Za-z0-9]+"

let read_write_condition = Condition.create ()
let read_write_mutex = Mutex.create ()

let worker cin =     
    let buffer = Bytes.create !Flags.xml_bufsize in
    while !Runtime.running do
        (* print_endline "wait for coqtop"; *)
        let len = input cin buffer 0 !Flags.xml_bufsize in
        (* print_endline "coqtop responsed"; *)
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
        Condition.signal read_write_condition
        (*;
        print_endline "received a feedback"*)
    done

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
        (*starting reading from repl*)
        (*In_thread.run (fun () -> worker cin);*)
        
        (*input header information*)
        let running_coqv = ref false in
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
            running_coqv := true
        end;
        ignore(Thread.create worker cin);
        request_init None;
        while !running do
            if not !running_coqv then begin
                Mutex.lock read_write_mutex;
                Condition.wait read_write_condition read_write_mutex;
                Mutex.unlock read_write_mutex;
                Thread.delay 0.01 (*waiting for the last input from coqtop to complete*)
            end;
            print_string "coqv> ";
            let input_str = read_line () in
            if (String.length input_str > 0) then begin
                if String.sub input_str 0 1 = ":" then begin
                    let cmd = (String.sub input_str 1 (String.length input_str - 1)) in
                    let cmd_str_list = Str.split (Str.regexp "[ \t]+") (String.trim cmd) in
                    running_coqv := interpret_cmd cmd_str_list
                end else begin
                    running_coqv := false;
                    let str_buffer = ref input_str in
                    while (String.sub !str_buffer (String.length !str_buffer - 1) 1 <> ".") do
                        print_string "    > ";
                        str_buffer := !str_buffer ^ (read_line ())
                    done;
                    handle_input !str_buffer cout
                end
            end else 
                running_coqv := true
        done
    end

let _ = 
    Arg.parse 
        [
            "-ip", Arg.String (fun s -> 
                    try
                        Runtime.vagent := Some (Communicate.get_visualize_agent s)
                    with _ -> print_endline ("connect to vmdv in "^s^" failed.")
                ), "\tIP address of the VMDV.";
        ]
        (fun s -> print_endline ("unknown option"^s))
        "Usage: coqv [-ip <ip_address>]";
    loop ["-xml";"-ideslave"; "-main-channel"; "stdfds"]
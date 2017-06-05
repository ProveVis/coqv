open Printf
open Str
open Runtime
open Interaction
open Interface

let in_chan = stdin
let out_chan = stdout
(*let running = ref true*)

let command_re = Str.regexp ":[-_A-Za-z0-9]+"

let read_write_condition = Condition.create ()
let read_write_mutex = Mutex.create ()

let worker cin =     
    let buffer = Bytes.create 4096 in
    while !Runtime.running do
        let len = input cin buffer 0 4096 in
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
    let pid = Unix.create_process "/home/jian/.opam/4.04.1/bin/coqtop" (Array.of_list args)  master2slave_in slave2master_out slave2master_out in
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
        if not !Flags.xml then begin
            let buffer = Bytes.create 1024 in
            let len = input cin buffer 0 1024 in
            let header = Bytes.sub_string buffer 0 len in
            printf "\t\tCoqV version 0.1 [coqtop version %s]\n\n" (Str.global_replace (Str.regexp "\n") "" (Str.global_replace (Str.regexp "Welcome to Coq ") "" header))
        end else begin
            Interaction.request_coq_info cout;
            Thread.delay 0.1;
            let buffer = Bytes.create 4096 in
            let len = input cin buffer 0 4096 in
            let header = Bytes.sub_string buffer 0 len in
            (*print_endline ("received coq info msg, length "^(string_of_int(String.length header)));*)
            (*print_endline ("msg content: "^header);*)
            let xparser = Xml_parser.make (Xml_parser.SString header) in
            (*print_endline ("xparser complete");*)
            let xml_fb = Xml_parser.parse xparser in
            (*print_endline ("xml_fb complete");*)
            (*let fb_val = Xmlprotocol.to_value (Xmlprotocol.to_coq_info) xml_fb in
            (*print_endline ("fb_val complete");*)
            (match fb_val with
            | Good fb -> begin
                    Runtime.coqtop_info := fb;
                    (*printf "Coqtop Info: \n\tversion %s, \n\tprotocol version %s, \n\trelease date %s, \n\tand compile date %s\n" 
                        fb.coqtop_version fb.protocol_version fb.release_date fb.compile_date;*)
                    printf "\t\tCoqV version 0.1 [coqtop version %s (%s)]\n\n" fb.coqtop_version fb.release_date;
                    flush stdout
                end
            | _ -> printf "parsing message fails");*)

            let fb_val2 = Xmlprotocol.to_answer (Xmlprotocol.About ()) xml_fb in
            (match fb_val2 with
            | Good fb -> begin
                    Runtime.coqtop_info := fb;
                    (*printf "Coqtop Info: \n\tversion %s, \n\tprotocol version %s, \n\trelease date %s, \n\tand compile date %s\n" 
                        fb.coqtop_version fb.protocol_version fb.release_date fb.compile_date;*)
                    printf "\t\tCoqV version 0.1 [coqtop version %s (%s)]\n\n" fb.coqtop_version fb.release_date;
                    flush stdout
                end
            | _ -> printf "parsing message fails");
            (*handle_feedback header;
            let coqtop_info = !Runtime.coqtop_info in
            printf "\t\tCoqV version 0.1 [coqtop version %s (%s)]\n\n" coqtop_info.coqtop_version coqtop_info.release_date;
            flush stdout;*)
            running_coqv := true
            (*;
            print_endline "init complete, ready to proceed"*)
        end;
        ignore(Thread.create worker cin);
        (*print_endline "have created worker";*)
        while !running do
            if not !running_coqv then begin
                Mutex.lock read_write_mutex;
                Condition.wait read_write_condition read_write_mutex;
                Mutex.unlock read_write_mutex;
                Thread.delay 0.01 (*waiting for the last input from coqtop to complete*)
            end;
            print_string "coqv> ";
            (*let input_buffer = Bytes.create 1024 in
            let len = input stdin input_buffer 0 1024 in
            let input_str = Bytes.sub_string input_buffer 0 len in*)
            let input_str = read_line () in
            (*printf "input length: %d\n" (String.length input_str);*)
            if (String.length input_str > 0) then begin
                if String.sub input_str 0 1 = ":" then begin
                    running_coqv := true;
                    let cmd = (String.sub input_str 1 (String.length input_str - 1)) in
                    (*printf "command name: %s\n" cmd;
                    flush stdout*)
                    let cmd_str_list = Str.split (Str.regexp "[ \t]+") (String.trim cmd) in
                    interpret_cmd cmd_str_list
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

let _ = loop ["-xml";"-ideslave"; "-main-channel"; "stdfds"]
    (*Arg.parse
        [
            "-xml", Arg.Unit (fun () -> Flags.xml := true), "\tUsing XML to communicate with coqtop.";
        ]
        (fun s -> print_endline ("unknown option: "^s))
        "Usage: coqv [-xml]";
    if !Flags.xml then begin
        (*print_endline "coqv with xml";*)
        loop ["-xml";"-ideslave"; "-main-channel"; "stdfds"]
    end else
        loop ["-ideslave"]*)
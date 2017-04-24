open Printf
open Str

let in_chan = stdin
let out_chan = stdout
let running = ref true

let command_re = Str.regexp ":[-_A-Za-z0-9]+"

let worker cin =     
    let buffer = Bytes.create 4096 in
    while !running do
        let len = input cin buffer 0 4096 in
        if len = 0 then
            running := false
        else begin
            printf "RECEIVED: %s\n" (Bytes.sub_string buffer 0 len);
            flush stdout
        end
    done

let rec loop args = 
    let master2slave_in, master2slave_out = Unix.pipe () 
    and slave2master_in, slave2master_out = Unix.pipe () in
    Unix.set_close_on_exec master2slave_out;
    Unix.set_close_on_exec slave2master_in;
    let pid = Unix.create_process "/usr/bin/coqtop" (Array.of_list args)  master2slave_in slave2master_out slave2master_out in
    if pid = 0 then 
        printf "create process error"
    else begin
        printf "created process %d\n" pid;
        let cin, cout = Unix.in_channel_of_descr slave2master_in, Unix.out_channel_of_descr master2slave_out in
        Unix.close master2slave_in;
        Unix.close slave2master_out;
        (*starting reading from repl*)
        (*In_thread.run (fun () -> worker cin);*)
        ignore(Thread.create worker cin);
        while !running do
            print_string "coqv> ";
            let input_str = read_line () in
            output_string cout (input_str^"\n");
            flush cout
        done
        
    end
    (*;while !running do
        print_string "my_coqv> ";
        let input_str = read_line () in
        if Str.string_match command_re input_str 0 then
            let cmd = Str.split (Str.regexp ":") input_str in
            if List.hd cmd = "exit" then 
                running := false
            else 
                printf "command found: %s\n" (List.hd cmd)
        else
            printf "regular string: %s\n" input_str;
        flush stdout
    done*)

let _ = loop ["-async-proofs";"on";"-ideslave"; "-toploop";"coqv"; "-main-channel";"stdfds"; "-control-channel";"stdfds"]
open Printf
open Str

let in_chan = stdin
let out_chan = stdout
let running = ref true

let command_re = Str.regexp ":[-_A-Za-z0-9]+"
let ignored_re = Str.regexp "Coq < "

let read_write_condition = Condition.create ()
let read_write_mutex = Mutex.create ()

let worker cin =     
    let buffer = Bytes.create 4096 in
    while !running do
        let len = input cin buffer 0 4096 in
        if len = 0 then
            running := false
        else begin
            let output_str = Bytes.sub_string buffer 0 len in
            printf "%s" (Str.global_replace ignored_re "" output_str);
            flush stdout
        end;
        Condition.signal read_write_condition
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
        (*input header information*)
        let buffer = Bytes.create 1024 in
        let len = input cin buffer 0 1024 in
        let header = Bytes.sub_string buffer 0 len in
        printf "Using coqtop version %s" (Str.global_replace (Str.regexp "Welcome to Coq ") "" header);
        while !running do
            Mutex.lock read_write_mutex;
            Condition.wait read_write_condition read_write_mutex;
            Mutex.unlock read_write_mutex;
            print_string "coqv> ";
            let input_str = read_line () in
            output_string cout (input_str^"\n");
            flush cout
        done
    end

let _ = loop ["-ideslave"]
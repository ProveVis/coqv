open Printf

let _ = 
    let master2slave_in, master2slave_out = Unix.pipe () 
    and slave2master_in, slave2master_out = Unix.pipe () in
    let pid = Unix.create_process "./coqv" [||]  master2slave_in slave2master_out slave2master_out in
    if pid = 0 then 
        printf "create process error"
    else begin
        printf "created process %d\n" pid;
        let cin, cout = Unix.in_channel_of_descr slave2master_in, Unix.out_channel_of_descr master2slave_out in
        let input_str = read_line () in
        output_string cout (input_str^"\n");
        let input_from_pipe = input_line cin in
        printf "received message: %s" input_from_pipe
    end 

open Printf

let _ = 
    let master2slave_in, master2slave_out = Unix.pipe () 
    and slave2master_in, slave2master_out = Unix.pipe () in
    Unix.set_close_on_exec master2slave_out;
    Unix.set_close_on_exec slave2master_in;
    let pid = Unix.create_process "./repl" [||]  master2slave_in slave2master_out slave2master_out in
    if pid = 0 then 
        printf "create process error"
    else begin
        printf "created process %d\n" pid;
        let cin, cout = Unix.in_channel_of_descr slave2master_in, Unix.out_channel_of_descr master2slave_out in
        Unix.close master2slave_in;
        Unix.close slave2master_out;
        (*starting reading from repl*)
        let buffer = Bytes.create 4096 in
        let running = ref true in
        while !running do
            let input_str = read_line () in
            output_string cout (input_str^"\n");
            flush cout;
            let len = input cin buffer 0 4096 in
            printf "RECEIVED: %s\n" (Bytes.sub_string buffer 0 len)
        done

(*
        let input_str = read_line () in
        output_string cout (input_str^"\n");
        let input_from_pipe = input_char cin in
        printf "received message: %c" input_from_pipe*)
    end 

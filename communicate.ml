open Printf
open Types

type message = 
    | New_node of string * string * node_state
    | New_edge of string * string

let protocol_version_no = "20170502"
let sending_queue = Queue.create ()
let sending_mutex = Mutex.create ()
let sending_conditional = Conditional.create ()
let receiving_queue = Queue.create ()

let handshake cin cout = 
    output_string cout protocol_version_no;
    flush cout;
    let opponent_version_no = read_line() in
    if protocol_version_no = opponent_version_no then begin
        print_endline "Protocol match, handshake success.";
        true    
    end
    else begin
        printf "Error: protocol version %s not match" opponent_version_no;
        flush stdout;
        false
    end




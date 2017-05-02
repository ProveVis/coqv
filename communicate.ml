open Printf
open Types
open Yojson


type message = 
    | New_node of node
    | New_edge of string * string
    | Terminate

let protocol_version_no = "20170502"
let sending_queue = Queue.create ()
let sending_mutex = Mutex.create ()
let sending_conditional = Conditional.create ()
let log_file = "log"
(*let receiving_queue = Queue.create ()*)

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

let wait_to_send msg = 
    Mutex.lock sending_mutex;
    Queue.push msg sending_queue;
    Conditional.signal sending_conditional;
    Mutex.unlock sending_mutex


let json_of_msg (msg:message) = 
    match msg with
    | Terminate -> 
        `Assoc [
            ("protocol_version", `String protocol_version_no);
            ("type", `Int 0); (*0 is request, 1 is response*)
            ("content", `String "Terminate");
        ]
    | New_node n ->
        `Assoc [
            ("protocol_version", `String protocol_version_no);
            ("type", `Int 0);
            ("content", `String "New_node");
            ("node_id", `String n.id);
            ("node_label", `String n.label);
            ("node_state", `String (str_node_state n.state))
        ]
    | New_edge e -> 
        `Assoc [
            ("protocol_version", `String protocol_version_no);
            ("type", `Int 0);
            ("content", `String "New_edge");
            ("from_id", `String (fst e));
            ("to_id", `String (snd e))
        ]

let sending cout =
    let running = ref true in
    let log_out = open_out log_file in
    while !running do
        if Queue.is_empty sending_queue then begin
            Mutex.lock sending_mutex;
            Conditional.wait sending_conditional;
            Mutex.unlock sending_mutex
        end else begin
            let msg = ref Terminate in
            Mutex.lock sending_mutex;
            msg := Queue.pop sending_queue;
            Mutex.unlock sending_mutex;
            begin
                match !msg with
                | Terminate -> running := false
                | _ -> ()
            end;
            let json_msg = json_of_msg !msg in
            Yojson.to_channel cout json_msg;
            flush cout;
            output_string log_out "JSON data sent:\n";
            output_string log_out (Yojson.to_string json_msg)
            output_string log_out "\n";
            flush log_out
        end
    done

let receiving cin = 
    let running = ref true in
    let log_out = open_out log_file in
    while !running do
        let msg = Yojson.from_channel cin in
        output_string log_out "JSON data received:\n";
        output_string log_out (Yojson.to_string msg);
        output_string log_out "\n";
        flush log_out
    done

let start_send_receive cin cout =
    ignore (Thread.create (fun cin -> receiving cin) cin);
    ignore (Thread.create (fun cout -> sending cout) cout)







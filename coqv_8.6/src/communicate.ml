open Printf
open Types
open Runtime
open Vmdv_protocol

type vmdv_agent =
    {
        mutable input: in_channel;
        mutable output: out_channel;
        mutable is_alive: bool;
        sending_queue: message Queue.t;
        sending_mutex: Mutex.t;
        sending_conditional: Condition.t;
    }
    
let vagent : (vmdv_agent option) ref = ref None

(*     
let log_if_possible str = 
    match !Runtime.logs with
    | None -> ()
    | Some logs -> output_string logs.coqtop_log str; flush logs.coqtop_log *)

let wait_to_send vagent msg = 
    Mutex.lock vagent.sending_mutex;
    Queue.push msg vagent.sending_queue;
    Condition.signal vagent.sending_conditional;
    Mutex.unlock vagent.sending_mutex

let create_session vagent (session: session) = wait_to_send vagent (Create_session (session.name, (str_proof_kind session.kind) ^" "^session.name, "Tree", node_state_list))
let remove_session vagent sid = wait_to_send vagent (Remove_session sid)
let add_node vagent sid node = wait_to_send vagent (Add_node (sid, node))
let remove_node vagent sid nid = wait_to_send vagent (Remove_node (sid, nid))
let add_edge vagent sid from_id to_id label = wait_to_send vagent (Add_edge (sid, from_id, to_id, label))
let remove_edge vagent sid from_id to_id = wait_to_send vagent (Remove_edge (sid, from_id, to_id))
let change_node_state vagent sid nid state = wait_to_send vagent (Change_node_state (sid, nid, state))
let change_proof_state vagent sid pstate = wait_to_send vagent (Change_proof_state (sid, pstate))
let highlight_node vagent sid nid = wait_to_send vagent (Highlight_node (sid, nid))
let unhighlight_node vagent sid nid = wait_to_send vagent (Unhighlight_node (sid, nid))
let clear_color vagent sid = wait_to_send vagent (Clear_color sid)
let feedback_ok vagent sid = wait_to_send vagent (Feedback_ok sid)
let feedback_fail vagent sid error_msg = wait_to_send vagent (Feedback_fail (sid, error_msg))


let sending vagent =
    let cout = vagent.output in
    while vagent.is_alive do
        if Queue.is_empty vagent.sending_queue then begin
            Mutex.lock vagent.sending_mutex;
            Condition.wait vagent.sending_conditional vagent.sending_mutex;
            Mutex.unlock vagent.sending_mutex
        end else begin
            let msg = ref (Feedback_ok "") in
            Mutex.lock vagent.sending_mutex;
            msg := Queue.pop vagent.sending_queue;
            Mutex.unlock vagent.sending_mutex;
            let json_msg = json_of_msg !msg in
            output_string cout ((Yojson.Basic.to_string json_msg)^"\n");
            flush cout;

            Log.log_vmdv true ("Sent: "^(Yojson.Basic.to_string json_msg)^"\n")
        end
    done

let parse vagent msg = 
    match msg with
    | Highlight_node (sid, nid) -> 
        printf "Highlight node %s in session %s\n" nid sid;
        flush stdout;
        feedback_ok vagent sid
    | Unhighlight_node (sid, nid) ->
        printf "Unhighlight node %s in session %s\n" nid sid;
        flush stdout;
        feedback_ok vagent sid
    | Feedback_ok sid ->
        printf "Feedback OK received from %s\n" sid;
        flush stdout
    | Feedback_fail (sid, error_msg) ->
        printf "Feedback Fail received from %s: %s\n" sid error_msg;
        flush stdout
    | _ -> 
        printf "Not supposed to recieve this message\n";
        flush stdout

let receiving vagent =
    let cin = vagent.input in 
    while vagent.is_alive do
        let buffer = Bytes.create !Flags.json_bufsize in
        let len = input cin buffer 0 !Flags.json_bufsize in
        let raw_str = Bytes.sub_string buffer 0 len in
        let json_msg = Yojson.Basic.from_string raw_str in
        Log.log_vmdv false ("Received: "^(Yojson.Basic.to_string json_msg)^"\n");
        let msg = msg_of_json json_msg in
        parse vagent msg
    done

let start_send_receive vagent =
    ignore (Thread.create (fun vagent -> receiving vagent) vagent);
    ignore (Thread.create (fun vagent -> sending vagent) vagent)

let get_vmdv_agent ip_addr = 
    let i,o = Unix.open_connection (Unix.ADDR_INET (Unix.inet_addr_of_string ip_addr, 3333)) in
    let vagent: visualize_agent = {
        input = i;
        output = o;
        is_alive = true;
        sending_queue = Queue.create ();
        sending_mutex = Mutex.create ();
        sending_conditional = Condition.create ()
    } in
    start_send_receive vagent;
    vagent

let init_vmdv_agent ip_addr = 
    try
        let vagent = get_vmdv_agent ip_addr in
        vagent := Some vagent
    with _ -> print_endline ("connect to vmdv in "^s^" failed.")

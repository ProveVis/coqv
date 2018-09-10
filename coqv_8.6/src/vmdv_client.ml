
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
let set_proof_rule vagent sid nid rule = wait_to_send vagent (Set_proof_rule (sid, nid, rule))
let remove_subproof vagent sid nid = wait_to_send vagent (Remove_subproof (sid, nid))
let feedback_ok vagent sid = wait_to_send vagent (Feedback_ok sid)
let feedback_fail vagent sid error_msg = wait_to_send vagent (Feedback_fail (sid, error_msg))


let sending vagent =
    let cout = vagent.output in
    begin try while vagent.is_alive do
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

            log_if_possible ("Sent: "^(Yojson.Basic.to_string json_msg)^"\n")
        end
    done with _ -> ()
    end;
    print_endline "vmdv message sending thread exit"

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
    | Remove_subproof (sid, nid) ->
        let node = Proof_model.get_node nid in
        let new_stateid = node.stateid in
        if new_stateid <> -1 then
            Interaction.request_edit_at new_stateid
    | Feedback_ok sid ->
        printf "Feedback OK received from %s\n" sid;
        flush stdout
    | Feedback_fail (sid, error_msg) ->
        printf "Feedback Fail received from %s: %s\n" sid error_msg;
        flush stdout
    | Clear_color sid -> ()
    | _ -> 
        printf "Not supposed to recieve this message: \n%s\n" (Yojson.Basic.to_string (json_of_msg msg));
        flush stdout

let receiving parameter =
    let vagent, parse_func = parameter in
    let cin = vagent.input in 
    begin try while vagent.is_alive do
        let buffer = Bytes.create !Flags.json_bufsize in
        let len = input cin buffer 0 !Flags.json_bufsize in
        let raw_str = Bytes.sub_string buffer 0 len in
        let json_msg = Yojson.Basic.from_string raw_str in
        log_if_possible ("Received: "^(Yojson.Basic.to_string json_msg)^"\n");
        let msg = msg_of_json json_msg in
        parse vagent msg
    done with _ -> ()
    end;
    print_endline "vmdv message receiving thread exit"

(* let start_send_receive vagent =
    ignore (Thread.create (fun vagent -> receiving vagent) vagent);
    ignore (Thread.create (fun vagent -> sending vagent) vagent) *)

let get_visualize_agent ip_addr parse_func = 
    let i,o = Unix.open_connection (Unix.ADDR_INET (Unix.inet_addr_of_string ip_addr, 3333)) in
    let vagent: visualize_agent = {
        input = i;
        output = o;
        is_alive = true;
        sending_queue = Queue.create ();
        sending_mutex = Mutex.create ();
        sending_conditional = Condition.create ();
        sending_thread = Thread.self ();
        receiving_thread = Thread.self ()
    } in
    let st = (Thread.create (fun para -> receiving para) (vagent, parse_func))
    and rt = (Thread.create (fun vagent -> sending vagent) vagent) in
    vagent.sending_thread <- st;
    vagent.receiving_thread <- rt;
    (* start_send_receive vagent; *)
    vagent

let close_current_visualize_agent () =
    match !vagent with
    | None -> ()
    | Some vagt ->
        vagt.is_alive <- false;
        (* Thread.kill vagt.sending_thread;
        Thread.kill vagt.receiving_thread; *)
        Unix.shutdown_connection vagt.input;
        close_out vagt.output;
        vagent := None
type task = 
  | Dummy
  (* | CoqtopFeedback of Feedback.feedback *)
  | CoqtopGoals of (Interface.goal Interface.pre_goals) Option
  | AddTactic of string
  | CoqvCommand of string
  | VMDVResponse of Vmdv_protocol.message

let task_queue_condition = Condition.create ()
let task_queue_mutex = Mutex.create ()

let pending_task_queue : (task Queue.t) = Queue.create ()

let exists_pending_task () = not (Queue.is_empty ())

let add_pending_task t = 
  Mutex.lock task_queue_mutex;
  Queue.push t pending_task_queue;
  Mutex.unlock task_queue_mutex;
  Condition.signal task_queue_condition

let fetch_pending_task () = 
  let t = ref Dummy in
  Mutex.lock task_queue_mutex;
  Condition.wait task_queue_condition task_queue_mutex;
  if not (Queue.is_empty ()) then
    t := Queue.pop ();
  Mutex.unlock task_queue_mutex;
  !t

let rec task_loop ()  = 
  while !Runtime.running do
    let t = fetch_pending_task () in
    process_task t 
  done
and process_task t = 
  match t with
  | Dummy -> ()
  | CoqtopGoals None -> 
    begin
      match !Coqv_utils.current_cmd_type with
      | ProofHandling ["Qed"] -> 
          Callbacks.on_change_proof_state Defined;
          current_session_id := ""
          (* History.record_step !Doc_model.current_stateid Dummy *)
      | ProofHandling ["Admitted"] ->
          (* let chosen_node = select_chosen_node () in
          begin match chosen_node with
          | None -> ()
          | Some cnode -> Callbacks.on_change_node_state cnode.id Admitted
          end; *)
          print_endline "current proof tree is admitted";
          Callbacks.on_change_proof_state Declared;
          current_session_id := ""
      | cmd -> print_endline ("Don't know what to do when receiving \"Good None\" in respose_goals with cmd type: "^(str_cmd_type cmd))
    end;
    Doc_model.commit ()
  | CoqtopGoals (Some goals) ->
    print_endline ("have received some goals after "^(str_cmd_type !Coqv_utils.current_cmd_type));
    on_receive_goals !Coqv_utils.current_cmd_type goals;
    Doc_model.commit ()
type task = 
    CoqtopResponse of string 
  | VMDVResponse of Vmdv_protocol.message

let task_queue_condition = Condition.create ()
let task_queue_mutex = Mutex.create ()

let pending_task_queue 
let coqtop_thread = ref None
let vmdv_thread = ref None

let create_coqtop_thread work cin = 
  try
    coqtop_thread := Some (Thread.create work cin);
    true
  with 
    _ -> false

let create_vmdv_thread
open Types
open Printf
open Interface
open Communicate

let coqtop_info : coq_info ref = ref {
    coqtop_version = "";
    protocol_version = "";
    release_date = "";
    compile_date = "";
}

type channels = {
    mutable cin: in_channel;
    mutable cout: out_channel;
}

let coq_channels : channels = {
    cin = stdin;
    cout = stdout;
}

let prompt = ref "Coq < "
let ignored_re () = Str.regexp !prompt

let new_stateid = ref 0
let running = ref true

let current_cmd_type = ref Other

let vagent:(visualize_agent option) ref = ref None

(* let current_coqtop_worker = ref "master" *)
type debug_info = {
    coqtop_log: out_channel;
    vmdv_log: out_channel;
}

let logs: (debug_info option) ref = ref None

let log_coqtop b str = 
    match !logs with
    | None -> ()
    | Some lgs -> 
        if b then
            output_string lgs.coqtop_log ("COQV --> COQTOP\n"^str^"\n")
        else
            output_string lgs.coqtop_log ("COQTOP --> COQV\n"^str^"\n");
        flush lgs.coqtop_log

let log_vmdv b str = 
    match !logs with
    | None -> ()
    | Some lgs ->
        if b then
            output_string lgs.vmdv_log ("COQV --> VMDV\n"^str^"\n")
        else
            output_string lgs.vmdv_log ("VMDV --> COQV\n"^str^"\n");
        flush lgs.vmdv_log






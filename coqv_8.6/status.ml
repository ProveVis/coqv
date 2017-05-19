open Printf
open Runtime

let str_status () = 
    let str_coqinfo = sprintf "coq version: %s" (!coqtop_info).coqtop_version in
    let str_stateid = sprintf "current stateid: %d" !new_stateid in
    let str_history = sprintf "history recorded: %d records" (List.length !History.history) in
    let str_session = sprintf "current session: %s" begin
            match !Proof_model.current_session_id with
            | None -> "None"
            | Some sname -> sname
        end in
    sprintf "\n\t%s\n\t%s\n\t%s\n\t%s" 
        str_coqinfo 
        str_stateid 
        str_history
        str_session


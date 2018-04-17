open Printf

type node_status =
    | Added
    | Committed

let str_node_status ns = 
    match ns with
    | Added -> "Added"
    | Committed -> "Committed"

type doc_built = {
    stateid: Stateid.t;
    mutable status: node_status;
    mutable last: (string * doc_built) option;
}

let rec str_doc_built docc = 
    let tmp_str = ref "" in
    tmp_str := !tmp_str^"stateid: "^(Stateid.to_string docc.stateid)^"; ";
    tmp_str := !tmp_str^"status: "^(str_node_status docc.status)^"; ";
    begin
        match docc.last with
        | None -> ()
        | Some (command, last_docc) -> tmp_str := (str_doc_built last_docc)^"\n\t--"^command^"-->\n"^(!tmp_str)
    end;
    !tmp_str


let current_doc : doc_built option ref = ref None

let init_doc () = 
    match !current_doc with
    | None -> current_doc := Some {stateid = Stateid.initial; status = Committed; last = None;}
    | Some docc -> 
        printf "Error: current doc is not empty, and cannot be initialized\n%s\n" (str_doc_built docc);
        flush stdout;
        exit 1

let doc_length () =
    let rec rec_doc_length docc = 
        match docc.last with
        | None -> 1
        | Some (cmd, docc') -> 1 + rec_doc_length docc' in
    match !current_doc with
    | None -> 0
    | Some docc -> rec_doc_length docc


let add stateid cmd = 
    match !current_doc with
    | None -> 
        print_endline "Error: the current doc cannot be empty";
        exit 1
    | Some docc ->
        current_doc := Some {
            stateid = stateid;
            status = Added;
            last = Some (cmd, docc);
        }

let commit () = 
    let rec commit_rec docc = 
        if docc.status = Added then begin
            docc.status <- Committed;
            match docc.last with
            | None -> ()
            | Some (_, docc') -> commit_rec docc'
        end in
    match !current_doc with
    | None -> ()
    | Some docc -> commit_rec docc

let discard_uncommitted () = 
    let rec rec_latest_committed docc = 
        if docc.status = Committed then
            Some docc
        else begin
            match docc.last with
            | None -> None
            | Some (cmd, docc') -> rec_latest_committed docc'
        end in
    match !current_doc with
    | None -> ()
    | Some docc -> current_doc := rec_latest_committed docc

let latest_committed_stateid () =
    let rec rec_latest_committed docc = 
        if docc.status = Committed then
            docc.stateid
        else begin
            match (docc.last) with
            | None -> Stateid.initial
            | Some (cmd, docc') -> rec_latest_committed docc'
        end in
    match !current_doc with
    | None -> Stateid.initial
    | Some docc -> rec_latest_committed docc

let get_committed_commands () = 
    let rec rec_committed_commands docc = 
        match docc.last with
        | None -> []
        | Some (cmd, docc') -> (rec_committed_commands docc') @ [cmd] in
    match !current_doc with
    | None -> []
    | Some docc -> rec_committed_commands docc

let uncommitted_command () = 
    match !current_doc with
    | None -> print_endline "Error: current doc should not be empty"; exit 1
    | Some docc -> 
        if docc.status = Committed then begin
            print_endline "Error: no command being processed"; exit 1
        end else begin
            match docc.last with
            | None -> 
                print_endline "Error: no command being processed";
                exit 1
            | Some (cmd, _) -> cmd
        end

let current_stateid = ref Stateid.dummy

let processing = ref 0
let processed = ref 0

let reset_process_stateid () = 
    processing := latest_committed_stateid ();
    processed := !processing

let processing_stateid sid = 
    if sid > !processing then
        processing := sid
let processed_stateid sid = 
    if sid > !processed then
        processed := sid
let is_processed () = !processed >= !processing

let goal_responsed = ref true

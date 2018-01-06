open Printf
open Coqv_utils

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
        | Some (command, last_docc) -> tmp_str := !tmp_str^"\n\t--"^command^"-->\n"^(str_doc_built last_docc)
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

(* ****Need to be modified from here!**** *)

(* type cmd_commited = Stateid.t * string
type doc_model = cmd_commited list

let doc : doc_model ref = ref []
let cache : (cmd_commited option) ref = ref None
 *)
(* let current_stateid = ref 0 *)

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

(* let add_to_doc cc = doc := cc :: !doc

let doc_length () = List.length !doc  *)

(* let nth_last_stateid n =
    if n=0 then ();
    let rec find_nth_last n lst = 
        if n = 1 then
            let (stateid, cmd) = List.hd lst in
            stateid
        else 
            find_nth_last (n-1) (List.tl lst) in
    find_nth_last n !doc

let try_add cmd = 
    cache := Some (!current_stateid, cmd)

let finish_add new_stateid =
    current_stateid := new_stateid; 
    match !cache with
    | None -> ()
    | Some d -> add_to_doc d *)


(* let move_focus_to stateid =
    let tmp_doc = ref !doc in
    let flag = ref true in
    while (List.length !tmp_doc <> 0 && !flag) do
        let head = List.hd !tmp_doc in
        if fst head = stateid then begin
            doc := List.tl !tmp_doc;
            current_stateid := stateid;
            flag := false    
        end else begin
            tmp_doc := List.tl !tmp_doc
        end
    done;
    if List.length !tmp_doc = 0 then begin
        printf "could not focus to command with state id %d in the current doc model.\n" (Stateid.to_int stateid)
    end

let get_cmd stateid = 
    let rec find_from_doc doc = 
        match doc with
        | [] -> "None"
        | (sid, cmd) :: doc' -> 
            if sid = stateid then
                cmd 
            else 
                find_from_doc doc' in
    find_from_doc !doc *)


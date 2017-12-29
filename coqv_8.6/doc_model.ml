open Printf

type cmd_commited = Stateid.t * string
type doc_model = cmd_commited list

let doc : doc_model ref = ref []
let cache : (cmd_commited option) ref = ref None

let current_stateid = ref 0

let processing_stateid = ref 0
let processed_stateid = ref 0

let coqtop_processing sid = 
    if sid > !processing_stateid then
        processing_stateid := sid
let coqtop_processed sid = 
    if sid > !processed_stateid then
        processed_stateid := sid
let coqtop_is_processed () = !processed_stateid >= !processing_stateid

let goal_responsed = ref true

let add_to_doc cc = doc := cc :: !doc

let doc_length () = List.length !doc 

let nth_last_stateid n =
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
    | Some d -> add_to_doc d


let move_focus_to stateid =
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
    find_from_doc !doc


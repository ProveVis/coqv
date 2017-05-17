open Printf

type cmd_commited = Stateid.t * string
type doc_model = cmd_commited list

let doc : doc_model ref = ref []
let cache : (cmd_commited option) ref = ref None

let add_to_doc cc = doc := cc :: !doc

let move_focus_to stateid =
    let tmp_doc = ref !doc in
    let flag = ref true in
    while (List.length !tmp_doc <> 0 && !flag) do
        let head = List.hd !tmp_doc in
        if fst head = stateid then begin
            doc := !tmp_doc;
            flag := false    
        end else begin
            tmp_doc := List.tl !tmp_doc
        end
    done;
    if List.length !tmp_doc = 0 then begin
        printf "could not focus to command with state id %d in the current doc model.\n" (Stateid.to_int stateid)
    end

let clear_cache () = cache := None

let raise_cache () = 
    match !cache with
    | Some cc -> add_to_doc cc; clear_cache ()
    | None -> ()


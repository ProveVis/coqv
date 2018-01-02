open Types
open Runtime


type step = 
      Change_state of string * node_state
    | Add_node of string
    (* | Change_label of string *)
    | Dummy

type history = (Stateid.t * (step list)) list

let history : history ref = ref [] 

let record_step stateid step =
    match !history with
    | [] -> history := [(stateid, [step])]
    | (sid, steps) :: _ ->
        if sid = stateid then
            history := (sid, step :: steps) :: (List.tl !history)
        else 
            history := (stateid, [step]) :: !history

let undo_upto stateid =
    let flag = ref true in
    let tmp_history = ref !history in
    while !tmp_history <> [] && !flag do
        let (sid, steps) = List.hd !tmp_history in
        if sid = stateid then begin
            flag := false;
            history := !tmp_history
        end else begin
            let undo_step step =
                match step with
                | Change_state (nid, from_state) -> Proof_model.change_node_state nid from_state
                | Add_node nid -> Proof_model.remove_node nid 
                | Dummy -> () in
            List.iter (fun s -> undo_step s) steps;
            tmp_history := List.tl !tmp_history
        end
    done

let str_history () = 
    match !history with
    | [] -> "None"
    | h :: hs ->
        let str_buf = ref ((string_of_int (fst h))^", "^(string_of_int (List.length (snd h)))^", "^(Doc_model.get_cmd (fst h))^"\n") in
        List.iter (fun (sid, steps) -> str_buf := !str_buf^"<--"^(string_of_int sid)^", "^(string_of_int (List.length steps))^", "^(Doc_model.get_cmd sid)^"\n") hs;
        !str_buf
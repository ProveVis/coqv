open Printf
open Types
open Yojson

type message = 
    | Create_session of string * string * string * (node_state list)
    | Remove_session of string
    | Add_node of string * node
    | Remove_node of string * string
    | Add_edge of string * string * string * string
    | Remove_edge of string * string * string
    | Change_node_state of string * string * node_state
    | Highlight_node of string * string
    | Unhighlight_node of string * string
    | Clear_color of string
    | Feedback_ok of string
    | Feedback_fail of string * string



let json_of_msg (msg:message) = 
    match msg with
    | Create_session (session_id, session_descr, graph_type, node_state_list) ->
        `Assoc [
            ("type", `String "create_session");
            ("session_id", `String session_id);
            ("session_descr", `String session_descr);
            ("graph_type", `String graph_type);
            ("node_states", `List (List.map (fun a -> `String (str_node_state a)) node_state_list))
        ]
    | Remove_session sid ->
        `Assoc [
            ("type", `String "remove_session");
            ("session_id", `String sid)
        ]
    | Add_node (sid, node) ->
        `Assoc [
            ("type", `String "add_node");
            ("session_id", `String sid);
            ("node", `Assoc [
                ("id", `String node.id);
                ("label", `String (str_label node.label));
                ("state", `String (str_node_state node.state))
            ])
        ]
    | Remove_node (sid, nid) ->
        `Assoc [
            ("type", `String "remove_node");
            ("session_id", `String sid);
            ("node_id", `String nid)
        ]
    | Add_edge (sid, from_id, to_id, label) ->
        `Assoc [
            ("type", `String "add_edge");
            ("session_id", `String sid);
            ("from_id", `String from_id);
            ("to_id", `String to_id);
            ("label", `String label)
        ]
    | Remove_edge (sid, from_id, to_id) ->
        `Assoc [
            ("type", `String "remove_edge");
            ("session_id", `String sid);
            ("from_id", `String from_id);
            ("to_id", `String to_id)
        ]
    | Change_node_state (sid, nid, new_state) ->
        `Assoc [
            ("type", `String "change_node_state");
            ("session_id", `String sid);
            ("node_id", `String nid);
            ("new_state", `String (str_node_state new_state))
        ]
    | Highlight_node (sid, nid) ->
        `Assoc [
            ("type", `String "highlight_node");
            ("session_id", `String sid);
            ("node_id", `String nid)
        ]
    | Unhighlight_node (sid, nid) ->
        `Assoc [
            ("type", `String "unhighlight_node");
            ("session_id", `String sid);
            ("node_id", `String nid)
        ]
    | Clear_color sid ->
        `Assoc [
            ("type", `String "clear_color");
            ("session_id", `String sid)
        ]
    | Feedback_ok sid ->
        `Assoc [
            ("type", `String "feedback");
            ("session_id", `String sid);
            ("status", `String "OK")
        ]
    | Feedback_fail (sid, error_msg) ->
        `Assoc [
            ("type", `String "feedback");
            ("session_id", `String sid);
            ("status", `String "Fail");
            ("error_msg", `String error_msg)
        ]



(* let rec get_json_of_key key str_json_list = 
    match str_json_list with
    | (str, json) :: str_json_list' -> 
        if str = key then
            json
        else 
            get_json_of_key key str_json_list'
    | [] -> printf "not find json for key %s\n" key; exit 1  *)
(* 
let get_json_of_key key json = Yojson.Basic.Util.member key json 


let get_string_of_json json = 
    match json with
    | `String str -> str
    | _ -> printf "%s is not a string\n" (Yojson.Basic.to_string json); exit 1 *)

let str_value_of_json key json = 
    Yojson.Basic.Util.to_string (Yojson.Basic.Util.member key json)


let msg_of_json json = 
    try
        match Yojson.Basic.Util.member "type" json with
        | `Null -> printf "%s has no key: type" (Yojson.Basic.to_string json); exit 1
        | json_value -> begin
                match json_value with
                | `String str -> begin
                    match str with
                        | "highlight_node" -> 
                            Highlight_node (str_value_of_json "session_id" json, str_value_of_json "node_id" json)
                            (* Highlight_node ((get_string_of_json (get_json_of_key "session_id" str_json_list)), (get_string_of_json (get_json_of_key "node_id" str_json_list))) *)
                        | "unhighlight_node" -> 
                            Unhighlight_node (str_value_of_json "session_id" json, str_value_of_json "node_id" json)
                            (* Unhighlight_node ((get_string_of_json (get_json_of_key "session_id" str_json_list)), (get_string_of_json (get_json_of_key "node_id" str_json_list))) *)
                        | "clear_color" ->
                            Clear_color (str_value_of_json "session_id" json)
                            (* Clear_color (get_string_of_json (get_json_of_key "session_id" str_json_list)) *)
                        | "feedback" -> 
                            let status = str_value_of_json "status" json in
                            if status = "OK" then
                                Feedback_ok (str_value_of_json "session_id" json)
                            else 
                                Feedback_fail (str_value_of_json "session_id" json, str_value_of_json "error_msg" json)
(*                             let status = get_string_of_json (get_json_of_key "status" str_json_list) in
                            if status = "OK" then
                                Feedback_ok (get_string_of_json (get_json_of_key "session_id" str_json_list))
                            else
                                Feedback_fail ((get_string_of_json (get_json_of_key "session_id" str_json_list)), (get_string_of_json (get_json_of_key "error_msg" str_json_list)))
 *)                        
                        | _ as s -> printf "not supposed to be received by coqv: %s\n" s; exit 1
                    end
                | _ -> printf "%s cannot be interpreted at the moment" (Yojson.Basic.to_string json_value); exit 1
            end
    with
    | _ -> failwith "not a valid json value"
    
(* 
    match json with
    | `Assoc str_json_list -> begin
            match get_string_of_json (get_json_of_key "type" str_json_list) with
            | "highlight_node" -> 
                Highlight_node ((get_string_of_json (get_json_of_key "session_id" str_json_list)), (get_string_of_json (get_json_of_key "node_id" str_json_list)))
            | "unhighlight_node" -> 
                Unhighlight_node ((get_string_of_json (get_json_of_key "session_id" str_json_list)), (get_string_of_json (get_json_of_key "node_id" str_json_list)))
            | "clear_color" ->
                Clear_color (get_string_of_json (get_json_of_key "session_id" str_json_list))
            | "feedback" -> 
                let status = get_string_of_json (get_json_of_key "status" str_json_list) in
                if status = "OK" then
                    Feedback_ok (get_string_of_json (get_json_of_key "session_id" str_json_list))
                else
                    Feedback_fail ((get_string_of_json (get_json_of_key "session_id" str_json_list)), (get_string_of_json (get_json_of_key "error_msg" str_json_list)))
            | _ as s -> printf "not supposed to be received by coqv: %s\n" s; exit 1
        end
    | _ -> printf "%s is not a message\n" (Yojson.Basic.to_string json); exit 1
 *)
open Printf
open Types
open Yojson




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
                ("state", `String (str_node_state node.state));
                ("state_id", `String (string_of_int node.stateid))
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
    | Change_proof_state (sid, pstate) ->
        `Assoc [
            ("type", `String "change_proof_state");
            ("session_id", `String sid);
            ("new_state", `String (str_proof_state pstate))
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
    | Set_proof_rule (sid, nid, rule) ->
        `Assoc [
            ("type", `String "set_proof_rule");
            ("session_id", `String sid);
            ("node_id", `String nid);
            ("rule", `String rule)
        ]
    | Remove_subproof (sid, nid) ->
        `Assoc [
            ("type", `String "remove_subproof");
            ("session_id", `String sid);
            ("node_id", `String nid)
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
                        | "remove_subproof" ->
                            Remove_subproof (str_value_of_json "session_id" json, str_value_of_json "node_id" json)
                        | "feedback" -> 
                            let status = str_value_of_json "status" json in
                            if status = "OK" then
                                Feedback_ok (str_value_of_json "session_id" json)
                            else 
                                Feedback_fail (str_value_of_json "session_id" json, str_value_of_json "error_msg" json)                       
                        | _ as s -> printf "not supposed to be received by coqv: %s\n" s; exit 1
                    end
                | _ -> printf "%s cannot be interpreted at the moment" (Yojson.Basic.to_string json_value); exit 1
            end
    with
    | _ -> failwith "not a valid json value"
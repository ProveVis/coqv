
type proof_kind = Lemma | Proposition | Theorem | Remark | Corollary | Fact | Goal
let str_proof_kind pk = 
    match pk with
    | Lemma -> "Lemma"
    | Proposition -> "Proposition"
    | Theorem -> "Theorem"
    | Remark -> "Remark"
    | Corollary -> "Corollary"
    | Fact -> "Fact"
    | Goal -> "Goal"

type proof_state = Proving | Defined | Declared | Aborted
let str_proof_state ps = 
    match ps with
    | Proving -> "Proving"      (*proposition is under proof*)
    | Defined -> "Defined"      (*proof was closed by "Qed."*)
    | Declared -> "Declared"    (*proof was closed by "Admitted."*)
    | Aborted -> "Aborted"      (*proof was closed by "Abort"*)

type node_state = Not_proved | Proved | Admitted | To_be_chosen | Chosen
let str_node_state ns =
    match ns with
    | Not_proved -> "Not_proved"
    | Proved -> "Proved"
    | Admitted -> "Admitted"
    | To_be_chosen -> "To_be_chosen"
    | Chosen -> "Chosen"

let node_state_list = [Not_proved; Proved; Admitted; To_be_chosen; Chosen]

type label = {
        id: string;
        hypos: (string * string) list;
        conclusion: string; 
    }

let str_label label = 
    let str_buf = ref "" in
    List.iter (fun (hn, hc) -> str_buf := !str_buf ^ hn ^ ":" ^ hc^"\n") label.hypos;
    str_buf := !str_buf ^ "==========================\n"^label.conclusion;
    !str_buf

type node = {
    id: string;
    mutable label: label;
    mutable state: node_state;
    mutable parent: node;
    mutable stateid: int; (*stateid when this node is chosen in a proof*)
}


type tactic = string list

type step = tactic * node * (node list)

type children = tactic * (string list)

let str_node n = 
    "Node "^n.id^"{\n\tstate: "^(str_node_state n.state)^";\n\tlabel: "^(str_label n.label)^";\n\tparent: "^(n.parent.id)^";\n\tstateid: "^(string_of_int (n.stateid))^";\n}"

type proof_tree = {
    mutable root : node;
    nodes: (string, node) Hashtbl.t; 
    edges: (string, children) Hashtbl.t
}

type session = {
    name: string;
    kind: proof_kind;
    mutable state: proof_state;
    proof_tree: proof_tree;
}

type session_tbl = (string, session) Hashtbl.t

type modul = {
    name: string;
    sessions: session_tbl;
    modul_tbl: (string, modul) Hashtbl.t;
}
(********************************************************************)

type message = 
    | Create_session of string * string * string * (node_state list)
    | Remove_session of string
    | Add_node of string * string (*prefix*) * node
    (* | Add_node_cut of string * node * string *)
    | Remove_node of string * string
    | Add_edge of string * string * string * string
    (* | Add_edge_cut of string * string * string * string * string *)
    | Remove_edge of string * string * string
    | Change_node_state of string * string * node_state
    | Change_proof_state of string * proof_state
    | Highlight_node of string * string
    | Unhighlight_node of string * string
    | Clear_color of string
    | Set_proof_rule of string * string * string
    (* | Set_proof_rule_cut of string * string * string * string *)
    | Remove_subproof of string * string (*Remove_subproof (sid, nid)*) 
    | Expand_cut of string * string * string
    | Feedback_ok of string
    | Feedback_fail of string * string

type visualize_agent =
    {
        mutable input: in_channel;
        mutable output: out_channel;
        mutable is_alive: bool;
        sending_queue: message Queue.t;
        sending_mutex: Mutex.t;
        sending_conditional: Condition.t;
        mutable sending_thread: Thread.t;
        mutable receiving_thread: Thread.t;
    }
    
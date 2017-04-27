
type kind = Lemma | Proposition | Theorem | Axiom

type proof_state = Start | Processing | Complete | Aborted | Assumed

type node_state = Not_proved | Proved | Assumed | To_be_choosed | Chosen

type node = {
    id: string;
    label: string;
    state: node_state;
    visible: bool;
}

let str_node_state ns = 
    match ns with
    | Not_proved -> "Not_proved"
    | Proved -> "Proved"
    | Assumed -> "Assumed"
    | To_be_choosed -> "To_be_choosed"
    | Chosen -> "Chosen"

let str_node node = 
    "Node "^node.id^"("^(str_node_state node.state)^"): "^node.label;

type proof_tree = {
    nodes: (string, node) Hashtbl.t; 
    edges: (string, string list) Hashtbl.t
}

type session = {
    name: string;
    kind: kind;
    mutable state: proof_state;
    proof_tree: proof_tree;
}

type session_tbl = (string, session) Hashtbl.t
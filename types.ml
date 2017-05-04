
type proof_kind = Lemma | Proposition | Theorem | Axiom
let str_proof_kind pk = 
    match pk with
    | Lemma -> "Lemma"
    | Proposition -> "Proposition"
    | Theorem -> "Theorem"
    | Axiom -> "Axiom"

type proof_state = Start | Processing | Complete | Aborted | Assumed
let str_proof_state ps = 
    match ps with
    | Start -> "Start"
    | Processing -> "Processing"
    | Complete -> "Complete"
    | Aborted -> "Aborted"
    | Assumed -> "Assumed"

type node_state = Not_proved | Proved | Assumed | To_be_chosen | Chosen
let str_node_state ns =
    match ns with
    | Not_proved -> "Not_proved"
    | Proved -> "Proved"
    | Assumed -> "Assumed"
    | To_be_chosen -> "To_be_chosen"
    | Chosen -> "Chosen"

type node = {
    id: string;
    label: string;
    mutable state: node_state;
    parent: node;
}

type tatic = string

type step = tatic * node * (node list)

type children = tatic * (node list)

let str_node n = 
    "Node "^n.id^"("^(str_node_state n.state)^"): "^n.label

type proof_tree = {
    nodes: (string, node) Hashtbl.t; 
    edges: (node, children) Hashtbl.t
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

type coq_input = 
    | Start_session of proof_kind * string * string
    | Proof 
    | Qed
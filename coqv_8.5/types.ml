
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
    label: label;
    mutable state: node_state;
    mutable parent: node;
}

type tatic = string

type step = tatic * node * (node list)

type children = tatic * (string list)

let str_node n = 
    "Node "^n.id^"("^(str_node_state n.state)^"): \n"^(str_label n.label)^"\n"

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

type cmd_type = 
      Module of string
    | End of string
    | Proof of string * proof_kind
    | Qed
    | Other
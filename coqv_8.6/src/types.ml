
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

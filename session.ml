
type kind = Lemma | Proposition | Theorem | Axiom

type proof_state = Start | Processing | Complete | Aborted | Assumed

type node_state = Not_proved | Proved | Assumed | To_be_choosed | Chosen

type node = 
    {
        label: string;
        state: node_state;
    }

type proof_tree = 
    {
        nodes: (string, node) Hashtbl.t; 
        edges: (string, string) Hashtbl.t
    }

type session = 
    {
        name: string;
        kind: kind;
        mutable state: proof_state;
        proof_tree: proof_tree;
    }
open Session
open Printf
(*
module Key = struct
    type t = node
    let compare n1 n2 = Pervasives.compare (n1.id) (n2.id)
end

module Node_set = Set.Make(Key)*)

let select_node node_list = 
    printf "%d nodes selected:\n----------------\n" List.length node_list;
    List.iter 
        (fun n -> 
            printf "Node %s(%s): %s\n" n.id (str_node_state n.state) n.label;
            printf ""
        ) node_list 


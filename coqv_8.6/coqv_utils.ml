open Printf
open Types

let rec richpp_to_string richpp = 
    Richpp.raw_print richpp

let print_xml chan xml =
  let rec print = function
  | Xml_datatype.PCData s -> output_string chan s
  | Xml_datatype.Element (_, _, children) -> List.iter print children
  in
  print xml

let goal_to_label goal = 
    let raw_hyp_list = List.map (fun h -> 
        (*printf "goal: \n%s\n" (Xml_printer.to_string_fmt h);*)
        richpp_to_string h) goal.goal_hyp in
        (*Serialize.to_list Serialize.to_string goal.goal_hyp in*)
        (*List.map (fun h -> Serialize.to_list Serialize.to_string h) goal.goal_hyp in*)
    let hyp_list = List.map (fun h ->
            let ch = caught_str h in
            let split_pos = String.index ch ':' in
            let hn, hc = String.sub ch 0 (split_pos), String.sub ch (split_pos+1) (String.length ch - split_pos-1) in
            String.trim hn, String.trim hc 
        ) raw_hyp_list in
    let conc = String.trim (caught_str (richpp_to_string goal.goal_ccl)) in
    {
        id = goal.goal_id;
        hypos = hyp_list;
        conclusion = conc;
    }
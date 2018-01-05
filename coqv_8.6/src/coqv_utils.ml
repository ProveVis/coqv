open Printf
open Interface
open Types


let escaped_str str = 
    let buffer = Bytes.create 1024 in
    let length = ref 0 in
    let add_str_to_buffer str = 
        String.iter (fun c -> Bytes.fill buffer !length 1 c; incr length) str in
    let eacape_char c = 
        match c with
        | '&' -> Some "&amp;"
        | '<' -> Some "&lt;"
        | '>' -> Some "&gt;"
        | '"' -> Some "&quot;"
        | '\'' -> Some "&apos;"
        | ' ' -> Some "&nbsp;";
        | _ -> None in
    String.iter (fun c -> 
        match eacape_char c with
        | Some str -> add_str_to_buffer str
        | None -> Bytes.fill buffer !length 1 c; incr length) str;
    let escapted_str = Bytes.sub_string buffer 0 !length in 
    (*printf "raw str: %s, escaped str: %s\n" str escapted_str;
    flush stdout;*)
    escapted_str

let caught_str str = 
    str |>
    Str.global_replace (Str.regexp "&nbsp;") " " |>
    Str.global_replace (Str.regexp "&apos;") "'" |>
    Str.global_replace (Str.regexp "&quot;") "\"" |>
    Str.global_replace (Str.regexp "&amp;") "&" |>
    Str.global_replace (Str.regexp "&gt;") ">" |>
    Str.global_replace (Str.regexp "&lt;") "<" |>
    Str.global_replace (Str.regexp "  ") " " |>
    Str.global_replace (Str.regexp "( ") "(" |>
    Str.global_replace (Str.regexp " )") ")" |>
    Str.global_replace (Str.regexp " , ") ", " |>
    Str.global_replace (Str.regexp " : ") ": "

type cmd_type = 
    Module of string
    | End of string
    | Proof of string * proof_kind
    | Qed
    | Focus of int
    | Require
    | Edit_label
    | Other

let current_cmd_type = ref Other

let get_cmd_type cmd =
    let tcmd = String.trim cmd in
    let splited = Str.split (Str.regexp "[ \t<:\\.]+") tcmd in
    match List.map (fun s -> String.trim s) splited with
    | [] -> print_endline ("empty command: "^cmd); exit 1
    | "Module" :: tl_split -> 
        if List.hd (List.tl splited) <> "Type" then
            Module (List.hd tl_split)
        else Other
    | "End" :: tl_split -> End (List.hd tl_split)
    | "Theorem" :: tl_split -> Proof (List.hd tl_split, Theorem)
    | "Lemma" :: tl_split -> Proof (List.hd tl_split, Lemma)
    | "Proposition" :: tl_split -> Proof (List.hd tl_split, Proposition)
    | "Corollary" :: tl_split -> Proof (List.hd tl_split, Corollary)
    | "Remark" :: tl_split -> Proof (List.hd tl_split, Remark)
    | "Fact" :: tl_split -> Proof (List.hd tl_split, Fact)
    | "Goal" :: tl_split -> Proof ("Unnamed_thm", Goal)
    | "Qed" :: tl_split -> Qed
    | "Require" :: tl_split -> Require
    | "move" :: tl_split -> Edit_label
    | "rename" :: tl_split -> Edit_label
    | "set" :: tl_split -> Edit_label
    | "remember" :: tl_split -> Edit_label
    | "pose" :: tl_split -> Edit_label
    | "clear" :: tl_split -> Edit_label
    | "clearbody" :: tl_split -> Edit_label
    | _ -> Other

let str_cmd_type ct = 
    match ct with
    | Module mname -> "Module "^mname
    | End mname -> "End "^mname
    | Proof (thm_name, pk) -> "Proof "^(str_proof_kind pk)^" "^thm_name
    | Qed -> "Qed"
    | Focus _ -> "Focus"
    | Require -> "Require"
    | Edit_label -> "Edit_label"
    | Other -> "Other"

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
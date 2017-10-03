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
    Str.global_replace (Str.regexp "&lt;") "<"


let get_cmd_type cmd =
    let tcmd = String.trim cmd in
    let splited = Str.split (Str.regexp "[ \t<:\\.]+") tcmd in
    match splited with
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
    | _ -> Other

let str_cmd_type ct = 
    match ct with
    | Module mname -> "Module "^mname
    | End mname -> "End "^mname
    | Proof (thm_name, pk) -> "Proof "^(str_proof_kind pk)^" "^thm_name
    | Qed -> "Qed"
    | Other -> "Other"
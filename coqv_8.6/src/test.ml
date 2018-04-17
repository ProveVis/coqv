open Printf

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
    printf "raw str: %s, escaped str: %s\n" str escapted_str;
    flush stdout;
    escapted_str

let caught_str str = 
    let str1 = Str.global_replace (Str.regexp "&nbsp;") " " str in
    let str2 = Str.global_replace (Str.regexp "&apos;") "'" str1 in
    let str3 = Str.global_replace (Str.regexp "&quot;") "\"" str2 in
    let str4 = Str.global_replace (Str.regexp "&amp;") "&" str3 in
    let str5 = Str.global_replace (Str.regexp "&gt;") ">" str4 in
    let str6 = Str.global_replace (Str.regexp "&lt;") "<" str5 in
    str6
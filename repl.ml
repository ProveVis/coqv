open Printf
open Str

let in_chan = stdin
let out_chan = stdout
let running = ref true

let command_re = Str.regexp ":[-_A-Za-z0-9]+"

let rec loop() = 
    print_endline "entering a loop";
    while !running do
        print_string "my_coqv> ";
        let input_str = read_line () in
        if Str.string_match command_re input_str 0 then
            let cmd = Str.split (Str.regexp ":") input_str in
            if List.hd cmd = "exit" then 
                running := false
            else 
                printf "command found: %s\n" (List.hd cmd)
        else
            printf "regular string: %s\n" input_str;
        flush stdout
    done

let _ = loop ()
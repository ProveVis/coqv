open Printf
open Runtime
open Types
open Lexing
(*open Parser*)

let interpret_cmd cmd = 
    printf "Interpreting command: %s\n" cmd;
    flush stdout

let handle_input input_str cout = 
    output_string cout (input_str^"\n");
    flush cout


let handle_feedback feedback = 
    printf "%s" (Str.global_replace (ignored_re ()) "" feedback);
    flush stdout
open Printf
open Runtime
open Types
open Lexing
(*open Parser*)

(*type request_mode =
    | *)

let get_coq_info cout = 
    let about = Xmlprotocol.About () in
    let xml_about = Xmlprotocol.of_call about in
    Xml_printer.print (Xml_printer.TChannel cout) xml_about

let interpret_cmd cmd = 
    printf "Interpreting command: %s\n" cmd;
    flush stdout

let handle_input input_str cout = 
    output_string cout (input_str^"\n");
    flush cout


let handle_feedback feedback = 
    printf "got feedback message length: %d\n" (String.length feedback);
    printf "%s\n" (Str.global_replace (ignored_re ()) "" feedback);
    flush stdout
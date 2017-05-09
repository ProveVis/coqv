open Printf
open Runtime
open Types
open Lexing
open Interface
(*open Parser*)

type request_mode = 
      Request_add           | Request_edit_at       | Request_query     | Request_goals
    | Request_evars         | Request_hints         | Request_status    | Request_search 
    | Request_getoptions    | Request_setoptions    | Request_mkcases   | Request_quit
    | Request_about         | Request_init          | Request_interp    | Request_stopworker 
    | Request_printast      | Request_annotate

let request_mode = ref Request_about 
(*************************************************************************************)
(**sending xml characters to coqtop**)
let request_coq_info cout = 
    request_mode := Request_about;
    let about = Xmlprotocol.About () in
    let xml_about = Xmlprotocol.of_call about in
    Xml_printer.print (Xml_printer.TChannel cout) xml_about

let request_init filename cout = 
    request_mode := Request_init;
    let init = Xmlprotocol.init filename in
    let xml_init = Xmlprotocol.of_call init in
    Xml_printer.print (Xml_printer.TChannel cout) xml_init

let request_quit cout = 
    request_mode := Request_quit;
    let quit = Xmlprotocol.quit () in
    let xml_quit = Xmlprotocol.of_call quit in
    Xml_printer.print (Xml_printer.TChannel cout) xml_quit

(*let request_add cmd cout = 
    request_mode := Request_add;
    let add = Xmlprotocol.add cmd*)

let request_goals cout = 
    request_mode := Request_goals;
    let goals = Xmlprotocol.goals () in
    let xml_goals = Xmlprotocol.of_call goals in
    Xml_printer.print (Xml_printer.TChannel cout) xml_goals

let request_evars cout = 
    request_mode := Request_evars;
    let evars = Xmlprotocol.evars () in
    let xml_evars = Xmlprotocol.of_call evars in
    Xml_printer.print (Xml_printer.TChannel cout) xml_evars

let request_hints cout = 
    request_mode := Request_hints;
    let hints = Xmlprotocol.hints () in
    let xml_hints = Xmlprotocol.of_call hints in
    Xml_printer.print (Xml_printer.TChannel cout) xml_hints

(*************************************************************************************)
let interpret_cmd cmd = 
    printf "Interpreting command: %s\n" cmd;
    flush stdout

let handle_input input_str cout = 
    output_string stdout (input_str^"\n");
    flush stdout


let handle_feedback feedback = 
    let fb_str = Str.global_replace (ignored_re ()) "" feedback in
    printf "got feedback message length: %d\n" (String.length fb_str);
    printf "%s\n\n" fb_str;
    if not !Flags.xml then begin
        printf "%s\n" fb_str
    end else begin
        let xparser = Xml_parser.make (Xml_parser.SString fb_str) in
        let xml_fb = Xml_parser.parse xparser in
        let fb_val = Xmlprotocol.to_value (Xmlprotocol.to_coq_info) xml_fb in
        match fb_val with
        | Good fb -> begin
                match !request_mode with
                | Request_about -> 
                    Runtime.coqtop_info := fb;
                    printf "Coqtop Info: \n\tversion %s, \n\tprotocol version %s, \n\trelease date %s, \n\tand compile date %s\n" 
                        fb.coqtop_version fb.protocol_version fb.release_date fb.compile_date
                | _ -> printf "handling other kind of feedback\n"

            end
        | _ -> printf "parsing message fails";
        flush stdout
    end
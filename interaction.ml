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
(**
let response_coq_info fb_str = 
    let xparser = Xml_parser.make (Xml_parser.SString fb_str) in
    let xml_fb = Xml_parser.parse xparser in
    let fb_val = Xmlprotocol.to_value (Xmlprotocol.to_coq_info) xml_fb in
    (match fb_val with
    | Good fb -> 
            Runtime.coqtop_info := fb;
            printf "Coqtop Info: \n\tversion %s, \n\tprotocol version %s, \n\trelease date %s, \n\tand compile date %s\n" 
                fb.coqtop_version fb.protocol_version fb.release_date fb.compile_date
    | _ -> printf "parsing coq info message fails");
    flush stdout

let request_init filename cout = 
    request_mode := Request_init;
    let init = Xmlprotocol.init filename in
    let xml_init = Xmlprotocol.of_call init in
    Xml_printer.print (Xml_printer.TChannel cout) xml_init

let response_init str_msg = 
    let xparser = Xml_parser.make (Xml_parser.SString str_msg) in
    let xml_msg = Xml_parser.parse xparser in
    let msg = Xmlprotocol.to_value (Xmlprotocol.to_stateid) xml_msg in
    match msg with
    | Good stateid -> print_endline ("got new stateid: "^(string_of_int stateid)); Runtime.new_stateid := stateid
    | _ -> printf "unknown from response init"; flush stdout    

let request_quit cout = 
    request_mode := Request_quit;
    let quit = Xmlprotocol.quit () in
    let xml_quit = Xmlprotocol.of_call quit in
    Xml_printer.print (Xml_printer.TChannel cout) xml_quit

let response_quit str_msg = 
    let xparser = Xml_parser.make (Xml_parser.SString str_msg) in
    let xml_msg = Xml_parser.parse xparser in
    let msg = Xmlprotocol.to_value (Serialize.to_unit) xml_msg in
    match msg with
    | Good _ -> print_endline ("now quit! "); Runtime.running := false
    | _ -> printf "unknown from response quit"; flush stdout  

let request_add cmd editid stateid verbose cout = 
    request_mode := Request_add;
    let add = Xmlprotocol.add ((cmd, editid), (stateid, verbose)) in
    let xml_add = Xmlprotocol.of_call add in
    Xml_printer.print (Xml_printer.TChannel cout) xml_add

let request_editat editid cout = 
    request_mode := Request_edit_at;
    let editat = Xmlprotocol.edit_at editid in
    let xml_editat = Xmlprotocol.of_call editat in
    Xml_printer.print (Xml_printer.TChannel cout) xml_editat

let request_query query stateid cout = 
    request_mode := Request_query;
    let query = Xmlprotocol.query query in
    let xml_query = Xmlprotocol.of_call query in
    Xml_printer.print (Xml_printer.TChannel cout) xml_query

let response_query str_msg = 
    let xparser = Xml_parser.make (Xml_parser.SString str_msg) in
    let xml_msg = Xml_parser.parse xparser in
    let msg = Xmlprotocol.to_value (Serialize.to_string) xml_msg in
    match msg with
    | Good query -> print_endline query
    | _ -> printf "unknown from response query"; flush stdout

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

*************************************************************************************)
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
        (*match !request_mode with
        | Request_about -> response_coq_info fb_str
        | Request_init -> response_init fb_str
        | Request_edit_at -> response_edit_at fb_str
        | Request_query -> response_query fb_str
        | Request_goals -> response_goals fb_str
        | Request_evars -> response_evars fb_str
        | Request_hints -> response_hints fb_str
        | Request_status -> response_status fb_str
        | Request_search -> response_search fb_str
        | Request_getoptions -> response_getoptions fb_str
        | Request_setoptions -> response_setoptions fb_str
        | Request_mkcases -> response_mkcases fb_str
        | Request_quit -> response_quit fb_str*)
    end
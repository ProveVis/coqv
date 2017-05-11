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
let escaped_cmd cmd = 
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
        | _ -> None in
    String.iter (fun c -> 
        match eacape_char c with
        | Some str -> add_str_to_buffer str
        | None -> Bytes.fill buffer !length 1 c; incr length) cmd;
    let escapted_str = Bytes.sub_string buffer 0 !length in 
    printf "raw cmd: %s, escaped cmd: %s\n" cmd escapted_str;
    flush stdout;
    escapted_str

let request_coq_info cout = 
    request_mode := Request_about;
    let about = Xmlprotocol.About () in
    let xml_about = Xmlprotocol.of_call about in
    Xml_printer.print (Xml_printer.TChannel cout) xml_about

let response_coq_info fb_val = 
    (match fb_val with
    | Good fb -> 
            Runtime.coqtop_info := fb;
            printf "Coqtop Info: \n\tversion %s, \n\tprotocol version %s, \n\trelease date %s, \n\tand compile date %s\n" 
                fb.coqtop_version fb.protocol_version fb.release_date fb.compile_date
    | _ -> printf "parsing coq info message fails\n");
    flush stdout

let request_init filename cout = 
    request_mode := Request_init;
    let init = Xmlprotocol.init filename in
    let xml_init = Xmlprotocol.of_call init in
    Xml_printer.print (Xml_printer.TChannel cout) xml_init

let response_init msg = 
    match msg with
    | Good stateid -> 
        print_endline ("got new stateid: "^(string_of_int stateid)); 
        Runtime.new_stateid := stateid
    | _ -> printf "unknown from response init\n"; flush stdout    

let request_quit cout = 
    request_mode := Request_quit;
    let quit = Xmlprotocol.quit () in
    let xml_quit = Xmlprotocol.of_call quit in
    Xml_printer.print (Xml_printer.TChannel cout) xml_quit

let response_quit msg = 
    match msg with
    | Good _ -> 
        print_endline ("now quit! "); 
        Runtime.running := false
    | _ -> printf "unknown from response quit\n"; flush stdout  

let request_goals () =
    let cout = Runtime.coq_channels.cout in 
    request_mode := Request_goals;
    let goals = Xmlprotocol.goals () in
    let xml_goals = Xmlprotocol.of_call goals in
    Xml_printer.print (Xml_printer.TChannel cout) xml_goals

let response_goals msg =
    match msg with
    | Good None -> 
        print_endline "no more goals";
        Doc_model.raise_cache ()
    | Good (Some goals) -> begin
            printf "focused goals number: %d,";
            Doc_model.raise_cache ()
        end
    | Fail -> Doc_model.clear_cache ()

let request_add cmd editid stateid verbose = 
    let ecmd = escaped_cmd cmd in
    let cout = Runtime.coq_channels.cout in
    request_mode := Request_add;
    let add = Xmlprotocol.add ((ecmd, editid), (stateid, verbose)) in
    let xml_add = Xmlprotocol.of_call add in
    Xml_printer.print (Xml_printer.TChannel cout) xml_add;
    Doc.cache := Some (stateid, cmd)

let response_add msg =
    begin
        match msg with
        | Good (stateid, CSig.Inl (), content) ->
            printf "new state id: %d, message content: %s\n" stateid content;
            Runtime.new_stateid := stateid;
            flush stdout
        | Good (stateid, CSig.Inr next_stateid, content) ->
            printf "finished current proof, move to state id: %d, message content: %s\n" next_stateid content;
            Runtime.new_stateid := next_stateid;
            flush stdout
        | Fail (stateid, _, Xml_datatype.PCData content) -> 
            printf "error add in state id %d, message content: %s\n" stateid content;
            Doc_model.clear_cache ();
            flush stdout
    end;
    request_goals ()

let request_edit_at editid cout = 
    request_mode := Request_edit_at;
    let editat = Xmlprotocol.edit_at editid in
    let xml_editat = Xmlprotocol.of_call editat in
    Xml_printer.print (Xml_printer.TChannel cout) xml_editat

let response_edit_at msg =
    match msg with
    | Good (CSig.Inl ()) ->
        printf "simple backtract;\n";
        flush stdout
    | Good (CSig.Inr (focusedStateId, (focusedQedStateId, oldFocusedStateId))) ->
        printf "focusedStateId: %d, focusedQedStateId: %d, oldFocusedStateId: %d\n" focusedStateId focusedQedStateId oldFocusedStateId;
        flush stdout
    | Fail (errorFreeStateId, loc, Xml_datatype.PCData content) ->
        printf "errorFreeStateId: %d, message content: %s\n" errorFreeStateId content
        flush stdout

let request_query query stateid cout = 
    request_mode := Request_query;
    let query = Xmlprotocol.query query in
    let xml_query = Xmlprotocol.of_call query in
    Xml_printer.print (Xml_printer.TChannel cout) xml_query

let response_query msg = 
    match msg with
    | Good query -> print_endline query
    | _ -> printf "unknown from response query\n"; flush stdout



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
let interpret_feedback xml_fb = 
    let fb = Xmlprotocol.to_feedback xml_fb in


let interpret_cmd cmd = 
    printf "Interpreting command: %s\n" cmd;
    flush stdout

let handle_input input_str cout = 
    output_string stdout (input_str^"\n");
    flush stdout


let handle_answer feedback = 
    let fb_str = Str.global_replace (ignored_re ()) "" feedback in
    printf "got feedback message length: %d\n" (String.length fb_str);
    printf "%s\n\n" fb_str;
    if not !Flags.xml then begin
        printf "%s\n" fb_str
    end else begin
        let xparser = Xml_parser.make (Xml_parser.SString fb_str) in
        let xml_fb = Xml_parser.parse xparser in
        match Xmlprotocol.is_message xml_fb with
        | Some (level, loc, content) ->
            printf "%s: %s" level content;
            flush stdout
        | None -> 
            if Xmlprotocol.is_feedback xml_fb then
                (interpret_feedback xml_fb) (*does nothing on feedbacks*)
            else begin
                match !request_mode with
                | Request_about ->      response_coq_info (Xmlprotocol.to_answer (Xmlprotocol.About ()) xml_fb)
                | Request_init ->       response_init (Xmlprotocol.to_answer (Xmlprotocol.init None) xml_fb)
                | Request_edit_at ->    response_edit_at (Xmlprotocol.to_answer (Xmlprotocol.edit_at 0) xml_fb)
                | Request_query ->      response_query (Xmlprotocol.to_answer (Xmlprotocol.query ("", 0)) xml_fb)
                | Request_goals ->      response_goals (Xmlprotocol.to_answer (Xmlprotocol.goals ()) xml_fb)
                | Request_evars ->      response_evars (Xmlprotocol.to_answer (Xmlprotocol.evars ()) xml_fb)
                | Request_hints ->      response_hints (Xmlprotocol.to_answer (Xmlprotocol.hints ()) xml_fb)
                | Request_status ->     response_status (Xmlprotocol.to_answer (Xmlprotocol.status true) xml_fb)
                | Request_search ->     response_search (Xmlprotocol.to_answer (Xmlprotocol.search []) xml_fb)
                | Request_getoptions -> response_getoptions (Xmlprotocol.to_answer (Xmlprotocol.get_options ()) xml_fb)
                | Request_setoptions -> response_setoptions (Xmlprotocol.to_answer (Xmlprotocol.set_options []) xml_fb)
                | Request_mkcases ->    response_mkcases (Xmlprotocol.to_answer (Xmlprotocol.mkcases "") xml_fb)
                | Request_quit ->       response_quit (Xmlprotocol.to_answer (Xmlprotocol.quit ()) xml_fb)
                | Request_add ->        response_add (Xmlprotocol.to_answer (Xmlprotocol.add (("",0),(0,true))) xml_fb)
                | Request_interp ->     response_iterp (Xmlprotocol.to_answer (Xmlprotocol.interp ((true, true),"")) xml_fb)
                | Request_stopworker -> response_stopworker (Xmlprotocol.to_answer (Xmlprotocol.stop_worker "") xml_fb)
                | Request_printast ->   response_printast (Xmlprotocol.to_answer (Xmlprotocol.print_ast 0) xml_fb)
                | Request_annotate ->   response_annotate (Xmlprotocol.to_answer (Xmlprotocol.annotate "") xml_fb)
            end
    end
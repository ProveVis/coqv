type level = Info | Warning | Error 

let str_level = function
    | Info -> "Info"
    | Warning -> "Warning"
    | Error -> "Error"

(* let current_coqtop_worker = ref "master" *)
type log_info = {
    coqtop_log: out_channel;
    vmdv_log: out_channel;
}

let logs: (log_info option) ref = ref None

let init_log_files () = 
    if not (Sys.file_exists "log") then begin
        let log_dir_created = Sys.command "mkdir log" in
        if log_dir_created <> 0 then begin
            print_endline "Warning: cannot create directory log, now running in non-debug mode";
        end else begin
            Flags.debug := true
        end
    end else begin
        Flags.debug := true
    end;
    if !Flags.debug = true then begin
        logs := Some {
            coqtop_log = open_out ("./log/"^(!Flags.xml_log_file)); 
            vmdv_log = open_out ("./log/"^(!Flags.json_log_file))}
    end

let log_coqv levl str = 
    print_endline ("["^(str_level levl)^"] "^str)

let log_coqtop is_sending str = 
    Options.action (fun lgs -> 
        if is_sending then
            output_string lgs.coqtop_log ("COQV --> COQTOP\n"^str^"\n")
        else
            output_string lgs.coqtop_log ("COQTOP --> COQV\n"^str^"\n");
        flush lgs.coqtop_log
    ) !logs

let log_vmdv is_sending str = 
    Options.action (fun lgs ->
        if is_sending then
            output_string lgs.vmdv_log ("COQV --> VMDV\n"^str^"\n")
        else
            output_string lgs.vmdv_log ("VMDV --> COQV\n"^str^"\n");
        flush lgs.vmdv_log        
    ) !logs

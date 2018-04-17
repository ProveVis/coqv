
let work cin =     
    let buffer = Bytes.create !Flags.xml_bufsize in
    while !Runtime.running do
        (* print_endline "wait for coqtop"; *)
        let len = input cin buffer 0 !Flags.xml_bufsize in
        (* print_endline ("coqtop responsed with length "^(string_of_int len)); *)
        if len = 0 then
            running := false
        else begin
            let output_str = Bytes.sub_string buffer 0 len in
            (*printf "%s" (Str.global_replace (ignored_re ()) "" output_str);
            flush stdout*)
            if(len <> 0) then
                handle_answer output_str;
            flush stdout
        end;
        (* print_endline ("there are "^(string_of_int (List.length !batch_commands))^" commands wait to send to coqtop"); *)
        if Doc_model.is_processed () && !Doc_model.goal_responsed then begin
            (* print_endline ("coqtop processed "); *)
            if !Flags.batch_mode = false || (!batch_commands = []) then begin
                Flags.batch_mode := false;
                Flags.running_coqv := true;
                Condition.signal read_write_condition
            end else begin
                let bch, bct = List.hd !batch_commands, List.tl !batch_commands in
                handle_input bch;
                batch_commands := bct
            end
        end
    done;
    print_endline "coqtop work done"
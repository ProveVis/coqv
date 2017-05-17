open Types
open Printf
open Interface

let coqtop_info = ref {
    coqtop_version = "";
    protocol_version = "";
    release_date = "";
    compile_date = "";
}

type channels = {
    mutable cin: in_channel;
    mutable cout: out_channel;
}

let coq_channels : channels = {
    cin = stdin;
    cout = stdout;
}

let prompt = ref "Coq < "
let ignored_re () = Str.regexp !prompt

let new_stateid = ref 0
let running = ref true






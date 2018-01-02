let version_no = "0.1"
let xml = ref true (* whether communicating with coqtop using xml protocol, default: true *)
let xml_bufsize = ref 40960 (* buffer size when receiving a message from coqtop *)
let json_bufsize = ref 40960 (* buffer size when receiving a message from vmdv *)
let xml_log_file = ref "log_xml.log" (* logging xml messages *)
let json_log_file = ref "log_json.log" (* logging json messages *)
let debug = ref false (*using debug mode or not*)
let batch_mode = ref false (*whether in batch mode, i.e., read mutiple commands at once*)
let running_coqv = ref true (*whether we are receiving coqv commands or coqtop commands*)


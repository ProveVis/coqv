let _ = 
    Xml_printer.print (Xml_printer.TChannel stdout) (Serialize.of_unit ());
    print_endline "";
    Xml_printer.print (Xml_printer.TChannel stdout) (Serialize.of_edit_id 1);
    print_endline "";
    Xml_printer.print (Xml_printer.TChannel stdout) (Serialize.of_list (Serialize.of_string) ["1";"2";"3"]);
    print_endline "";
    let xparser = Xml_parser.make (Xml_parser.SString "<unit/>") in
    let xml_unit = Xml_parser.parse xparser in
    Xml_printer.print (Xml_printer.TChannel stdout) xml_unit;
    print_endline "";
    let xparser = Xml_parser.make (Xml_parser.SString "<edit_id val=\"-1\"/>") in
    let xml_editid = Xml_parser.parse xparser in
    Xml_printer.print (Xml_printer.TChannel stdout) xml_editid;
    print_endline "";
    print_endline (string_of_int (Serialize.to_edit_id xml_editid));
    let xparser = Xml_parser.make (Xml_parser.SString "<list><string>1</string><string>2</string><string>3</string></list>") in
    let xml_list = Xml_parser.parse xparser in
    Xml_printer.print (Xml_printer.TChannel stdout) xml_list;
    print_endline "";
    let str_list = Serialize.to_list (Serialize.to_string) xml_list in
    List.iter (fun s -> print_string (s^" ")) str_list;
    print_endline "";
    let about = Xmlprotocol.About () in
    let xml_about = Xmlprotocol.of_call about in
    Xml_printer.print (Xml_printer.TChannel stdout) xml_about;
    print_endline "";
    let init = Xmlprotocol.Init None in
    let xml_init = Xmlprotocol.of_call init in
    Xml_printer.print (Xml_printer.TChannel stdout) xml_init;
    print_endline ""


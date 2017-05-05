xml:
	ocamlc -c xml_datatype.mli
	ocamllex xml_lexer.mll
	ocamlc -c xml_lexer.ml
	ocamlc -c xml_parser.ml
	ocamlc -c xml_printer.ml
	ocamlc -c cSig.mli
	ocamlc -c store.ml
	ocamlc -thread -c threads.cma unix.cma exninfo.ml
	ocamlc -c loc.ml
	ocamlc -c serialize.ml

test:
	ocamlc -c xml_datatype.mli
	ocamllex xml_lexer.mll
	ocamlc -c xml_lexer.ml
	ocamlc -c xml_parser.ml
	ocamlc -c xml_printer.ml
	ocamlc -c cSig.mli
	ocamlc -c store.ml
	ocamlc -thread -c threads.cma unix.cma exninfo.ml
	ocamlc -c loc.ml
	ocamlc -c serialize.ml
	ocamlc -thread -o test_xml unix.cma threads.cma xml_datatype.mli xml_lexer.cmo xml_parser.cmo xml_printer.cmo cSig.mli store.cmo  exninfo.cmo loc.cmo serialize.cmo test_xml.ml

clean:
	rm -f *.cm[ioxa]
	rm -f xml_lexer.ml
	rm -f test_xml


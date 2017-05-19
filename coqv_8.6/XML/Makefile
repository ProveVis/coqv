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
	ocamlc -c stateid.ml
	ocamlc -c util.ml
	ocamlc -c feedback.ml
	ocamlc -c interface.mli
	ocamlc -c dyn.ml
	ocamlc -c pp_control.ml
	ocamlc -c backtrace.ml
	ocamlc -c pp.ml
	ocamlc -c terminal.ml
	ocamlc -c ppstyle.ml
	ocamlc -c cErrors.ml
	ocamlc -c richpp.ml
	ocamlc -c xmlprotocol.ml
	ocamlc -thread -a -o xml.cma threads.cma xml_datatype.mli xml_lexer.cmo xml_parser.cmo xml_printer.cmo \
	 cSig.mli store.cmo  exninfo.cmo loc.cmo serialize.cmo stateid.cmo \
	 util.cmo feedback.cmo interface.mli dyn.cmo pp_control.cmo backtrace.cmo pp.cmo terminal.cmo \
	 ppstyle.cmo cErrors.cmo richpp.cmo xmlprotocol.cmo

test:
	ocamlc -thread -o test threads.cma unix.cma xml.cma test_xml.ml


# test:
# 	ocamlc -c xml_datatype.mli
# 	ocamllex xml_lexer.mll
# 	ocamlc -c xml_lexer.ml
# 	ocamlc -c xml_parser.ml
# 	ocamlc -c xml_printer.ml
# 	ocamlc -c cSig.mli
# 	ocamlc -c store.ml
# 	ocamlc -thread -c threads.cma unix.cma exninfo.ml
# 	ocamlc -c loc.ml
# 	ocamlc -c serialize.ml
# 	ocamlc -c stateid.ml
# 	ocamlc -c util.ml
# 	ocamlc -c feedback.ml
# 	ocamlc -c interface.mli
# 	ocamlc -c dyn.ml
# 	ocamlc -c pp_control.ml
# 	ocamlc -c backtrace.ml
# 	ocamlc -c pp.ml
# 	ocamlc -c terminal.ml
# 	ocamlc -c ppstyle.ml
# 	ocamlc -c cErrors.ml
# 	ocamlc -c richpp.ml
# 	ocamlc -c xmlprotocol.ml
# 	ocamlc -thread -o test_xml unix.cma threads.cma xml_datatype.mli xml_lexer.cmo xml_parser.cmo xml_printer.cmo \
# 	 cSig.mli store.cmo  exninfo.cmo loc.cmo serialize.cmo stateid.cmo \
# 	 util.cmo feedback.cmo interface.mli dyn.cmo pp_control.cmo backtrace.cmo pp.cmo terminal.cmo \
# 	 ppstyle.cmo cErrors.cmo richpp.cmo xmlprotocol.cmo test_xml.ml

clean:
	rm -f *.cm[ioxa]
	rm -f xml_lexer.ml
	rm -f test_xml
	rm -f test


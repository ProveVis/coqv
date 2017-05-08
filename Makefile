all: repl.ml
	ocamlfind ocamlc -thread -o coqv -package yojson -linkpkg -g str.cma threads.cma flags.ml types.ml communicate.ml runtime.ml handle_interaction.ml repl.ml

xml:
	make -C utils xml
	# ocamlc -c xml_datatype.mli
	# ocamllex xml_lexer.mll
	# ocamlc -c xml_lexer.ml
	# ocamlc -c xml_parser.ml
	# ocamlc -c xml_printer.ml
	# ocamlc -c cSig.mli
	# ocamlc -c store.ml
	# ocamlc -thread -c threads.cma unix.cma exninfo.ml
	# ocamlc -c loc.ml
	# ocamlc -c serialize.ml

clean:
	rm -f *.cm[ioxa]
	rm -f coqv coqv.exe
	rm -f comm
	rm -f repl
	rm -f *.eventlog
	make -C utils clean
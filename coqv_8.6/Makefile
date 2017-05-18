all: 
	ocamlfind ocamlc -I XML -thread -o coqv -package yojson -linkpkg -g str.cma threads.cma xml.cma \
	 flags.ml types.ml communicate.ml runtime.ml cmd.ml doc_model.ml proof_model.ml history.ml interaction.ml repl.ml

xml:
	make -C XML xml
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
	make -C XML clean

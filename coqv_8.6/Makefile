all: 
	make xml
	ocamlfind ocamlc -I utils -thread -o coqv -package yojson -linkpkg -g str.cma threads.cma xml.cma \
	 flags.ml types.ml protocol_json.ml communicate.ml runtime.ml coqv_utils.ml doc_model.ml \
	 proof_model.ml history.ml status.ml callbacks.ml coqv_doc.ml interaction.ml repl.ml

xml:
	make -C utils xml
clean:
	rm -f *.cm[ioxa]
	rm -f coqv coqv.exe
	rm -f comm
	rm -f repl
	rm -f *.eventlog
	rm -f log
	rm -f *.log
	make -C utils clean

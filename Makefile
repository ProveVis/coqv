all: repl.ml
	ocamlfind ocamlc -thread -o coqv.exe -package yojson -linkpkg -g str.cma threads.cma flags.ml types.ml communicate.ml runtime.ml handle_interaction.ml repl.ml

clean:
	rm -f *.cm[ioxa]
	rm -f coqv coqv.exe
	rm -f comm
	rm -f repl
	rm -f *.eventlog
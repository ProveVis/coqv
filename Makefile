all: repl.ml
	ocamlfind ocamlc -thread -o coqv -package yojson -linkpkg -g str.cma threads.cma types.ml communicate.ml runtime.ml script.ml repl.ml

clean:
	rm -f *.cm[ioxa]
	rm -f coqv coqv.exe
	rm -f comm
	rm -f repl
	rm -f *.eventlog
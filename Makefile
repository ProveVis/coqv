all: repl.ml
	ocamlfind ocamlc -thread -o coqv str.cma unix.cma threads.cma repl.ml

clean:
	rm -f *.cm[ioxa]
	rm -f coqv
	rm -f comm
	rm -f repl
	rm -f *.eventlog
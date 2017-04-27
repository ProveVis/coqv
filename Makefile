all: repl.ml
	ocamlc -thread -o coqv.exe str.cma unix.cma threads.cma repl.ml

clean:
	rm -f *.cm[ioxa]
	rm -f coqv
	rm -f comm
	rm -f repl
	rm -f *.eventlog
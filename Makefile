all: repl.ml
	ocamlfind ocamlc -o coqv str.cma unix.cma repl.ml communicate.ml

repl: repl.ml
	ocamlc -o repl str.cma repl.ml

comm: communicate.ml
	ocamlfind ocamlc -thread -o comm unix.cma threads.cma communicate.ml
	# ocamlfind ocamlc -thread -o comm unix.cma threads.cma -package async -linkpkg -g communicate.ml

clean:
	rm -f *.cm[ioxa]
	rm -f coqv
	rm -f comm
	rm -f repl
	rm -f *.eventlog
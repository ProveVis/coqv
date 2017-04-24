all: repl.ml
	ocamlfind ocamlc -o coqv str.cma unix.cma repl.ml communicate.ml

repl: repl.ml
	ocamlc -o repl str.cma repl.ml

comm: communicate.ml
	ocamlc -o comm unix.cma communicate.ml

clean:
	rm -f *.cm[ioxa]
	rm -f coqv
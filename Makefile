all: repl.ml
	ocamlfind ocamlc -o coqv str.cma unix.cma repl.ml communicate.ml

clean:
	rm -f *.cm[ioxa]
	rm -f coqv
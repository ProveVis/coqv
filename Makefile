all: repl.ml
	ocamlfind ocamlc -o coqv str.cma repl.ml

clean:
	rm -f *.cm[ioxa]
	rm -f coqv
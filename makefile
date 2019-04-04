all: collections puzzlesolve tests

collections: collections.ml
	ocamlbuild -use-ocamlfind collections.byte

puzzlesolve: puzzlesolve.ml
	ocamlbuild -use-ocamlfind puzzlesolve.byte

tests: tests.ml
	ocamlbuild tests.byte

clean:
	rm -rf _build *.byte

all: build test

clean:
	ocamlbuild -clean

build:
	ocamlbuild -j 4 -use-ocamlfind test.native

test:
	./test.native

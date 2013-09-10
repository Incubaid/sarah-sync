default : build

all: build test

clean:
	ocamlbuild -clean

build:
	ocamlbuild -j 4 -use-ocamlfind test.native
	ocamlbuild -j 4 -use-ocamlfind command_line.native
	ln -s command_line.native ssync

test:
	./test.native --run-all-tests

indent-tabs-to-spaces:
	@echo "Converting tabs to spaces..."
	@find . -iname '*.ml' -o -iname '*.mli' -exec sed -i -e 's/\t/ /g' {} \;

indent-trailing-whitespace:
	@echo "Removing trailing whitespace..."
	@find . -iname '*.ml' -o -iname '*.mli' -exec sed -i -e 's/[[:space:]]*$$//' {} \;

indent-trailing-lines:
	@echo "Removing trailing newlines..."
	@find . -iname '*.ml' -o -iname '*.mli' -exec sed -i -e :a -e '/^\n*$$/{$$d;N;ba' -e '}' {} \;

indent-ocp-indent:
	@echo "Running ocp-indent..."
	@find . -iname '*.ml' -o -iname '*.mli' -exec ocp-indent -i {} \;

indent: indent-tabs-to-spaces indent-trailing-whitespace indent-trailing-lines indent-ocp-indent
	@echo "Done"

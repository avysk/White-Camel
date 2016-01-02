build: setup.data
	ocaml setup.ml -build
setup.ml: _oasis
	oasis setup
setup.data: setup.ml
	ocaml setup.ml -configure --enable-ui
clean:
	ocaml setup.ml -clean
.PHONY: clean

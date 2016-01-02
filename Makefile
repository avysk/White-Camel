dev:
	oasis setup
	ocaml setup.ml -configure --enable-ui
	ocaml setup.ml -build
clean:
	ocaml setup.ml -clean

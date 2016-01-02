build: setup.data
	ocaml setup.ml -build
setup.ml: _oasis
	oasis setup
setup.data: setup.ml
	ocaml setup.ml -configure --enable-ui
clean:
	ocaml setup.ml -clean
distclean: clean
	rm -f setup.data setup.log
pristine: distclean
	rm -f setup.ml _tags src/lib/whiteengine.mldylib src/lib/whiteengine.mllib
.PHONY: clean distclean pristine

main:
	ocamlbuild whitecamel.native

CURSES_LIB=-I,`ocamlfind query curses`
FLAGS=-lib curses -cflags ${CURSES_LIB} -lflags ${CURSES_LIB}
editor:
	ocamlbuild ${FLAGS} tools/poseator.native
all:
	ocamlbuild ${FLAGS} all.otarget
clean:
	ocamlbuild -clean
docs: all
	ocamlfind ocamldoc -html -m A -d docs -I _build *.mli *.ml

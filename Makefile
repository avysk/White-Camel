main:
	ocamlbuild whitecamel.native

CURSES_LIB=-I,`ocamlfind query curses`
FLAGS=-cflags ${CURSES_LIB} -lflags ${CURSES_LIB}
editor:
	ocamlbuild ${FLAGS} tools/poseator.native
clean:
	ocamlbuild -clean

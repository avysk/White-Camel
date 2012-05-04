main:
	ocamlbuild ${FLAGS} whitecamel.native
debug:
	ocamlbuild ${FLAGS}  whitecamel.d.byte
profile:
	ocamlbuild ${FLAGS} whitecamel.p.native

CURSES_LIB=-I,`ocamlfind query curses`
#FLAGS=-lib curses -cflags -g,${CURSES_LIB} -lflags -g,${CURSES_LIB}
FLAGS=-lib curses -cflags ${CURSES_LIB} -lflags ${CURSES_LIB}
editor:
	ocamlbuild ${FLAGS} tools/poseator.native
all:
	ocamlbuild ${FLAGS} all.otarget
clean:
	ocamlbuild -clean
	rm -f whitecamel.mltop
docs:
	ocamlbuild whitecamel.docdir/index.html

whitecamel.byte:
	ocamlbuild ${FLAGS} whitecamel.byte
whitecamel.mltop: whitecamel.byte
	find _build -regex .*cmo | sed 's/_build\///; s/.cmo//' > whitecamel.mltop
whitecamel.top: whitecamel.mltop
	ocamlbuild ${FLAGS} whitecamel.top
top: whitecamel.top

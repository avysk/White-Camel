open Types
open Curses

let win = initscr ();
  for i = 1 to 5 do
    let _ = wmove win i i in
    let _ = addch '*'
  done
  let () = endwin ()

open Curses

let do_init win =
  let _ = noecho () in
  let _ = curs_set 0 in
  let _ = keypad win true in
  let _ = start_color () in
  let _ = use_default_colors () in
  let _ = init_pair 0 0 (-1) in
  let _ = init_pair 1 Color.red (-1)  in
  ()

let normal () = attrset (A.color_pair 0)
let red () = attrset (A.color_pair 1 lor A.bold)
let bold () = attrset (A.color_pair 0 lor A.bold)

(*
vim:sw=2
*)

open Curses
open Pos_curses_utils

type cursor_t = {
  mutable x : int ;
  mutable y : int
}

let cursor = { x = 0; y = 0 }

let space = int_of_char ' '
let more = int_of_char '>'
let less = int_of_char '<'

let update_cursor x y =
  let x' = cursor.x in
  let y' = cursor.y in
  let _ = cursor.x <- x in
  let _ = cursor.y <- y in
  let cy n = 9 - 2 * n in
  let cx1 n = 5 * n + 1 in
  let cx2 n = 5 * n + 4 in
  let () = bold () in
  let _ = mvaddch (cy y') (cx1 x') space in
  let _ = mvaddch (cy y') (cx2 x') space in
  let _ = mvaddch (cy y) (cx1 x) more in
  let _ = mvaddch (cy y) (cx2 x) less in
  ()

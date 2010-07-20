open Utils
open Types
open Curses
open Acs
open Position

type cursor_t = {
  mutable x : int ;
  mutable y : int
}

let left_corner_x = 5
let left_corner_y = 5

let cursor = { x = 0; y = 0 }

let win = initscr ()

let symbols = get_acs_codes ()

let curpos = ref start_position

let hline2 y x  =
  let _ = mvaddch y x symbols.hline in
  let _ = addch symbols.hline in
  ()

let board_skeleton y x =
  let _ = attron A.bold in
  let _ = mvaddch y x symbols.ulcorner in
  let _ = mvaddch y (x + 15) symbols.urcorner in
  let _ = mvaddch  (y + 10) x symbols.llcorner in
  let _ = mvaddch (y + 10) (x + 15) symbols.lrcorner in
  let _ =
    for i = 0 to 5 do
      for j = 0 to 4 do
	hline2 (y + 2 * i) (x + 3 * j + 1)
      done
    done in
  let _ =
    for i = 0 to 4 do
      for j = 0 to 5 do
	let _ = mvaddch (y + 2 * i + 1) (x + 3 * j) symbols.vline in
	()
      done
    done in
  let _ =
    for i = 1 to 4 do
      let t = y + 2 * i in
      let _ = mvaddch t x symbols.ltee in
      let _ = mvaddch t (x + 15) symbols.rtee in
      let t' = x + 3 * i in
      let _ = mvaddch y t' symbols.ttee in
      let _ = mvaddch (y + 10) t' symbols.btee in
      let _ =
	for j = 1 to 4 do
	  let _ = mvaddch t (3 * j + x) symbols.plus in
	  ()
	done in
      ()
    done in
  let _ = attroff A.bold in
  ()

let empty_cell y x =
  let _ = mvaddch y x symbols.bullet in
  let _ = addch symbols.bullet in
  ()
(*
let draw_piece x y pc (* board coordinates, not cursed coordinates *) =
  let cursed_y = left_corner_y + 9 - 2 * y in
  let cursed_x = left_corner_x + 1 + 3 * x in
  match pc with
    | None -> empty_cell cursed_y cursed_x
    | (Sente, p) ->
      let () = A.attr_set A.color *)

;;
let _ = board_skeleton left_corner_y left_corner_x  in
let _ = getch() in
endwin ()

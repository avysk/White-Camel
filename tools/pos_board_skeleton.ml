open Curses
open Acs

open Pos_curses_utils

let board_skeleton y x =
  let symbols = get_acs_codes () in
  let () = normal () in
  let _ = mvaddch y x symbols.ulcorner in
  let _ = mvaddch y (x + 25) symbols.urcorner in
  let _ = mvaddch  (y + 10) x symbols.llcorner in
  let _ = mvaddch (y + 10) (x + 25) symbols.lrcorner in
  let _ =
    for i = 0 to 5 do
      for j = 0 to 4 do
        mvhline (y + 2 * i) (x + 5 * j + 1) symbols.hline 4 ;
        let _ = mvaddch (y + 2 * j + 1) (x + 5 * i) symbols.vline in
        ()
      done
    done in
  let _ =
    for i = 1 to 4 do
      let t = y + 2 * i in
      let s = x + 5 * i in
      let _ = mvaddch t x symbols.ltee in
      let _ = mvaddch t (x + 25) symbols.rtee in
      let _ = mvaddch y s symbols.ttee in
      let _ = mvaddch (y + 10) s symbols.btee in
      let _ =
        for j = 1 to 4 do
          let _ = mvaddch t (5 * j + x) symbols.plus in
          ()
        done in
      ()
    done in
  let _ = mvaddstr 11 11 " to move" in
  let _ = mvaddch 15 0 symbols.ulcorner in
  let _ = hline symbols.hline 2 in
  let _ = mvaddch 15 3 symbols.urcorner in
  let _ = mvaddch 16 0 symbols.vline in
  let _ = mvaddch 16 3 symbols.vline in
  let _ = mvaddch 17 0 symbols.llcorner in
  let _ = hline symbols.hline 2 in
  let _ = mvaddch 17 3 symbols.lrcorner in
  ()

open Curses
open Acs
open Types
open Pos_curses_utils

let empty_cell () =
  let () = normal () in
  let symbols = get_acs_codes () in
  let _ = hline symbols.bullet 2 in
  ()

let show_piece pc (* at the given point *) =
  match pc with
  | None -> empty_cell ()
  | Some (s, p) ->
      let () =
        begin
          match s with
          | Sente -> red ()
          | Gote -> normal ()
        end in
      let _ =
        begin
          match p with
          | Pawn -> addstr "Pn"
          | King -> addstr "Kg"
          | Rook -> addstr "Rk"
          | Bishop -> addstr "Bp"
          | Gold -> addstr "Gd"
          | Silver -> addstr "Sr"
          | Tokin -> addstr "Tn"
          | GoldS -> addstr "Gs"
          | DragonHorse -> addstr "DH"
          | DragonKing -> addstr "DK"
        end in
      ()

let draw_piece x y pc (* board coordinates, not cursed coordinates *) =
  let cursed_y = 9 - 2 * y in
  let cursed_x = 2 + 5 * x in
  let _ = move cursed_y cursed_x in
  show_piece pc

(*
 vim:sw=2
 *)

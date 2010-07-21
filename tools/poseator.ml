open Curses
open Acs

open Utils
open Types
open Rules
open Position

open Pos_keybindings
open Pos_board_skeleton
open Pos_curses_utils
open Pos_cursor
open Pos_piece

let buf = ref (None : cell)
let cur_pos = ref start_position

let draw_position () =
  let brd = !cur_pos.Types.board  in
  let _ = 
    for i = 0 to 4 do
      for j = 0 to 4 do
	draw_piece i j brd.(i).(j)
      done
    done in
  let _ = move 11 6 in
  let _ = match !cur_pos.to_move with
    | Sente -> let () = red () in addstr "Sente"
    | Gote -> let () = normal () in addstr " Gote" in
  let _ = move 16 1 in
  let _ = show_piece !buf in
  ()

exception Quit
exception Impossible

let curs_switch_color () =
  match !cur_pos.Types.board.(cursor.x).(cursor.y) with
    | None -> raise Impossible
    | Some (s, p) when p = King -> raise Impossible
    | Some (s, p) ->
      let _ = !cur_pos.Types.board.(cursor.x).(cursor.y) <- Some (other s, p) in
      draw_position ()

let curs_turnover () =
  match !cur_pos.Types.board.(cursor.x).(cursor.y) with
    | None -> raise Impossible
    | Some (s, p) ->
      try
	let _ = !cur_pos.Types.board.(cursor.x).(cursor.y) <- Some (s, turnover p) in
	draw_position ()
      with Cannot _ -> raise Impossible (* FIXME *)

let take_or_place () =
  match !buf, !cur_pos.Types.board.(cursor.x).(cursor.y) with
    | None, None -> raise Impossible
    | None, p ->
      let _ =
	begin
	  buf := p ;
	  !cur_pos.Types.board.(cursor.x).(cursor.y) <- None
	end in
      draw_position ()
    | p, None ->
      let _ =
	begin
	  buf := None ;
	  !cur_pos.Types.board.(cursor.x).(cursor.y) <- p
	end in
      draw_position ()
    | _, _ -> raise Impossible

let rec mainloop () =
  try
    let _ =
      match getch () with
	| c when c = cmd.quit -> raise Quit
	| c when c = cmd.up ->
	  update_cursor cursor.x ((cursor.y + 1) mod 5)
	| c when c = cmd.down ->
	  update_cursor cursor.x ((cursor.y + 4) mod 5)
	| c when c = cmd.right ->
	  update_cursor ((cursor.x + 1) mod 5) cursor.y
	| c when c = cmd.left ->
	  update_cursor ((cursor.x + 4) mod 5) cursor.y
	| c when c = cmd.switch_color -> curs_switch_color ()
	| c when c = cmd.turnover -> curs_turnover ()
	| c when c = cmd.switch_move ->
	  let _ = cur_pos := {!cur_pos with to_move = other !cur_pos.to_move} in
	  draw_position ()
	| c when c = cmd.take_or_place -> take_or_place ()
	| _ -> raise Impossible
    in
    mainloop ()
  with
    | Impossible _ -> let _ = flash () in mainloop ()
    | Quit _ -> ()

let _ =
  let win = initscr () in
  let () = do_init win in
  let () = board_skeleton 0 0 in
  let () = update_cursor 0 0 in
  let () = draw_position () in
  let _ = mainloop () in
  endwin ()

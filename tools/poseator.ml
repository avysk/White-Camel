open Utils
open Types
open Rules
open Curses
open Acs
open Position

open Pos_keybindings
open Pos_board_skeleton
open Pos_curses_utils
open Pos_cursor

let buf = ref (None : cell)

let win = initscr ()
let symbols = get_acs_codes ()

let cur_pos = ref start_position

let empty_cell () =
  let () = normal () in
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
	| c when c = cmd.switch_color ->
	  let pc = !cur_pos.Types.board.(cursor.x).(cursor.y) in
	  begin
	    match pc with
	      | None -> raise Impossible
	      | Some (s, p) when p = King -> raise Impossible
	      | Some (s, p) ->
		let _ = !cur_pos.Types.board.(cursor.x).(cursor.y) <- Some (other s, p) in
		draw_position ()
	  end
	| c when c = cmd.turnover ->
	  let pc = !cur_pos.Types.board.(cursor.x).(cursor.y) in
	  begin
	    match pc with
	      | None -> raise Impossible
	      | Some (s, p) ->
		try
		  let _ = !cur_pos.Types.board.(cursor.x).(cursor.y) <- Some (s, turnover p) in
		  draw_position ()
		with Cannot _ -> raise Impossible
	  end
	| c when c = cmd.switch_move ->
	  let _ = cur_pos := {!cur_pos with to_move = other !cur_pos.to_move} in
	  draw_position ()
	| c when c = cmd.take_or_place ->
	  begin
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
	  end
	| _ -> raise Impossible
    in
    mainloop ()
  with
    | Impossible _ -> let _ = flash () in mainloop ()
    | Quit _ -> ()

let _ = 
  let () = do_init win in
  let () = board_skeleton symbols 0 0 in
  let () = update_cursor 0 0 in
  let () = draw_position () in
  let _ = mainloop () in
  endwin ()

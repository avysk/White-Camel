open Utils
open Types
open Curses
open Acs
open Position

type cursor_t = {
  mutable x : int ;
  mutable y : int
}

let cursor = { x = 0; y = 0 }

let win = initscr ()
let symbols = get_acs_codes ()

let space = int_of_char ' '
let more = int_of_char '>'
let less = int_of_char '<'

let do_init () =
  let _ = noecho () in
  let _ = curs_set 0 in
  let _ = start_color () in
  let _ = use_default_colors () in
  let _ = init_pair 0 0 (-1) in
  let _ = init_pair 1 Color.red (-1)  in
  ()

let normal_colors () = attrset (A.color_pair 0)
let red_pieces () = attrset (A.color_pair 1 lor A.bold)
let bold () = attrset (A.color_pair 0 lor A.bold)

let cur_pos = ref start_position

let board_skeleton y x =
  let _ = normal_colors () in
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
  ()

let empty_cell () =
  let _ = normal_colors () in
  let _ = hline symbols.bullet 2 in
  ()

let draw_piece x y pc (* board coordinates, not cursed coordinates *) =
  let cursed_y = 9 - 2 * y in
  let cursed_x = 2 + 5 * x in
  let _ = move cursed_y cursed_x in
  match pc with
    | None -> empty_cell ()
    | Some (s, p) ->
      let _ =
	begin
	  match s with
	    | Sente -> red_pieces ()
	    | Gote -> normal_colors ()
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

let update_cursor x y =
  let x' = cursor.x in
  let y' = cursor.y in
  let _ = cursor.x <- x in
  let _ = cursor.y <- y in
  let cy n = 9 - 2 * n in
  let cx1 n = 5 * n + 1 in
  let cx2 n = 5 * n + 4 in
  let _ = bold () in
  let _ = mvaddch (cy y') (cx1 x') space in
  let _ = mvaddch (cy y') (cx2 x') space in
  let _ = mvaddch (cy y) (cx1 x) more in
  let _ = mvaddch (cy y) (cx2 x) less in
  ()

let draw_position () =
  let brd = !cur_pos.Types.board  in
  let _ = 
    for i = 0 to 4 do
      for j = 0 to 4 do
	draw_piece i j brd.(i).(j)
      done
    done in
  ()

type keybindings = {
  quit : int ;
  up : int ;
  down : int ;
  left : int ;
  right : int 
}

let cmd = {
  quit = int_of_char 'Q' ;
  up = int_of_char 'k' ;
  down = int_of_char 'j' ;
  left = int_of_char 'h' ;
  right = int_of_char 'l'
}

exception Quit

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
	| _ -> () in
    mainloop()
  with Quit _ -> ()

let _ = 
  let () = do_init () in
  let () = board_skeleton 0 0 in
  let () = update_cursor 0 0 in
  let () = draw_position () in
  let _ = mainloop () in
  endwin ()

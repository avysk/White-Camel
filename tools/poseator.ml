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
  let _ = mvaddstr 12 0 "                        " in (* 24 spaces *)
  let _ = move 12 0 in
  let _ = List.iter
    (fun p -> let _ = addstr " " in show_piece (Some (Sente, p)))
    !cur_pos.sente_hand in
  let _ = mvaddstr 13 0 "                        " in (* 24 spaces *)
  let _ = move 13 0 in
  let _ = List.iter
    (fun p -> let _ = addstr " " in show_piece (Some (Gote, p)))
    !cur_pos.gote_hand in
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
	  !cur_pos.Types.board.(cursor.x).(cursor.y) <- p ;
	  match p with
	    | Some (Sente, King) -> cur_pos := {!cur_pos with sente_king = (cursor.x, cursor.y)}
	    | Some (Gote, King) -> cur_pos := {!cur_pos with gote_king = (cursor.x, cursor.y)}
	    | _ -> ()
	end in
      draw_position ()
    | _, _ -> raise Impossible

let curs_to_hand () =
  let _ =  match !cur_pos.Types.board.(cursor.x).(cursor.y) with
    | None -> raise Impossible
    | Some (_, King) -> raise Impossible
    | Some (s, pc) ->
	!cur_pos.Types.board.(cursor.x).(cursor.y) <- None ;
	let shand, ghand =
	  begin
	    match s with
	      | Sente -> !cur_pos.sente_hand, basic_state pc :: !cur_pos.gote_hand
	      | Gote -> basic_state pc :: !cur_pos.sente_hand, !cur_pos.gote_hand
	  end in
	cur_pos := {!cur_pos with sente_hand = shand; gote_hand = ghand}
  in
  draw_position ()

let drop_from_hand () =
  if !cur_pos.Types.board.(cursor.x).(cursor.y) != None
  (* Not possible to drop if there's a piece under cursor already *)
  then raise Impossible
  else
    let shand = !cur_pos.sente_hand in
    let ghand = !cur_pos.gote_hand in
    let choose_side () =
      match getch () with
	| c when c = int_of_char 'g' -> Gote
	| c when c = int_of_char 's' -> Sente 
	| _ -> raise Impossible in
    let choose_piece lst =
      let pc = match getch () with
	| c when c = int_of_char 'p' -> Pawn
	| c when c = int_of_char 's' -> Silver
	| c when c = int_of_char 'g' -> Gold
	| c when c = int_of_char 'b' -> Bishop
	| c when c = int_of_char 'r' -> Rook
	| _ -> raise Impossible
      in
      if List.mem pc lst
      then pc
      else raise Impossible (* Cannot drop piece not in hand *)
    in
    let s, p = match (List.length shand), (List.length ghand) with
      | 0, 0 -> raise Impossible (* both hands are empty, cannot drop *)
      | 1, 0 -> Sente, List.hd shand (* only one piece in sente hand, gote hand is empty *)
      | _, 0 -> Sente, choose_piece shand (* chooee sente piece, gote hand is empty *)
      | 0, 1 -> Gote, List.hd ghand (* only one piece in gote hand, sente hand is empty *)
      | 0, _ -> Gote, choose_piece ghand (* choose gote piece, sente hand is empty *)
      | 1, 1 -> (* choose sente/gote, only one piece in both hands *)
	let sd = choose_side () in
	sd, List.hd (if sd = Sente then shand else ghand)
      | 1, _ -> (* choose sente/gote; if gote choose the piece *)
	let sd = choose_side () in
	sd, if sd = Sente then List.hd shand else choose_piece ghand
      | _, 1 -> (* choose sente/gote; if sente choose the piece *)
	let sd = choose_side () in
	sd, if sd = Sente then choose_piece shand else List.hd ghand
      | _, _ -> (* choose sente/gote then choose piece *)
	let sd = choose_side () in
	sd, choose_piece (if sd = Sente then shand else ghand)
    in
    let _ = begin
      !cur_pos.Types.board.(cursor.x).(cursor.y) <- Some (s, p) ;
      let filter_one_if_two elt lst =
	(* if we remove two elts from lst, put one back *)
	let tmp = List.filter ((!=) elt) lst in
	if List.length lst - List.length tmp = 1
	then tmp
	else elt :: tmp
      in
      match s with
	| Sente -> cur_pos := {!cur_pos with sente_hand = filter_one_if_two p shand}
	| Gote -> cur_pos := {!cur_pos with gote_hand = filter_one_if_two p ghand}
    end in
    draw_position ()

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
	| c when c = cmd.to_hand -> curs_to_hand ()
	| c when c = cmd.from_hand -> drop_from_hand ()
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

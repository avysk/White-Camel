open Curses
open Acs

open Utils
open Types
open Rules
open Position
open Check

open Pos_keybindings
open Pos_board_skeleton
open Pos_curses_utils
open Pos_cursor
open Pos_piece

let buf = ref (None : cell)
let cur_pos = ref start_position

let status_line = 19
let sente_hand_line = 12
let gote_hand_line = 13

let clear_line n =
  let _ = move n 0 in
  let _ = deleteln () in
  let _ = insertln () in
  ()

let clear_status () = clear_line status_line

let set_status str =
  let _ = clear_status () in
  let () = normal () in
  let _ = mvaddstr status_line 0 str in
  ()

let draw_hand = function
  | Sente ->
      let () = clear_line sente_hand_line in
      let _ =
        List.iter (fun p -> let _ = addstr " " in show_piece (Some (Sente, p)))
                  !cur_pos.sente_hand in
      ()
  | Gote ->
      let () = clear_line gote_hand_line in
      let _ =
        List.iter (fun p -> let _ = addstr " " in show_piece (Some (Gote, p)))
                  !cur_pos.gote_hand in
      ()

let draw_position () =
  let brd = !cur_pos.Types.board  in
  let _ =
    for i = 0 to 4 do
      for j = 0 to 4 do
        draw_piece i j brd.(i).(j)
      done
    done in
  let _ = move to_move_y to_move_x in
  let _ =
    match !cur_pos.to_move with
    | Sente -> let () = red () in addstr "Sente"
    | Gote -> let () = normal () in addstr " Gote" in
  let _ = move buffer_y buffer_x in
  let _ = show_piece !buf in
  let () = draw_hand Sente in
  let () = draw_hand Gote in
  ()

let curs_switch_color () =
  match !cur_pos.Types.board.(cursor.x).(cursor.y) with
  | None -> failwith "No piece to switch color"
  | Some (s, p) when p = King -> failwith "Cannot change king's color"
  | Some (s, p) ->
      let _ = !cur_pos.Types.board.(cursor.x).(cursor.y) <- Some (other s, p) in
      draw_position ()

let curs_turnover () =
  match !cur_pos.Types.board.(cursor.x).(cursor.y) with
  | None -> failwith "No piece to turn over"
  | Some (s, p) ->
      (* NB: turnover p may raise Failure *)
      let _ = !cur_pos.Types.board.(cursor.x).(cursor.y) <- Some (s, turnover p) in
      draw_position ()

let take_or_place () =
  match !buf, !cur_pos.Types.board.(cursor.x).(cursor.y) with
  | None, None -> failwith "No piece on board and no piece in buffer"
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
          | Some (Sente, King) ->
              cur_pos := {!cur_pos with sente_king = (cursor.x, cursor.y)}
          | Some (Gote, King) ->
              cur_pos := {!cur_pos with gote_king = (cursor.x, cursor.y)}
          | _ -> ()
        end in
      draw_position ()
          | _, _ -> failwith "Buffer is not empty but there's already piece on board"

let curs_to_hand () =
  let _ = 
    match !cur_pos.Types.board.(cursor.x).(cursor.y) with
    | None -> failwith "No piece on board to put in hand"
    | Some (_, King) -> failwith "Cannot put king in hand"
    | Some (s, pc) ->
        !cur_pos.Types.board.(cursor.x).(cursor.y) <- None ;
        let shand, ghand =
          begin
            match s with
            | Sente -> !cur_pos.sente_hand, basic_state pc :: !cur_pos.gote_hand
            | Gote -> basic_state pc :: !cur_pos.sente_hand, !cur_pos.gote_hand
          end in
        cur_pos := {!cur_pos with sente_hand = shand; gote_hand = ghand} in
  draw_position ()

let drop_from_hand () =
  if !cur_pos.Types.board.(cursor.x).(cursor.y) != None
  then failwith "Cannot drop over another piece"
  else
    let shand = !cur_pos.sente_hand in
    let ghand = !cur_pos.gote_hand in
    let choose_side () =
      let () = set_status "(s)ente or (g)ote" in
      match getch () with
      | c when c = int_of_char 'g' -> Gote
      | c when c = int_of_char 's' -> Sente 
      | _ -> failwith "No sente or gote was chosen" in
    let choose_piece lst =
      let () = set_status "(p)awn, (s)ilver, (g)old, (b)ishop, or (r)ook" in
      let pc =
        match getch () with
        | c when c = int_of_char 'p' -> Pawn
        | c when c = int_of_char 's' -> Silver
        | c when c = int_of_char 'g' -> Gold
        | c when c = int_of_char 'b' -> Bishop
        | c when c = int_of_char 'r' -> Rook
        | _ -> failwith "No pawn, silver, gold, bishop or rook was chosen" in
      if List.mem pc lst then pc else failwith "Chosen piece is not in hand" in
    let s, p =
      match (List.length shand), (List.length ghand) with
      | 0, 0 -> failwith "Both hands are empty, cannot drop."
      | 1, 0 -> Sente, List.hd shand
      | _, 0 -> Sente, choose_piece shand
      | 0, 1 -> Gote, List.hd ghand
      | 0, _ -> Gote, choose_piece ghand
      | 1, 1 ->
          let sd = choose_side () in
          sd, List.hd (if sd = Sente then shand else ghand)
      | 1, _ ->
          let sd = choose_side () in
          sd, if sd = Sente then List.hd shand else choose_piece ghand
      | _, 1 ->
          let sd = choose_side () in
          sd, if sd = Sente then choose_piece shand else List.hd ghand
      | _, _ ->
          let sd = choose_side () in
          sd, choose_piece (if sd = Sente then shand else ghand)
      in
    let _ =
      begin
        !cur_pos.Types.board.(cursor.x).(cursor.y) <- Some (s, p) ;
        match s with
        | Sente -> cur_pos := {!cur_pos with sente_hand = remove_one p shand}
        | Gote -> cur_pos := {!cur_pos with gote_hand = remove_one p ghand}
      end in
    draw_position ()

let verify () =
  if !buf != None
  then failwith "Buffer is not empty"
  else 
    let res1 =
      if under_check !cur_pos Sente then "Sente is under check " else "Sente: ok " in
    let res2 =
      if under_check !cur_pos Gote then "Gote is under check" else "Gote: ok" in
    failwith (res1 ^ res2)

let rec mainloop () =
  try
    let _ =
      match getch () with
      | c when c = cmd.quit -> raise Exit
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
      | c when c = cmd.verify -> verify ()
      | _ -> failwith "Unknown command"
    in
    let () = clear_status () in
    mainloop ()
  with
  | Failure reason ->
      let _ = flash () in
      let () = set_status reason
      in mainloop ()
  | Exit -> ()

let _ =
  let win = initscr () in
  let () = do_init win in
  let () = board_skeleton 0 0 in
  let () = update_cursor 0 0 in
  let () = draw_position () in
  let _ = mainloop () in
  endwin ()

(*
vim:sw=2
*)

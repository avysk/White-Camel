open Utils
open Types
open Rules

type hand = piece_t list
type position = {
  board : board_t ;
  to_move : side ;
  sente_hand : hand ;
  gote_hand : hand ;
  (* The coordinates of kings are needed often *)
  sente_king : int * int ;
  gote_king : int * int ;
}

let under_check_close position side =
  let fw, king = match side with
    | Sente -> 1, position.sente_king
    | Gote -> -1, position.gote_king in
  let side' = other side in
  let brd = position.board in
  let piece_at delta pcs =
    try
      begin
	match brd @@ (king ++ delta) with
	  | Some (s, p) when s = side' -> List.mem p pcs
	  | _ -> false
      end
    with Invalid_argument _ -> false in
  (* forward from king *)
  piece_at (0, fw) forward_attackers ||
    (* forward diagonals from king *)
    piece_at (-1, fw) forward_diag_attackers ||
    piece_at (1, fw) forward_diag_attackers ||
    (* sideways from king *)
    piece_at (-1, 0) sideways_attackers ||
    piece_at (1, 0) sideways_attackers ||
    (* backward from king *)
    piece_at (0, -fw) backward_attackers ||
    (* backward diagonals from king *)
    piece_at (-1, -fw) backward_diag_attackers ||
    piece_at (1, -fw) backward_diag_attackers

let under_check_far position side =
  let rec piece_along dx dy = assert false
in
  assert false

let under_check position side =
  under_check_close position side || under_check_far position side

let init_position plist stm shd ghd =
  let ar = Array.make_matrix 5 5 None in
  let put_piece (x, y, p) = ar.(x).(y) <- Some p in
  let () = List.iter put_piece plist in
  {
    board = ar ;
    to_move = stm ;
    sente_hand = shd ;
    gote_hand = ghd ;
    sente_king = (0, 0) ;
    gote_king = (4, 4) ;
  }

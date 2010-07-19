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
  let fw, (kx, ky) = match side with
    | Sente -> 1, position.sente_king
    | Gote -> -1, position.gote_king in
  let side' = other side in
  let brd = position.board in
  let piece_at x y pcs =
    try
      begin
	match brd.(x).(y) with
	  | Some (s, p) when s = side' -> List.mem p pcs
	  | _ -> false
      end
    with Invalid_argument _ -> false in
  (* forward from king *)
  piece_at kx (ky + fw) forward_attackers ||
    (* backward from king *)
    piece_at kx (ky - fw) [King; Gold; Tokin; GoldS; DragonHorse] ||
    (* sideways from king *)
    piece_at (kx - 1) ky [King; Gold; Tokin; GoldS; DragonHorse] ||
    piece_at (kx + 1) ky [King; Gold; Tokin; GoldS; DragonHorse] ||
    (* forward diagonals from king *)
    piece_at (kx - 1) (ky + fw) [King; Gold; Silver; Tokin; GoldS; DragonKing] ||
    piece_at (kx + 1) (ky + fw) [King; Gold; Silver; Tokin; GoldS; DragonKing] ||
    (* backward diagonals from king *)
    piece_at (kx - 1) (ky - fw) [King; Silver; DragonKing] ||
    piece_at (kx + 1) (ky - fw) [King; Silver; DragonKing]

let under_check_far position side =
  let piece_along dx dy = assert false in
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

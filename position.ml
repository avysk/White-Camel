open Utils
open Types
open Rules

let under_check_close brd side' king =
  (* Checks if the king is attacked via one-step move by some side' piece *)
  let fw = match side' with
    (* Meaning of "forward" for the king *)
    | Sente -> -1
    | Gote -> 1 in
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

let under_check_far brd side' king =
  let rec piece_along current delta pcs =
    try
      begin
	let next = current ++ delta in
	match brd @@ next with
	  (* If cell is empty, continue search *)
	  | None -> piece_along next delta pcs
	  (* If there's a piece in a cell, check if is the one we're searching for *)
	  | Some (s, p) when s = side' -> List.mem p pcs
	  (* Piece belonging to the other side block checks *)
	  | _ -> false
      end
    (* If board is over, no attack from this line *)
    with Invalid_argument _ -> false in
  piece_along king (0, 1) straight_sliders ||
    piece_along king (0, -1) straight_sliders ||
    piece_along king (1, 0) straight_sliders ||
    piece_along king (-1, 0) straight_sliders ||
    piece_along king (1, 1) diag_sliders ||
    piece_along king (-1, 1) diag_sliders ||
    piece_along king (1, -1) diag_sliders ||
    piece_along king (-1, -1) diag_sliders

let under_check position side =
  let king = match side with
    | Sente -> position.sente_king
    | Gote -> position.gote_king in
  let side' = other side in
  let brd = position.board in
  under_check_close brd side' king || under_check_far brd side' king

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

let apply_move position move =
  let (pc, st, fn) = move in
  let brd = position.board in
  let mv = position.to_move in
  let shand = position.sente_hand in
  let ghand = position.gote_hand in
  let sking = position.sente_king in
  let gking = position.gote_king in
  match st with
    (* drop move *)
    | None -> assert false
    (* normal move *)
    | _ ->
      begin
	match pc with
	  (* king's move *)
	  | (side, King) -> assert false
	  (* other move *)
	  | _  -> assert false
      end

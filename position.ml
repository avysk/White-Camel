open Utils

(* Checks if the king is attacked via one-step move by some side' piece *)
let under_check_close brd side' king =
  let fw =
    match side' with
    (* Meaning of "forward" for the king *)
    | Types.Sente -> -1
    | Types.Gote -> 1
  in
  let piece_at delta pcs =
    try
      begin
        match brd @@ (king ++ delta) with
        | Some (s, p) when s = side' -> List.mem p pcs
        | _ -> false
      end
    with Invalid_argument _ -> false
  in
  (* forward from king *)
  piece_at (0, fw) Rules.forward_attackers ||
  (* forward diagonals from king *)
  piece_at (-1, fw) Rules.forward_diag_attackers ||
  piece_at (1, fw) Rules.forward_diag_attackers ||
  (* sideways from king *)
  piece_at (-1, 0) Rules.sideways_attackers ||
  piece_at (1, 0) Rules.sideways_attackers ||
  (* backward from king *)
  piece_at (0, -fw) Rules.backward_attackers ||
  (* backward diagonals from king *)
  piece_at (-1, -fw) Rules.backward_diag_attackers ||
  piece_at (1, -fw) Rules.backward_diag_attackers

let under_check_far brd side' king =
  let rec piece_along current delta pcs =
    try
      begin
        let next = current ++ delta in
        match brd @@ next with
        (* If cell is empty, continue search *)
        | None -> piece_along next delta pcs
        (* If there's a piece in a cell, check if is the one
         * we're searching for *)
        | Some (s, p) when s = side' -> List.mem p pcs
        (* Piece belonging to the other side block checks *)
        | _ -> false
      end
    (* If board is over, no attack from this line *)
    with Invalid_argument _ -> false
  in
  piece_along king (0, 1) Rules.straight_sliders ||
  piece_along king (0, -1) Rules.straight_sliders ||
  piece_along king (1, 0) Rules.straight_sliders ||
  piece_along king (-1, 0) Rules.straight_sliders ||
  piece_along king (1, 1) Rules.diag_sliders ||
  piece_along king (-1, 1) Rules.diag_sliders ||
  piece_along king (1, -1) Rules.diag_sliders ||
  piece_along king (-1, -1) Rules.diag_sliders

let under_check position side =
  let king =
    match side with
    | Types.Sente -> position.Types.sente_king
    | Types.Gote -> position.Types.gote_king
  in
  let side' = Types.other side in
  let brd = position.Types.board in
  under_check_close brd side' king || under_check_far brd side' king

let init_position plist stm shd ghd =
  let ar = Array.make_matrix 5 5 None in
  let put_piece (x, y, p) = ar.(x).(y) <- Some p in
  let () = List.iter put_piece plist in
  {
    Types.board = ar ;
    to_move = stm ;
    sente_hand = shd ;
    gote_hand = ghd ;
    sente_king = (0, 0) ;
    gote_king = (4, 4) ;
  }

let start_position =
  init_position [(0, 0, (Types.Sente, Types.King));
                 (1, 0, (Types.Sente, Types.Gold));
                 (2, 0, (Types.Sente, Types.Silver));
                 (3, 0, (Types.Sente, Types.Bishop));
                 (4, 0, (Types.Sente, Types.Rook));
                 (0, 1, (Types.Sente, Types.Pawn));
                 (4, 3, (Types.Gote, Types.Pawn));
                 (0, 4, (Types.Gote, Types.Rook));
                 (1, 4, (Types.Gote, Types.Bishop));
                 (2, 4, (Types.Gote, Types.Silver));
                 (3, 4, (Types.Gote, Types.Gold));
                 (4, 4, (Types.Gote, Types.King))] Types.Sente [] []

(*
 vim:sw=2
 *)

open Utils
open Types

let do_or_false brd f = do_or_default brd f false

(* Checks if the king is attacked via one-step move by some side' piece *)
let under_check_close brd side' king =
  let fw =
    match side' with
    (* Meaning of "forward" for the king *)
    | Sente -> -1
    | Gote -> 1
  in
  let piece_at delta pcs =
    let chk_pc = function
      | Some (s, p) when s = side' -> List.mem p pcs
      | _ -> false
    in
    do_or_false brd chk_pc (king ++ delta)
(*
    try
      begin
        match brd @@ (king ++ delta) with
        | Some (s, p) when s = side' -> List.mem p pcs
        | _ -> false
      end
    with Invalid_argument _ -> false
*)
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
  (* this closure can be evaluated early *)
  let do_or_false_brd = do_or_false brd in
  let rec piece_along current delta pcs =
    let next = current ++ delta in
    let chk_pc = function
      (* If cell is empty, continue search *)
      | None -> piece_along next delta pcs
      (* If there's a piece in a cell, check if is the one
       * we're searching for *)
      | Some (s, p) when s = side' -> List.mem p pcs
      (* Piece belonging to the other side block checks *)
      | _ -> false
    in
    do_or_false_brd chk_pc next
(*
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
*)
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
    | Sente -> position.sente_king
    | Gote -> position.gote_king
  in
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
    prev_move = no_move ;
    evaluation = not_evaluated ;
  }

let start_position =
  init_position [(0, 0, (Sente, King));
                 (1, 0, (Sente, Gold));
                 (2, 0, (Sente, Silver));
                 (3, 0, (Sente, Bishop));
                 (4, 0, (Sente, Rook));
                 (0, 1, (Sente, Pawn));
                 (4, 3, (Gote, Pawn));
                 (0, 4, (Gote, Rook));
                 (1, 4, (Gote, Bishop));
                 (2, 4, (Gote, Silver));
                 (3, 4, (Gote, Gold));
                 (4, 4, (Gote, King))] Sente [] []

(*
 vim:sw=2
 *)

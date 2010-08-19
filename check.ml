open Utils
open Types

let do_or_false brd f = do_or_default brd f false

let under_check_close brd side' king =
(* Checks if the king is attacked via one-step move by some side' piece *)
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
      (* If there's a piece in a cell, check if is the one we're searching for *)
      | Some (s, p) when s = side' -> List.mem p pcs
      (* Piece belonging to the other side block checks *)
      | _ -> false
    in
    do_or_false_brd chk_pc next
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

(*
 * vim:sw=2
 *)

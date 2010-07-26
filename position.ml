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

let apply_move position move =
  let (pc, st, (fx, fy)) = move in
  let brd' = copy_board position.Types.board in
  let mv = position.Types.to_move in
  let shand = position.Types.sente_hand in
  let ghand = position.Types.gote_hand in
  let sking = position.Types.sente_king in
  let gking = position.Types.gote_king in
  match st with
  (* drop move *)
  | None ->
      begin
        match mv with
        | Types.Sente ->
            let shand' = remove_one pc shand in
            let _ = brd'.(fx).(fy) <- Some (Types.Sente, pc) in
            {position with Types.board = brd';
             to_move = Types.Gote; sente_hand = shand'}
        | Types.Gote ->
            let ghand' = remove_one pc ghand in
            let _ = brd'.(fx).(fy) <- Some (Types.Gote, pc) in
            {position with Types.board = brd';
             to_move = Types.Sente; gote_hand = ghand'}
      end
  (* normal move *)
  | Some (sx, sy) ->
      begin
        let sking', gking' =
          if pc != Types.King then sking, gking
          else begin
            match mv with
            | Types.Sente -> (fx, fy), gking
            | Types.Gote -> sking, (fx, fy)
          end in
        let _ = brd'.(sx).(sy) <- None in
        let _ = brd'.(fx).(fy) <- Some (mv, pc) in
        let shand', ghand' =
          begin
            match position.Types.board.(fx).(fy) with
            | None -> shand, ghand
            | Some (Types.Sente, tpc) -> shand, tpc :: ghand
            | Some (Types.Gote, tpc) -> tpc :: shand, ghand
          end in
        { Types.board = brd';
          to_move = Types.other mv;
          sente_king = sking';
          gote_king = gking';
          sente_hand = shand';
          gote_hand = ghand'}
      end

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

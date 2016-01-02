open Utils
open Types

let apply_move position move =
  let brd' = copy_board position.board in
  let shand = position.sente_hand in
  let ghand = position.gote_hand in
  let sking = position.sente_king in
  let gking = position.gote_king in
  let { what = (mv, pc) ; start = start ; finish = (fx, fy) } = move in
  match start with
  (* drop move *)
  | None ->
      begin
        match mv with
        | Sente ->
            let shand' = remove_one pc shand in
            let _ = brd'.(fx).(fy) <- Some (Sente, pc) in
            {position with board = brd';
             to_move = Gote; sente_hand = shand';
             evaluation = not_evaluated; prev_move = move}
        | Gote ->
            let ghand' = remove_one pc ghand in
            let _ = brd'.(fx).(fy) <- Some (Gote, pc) in
            {position with board = brd';
             to_move = Sente; gote_hand = ghand';
             evaluation = not_evaluated; prev_move = move}
      end
  (* normal move *)
  | Some (sx, sy) ->
      begin
        let sking', gking' =
          if pc != King then sking, gking
          else begin
            match mv with
            | Sente -> (fx, fy), gking
            | Gote -> sking, (fx, fy)
          end in
        let _ = brd'.(sx).(sy) <- None in
        let _ = brd'.(fx).(fy) <- Some (mv, pc) in
        let shand', ghand' =
          begin
            match position.board.(fx).(fy) with
            | None -> shand, ghand
            | Some (Sente, tpc) -> shand, tpc :: ghand
            | Some (Gote, tpc) -> tpc :: shand, ghand
          end in
        { board = brd' ;
          to_move = other mv ;
          sente_king = sking' ;
          gote_king = gking' ;
          sente_hand = shand' ;
          gote_hand = ghand' ;
          prev_move = move ;
          evaluation = not_evaluated}
      end

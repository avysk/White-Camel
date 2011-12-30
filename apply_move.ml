open Utils
open Types

let apply_move position move =
  let brd' = copy_board position.board in
  let shand = position.sente_hand in
  let ghand = position.gote_hand in
  let sking = position.sente_king in
  let gking = position.gote_king in
  let old_hash = position.hash in
  let zh = Zobrist.update_hand in
  let zb = Zobirst.update_board in
  let { what = (mv, pc) ; start ; finish = (fx, fy) } = move in
  match start with
  (* drop move *)
  | None ->
      begin
        match mv with
        | Sente ->
            let shand' = remove_one pc shand in
            let _ = brd'.(fx).(fy) <- Some (Sente, pc) in
            let new_hash = (zb (Sente, pc) fx fy $ zh (Sente, pc)) old_hash in
            {position with board = brd';
             to_move = Gote; sente_hand = shand';
             evaluation = not_evaluated; prev_move = move;
             hash = new_hash}
        | Gote ->
            let ghand' = remove_one pc ghand in
            let _ = brd'.(fx).(fy) <- Some (Gote, pc) in
            let hash_tmp = Zobrist.update_hand old_hash (Gote, pc) in
            let new_hash = Zobrist.update_board hash_tmp (Gote, pc) fx fy in
            {position with board = brd';
             to_move = Sente; gote_hand = ghand';
             evaluation = not_evaluated; prev_move = move;
             hash = new_hash}
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
        let hash_tmp = Zobrist.update_hanh (Zobrist.update_hand
                                              old_hash

        let shand', ghand', new_hash =
          begin
            match position.board.(fx).(fy) with
            | None -> shand, ghand, hash_tmp
            | Some (Sente, tpc) ->
                shand, tpc :: ghand, Zobrist.update_hand hash_tmp (Gote, tpc)
            | Some (Gote, tpc) ->
                tpc :: shand, ghand, Zobrist.update_hand hash_tmp (Sente, tpc)
          end in
        { board = brd' ;
          to_move = other mv ;
          sente_king = sking' ;
          gote_king = gking' ;
          sente_hand = shand' ;
          gote_hand = ghand' ;
          prev_move = move ;
          evaluation = not_evaluated ;
          hash = new_hash}
      end

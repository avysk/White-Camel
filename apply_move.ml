open Utils

let apply_move position move =
  let {Types.what=(mv, pc); start=st; finish=(fx, fy)} = move in
  let brd' = copy_board position.Types.board in
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
             to_move = Types.Gote; sente_hand = shand';
             evaluation = Types.not_evaluated; prev_move = move}
        | Types.Gote ->
            let ghand' = remove_one pc ghand in
            let _ = brd'.(fx).(fy) <- Some (Types.Gote, pc) in
            {position with Types.board = brd';
             to_move = Types.Sente; gote_hand = ghand';
             evaluation = Types.not_evaluated; prev_move = move}
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
        { Types.board = brd' ;
          to_move = Types.other mv ;
          sente_king = sking' ;
          gote_king = gking' ;
          sente_hand = shand' ;
          gote_hand = ghand' ;
          prev_move = move ;
          evaluation = Types.not_evaluated}
      end

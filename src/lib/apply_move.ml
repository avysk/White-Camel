open Batteries
open Utils
open Types

let apply_move position move =
  let revert_side = function
    | Sente -> Gote
    | Gote -> Sente in
  let brd' = copy_board position.board in
  let shand = position.sente_hand in
  let ghand = position.gote_hand in
  let sking = position.sente_king in
  let gking = position.gote_king in
  let old_hash = position.hash in
  let zh = Zobrist.update_hand in
  let zb = Zobrist.update_board in
  let { what = (mv, pc) ; start = start; finish = (fx, fy) } = move in
  match start with
  (* drop move *)
  | None ->
      begin
        let new_hash_hand = zh old_hash (mv, pc) in
        let new_hash = zb new_hash_hand (mv, pc) fx fy in
        let () = brd'.(fx).(fy) <- Some (mv, pc) in
        match mv with
          | Sente ->
              {position with
                board = brd';
                to_move = revert_side mv;
                sente_hand = List.remove shand pc;
                evaluation = not_evaluated;
                prev_move = move;
                hash = new_hash}
          | Gote ->
              {position with
                board = brd';
                to_move = revert_side mv;
                gote_hand = List.remove ghand pc;
                evaluation = not_evaluated;
                prev_move = move;
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
        let new_hash_board_from = zb old_hash (mv, pc) sx sy in
        let new_hash_board_to = zb new_hash_board_from (mv, pc) fx fy in
        let shand', ghand', new_hash =
          begin
            match position.board.(fx).(fy) with
            | None -> shand, ghand, new_hash_board_to
            | Some (side, tpc) ->
                let tpc' = Rules.basic_state tpc in
                (* Remove piece from board *)
                let new_hash_removed = zb new_hash_board_to (side, tpc) fx fy in
                (* Add it to hand *)
                let new_hash_hand = zh new_hash_removed (mv, tpc') in
                match side with
                  | Sente -> shand, tpc' :: ghand, new_hash_hand
                  | Gote -> tpc' :: shand, ghand, new_hash_hand
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

open Utils
open Types

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
    hash = Zobrist.zhash ar shd ghd ;
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

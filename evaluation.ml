open Utils
open Types

exception Checkmated


let better_for_sente ev1 ev2 =
  match ev1, ev2 with
  | Gote_won, _ -> ev2
  | Sente_won, _ -> ev1
  | _, Gote_won -> ev1
  | _, Sente_won -> ev2
  | Eval e1, Eval e2 -> Eval (max e1 e2)

let better_for_gote ev1 ev2 =
  match ev1, ev2 with
  | Gote_won, _ -> ev1
  | Sente_won, _ -> ev2
  | _, Gote_won -> ev2
  | _, Sente_won -> ev1
  | Eval e1, Eval e2 -> Eval (min e1 e2)

let better = function
  | Sente -> better_for_sente
  | Gote -> better_for_gote

let evaluate_current_board pos = 
  (* TODO: This is a main thing to determine the playing strength. Do it better! *)
  let brd = pos.board in
  let lst = board_to_list brd in
  let _addv acc = (+) acc $ Dna.piece_value in
  let ev_brd = List.fold_left _addv 0 lst in
  let ev_sh = Dna.hand_value pos.sente_hand in
  let ev_gh = Dna.hand_value pos.gote_hand in
  ev_brd + ev_sh - ev_gh

let we_lost = function
  | Sente -> Gote_won
  | Gote -> Sente_won

let we_won = function
  | Sente -> Sente_won
  | Gote -> Gote_won

let rec update_evaluation depth tree =
  (* Side effects! 'Evaluation' field in tree positions may be updated! *)
  let Gametree.Gametree (pos, branches) = tree in
  match pos.evaluation with
  (* nothing to do in case of a terminal position *)
  | (Sente_won, _) -> ()
  | (Gote_won, _) -> ()
  (* if position never was evaluated and we do not want evaluating branches,
   * evaluate the board position and that's it *)
  | e when e = not_evaluated && depth = 0 ->
      pos.evaluation <-
        (Eval (evaluate_current_board pos), Depth 0)
  (* if we already have better evaluation than needed, nothing to do *)
  | (_, Depth d) when d >= depth -> ()
  | (Eval e, Depth d) ->
      (* NB: this includes not_evaluated case *)
      if List.length (Lazy.force branches) = 0
      (* ILLEGAL_PAWN_DROP *)
      (* if checkmated, check if it happened by illegal pawn drop move *)
      then
        let pm = pos.prev_move in
        let pc = snd pm.what in
        let st = pm.start in
        if pc = Pawn && st = None
        then pos.evaluation <- (we_won pos.to_move, Depth 0)
        else
        (* No, checkmated legally, terminal position. Raise exception to
         * indicate no need to evaluate other moves one level up *)
        pos.evaluation <- (we_lost pos.to_move, Depth 0) ;
        raise Checkmated
      else
        begin
          try
            (* Update evaluation in branches *)
            let () = List.iter (update_evaluation (depth - 1)) (Lazy.force branches) in
            let side = pos.to_move in
            let bcmp x = function
              | Gametree.Gametree (p, _) ->
                  (better side) x (fst p.evaluation)
            in
            let min_eval = we_lost side in
            (* Find the best move *)
            let e = List.fold_left bcmp min_eval (Lazy.force branches) in
            pos.evaluation <- (e, Depth depth)
          with Checkmated ->
            (* One of the moves checkmates the opponent. No need to evaluate
             * other branches. *)
            pos.evaluation <- (we_won pos.to_move, Depth 1)
        end


(*
 * vim:sw=2
 *)

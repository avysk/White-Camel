let better_for_sente ev1 ev2 =
  match ev1, ev2 with
  | Gote_won, _ -> ev2
  | Sente_won, _ -> ev1
  | _, Gote_won -> ev1
  | _, Sente_won -> ev2
  | Eval of e1, Eval of e2 -> Eval of (max e1 e2)

let better_for_gote ev1 ev2 =
  match ev1, ev2 with
  | Gote_won, _ -> ev1
  | Sente_won, _ -> ev2
  | _, Gote_won -> ev2
  | _, Sente_won -> ev1
  | Eval of e1, Eval of e2 -> Eval of (min e1 e2)

let better = function
  | Types.Sente -> better_for_sente
  | Types.Gote -> better_for_gote

let evaluate_board board side = 0 (* FIXME *)

exception Checkmated

let current_lost = function
  | Types.Sente -> Gote_won
  | Types.Gote -> Sente_won

let current_won = function
  | Types.Sente -> Sente_won
  | Types.Gote -> Gote_won

let rec update_evaluation depth =
  (* Side effects! 'Evaluation' field in tree positions may be updated! *)
  let Gametree (pos, branches) = tree in
  match pos.evaluation with
  (* nothing to do in case of a terminal position *)
  | (Sente_won, _) -> ()
  | (Gote_won, _) -> ()
  (* if position never was evaluated and we do not want evaluating branches,
   * evaluate the board position and that's it *)
  | (_, -1) when d = 0 ->
      pos.evaluation <- (Eval (evaluate_board pos.board, side), 0)
  (* if we already have better evaluation than needed, nothing to do *)
  | (_, Depth d) when d >= depth -> ()
  | (Eval e, Depth d) ->
      if List.length (Lazy.force branches) = 0
      (* if checkmated, check if it happened by illegal pawn drop move *)
      then
        (* if pawn drop move then
          * begin
            * pos.evaluation <- (Someone_won, 0) ;
            * raise (Won 0);
          * end
          * else
            *)
        (* No, checkmated legally, terminal position. Raise exception to
         * indicate no need to evaluate other moves one level up *)
        pos.evaluation <- (current_lost, 0) ; raise Checkmated
      else
        begin
          try
            (* Update evaluation in branches *)
            let () = List.iter (update_evaluation (depth - 1)) (Lazy.force branches) in
            let side = pos.to_move in
            let bcmp x y = (better side) x y.evaluation in
            let min_eval = current_lost side in
            (* Find the best move *)
            let e = List.fold_left bcmp min_eval (Lazy.force branches) in
            pos.evaluation <- (e, depth)
          with Checkmated ->
            (* One of the moves checkmates the opponent. No need to evaluate
             * other branches. *)
            pos.evaluation <- (current_won, 1)
        end


(*
 * vim:sw=2
 *)

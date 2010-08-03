open Utils

let better_for_sente ev1 ev2 =
  match ev1, ev2 with
  | Types.Gote_won, _ -> ev2
  | Types.Sente_won, _ -> ev1
  | _, Types.Gote_won -> ev1
  | _, Types.Sente_won -> ev2
  | Types.Eval e1, Types.Eval e2 -> Types.Eval (max e1 e2)

let better_for_gote ev1 ev2 =
  match ev1, ev2 with
  | Types.Gote_won, _ -> ev1
  | Types.Sente_won, _ -> ev2
  | _, Types.Gote_won -> ev2
  | _, Types.Sente_won -> ev1
  | Types.Eval e1, Types.Eval e2 -> Types.Eval (min e1 e2)

let better = function
  | Types.Sente -> better_for_sente
  | Types.Gote -> better_for_gote

let evaluate_right_away pos = 
  let brd = pos.Types.board in
  let tmp1 = Array.map Array.to_list brd in
  let tmp2 = Array.to_list tmp1 in
  let lst = List.flatten tmp2 in
  let _addv acc = (+) acc $ Dna.piece_value in
  let ev_brd = List.fold_left _addv 0 lst in
  let ev_sh = Dna.hand_value pos.Types.sente_hand in
  let ev_gh = Dna.hand_value pos.Types.gote_hand in
  ev_brd + ev_sh - ev_gh

exception Checkmated

let current_lost = function
  | Types.Sente -> Types.Gote_won
  | Types.Gote -> Types.Sente_won

let current_won = function
  | Types.Sente -> Types.Sente_won
  | Types.Gote -> Types.Gote_won

let rec update_evaluation depth tree =
  (* Side effects! 'Types.Evaluation' field in tree positions may be updated! *)
  let Gametree.Gametree (pos, branches) = tree in
  match pos.Types.evaluation with
  (* nothing to do in case of a terminal position *)
  | (Types.Sente_won, _) -> ()
  | (Types.Gote_won, _) -> ()
  (* if position never was evaluated and we do not want evaluating branches,
   * evaluate the board position and that's it *)
  | e when e = Types.not_evaluated && depth = 0 ->
      pos.Types.evaluation <-
        (Types.Eval (evaluate_right_away pos), Types.Depth 0)
  (* if we already have better evaluation than needed, nothing to do *)
  | (_, Types.Depth d) when d >= depth -> ()
  | (Types.Eval e, Types.Depth d) ->
      if List.length (Lazy.force branches) = 0
      (* if checkmated, check if it happened by illegal pawn drop move *)
      then
        let pm = pos.Types.prev_move in
        let pc = snd pm.Types.what in
        let st = pm.Types.start in
        if pc = Types.Pawn && st = None
        then pos.Types.evaluation <- (current_won pos.Types.to_move, Types.Depth 0)
        else
        (* No, checkmated legally, terminal position. Raise exception to
         * indicate no need to evaluate other moves one level up *)
        pos.Types.evaluation <- (current_lost pos.Types.to_move, Types.Depth 0) ;
        raise Checkmated
      else
        begin
          try
            (* Update evaluation in branches *)
            let () = List.iter (update_evaluation (depth - 1)) (Lazy.force branches) in
            let side = pos.Types.to_move in
            let bcmp x = function
              | Gametree.Gametree (p, _) ->
                  (better side) x (fst p.Types.evaluation)
            in
            let min_eval = current_lost side in
            (* Find the best move *)
            let e = List.fold_left bcmp min_eval (Lazy.force branches) in
            pos.Types.evaluation <- (e, Types.Depth depth)
          with Checkmated ->
            (* One of the moves checkmates the opponent. No need to evaluate
             * other branches. *)
            pos.Types.evaluation <- (current_won pos.Types.to_move, Types.Depth 1)
        end


(*
 * vim:sw=2
 *)

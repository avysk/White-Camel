open Utils
open Types

exception Checkmated

let piece_t_to_string = function
  | Pawn -> "Pn"
  | King -> "Kg"
  | Gold -> "Gd"
  | Silver -> "Sr"
  | Bishop -> "Bp"
  | Rook -> "Rk"
  | Tokin -> "Tn"
  | GoldS -> "Gs"
  | DragonKing -> "DK"
  | DragonHorse -> "DH"

let color_to_string = function
  | Sente -> "_"
  | Gote -> "*"

let pts = function
  | None -> "   "
  | Some (sd, pct) -> color_to_string sd ^ (piece_t_to_string pct)

let print_piece pc (x, y) =
  let ofile = open_out_gen [Open_append] 0666 "/tmp/out" in
  let _ = Printf.fprintf ofile "Taking %s at %d %d\n" (pts pc) x y in
  close_out ofile

let print_position pos =
  let ofile = open_out_gen [Open_append] 0666 "/tmp/out" in
  let brd = pos.board in
  begin
    Printf.fprintf ofile "+---+---+---+---+---+\n" ;
    for row = 4 downto 0 do
      (* Printf.printf "|   |   |   |   |   |\n" ; *)
      Printf.fprintf ofile "|%s|%s|%s|%s|%s|\n"
      (pts brd.(0).(row))
      (pts brd.(1).(row))
      (pts brd.(2).(row))
      (pts brd.(3).(row))
      (pts brd.(4).(row)) ;
      (* Printf.printf "|   |   |   |   |   |\n" ; *)
      Printf.fprintf ofile "+---+---+---+---+---+\n" ;
    done ;
    let ppc = Printf.fprintf ofile " %s" $ piece_t_to_string in
    begin
      Printf.fprintf ofile "Sente hand:" ;
      List.iter ppc pos.sente_hand ;
      Printf.fprintf ofile"\nGote hand:" ;
      List.iter ppc pos.gote_hand ;
    end ;
    Printf.fprintf ofile"\n%s to move.\n" (
      match pos.to_move with
      | Sente -> "Sente"
      | Gote -> "Gote") ;
    close_out ofile
  end

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
      begin
        let lst = Lazy.force branches in
        let side = pos.to_move in
        let bcmp x = function
          | Gametree.Gametree (p, _) ->
              (better side) x (fst p.evaluation) in
        let taking_move = function
          | Gametree.Gametree (p, _) ->
(*
              Check.under_check p p.to_move ||
*)
              (List.length pos.sente_hand) +
              (List.length pos.gote_hand) <
              (List.length p.sente_hand) +
              (List.length p.gote_hand) in
(*
        let checking_move = function
          | Gametree.Gametree (p, _) ->
              Check.under_check p p.to_move in
*)
        let ev = List.fold_left
        (fun e t ->
          if not (taking_move t)
          then e
(*
            begin 
              if not (checking_move t) then e
              else
                let () = update_evaluation 1 t in
                bcmp e t
            end
*)
          else
(*
            let _ = print_position pos in
            let Gametree.Gametree (p, _) = t in
            let _ = print_piece (pos.board @@ p.prev_move.finish) p.prev_move.finish in
            let _ = print_position p in
*)
            let () = update_evaluation 0 t in
            bcmp e t) 
          (Eval (evaluate_current_board pos))
          lst in
        pos.evaluation <- (ev, Depth 0)
      end
  (* if we already have better evaluation than needed, nothing to do *)
  | (_, Depth d) when d >= depth -> ()
  | (Eval e, Depth d) ->
      (* NB: this includes not_evaluated case *)
      if List.length (Lazy.force branches) = 0
      (* ILLEGAL_PAWN_DROP *)
      (* if checkmated, check if it happened by illegal pawn drop move *)
      (* FIXME: THIS CODE IS BUGGY! The bug is unlikely to manifest, but
       * if the position with checkmated pawn drop can be later achieved by
       * normal pawn move, it will be rejected. *)
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



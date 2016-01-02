open Utils
open Types

(* Return the list of the moves the given piece may do from 'point' by moving
 * one stop to point'. Expects that the point' is inside the board and cl is the
 * cell value at point'. Returns the empty list when the move is blocked by own
 * piece. Otherwise returns the list of one or two (when promotion is available)
 * moves. *)
let check_step (brd, side, piece, point) point' = function
  | Some (x, _) when x = side -> [] (* cannot eat own piece *)
  | _ ->
      let st = Some point in
      let normal_move = {what = piece; start = st; finish = point'} in
      let promotion_move_lst =
        let j = snd point in
        let nj = snd point' in
        if side = Sente && nj < 4 && j < 4 ||
        side = Gote && nj > 0 && j > 0
        then [] (* the move is not to or from promotion area *)
        else
          match snd piece with (* promotion is possible *)
          | Pawn ->
              [{what = (side, Rules.turnover Pawn);
              start = st; finish = point'}]
          | Silver ->
              [{what = (side, Rules.turnover Silver);
              start = st; finish = point'}]
          | Bishop ->
              [{what = (side, Rules.turnover Bishop);
              start = st; finish = point'}]
          | Rook ->
              [{what = (side, Rules.turnover Rook);
              start = st; finish = point'}]
          | _ -> [] (* nothing else can be promoted *)
      in
      normal_move :: promotion_move_lst

(* Construct the list of all sliding moves of the given piece from 'point'
 * along 'delta' vector.  NB: the 'start' value in returned moves
 * may be wrong since it may not be the real start value for the move.
 * So the 'start' value should be fixed by calling function. *)
let rec check_slide_r ?(acc=[]) (brd, side, piece, point) delta =
  let point' = point ++ delta in
  let chk_mv cl =
    let one = check_step (brd, side, piece, point) point' cl in
    (* record found one-step moves *)
    let acc' = one :: acc in
    (* if we moved to empty cell, continue search *)
    if cl = None then check_slide_r ~acc:acc' (brd, side, piece, point') delta
    (* otherwise we hit own or opponent's piece so we are done *)
    else acc'
  in
  do_or_default brd chk_mv acc point'

(* Return the list of all sliding moves in the given situation
 * (meaning given piece on the board) along delta vector *)
let check_slide situation delta =
  let (_, _, piece, point) = situation in
  (* 'start' value in the moves, returned by check_slide_r, may be wrong,
   * so it should be fixed here *)
  let fix_move m =
    match m.start with
    | Some x when x = point -> m
    | _ -> {what = piece; start = Some point; finish = m.finish } in
  let sliding_moves = check_slide_r situation delta in
  List.map fix_move (List.flatten sliding_moves)

(* Return the list of moves for the given situation (piece on board)
 * according to the move rule mv *)
let check_one_rule situation = function
  | (Step, delta) ->
      let (brd, _, _, point) = situation in
      let point' = point ++ delta in
      do_or_default brd (check_step situation point') [] point'
  | (Slide, delta) -> check_slide situation delta

(* Generate the list of all moves of the given piece at the given point.
 * Move validity (check situation) is not checked. *)
let moves_for_piece situation =
  let (_, _, piece, _) = situation in
  let pm = Rules.possible_moves piece in
  List.flatten (List.map (fun x -> check_one_rule situation x) pm)

(* Generate the list of all possible drops from the given hand
 * to the 'point' square. Move validity (check situation, pawn drops)
 * is not checked. To find how illegal pawn drops are dealt with, grep
 * ILLEGAL_PAWN_DROP *)
let generate_drops hand side point =
  let drop1 piece = {what = (side, piece);
                     start = None; finish = point} in
  List.map drop1 hand

(* Recursive helper function: Generate all moves in the given position for the
 * given side to move.  The validity of moves (check situation, pawn drops) is
 * not checked.  Includes but does not force promotions. *)
let rec find_all_moves_r acc brd point hand side =
  let (nx, ny) as next = incr point in
  if ny = 5 then acc
  else
    match brd @@ point with
    | None ->
        let drops = generate_drops hand side point in
        find_all_moves_r (drops @ acc) brd next hand side
    | Some ((s, p) as piece) when s = side ->
        let mvs = moves_for_piece (brd, side, piece, point) in
        find_all_moves_r (mvs @ acc) brd next hand side
    (* we can move pieces only of own color *)
    | _ -> find_all_moves_r acc brd next hand side

(* Externally visible function.  Generate all moves in the given position for
 * the given side to move.  The validity of moves (check situation, pawn drops)
 * is not checked.  Includes but does not force promotions. *)
let find_all_moves pos side =
  let hand =
    if side = Sente then pos.sente_hand else pos.gote_hand in
  find_all_moves_r [] pos.board (0, 0) hand side

(*
vim:sw=2
*)

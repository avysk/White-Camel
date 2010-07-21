open Utils
open Types
open Rules

let check_step (brd, side, piece, point) delta =

  (* Return the list of the moves the given piece may do
   * from 'point' by moving one step along 'delta' vector.
   * Raises 'Invalid argument' when the move is out of the board's borders.
   * Raises 'Not found' when the move is blocked by own piece.
   * Otherwise returns the list of one or two (when promotion is available)
   * moves. *)

  let point' = point ++ delta in
  let t = brd @@ point' in (* may raise Invalid_argument here *)
  match t with
    | Some (x, _) when x = side -> raise Not_found (* cannot eat own piece *)
    | _ ->
      let st = Some point in
      (* The move without promotion *)
      {what = piece; start = st; finish = point'} ::
	(* Adding possible promotion move *)
        let j = snd point in 
	let nj = snd point' in
	if side = Sente && nj < 4 && j < 4 || side = Gote && nj > 0 && j > 0
	then [] (* the move is not to or from promotion area *)
	else match snd piece with (* promotion is possible *)
          | Pawn -> [{what = (side, turnover Pawn); start = st; finish = point'}]
          | Silver -> [{what = (side, turnover Silver); start = st; finish = point'}]
          | Bishop -> [{what = (side, turnover Bishop); start = st; finish = point'}]
          | Rook -> [{what = (side, turnover Rook); start = st; finish = point'}]
          | _ -> [] (* nothing else can be promoted *)
            
let rec check_slide_r acc (brd, side, piece, point) delta =

  (* Construct the list of all sliding moves of the given piece from 'point'
   * along 'delta' vector.  NB: the 'start' value in returned moves
   * may be wrong since it may not be the real start value for the move.
   * So the 'start' value should be fixed by calling function. *)

  try let one = check_step (brd, side, piece, point) delta in
      let acc' = one :: acc in
      let point' = point ++ delta in
      if brd @@ point' = None
      then check_slide_r acc' (brd, side, piece, point') delta
      else acc'
  with
    (* If we moved past the border of the board , stop searching. *)
    | Invalid_argument _ -> acc
    (* If further moves are blocked by own piece, stop searching. *)
    | Not_found _ -> acc

let check_slide situation delta =

  (* Return the list of all sliding moves in the given situation
   * (meaning given piece on the board) along delta vector *)

  let (_, _, piece, point) = situation in
  (* 'start' value in the moves, returned by check_slide_r, may be wrong,
   * so it should be fixed here *)
  let fix_move m =
    match m.start with
      | Some x when x = point -> m
      | _ -> {what = piece; start = Some point; finish = m.finish } in
  let sliding_moves = check_slide_r [] situation delta in
  List.map fix_move (List.flatten sliding_moves)

let check_one_rule situation mv =

  (* Return the list of moves for the given situation (piece on board)
   * according to the move rule mv *)

  match mv with
    | (Step, delta) ->
      begin
        try check_step situation delta
        with
	  | Invalid_argument _ ->
	    [] (* the move is out of the board's borders *)
	  | Not_found _ ->
	    [] (* the move is blocked by own piece *)
      end
    | (Slide, delta) -> check_slide situation delta

let moves_for_piece situation =

  (* Generate the list of all moves of the given piece at the given point.
   * Move validity (check situation) is not checked. *)

  let (_, _, piece, _) = situation in
  let pm = possible_moves piece in
  List.flatten (List.map (fun x -> check_one_rule situation x) pm)

let generate_drops hand side point =

  (* Generate the list of all possible drops from the given hand
   * to the 'point' square. Move validity (check situation, pawn drops)
   * is not checked. *)

  let drop1 piece = { what = (side, piece); start = None; finish = point} in
  List.map drop1 hand

let rec find_all_moves_r acc brd point hand side =

  (* Recursive helper function:
   * Generate all moves in the given position for the given side to move.
   * The validity of moves (check situation, pawn drops) is not checked.
   * Includes but does not force promotions. *)

  try
    let next = incr point in
    match brd @@ point with (* may raise Invalid_argument *)
      | None ->
        let drops = generate_drops hand side point in
        find_all_moves_r (drops @ acc) brd next hand side
      | Some ((s, p) as piece) when s = side ->
	let mvs = moves_for_piece (brd, side, piece, point) in
	find_all_moves_r (mvs @ acc) brd next hand side
	(* we can move pieces only of own color *)
      | _ -> find_all_moves_r acc brd next hand side
  with Invalid_argument _ -> acc (* We came to the 6th row, so finished *)

let find_all_moves pos side =

  (* Externally visible function.
   * Generate all moves in the given position for the given side to move.
   * The validity of moves (check situation, pawn drops) is not checked.
   * Includes but does not force promotions. *)

  let hand = if side = Sente then pos.sente_hand else pos.gote_hand in
  find_all_moves_r [] pos.board (0, 0) hand side

(* -*- mode: tuareg; indent-tabs-mode: nil -*- *)

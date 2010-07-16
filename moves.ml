open Utils
open Types
open Position

type move = {
  what : piece ;
  start : (int * int) option ;
  finish : int * int ;
}

type _sliding = Slide | Step

(* "Forward" for sente side *)
let m_sente = [(0, 1)]
(* "Forward" for gote side *)
let m_gote = [(0, -1)]
(* "Forward diagonals" for sente side *)
let m_sente_diag = [(-1, 1); (1, 1)]
(* "Forward diagonals" for gote side *)
let m_gote_diag = [(-1, -1); (1, -1)]
(* "Sideways" *)
let m_sides = [(-1, 0); (1, 0)]
(* "Diagonally" *)
let m_diag = m_sente_diag @ m_gote_diag
(* "Vertically or horizontally" *)
let m_raw = m_sente @ m_gote @ m_sides

(* '@*' operator constructs the list of tuples as direct product of
   the element and the list *)

(* Pawn moves one step forward *)
let mv_sente_pawn = Step @* m_sente
let mv_gote_pawn = Step @* m_gote

(* King moves one step vertically or horizontaly or diagonaly *)
let mv_king = Step @* (m_diag @ m_raw)

(* Gold general moves vertically or horizontally or forward diagonally *)
let mv_sente_gold = Step @* (m_raw @ m_sente_diag)
let mv_gote_gold =  Step @* (m_raw @ m_gote_diag)

(* Silver general moves diagonally and forward *)
let mv_sente_silver = Step @* (m_diag @ m_sente)
let mv_gote_silver = Step @* (m_diag @ m_gote)

(* Bishop moves diagnally, sliding *)
let mv_bishop = Slide @* m_diag

(* Rook moves horizontally or vertiacally, sliding *)
let mv_rook = Slide @* m_raw

(* Dragon horse (promoted bishop) moves like bishop or one step
   horizontally or vertically *)
let mv_dragonhorse = mv_bishop @ (Step @* m_raw)

(* Dragon king (promoted rook) moves like rook or one step diagonally *)
let mv_dragonking = mv_rook @ (Step @* m_diag)

let possible_moves = function
  | Sente, Pawn -> mv_sente_pawn
  | Gote, Pawn -> mv_gote_pawn
  | _, King -> mv_king
  | Sente, Silver -> mv_sente_silver
  | Gote, Silver -> mv_gote_silver
  | _, Bishop -> mv_bishop
  | _, Rook -> mv_rook
  | _, DragonHorse -> mv_dragonhorse
  | _, DragonKing -> mv_dragonking
(* what's left: gold generals, tokins and promoted silvers,
   all of them move as gold generals *)
  | Sente, _ -> mv_sente_gold
  | Gote, _ -> mv_gote_gold

let check_step (brd, side, piece, (i, j)) (dx, dy) =

  (* Returns the list of the moves the given piece may do
   * from (i, j) point by moving one step along (dx, dy) vector.
   * Raises 'Invalid argument' when the move is out of the board's borders.
   * Raises 'Not found' when the move is blocked by own piece.
   * Otherwise returns the list of one or two (when promotion is available)
   * moves. *)

  let ni, nj = i + dx, j + dy in
  let t = brd.(ni).(nj) in (* may raise Invalid_argument here *)
  match t with
    | Some (x, _) when x = side -> raise Not_found (* cannot eat own piece *)
    | _ ->
      let st = Some (i, j) in
      let fn = (ni, nj) in

      (* The move without promotion *)
      {what = piece; start = st; finish = fn} ::

          (* Adding possible promotion move *)
	  if side = Sente && nj < 4 && j < 4 || side = Gote && nj > 0 && j > 0
	  then [] (* the move is not to or from promotion area *)
	  else match snd piece with (* promotion is possible *)
            | Pawn -> [{what = (side, Tokin); start = st; finish = fn}]
            | Silver -> [{what = (side, GoldS); start = st; finish = fn}]
            | Bishop -> [{what = (side, DragonHorse); start = st; finish = fn}]
            | Rook -> [{what = (side, DragonKing); start = st; finish = fn}]
            | _ -> [] (* nothing else can be promoted *)
          
let rec check_slide_r acc (brd, side, piece, (i, j)) (dx, dy) =

  (* Constructs the list of all sliding moves of the given piece from (i, j)
   * along (dx, dy) vector.  NB: the 'start' value in returned moves
   * may be wrong since it may not be the real start value for the move.
   * So the 'start' value should be fixed by calling function. *)

  try let one = check_step (brd, side, piece, (i, j)) (dx, dy) in
      let acc' = one :: acc in
      let ni, nj = i + dx, j + dy in
      if brd.(ni).(nj) = None
      then check_slide_r acc' (brd, side, piece, (ni, nj)) (dx, dy)
      else acc'
  with
    (* If we moved past the border of the board , stop searching. *)
    | Invalid_argument _ -> acc
    (* If further moves are blocked by own piece, stop searching. *)
    | Not_found _ -> acc


let check_slide situation delta =

  (* Returns the list of all sliding moves in the given situation
   * (meaning given piece on the board) along delta vector *)

  let (_, _, piece, (i, j)) = situation in
  (* 'start' value in themoves, returned by check_slide_r, may be wrong,
   * so it should be fixed here *)
  let fix_move m =
    match m.start with
      | Some (x, y) when x = i && y = j -> m
      | _ -> {what = piece; start = Some (i, j); finish = m.finish } in
  let sliding_moves = check_slide_r [] situation delta in
  List.map fix_move (List.flatten sliding_moves)


let check_one_dir situation mv =

  (* Returns the list of moves for the given situation (piece on board)
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
  (* generate the list of all moves of the given piece at the given point *)
  let (_, _, piece, _) = situation in
  let pm = possible_moves piece in
  List.flatten (List.map (fun x -> check_one_dir situation x) pm)

let generate_drops hand side point =
  (* generate the list of all possible drops to the 'point' square *)
  let drop1 pc =
    { what = (side, pc); start = None; finish = point} in
  List.map drop1 hand

let incr = function
  | (4, j) -> (0, j + 1)
  | (i, j) -> (i + 1, j)

let rec find_all_moves_r acc brd (i, j) hand side =
  try
    let next = incr (i, j) in
    match brd.(i).(j) with
      | None ->
        let drops = generate_drops hand side (i, j) in
        find_all_moves_r (drops @ acc) brd next hand side
    | Some (s, p) when s = side ->
      let mvs = moves_for_piece (brd, side, (s, p), (i, j)) in
      find_all_moves_r (mvs @ acc) brd next hand side
    (* we can move pieces only of own color *)
    | _ -> find_all_moves_r acc brd next hand side
  with Invalid_argument _ -> acc (* We came to the 6th row, so finished *)

let find_all_moves pos side =
  let hand = if side = Sente then pos.sente_hand else pos.gote_hand in
  find_all_moves_r [] pos.board (0, 0) hand side

(* -*- mode: tuareg; indent-tabs-mode: nil -*- *)

(* -------------------- Mini-shogi specific -------------------- *)
type piece_t = Pawn | King | Gold | Silver | Bishop | Rook | Tokin | GoldS | DragonKing | DragonHorse
type side = Sente | Gote
let other = function
  | Sente -> Gote
  | Gote -> Sente
type piece = side * piece_t
type cell = piece option
type board_t = cell array array
type hand = piece_t list
type position = {
  board : board_t ;
  to_move : side ;
  sente_hand : hand ;
  gote_hand : hand ;
}

(* -------------------- Utilities -------------------- *)

(* function composition *)
let ($) f g x = f (g x)

(* constructing tuples of list *)
let (@*) elt lst = List.map (fun t -> (elt, t)) lst

(* -------------------- Position handling --------------------*)
let init_position plist stm shd ghd =
  let ar = Array.make_matrix 5 5 None in
  let put_piece (x, y, p) = ar.(x).(y) <- Some p in
  let () = List.iter put_piece plist in
  {
    board = ar ;
    to_move = stm ;
    sente_hand = shd ;
    gote_hand = ghd ;
  }

(* -------------------- Moves -------------------- *)

type move = {
  what : piece ;
  start : (int * int) option ;
  finish : int * int ;
}

type sliding = Slide | JustOne

let m_sente = [(0, 1)]
let m_gote = [(0, -1)]
let m_sente_diag = [(-1, 1); (1, 1)]
let m_gote_diag = [(-1, -1); (1, -1)]
let m_sides = [(-1, 0); (1, 0)]
let m_diag = m_sente_diag @ m_gote_diag
let m_raw = m_sente @ m_gote @ m_sides

let mv_sente_pawn = JustOne @* m_sente
let mv_gote_pawn = JustOne @* m_gote
let mv_king = JustOne @* (m_diag @ m_raw)
let mv_sente_gold = JustOne @* (m_raw @ m_sente_diag)
let mv_gote_gold =  JustOne @* (m_raw @ m_gote_diag)
let mv_sente_silver = JustOne @* (m_diag @ m_sente)
let mv_gote_silver = JustOne @* (m_diag @ m_gote)
let mv_bishop = Slide @* m_diag
let mv_rook = Slide @* m_raw
(*
let mv_sente_tokin = mv_sente_gold
let mv_gote_tokin = mv_gote_gold
let mv_sente_golds = mv_sente_gold
let mv_gote_golds = mv_gote_gold
*)
let mv_dragonking = mv_bishop @ (JustOne @* m_raw)
let mv_dragonhorse = mv_rook @ (JustOne @* m_diag)

let possible_moves = function
  | Sente, Pawn -> mv_sente_pawn
  | Gote, Pawn -> mv_gote_pawn
  | _, King -> mv_king
  | Sente, Silver -> mv_sente_silver
  | Gote, Silver -> mv_gote_silver
  | _, Bishop -> mv_bishop
  | _, Rook -> mv_rook
  | _, DragonKing -> mv_dragonking
  | _, DragonHorse -> mv_dragonhorse
(* what's left: gold generals, tokins and promoted silvers,
   all move as gold generals *)
  | Sente, _ -> mv_sente_gold
  | Gote, _ -> mv_gote_gold

let check_no_slide (board : cell array array) side piece i j (dx, dy) =
  (* May throw 'Invalid_argument' if move is out of the board's borders *)
  let ni = i + dx in
  let nj = j + dy in
  let t = board.(ni).(nj) in
  match t with
    | Some (x, _) when x = side -> [] (* cannot eat own piece *)
    | _ ->
      let st = Some (i, j) in
      let fn = (ni, nj) in
      if (side = Sente && nj < 4 && j < 4) || (side = Gote && nj > 0 && j > 0)
      then (* the move is not to or from promotion area *)
	[{what = piece; start = st; finish = fn}]
      else (* promotion is possible *)
	begin
	  match snd piece with
	    | Pawn ->
	      [{what = (side, Tokin); start = st; finish = fn}]
	    | Silver -> (* It may make sense not to promote silver general *)
	      [{what = (side, Silver); start = st; finish = fn};
	       {what = (side, GoldS); start = st; finish = fn}]
	    | Bishop ->
	      [{what = (side, DragonKing); start = st; finish = fn}]
	    | Rook ->
	      [{what = (side, DragonHorse); start = st; finish = fn}]
	    | _ -> (* nothing else can be promoted *)
	      [{what = piece; start = st; finish = fn}]
	end (* possible promotion *)
	  
let rec check_slide_r found board side piece i j (dx, dy) =
  (* the 'start' value in returned moves is wrong *)
  try
    let one = check_no_slide board side piece i j (dx, dy) in
    let ni = i + dx in
    let nj = j + dy in
    let res = one::found in
    if board.(ni).(nj) = None
    then check_slide_r res board side piece (i+dx) (j+dy) (dx, dy)
    else res
  with Invalid_argument _ -> found

let check_slide board side piece i j delta =
  let fix_move m =
    match m.start with
      | Some (x, y) when (x = i && y = j) -> m
      | _ -> {what = piece; start = Some (i, j); finish = m.finish } in
  let sliding_moves = check_slide_r [] board side piece i j delta in
  List.map fix_move (List.flatten sliding_moves)

let check_one (board : cell array array) side piece i j mv =
  match mv with
    | (JustOne, (dx, dy)) ->
      begin
	try check_no_slide board side piece i j (dx, dy)
	with Invalid_argument _ -> [] (* the move is out of the board's borders *)
      end
    | (Slide, (dx, dy)) -> check_slide board side piece i j (dx, dy)

let rec check_opm_r found (board : cell array array) side piece i j pm =
    match pm with
      | [] -> found
      | hd :: tl ->
	check_opm_r ((check_one board side piece i j hd) @ found) board side piece i j tl

let check_opm (board : cell array array) side piece i j pm =
  check_opm_r [] board side piece i j pm 
;;
let gopm (board : cell array array) side piece i j =
  let pm = possible_moves piece in
  check_opm board side piece i j pm
;;
let generate_drops hand side i j =
  let generate_one pc =
    { what = (side, pc); start = None; finish = (i, j)} in
  List.map generate_one hand
;;
let rec find_all_moves_r found brd (i, j) hand side = 
  if j == 5
  then found
  else let incr (i, j) =
	 begin
	   match (i, j) with
	     | (4, _) -> (0, j+1)
	     | _ -> (i+1, j)
	 end
       in
       match brd.(i).(j) with
 	 | None ->
	   let drops = generate_drops hand side i j in
	   find_all_moves_r (drops @ found) brd (incr (i, j)) hand side
	 | Some (s, p) when s = side ->
	   let mvs = gopm brd side (s, p) i j in
	   find_all_moves_r (mvs @ found) brd (incr (i, j)) hand side
	 (* and if not our color... *)
	 | _ -> find_all_moves_r found brd (incr (i, j)) hand side

let find_all_moves pos side =
  let hand = if side == Sente then pos.sente_hand else pos.gote_hand in
  let board = pos.board in
  find_all_moves_r [] board (0, 0) hand side
  

;; (* ---------------------------------------- *)

let print_position pos =
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
    | DragonHorse -> "DH" in
  let color_to_string = function
    | Sente -> "_"
    | Gote -> "*" in
  let pts = function
    | None -> "   "
    | Some (sd, pct) -> color_to_string sd ^ (piece_t_to_string pct) in
  let brd = pos.board in
  begin
    Printf.printf "+---+---+---+---+---+\n" ;
    for row = 4 downto 0 do
      (* Printf.printf "|   |   |   |   |   |\n" ; *)
      Printf.printf "|%s|%s|%s|%s|%s|\n"
	(pts brd.(0).(row))
	(pts brd.(1).(row))
	(pts brd.(2).(row))
	(pts brd.(3).(row))
	(pts brd.(4).(row)) ;
      (* Printf.printf "|   |   |   |   |   |\n" ; *)
      Printf.printf "+---+---+---+---+---+\n" ;
    done ;
    let ppc = Printf.printf " %s" $ piece_t_to_string in
    begin
      Printf.printf "Sente hand:" ;
      List.iter ppc pos.sente_hand ;
      Printf.printf "\nGote hand:" ;
      List.iter ppc pos.gote_hand ;
    end ;
    Printf.printf "\n%s to move.\n" (
      match pos.to_move with
	| Sente -> "Sente"
	| Gote -> "Gote")
  end

let start_position = init_position
  [(0, 0, (Sente, King));
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
   (4, 4, (Gote, King))]
  Sente [] []


let under_check pos side = assert false

let find_moves pos side = []

let won_position pos side = (List.length $ find_moves pos $ other) side == 0

;;
print_position start_position 

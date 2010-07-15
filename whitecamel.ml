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

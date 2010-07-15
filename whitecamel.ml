open Utils
open Types
open Position
open Moves


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
open Utils
open Types

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
    | DragonHorse -> "DH"
  in
  let color_to_string = function
    | Sente -> "_"
    | Gote -> "*"
  in
  let pts = function
    | None -> "   "
    | Some (sd, pct) -> color_to_string sd ^ (piece_t_to_string pct)
  in
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

;;

Gc.set { (Gc.get ()) with
         Gc.minor_heap_size = 100000000;
         Gc.major_heap_increment = 25000000 }

;;

let gt = Gametree.create_gametree Position.start_position in
Evaluation.update_evaluation 3 gt

(*
vim:sw=2
*)

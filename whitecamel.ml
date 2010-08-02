open Utils

let print_position pos =
  let piece_t_to_string = function
    | Types.Pawn -> "Pn"
    | Types.King -> "Kg"
    | Types.Gold -> "Gd"
    | Types.Silver -> "Sr"
    | Types.Bishop -> "Bp"
    | Types.Rook -> "Rk"
    | Types.Tokin -> "Tn"
    | Types.GoldS -> "Gs"
    | Types.DragonKing -> "DK"
    | Types.DragonHorse -> "DH"
  in
  let color_to_string = function
    | Types.Sente -> "_"
    | Types.Gote -> "*"
  in
  let pts = function
    | None -> "   "
    | Some (sd, pct) -> color_to_string sd ^ (piece_t_to_string pct)
  in
  let brd = pos.Types.board in
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
      List.iter ppc pos.Types.sente_hand ;
      Printf.printf "\nGote hand:" ;
      List.iter ppc pos.Types.gote_hand ;
    end ;
    Printf.printf "\n%s to move.\n" (
      match pos.Types.to_move with
      | Types.Sente -> "Sente"
      | Types.Gote -> "Gote")
    end

(* FIXME *)
let find_moves pos side = []

let won_position pos side =
  (List.length $ find_moves pos $ Types.other) side == 0

let () = print_position Position.start_position
;;

let gt = Gametree.create_gametree Position.start_position in
let Gametree.Gametree (_, brl) = gt in
let brlf = Lazy.force brl in
List.iter (fun (Gametree.Gametree (pp, _)) -> print_position pp) brlf
(*
let fb = List.hd brlf in
let Gametree.Gametree (fp, _) = fb in
print_position fp
*)

(*
vim:sw=2
*)

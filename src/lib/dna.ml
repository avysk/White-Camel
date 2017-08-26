open BatPervasives
open Utils
open Types

let _pv = function
  | Pawn -> 10
  | Gold -> 60
  | Silver -> 50
  | Bishop -> 70
  | Rook -> 80
  | Tokin -> 60
  | GoldS -> 60
  | DragonHorse -> 110
  | DragonKing -> 120
  | King -> 0

let hand_value =
  let _addv acc = (+) acc % _pv in
  List.fold_left _addv 0

let piece_value = function
  | None -> 0
  | Some (Sente, p) -> _pv p
  | Some (Gote, p) -> - (_pv p)

let ten_array = Array.make_matrix 5 5 10

let weights = function
  | _ -> ten_array

let weights_ih = function
  | _ -> 10

(*
 * vim:sw=2
 *)


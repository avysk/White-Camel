open Utils
open Types

let pc_to_num = function
  | Pawn -> 1
  | King -> 2
  | Gold -> 3
  | Silver -> 4
  | Bishop -> 5
  | Rook -> 6
  | Tokin -> 7
  | GoldS -> 8
  | DragonKing -> 9
  | DragonHorse -> 10

let cell_to_num = function
  | None -> 0
  | Some (Sente, p) -> pc_to_num p
  | Some (Gote, p) -> (pc_to_num p) lsl 4

let crc16 =
  let crc_step crc chr =
    let index = (lxor) (crc lsr 8) in
    (crc lsl 8 land 0xFFFF) lxor Crc_table.table.(index chr) in
  (List.fold_left crc_step 0xFFFF)

let pos_crc pos =
  let brd = pos.board in
  (crc16 $ (List.map cell_to_num) $ board_to_list) brd
(*
 * vim:sw=2
 *)

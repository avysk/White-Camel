type piece_t = Pawn | King | Gold | Silver | Bishop | Rook | Tokin | GoldS | DragonKing | DragonHorse

let all_pieces =
  [Pawn; King; Gold; Silver; Bishop; Rook; Tokin; GoldS; DragonKing; DragonHorse]

type side = Sente | Gote
let other = function
  | Sente -> Gote
  | Gote -> Sente

type sliding = Step | Slide

type piece = side * piece_t
type cell = piece option
type board_t = cell array array

type move = {
  what : piece ;
  start : (int * int) option ;
  finish : int * int ;
}

type hand = piece_t list

type position = {
  board : board_t ;
  to_move : side ;
  sente_hand : hand ;
  gote_hand : hand ;
  (* The coordinates of kings are needed often *)
  sente_king : int * int ;
  gote_king : int * int ;
}

(*
vim:sw=2
*)

type piece_t =
  | Pawn
  | King
  | Gold
  | Silver
  | Bishop
  | Rook
  | Tokin
  | GoldS
  | DragonHorse
  | DragonKing

let all_pieces =
  [Pawn; King; Gold; Silver; Bishop; Rook; Tokin; GoldS; DragonHorse; DragonKing]

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

let no_move = { what = (Gote, King); start = None ; finish = (5, 5) }

type hand = piece_t list

type eval_t = Eval of int | Sente_won | Gote_won
type depth_t = Depth of int

let not_evaluated = (Eval 0, Depth (-1))

type position = {
  board : board_t ;
  to_move : side ;
  sente_hand : hand ;
  gote_hand : hand ;
  sente_king : int * int ;
  gote_king : int * int ;
  prev_move : move ;
  mutable evaluation : eval_t * depth_t
}


(*
vim:sw=2
*)

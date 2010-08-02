type piece_t =
  Pawn | King | Gold | Silver | Bishop | Rook | Tokin | GoldS | DragonKing | DragonHorse

val all_pieces : piece_t list

type side = Sente | Gote
val other : side -> side

(* used to separate one-step moves from sliding moves *)
type sliding = Step | Slide

type piece = side * piece_t
type cell = piece option
type board_t = cell array array

type move = {
  (* the piece to move *)
  what : piece ; 
  (* None in case of drop move; the point to move from otherwise *)
  start : (int * int) option ;
  (* The point to move to *)
  finish : int * int ;
}

type hand = piece_t list

type eval_t = Eval of int | Sente_won | Gote_won
type depth_t = Depth of int

val not_evaluated : eval_t * depth_t

type position = {
  board : board_t ;
  to_move : side ;
  sente_hand : hand ;
  gote_hand : hand ;
  (* The coordinates of kings are needed often *)
  sente_king : int * int ;
  gote_king : int * int ;
  mutable evaluation : eval_t * depth_t
}

(*
vim:sw=2
*)

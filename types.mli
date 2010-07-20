type piece_t =
    Pawn | King | Gold | Silver | Bishop | Rook | Tokin | GoldS | DragonKing | DragonHorse
val all_pieces : piece_t list
type side = Sente | Gote
val other : side -> side
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

(* used to separate one-step moves from sliding moves *)
type sliding = Step | Slide

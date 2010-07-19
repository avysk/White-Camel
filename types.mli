type piece_t = Pawn | King | Gold | Silver | Bishop | Rook | Tokin | GoldS | DragonKing | DragonHorse
val all_pieces : piece_t list
type side = Sente | Gote
type sliding = Step | Slide
val other : side -> side
type piece = side * piece_t
type cell = piece option
type board_t = cell array array

type move = {
  what : piece ;
  start : (int * int) option ;
  finish : int * int ;
}

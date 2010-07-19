(* -------------------- Mini-shogi specific -------------------- *)
type piece_t = Pawn | King | Gold | Silver | Bishop | Rook | Tokin | GoldS | DragonKing | DragonHorse
let all_pieces = [Pawn; King; Gold; Silver; Bishop; Rook; Tokin; GoldS; DragonKing; DragonHorse]
type side = Sente | Gote
type sliding = Step | Slide
let other = function
  | Sente -> Gote
  | Gote -> Sente
type piece = side * piece_t
type cell = piece option
type board_t = cell array array

type move = {
  what : piece ;
  start : (int * int) option ;
  finish : int * int ;
}

type piece_t = Pawn | King | Gold | Silver | Bishop | Rook | Tokin | GoldS | DragonKing | DragonHorse
type side = Sente | Gote
val other : side -> side
type piece = side * piece_t
type cell = piece option
type board_t = cell array array

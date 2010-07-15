open Types

type hand = piece_t list
type position = {
  board : board_t ;
  to_move : side ;
  sente_hand : hand ;
  gote_hand : hand ;
}

val init_position : (int * int * piece) list -> side -> hand -> hand -> position

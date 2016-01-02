open Types

val zobrist_hash : board_t -> hand -> hand -> int
val update_board : int -> piece -> int -> int -> int
val update_hand : int -> piece -> int

open Types

val init_position : (int * int * piece) list -> side -> hand -> hand -> position
val start_position : position
val apply_move : position -> move -> position

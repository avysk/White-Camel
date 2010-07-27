open Types

val init_position : (int * int * piece) list -> side -> hand -> hand -> position
val under_check : position -> side -> bool
val start_position : position
val apply_move : position -> move -> position

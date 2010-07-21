open Types

val possible_moves : side * piece_t -> (sliding * (int * int)) list

val forward_attackers : piece_t list
val backward_attackers : piece_t list
val forward_diag_attackers : piece_t list
val backward_diag_attackers: piece_t list
val sideways_attackers : piece_t list
val straight_sliders : piece_t list
val diag_sliders : piece_t list

val turnover : piece_t -> piece_t

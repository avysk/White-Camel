open Types
open Position
open Rules

(* Generate all moves in the given position for the given side to move.  The
 * validity of moves (check situation, pawn drops) is not checked.  Includes but
 * does not force promotions. *)
val find_all_moves : position -> side -> move list

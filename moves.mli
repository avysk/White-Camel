open Types
open Position

type move = {
  what : piece ;
  start : (int * int) option ;
  finish : int * int ;
}

val find_all_moves : position -> side -> move list
(* Generate all moves in the given position for the given side to move.
 * The validity of moves (check situation, pawn drops) is not checked.
 * Includes but does not force promotions. *)

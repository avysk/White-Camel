open Types
open Position

type move = {
  what : piece ;
  start : (int * int) option ;
  finish : int * int ;
}

val find_all_moves : position -> side -> move list

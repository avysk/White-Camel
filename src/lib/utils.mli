(** Different utility functions.
  @author Alexey Vyskubov *)

open Types

(** {2 General utilities} *)

(** Calculate a component-wise sum of two pairs of integers.
@return The pair of sums. *)
val (++) : int * int -> int * int -> int * int

(** {2 Working with a board} *)

(** Calculate next cell on the board.
Given the coordinates of the cell on the board, the funcion finds the next
cell, incrementing column if possible; if not, column is set to 0 and row is
incremented instead. The number of columns is 5; the number of rows is not
limited. Counting starts from 0. Column number is the first element of a pair.
@return the coordinates of the next cell on the board. *)
val incr : int * int -> int * int

(** Access matrix by coordinates tuple.
@return the element of the given matrix indexed by the given integer 2-tuple. *)
val (@@) : 'a array array -> int * int -> 'a

(** Create the deep copy of the given board (which should be an array 5 x N).
@return the deep copy of the given board. *)
val copy_board : 'a array array -> 'a array array

(** Convert board to a list *)
val board_to_list : 'a array array -> 'a list

(** Compare two lists, ignoring the order of elements *)
val (@=@) : 'a list -> 'a list -> bool

(** Do something with board cell *)
val do_or_default : board_t -> (cell -> 'a) -> 'a -> int * int -> 'a

(*
 vim:sw=2
 *)

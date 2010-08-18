(** Contains different utility functions *)

(** Function composition *)
val ($) : ('a -> 'b) -> ('c -> 'a) -> ('c -> 'b)

(** Generate direct product of one element and a list *)
val (@*) : 'a -> ('b list) -> ('a * 'b) list

(** Sum integer tuples *)
val (++) : int * int -> int * int -> int * int

(** Calculate next cell on the board *)
val incr : int * int -> int * int

(** Access matrix by coordinates tuple *)
val (@@) : 'a array array -> int * int -> 'a

(** Copy matrix *)
val copy_board : 'a array array -> 'a array array

(** Remove exactly one element from list which may contain two of those *)
val remove_one : 'a -> 'a list -> 'a list

(** Convert board to a list *)
val board_to_list : 'a array array -> 'a list

(** Compare two lists, ignoring the order of elements *)
val (@=@) : 'a list -> 'a list -> bool

(*
 vim:sw=2
 *)

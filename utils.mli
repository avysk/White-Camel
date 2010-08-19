(** Contains different utility functions *)

open Types

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

(** Remove exactly one element from list which may contain many
    @raise Not_found if [lst] does not contain [elt] *)
val remove_one : ?acc:'a list -> 'a -> 'a list -> 'a list

(** Convert board to a list *)
val board_to_list : 'a array array -> 'a list

(** Compare two lists, ignoring the order of elements *)
val (@=@) : 'a list -> 'a list -> bool

(** Do something with board cell *)
val do_or_default : board_t -> (cell -> 'a) -> 'a -> int * int -> 'a

(*
 vim:sw=2
 *)

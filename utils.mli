val ($) : ('a -> 'b) -> ('c -> 'a) -> ('c -> 'b)
val (@*) : 'a -> ('b list) -> ('a * 'b) list
val (++) : int * int -> int * int -> int * int
val incr : int * int -> int * int
val (@@) : 'a array array -> int * int -> 'a
val copy_board : 'a array array -> 'a array array

(* remove exactly one element from list which may contain two of those *)
val remove_one : 'a -> 'a list -> 'a list
(*
 vim:sw=2
 *)

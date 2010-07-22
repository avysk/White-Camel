val ($) : ('a -> 'b) -> ('c -> 'a) -> ('c -> 'b)
val (@*) : 'a -> ('b list) -> ('a * 'b) list
val (++) : int * int -> int * int -> int * int
val incr : int * int -> int * int
val (@@) : 'a array array -> int * int -> 'a
val copy_board : 'a array array -> 'a array array

(*
 vim:sw=2
 *)

type cursor_t = {
  mutable x : int ;
  mutable y : int
}

val cursor : cursor_t

val update_cursor : int -> int -> unit

(*
 vim:sw=2
 *)

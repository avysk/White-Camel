(* function composition *)
let ($) f g x = f (g x)

(* constructing tuples of list *)
let ( @* ) elt lst = List.map (fun t -> (elt, t)) lst

(* iterating over the board *)
let incr = function
  | (4, j) -> (0, j + 1)
  | (i, j) -> (i + 1, j)

(* adding tuples *)
let (++) (a, b) (c, d) = (a + c, b + d)

(* accessing matrix *)
let (@@) m (x, y) = m.(x).(y)

let copy_board brd =
  Array.init 5 (fun i -> Array.copy brd.(i))

(* remove exactly one element from list which may contain two of those *)
let remove_one elt lst =
  let tmp = List.filter ((!=) elt) lst in
  if List.length lst - List.length tmp = 1 then tmp else elt :: tmp

(*
vim:sw=2
*)

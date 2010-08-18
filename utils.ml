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
(* Should be rewritten as recursive function with accumulator, to have tail
 * recursion FIXME *)
let remove_one elt lst =
  let tmp = List.filter ((!=) elt) lst in
  if List.length lst - List.length tmp = 1 then tmp else elt :: tmp

(* convert board to a list *)
let board_to_list brd =
  let tmp = Array.map Array.to_list brd in
  (List.flatten $ Array.to_list) tmp

(* check two lists for equality, ignoring the order of elements *)
let rec (@=@) l1 l2 =
  match l1, l2 with
  | [], [] -> true
  | [], _ -> false
  | _, [] -> false
  | hd1 :: [], hd2 :: [] -> hd1 = hd2
  | hd :: tl, _ ->
      let l1p1, l1p2 = List.partition ( (=) hd ) l1 in
      let l2p1, l2p2 = List.partition ( (=) hd ) l2 in
      List.length l1p1 = List.length l2p1 &&
      l1p2 @=@ l2p2

(*
vim:sw=2
*)

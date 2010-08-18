(** Utilities. *)

(** @return [f(g(x))]; normally third argument is omitted so composition of [f]
    and [g] is returned 
    @param f first function
    @param g second function @param x argument for the second function, normally omitted *)
let ($) f g x = f (g x)

(** @return the list of tuples [\[(elt, l1); (elt, l2); ...\]]
    @param elt some element
    @param lst a list [\[l1; l2; ...\]] *)
let (@*) elt lst = List.map (fun t -> (elt, t)) lst

(** Given the coordinates of the cell on the board, the funcion finds the next
    cell, incrementing column if possible; if not, column is set to 0 and row is
    incremented instead. The number of columns is 5; the number of rows is not
    limited. Counting starts from 0.
    @return the coordinates of the next cell on the board *)
let incr = function
  | (4, j) -> (0, j + 1)
  | (i, j) -> (i + 1, j)

(** @return integer 2-tuple which is a coordinate-wise sum of two integer 2-tuples *)
let (++) (a, b) (c, d) = (a + c, b + d)

(* accessing matrix *)
(** @param m matrix
    @return the element of [m] indexed by an integer 2-tuple *)
let (@@) m (x, y) = m.(x).(y)

(** @param brd An array 5 x N
    @return copy of [brd] which does not share any elements with [brd] *)
let copy_board brd =
  Array.init 5 (fun i -> Array.copy brd.(i))

 (** @return [lst] with the first occurence of [elt] removed
    when called as {!Utils.remove_one} [elt lst]
    @param acc accumulator for internal purposes, should not be used
    @param lst a list
    @param elt element to remove *)
let rec remove_one ?(acc=[]) elt lst =
  match lst with
  | e :: tl when e = elt -> List.rev_append acc tl
  | [] -> raise Not_found
  | hd :: tl ->
      let new_acc = hd :: acc in
      remove_one ~acc:new_acc elt tl

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

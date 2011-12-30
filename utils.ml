let ($) f g x = f (g x)

let (@*) elt lst = List.map (fun t -> (elt, t)) lst

let (++) (a, b) (c, d) = (a + c, b + d)

let incr = function
  | (4, j) -> (0, j + 1)
  | (i, j) -> (i + 1, j)

let (@@) m (x, y) = m.(x).(y)

let copy_board brd =
  Array.init 5 (fun i -> Array.copy brd.(i))

let rec remove_one_rec acc elt lst =
  match lst with
  | e :: tl when e = elt -> List.rev_append acc tl
  | [] -> raise Not_found
  | hd :: tl ->
      let new_acc = hd :: acc in
      remove_one_rec new_acc elt tl

let remove_one elt lst = remove_one_rec [] elt lst

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

(** @return function which takes coordinates tuple and returns default value if
    coordinates are out of board; otherwise returns result of applying f to the
    corresponding board cell
    @param brd Board
    @param f Function to apply to cell
    @param default Default value to return if cell is out of board *)
let do_or_default brd f default = function
  | -1, _ -> default
  | 5, _ -> default
  | _, -1 -> default
  | _, 5 -> default
  | coord -> f (brd @@ coord)

(*
vim:sw=2
*)

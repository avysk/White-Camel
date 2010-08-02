open Utils
type gametree = Gametree of (Types.position * (gametree list Lazy.t))

let rec create_gametree pos =
  let apply = lazy (Apply_move.apply_move pos) in
  let apply_moves = lazy (create_gametree $ (Lazy.force apply)) in
  let all_moves = lazy (Moves.find_all_moves pos pos.Types.to_move) in
  let possible = lazy (function
    | Gametree (p, _) -> not (Check.under_check p pos.Types.to_move)) in
  let all_branches = lazy (
    List.map
    (Lazy.force apply_moves)
    (Lazy.force all_moves)
  ) in
  let branches = lazy (
    List.filter
    (Lazy.force possible)
    (Lazy.force all_branches)
  ) in
  (* NB: illegal mate by pawn drop should be excluded at position evaluation *)
  Gametree (pos, branches)
(*
 * vim:sw=2
 *)

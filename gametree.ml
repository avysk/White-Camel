open Utils
type gametree = Gametree of (Types.position * (gametree list lazy_t))

let rec create_gametree pos =
  let branches = lazy (List.map (create_gametree $ (Apply_move.apply_move pos))
  (* FIXME: not all moves should be here, some filtering should be done! *)
  (Moves.find_all_moves pos pos.Types.to_move)) in
  Gametree (pos, branches)

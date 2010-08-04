open Utils
open Types

type gametree = Gametree of (Types.position * (gametree list Lazy.t))

module PosHash = 
  struct
    type t = gametree
    let hash = function
      | Gametree (pos, _) -> Crc.pos_crc pos
    let equal gt1 gt2 =
      let Gametree (pos1, _) = gt1 in
      let Gametree (pos2, _) = gt2 in
      pos1.to_move = pos2.to_move &&
      pos1.sente_king = pos2.gote_king &&
      pos1.board = pos2.board &&
      begin
        let sh1 = pos1.sente_hand in
        let gh1 = pos1.gote_hand in
        let sh2 = pos2.sente_hand in
        let gh2 = pos2.gote_hand in
        List.length sh1 = List.length sh2 &&
        List.length gh1 = List.length gh2 &&
        sh1 @=@ sh2 && gh1 @=@ gh2
      end
  end

module WeakPosHash = Weak.Make(PosHash)

let global_weak_pos_hash = WeakPosHash.create 100000 (* TODO: 100000 ? *)

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

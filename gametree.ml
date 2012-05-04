open Utils
open Types

(* This is a state of the art code! It uses lazy evaluation, and recursive
 * types, and weak hash table! :-) *)

(* Gametree consists of the root position, and the lazy list of gametrees with
 * roots in positions arising after all possible moves. When a child gametree is
 * instantiated (by Lazy.force-ing the list of gametrees) its existance in
 * checked in the global weak hash table. If it's not there, it's added there.
 * If it's there, we take gametree from hash table instead. The benefit is that
 * the same root position in different places of the starting tree will give us
 * the same (phisically) object as a gametree, so after the position is
 * evaluated somewhere in a tree (forcing child branches as deep as needed), in
 * all other places it's available as already evaluated *)

type gametree = Gametree of (position * (gametree list Lazy.t))

module PosHash =
  struct
    type t = gametree
    let hash = function
      | Gametree (pos, _) ->
          Zobrist.zobrist_hash pos.board pos.sente_hand pos.gote_hand
    let equal gt1 gt2 =
      let Gametree (pos1, _) = gt1 in
      let Gametree (pos2, _) = gt2 in
      pos1.to_move = pos2.to_move &&
      pos1.sente_king = pos2.sente_king &&
      pos1.gote_king = pos2.gote_king &&
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

;;

let global_weak_pos_hash = WeakPosHash.create 1000000 (* TODO: 500000 ? *)

let rec create_gametree pos =
  let apply = Apply_move.apply_move pos in
  let possible = lazy (function
    | Gametree (p, _) -> not (Check.under_check p pos.to_move)) in
  let branches = lazy (
    List.filter
      (Lazy.force possible)
      (List.map
      (create_gametree $ apply)
      (Moves.find_all_moves pos pos.to_move))
    ) in
  (* NB: illegal mate by pawn drop should be excluded at position evaluation *)
  let gt_tmp = Gametree (pos, branches) in
  WeakPosHash.merge global_weak_pos_hash gt_tmp

let get_evaluation = function
  | Gametree (p, _) -> (fst (p.evaluation))
(*
 * vim:sw=2
 *)

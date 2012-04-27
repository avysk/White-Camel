open Types

let one_piece () =
  let piece_on_board = Array.make_matrix 5 5 0 in
  for i = 0 to 4 do
    for j = 0 to 4 do
      piece_on_board.(i).(j) <- Random.bits ()
    done
  done ;
  piece_on_board

let pieces_on_board =
  [ ((Sente, Pawn), one_piece ()) ;
    ((Sente, King), one_piece ()) ;
    ((Sente, Gold), one_piece ()) ;
    ((Sente, Silver), one_piece ()) ;
    ((Sente, Bishop), one_piece ()) ;
    ((Sente, Rook), one_piece ()) ;
    ((Sente, Tokin), one_piece ()) ;
    ((Sente, GoldS), one_piece ()) ;
    ((Sente, DragonHorse), one_piece ()) ;
    ((Sente, DragonKing), one_piece ()) ;
    ((Gote, Pawn), one_piece ()) ;
    ((Gote, King), one_piece ()) ;
    ((Gote, Gold), one_piece ()) ;
    ((Gote, Silver), one_piece ()) ;
    ((Gote, Bishop), one_piece ()) ;
    ((Gote, Rook), one_piece ()) ;
    ((Gote, Tokin), one_piece ()) ;
    ((Gote, GoldS), one_piece ()) ;
    ((Gote, DragonHorse), one_piece ()) ;
    ((Gote, DragonKing), one_piece ()) ]

let pieces_in_hand =
  [ ((Sente, Pawn), Random.bits ()) ;
    ((Sente, Gold), Random.bits ()) ;
    ((Sente, Silver), Random.bits ()) ;
    ((Sente, Bishop), Random.bits ()) ;
    ((Sente, Rook), Random.bits ()) ;
    ((Gote, Pawn), Random.bits ()) ;
    ((Gote, Gold), Random.bits ()) ;
    ((Gote, Silver), Random.bits ()) ;
    ((Gote, Bishop), Random.bits ()) ;
    ((Gote, Rook), Random.bits ()) ]

(*
 * The main constructor of Zobrist hash.
 *
 * The hash for the given state (board, sente hand and gote hand) is constructed
 * by XORing the values corresponding to the pieces at the given spots and all
 * the value for pieces in hand.
 *)
let zobrist_hash brd shand ghand =
  let hash = ref 0 in
  for i = 0 to 4 do
    for j = 0 to 4 do
      match brd.(i).(j) with
      | Some piece ->
          let arr = List.assoc piece pieces_on_board in
          let board_val = arr.(i).(j) in
          hash := !hash lxor board_val
      | None -> ()
    done
  done ;
  List.iter (fun pc ->
    let hand_val = List.assoc (Sente, pc) pieces_in_hand in
    hash := !hash lxor hand_val) shand ;
  List.iter (fun pc ->
    let hand_val = List.assoc (Gote, pc) pieces_in_hand in
    hash := !hash lxor hand_val) ghand ;
  !hash

(*
 * The following two functions update Zobrist hash value.
 *)

(*
 * Adds or removes piece at the given point.
 *
 * Because Zobrist hash is using XOR, it does not matter if the piece is added
 * or removed.
 *)
let update_board old_hash piece x y =
  let arr = List.assoc piece pieces_on_board in
  let board_val = arr.(x).(y) in
  old_hash lxor board_val

(*
 * Adds or remove piece in hand.
 *
 * Because Zobrist hash is using XOR, it does not matter if the piece is added
 * or removed.
 *)
let update_hand old_hash piece =
  let hand_val = List.assoc piece pieces_in_hand in
  old_hash lxor hand_val

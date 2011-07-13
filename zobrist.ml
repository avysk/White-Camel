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

let zhash brd shand ghand =
  let hash = ref 0 in
  for i = 0 to 4 do
    for j = 0 to 4 do
      match brd.(i).(j) with
      | Some piece ->
          let arr = List.assoc piece pieces_on_board in
          let rndval = arr.(i).(j) in
          hash := !hash lxor rndval
      | None -> ()
    done
  done ;
  List.iter (fun pc ->
    let rndval = List.assoc (Sente, pc) pieces_in_hand in
    hash := !hash lxor rndval) shand ;
  List.iter (fun pc ->
    let rndval = List.assoc (Gote, pc) pieces_in_hand in
    hash := !hash lxor rndval) ghand ;
  !hash


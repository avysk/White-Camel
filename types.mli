(** Defines the common types and some global constants and functions,
  directly related with types definitions. *)

(** Possible kinds of pieces *) 
type piece_t =
  | Pawn (** Pawn *)
  | King (** King *)
  | Gold (** Gold general *)
  | Silver (** Silver general *)
  | Bishop (** Bishop *)
  | Rook (** Rook *)
  | Tokin (** Tokin = promoted Pawn *)
  | GoldS (** Gold general which is a promoted silver general *)
  | DragonHorse (** Dragon Horse = Promoted Bishop *)
  | DragonKing (** Dragon King = Promoted Rook *)

(** The list of all possible pieces *)
val all_pieces : piece_t list

(** Players *)
type side = Sente | Gote

(** Returns other player *)
val other : side -> side

(** The flag to separate one-step possible moves from sliding moves *)
type sliding = Step | Slide

(** Piece *)
type piece = side * piece_t

(** Cell on board *)
type cell = piece option

(** Board *)
type board_t = cell array array

(** Move *)
type move = {
  what : piece ; (** The piece that moves *)
  start : (int * int) option ; (** The point to move from; None in case of drop move *)
  finish : int * int ; (** The point to move to *)
}

(** Helper value: does not describe any legal move *)
val no_move : move

(** List of pieces in hand *)
type hand = piece_t list

(** Position evaluation value *)
type eval_t = Eval of int | Sente_won | Gote_won

(** Position evaluation depth *)
type depth_t = Depth of int

(** Helper value: does not describe any legal evaluation *) 
val not_evaluated : eval_t * depth_t

(** Position *)
type position = {
  board : board_t ; (** The current board *)
  to_move : side ; (** Player to move *)
  sente_hand : hand ; (** Sente player's pieces in hand *)
  gote_hand : hand ; (** Gote player's pieces in hand *)
  (* The coordinates of kings are needed often *)
  sente_king : int * int ; (** Coordinates of sente player's king *)
  gote_king : int * int ; (** Coordinates of gote player's king *)
  prev_move : move ; (** Previous move. Can contain totaly wrong value. *)
  mutable evaluation : eval_t * depth_t (** Position evaluation. *)
}

(*
vim:sw=2
*)

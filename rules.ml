open Utils

(* "Forward" for sente side *)
let m_sente = [(0, 1)]
(* "Forward" for gote side *)
let m_gote = [(0, -1)]
(* "Forward diagonals" for sente side *)
let m_sente_diag = [(-1, 1); (1, 1)]
(* "Forward diagonals" for gote side *)
let m_gote_diag = [(-1, -1); (1, -1)]
(* "Sideways" *)
let m_sides = [(-1, 0); (1, 0)]
(* "Diagonally" *)
let m_diag = m_sente_diag @ m_gote_diag
(* "Vertically or horizontally" *)
let m_raw = m_sente @ m_gote @ m_sides

(* '@*' operator constructs the list of tuples as direct product of
the element and the list, see utils.ml *)

(* Pawn moves one step forward *)
let mv_sente_pawn = Types.Step @* m_sente
let mv_gote_pawn = Types.Step @* m_gote

(* King moves one step vertically or horizontaly or diagonaly *)
let mv_king = Types.Step @* (m_diag @ m_raw)

(* Gold general moves vertically or horizontally or forward diagonally *)
let mv_sente_gold = Types.Step @* (m_raw @ m_sente_diag)
let mv_gote_gold =  Types.Step @* (m_raw @ m_gote_diag)

(* Silver general moves diagonally and forward *)
let mv_sente_silver = Types.Step @* (m_diag @ m_sente)
let mv_gote_silver = Types.Step @* (m_diag @ m_gote)

(* Bishop moves diagnally, sliding *)
let mv_bishop = Types.Slide @* m_diag

(* Rook moves horizontally or vertiacally, sliding *)
let mv_rook = Types.Slide @* m_raw

(* Dragon horse (promoted bishop) moves like bishop or one step
horizontally or vertically *)
let mv_dragonhorse = mv_bishop @ (Types.Step @* m_raw)

(* Dragon king (promoted rook) moves like rook or one step diagonally *)
let mv_dragonking = mv_rook @ (Types.Step @* m_diag)

let possible_moves = function
  | Types.Sente, Types.Pawn -> mv_sente_pawn
  | Types.Gote, Types.Pawn -> mv_gote_pawn
  | _, Types.King -> mv_king
  | Types.Sente, Types.Silver -> mv_sente_silver
  | Types.Gote, Types.Silver -> mv_gote_silver
  | _, Types.Bishop -> mv_bishop
  | _, Types.Rook -> mv_rook
  | _, Types.DragonHorse -> mv_dragonhorse
  | _, Types.DragonKing -> mv_dragonking
  (* what's left: gold generals, tokins and promoted silvers; all of them move
   * as gold generals *)
  | Types.Sente, _ -> mv_sente_gold
  | Types.Gote, _ -> mv_gote_gold

let _find_movers kind_of_move =
  (* find the kinds of (sente) pieces which can do the given kind of move *)
  let all_pcs = Types.Sente @* Types.all_pieces in
  let filt = List.mem kind_of_move $ possible_moves in
  List.map snd (List.filter filt all_pcs)

let _find_attackers delta = _find_movers (Types.Step, delta)

let _find_sliders delta = _find_movers (Types.Slide, delta)

(* The following listn are calculated only once, to avoid hardcoding the rules
 * of the game in one more place. The lists are visible outside of this file. *)
let forward_attackers = _find_attackers (0, 1)
let backward_attackers = _find_attackers (0, -1)
let forward_diag_attackers = _find_attackers (1, 1)
let backward_diag_attackers = _find_attackers (-1, -1)
let sideways_attackers = _find_attackers (1, 0)
let straight_sliders = _find_sliders (0, 1)
let diag_sliders = _find_sliders (1, 1)

let turnover = function
  | Types.King -> failwith "King cannot be promoted"
  | Types.Pawn -> Types.Tokin
  | Types.Tokin -> Types.Pawn
  | Types.Gold -> failwith "Gold general cannot be promoted"
  | Types.Silver -> Types.GoldS
  | Types.GoldS -> Types.Silver
  | Types.Bishop -> Types.DragonHorse
  | Types.DragonHorse -> Types.Bishop
  | Types.Rook -> Types.DragonKing
  | Types.DragonKing -> Types.Rook

let basic_state = function
  | Types.Tokin -> Types.Pawn
  | Types.GoldS -> Types.Silver
  | Types.DragonHorse -> Types.Bishop
  | Types.DragonKing -> Types.Rook
  | whatever -> whatever

(*
 vim:sw=2
 *)

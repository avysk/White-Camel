open Curses

type keybindings = {
  quit : int ;
  up : int ;
  down : int ;
  left : int ;
  right : int ;
  switch_color : int ;
  turnover : int ;
  switch_move : int ;
  take_or_place : int ;
  to_hand : int ;
  from_hand : int ;
  verify : int
}

let cmd = {
  quit = int_of_char 'Q' ;
  up = Key.up ;
  down = Key.down ;
  left = Key.left ;
  right = Key.right ;
  switch_color = int_of_char 'c' ;
  turnover = int_of_char 't' ;
  switch_move = int_of_char 'm' ;
  take_or_place = int_of_char ' ' ;
  to_hand = int_of_char 'h' ;
  from_hand = int_of_char 'd' ;
  verify = int_of_char 'v'
}

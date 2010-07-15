(* -------------------- Utilities -------------------- *)

(* function composition *)
let ($) f g x = f (g x)

(* constructing tuples of list *)
let (@*) elt lst = List.map (fun t -> (elt, t)) lst

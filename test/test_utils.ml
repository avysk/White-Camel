open OUnit
open Utils

let test_composition () =
  let f x = x * x in
  let g = f $ succ in
  assert_equal 16 (g 3)

let test_direct_product () =
  assert_equal [("a", 1); ("a", 2); ("a", 3)] ("a" @* [1; 2; 3])

let test_componentwise_sum () =
  assert_equal (10, 20) ((5, -10) ++ (5, 30))

let test_remove_one () =
  assert_equal ~msg:"removing the first element only"
               ["a"; "b"; "c"; "d"; "b"; "c"; "a"]
               (remove_one "c" ["a"; "c"; "b"; "c"; "d"; "b"; "c"; "a"])

(* Board utilities *)

let test_incr () =
  todo "incr"

let test_access () =
  todo "@@"

let test_copy_board () =
  todo "copy_board"

let suite =
  "test_utils" >::: [
    (>::) "function composition" test_composition ;
    "direct product of element and list" >:: test_direct_product ;
    "remove one element" >:: test_remove_one ;
    "componentwise sum" >:: test_componentwise_sum ;
    "next board cell" >:: test_incr ;
    "access matrix" >:: test_access ;
    "copy board" >:: test_copy_board
  ]

open OUnit2
open Deck

let deck_tests = [
  "empty_deck" >:: (fun _ -> assert_equal [] ([]));
]



let suite = "Final Project test suite" >:::
  deck_tests

(* The following line must be the one and only place
 * in your entire source code that calls [OUnit2.run_test_tt_main]. *)
let _ = run_test_tt_main suite
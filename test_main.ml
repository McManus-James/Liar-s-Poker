open OUnit2

let suite = "Final Project test suite" >:::
  Test_deck.tests @ Test_round.tests

(* The following line must be the one and only place
 * in your entire source code that calls [OUnit2.run_test_tt_main]. *)
let _ = run_test_tt_main suite
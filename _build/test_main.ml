open OUnit2

open Modules
open Game

let tests = []

let suite = "Final Project test suite" >:::
  tests

(* The following line must be the one and only place
 * in your entire source code that calls [OUnit2.run_test_tt_main]. *)
let _ = run_test_tt_main suite
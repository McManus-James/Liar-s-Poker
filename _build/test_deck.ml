open OUnit2
open Deck

let deck_tests = [
  "empty_deck" >:: (fun _ -> assert_equal [] ([]));
]



let tests = deck_tests
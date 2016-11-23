open OUnit2
open Deck
open Round

let round_tests = [
  "empty_deck" >:: (fun _ -> assert_equal [] ([]));
]



let tests = round_tests
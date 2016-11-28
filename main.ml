
(* IMPORTANT NOTE:
 * You should not need to modify this file, though perhaps for karma
 * reasons you might choose to do so.  The reason this file is factored
 * out from [game.ml] is for testing purposes:  if we're going to unit
 * test the [Game] module, it can't itself invoke its [main] function;
 * if it did, the game would try to launch and would interfere with OUnit.
 *)

let () =
  print_endline ("Welcome to Liar's Poker! "
                ^"Please enter the number of players.\n");
  print_string  "> ";
  let num_players = int_of_string (read_line ()) in
  Game.main num_players
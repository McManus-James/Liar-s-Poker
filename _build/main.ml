
(* IMPORTANT NOTE:
 * You should not need to modify this file, though perhaps for karma
 * reasons you might choose to do so.  The reason this file is factored
 * out from [game.ml] is for testing purposes:  if we're going to unit
 * test the [Game] module, it can't itself invoke its [main] function;
 * if it did, the game would try to launch and would interfere with OUnit.
 *)
exception Invalidplayers
exception Invaliddifficulty

let () =
  print_endline ("Welcome to Liar's Poker! "
                ^"Please enter the number of players.\n");
  print_string  "> ";
  let num_players = int_of_string (read_line ()) in
  print_endline ("Please enter the difficulty 1-3.\n");
  print_string  "> ";
  let diff = int_of_string (read_line ()) in
  Game.main num_players diff
let rec get_number_players i =
  try (print_endline "Please enter the number of players.";
  print_string ">";
  let num_players = read_line () |> int_of_string in
  if (num_players < 2) then
    (print_endline "The number of players must be at least 2. Please try again.";
    get_number_players 0)
  else if num_players > 9 then
    (print_endline "The number of players must be at most 9. Please try again.";
    get_number_players 0)
  else num_players)
  with Failure "int_of_string" -> (let () = print_endline ("The number of players must be "^
                                 "an integer between 2 and 9. Please try again.") in
                                 get_number_players 0)

let rec get_difficulty i =
  try (print_endline ("Please enter the difficulty 1-3.");
  print_string ">";
  let diff = read_line () |> int_of_string in
  if (diff < 1) then
    (print_endline "The difficulty must be at least 1. Please try again.";
    get_difficulty 0)
  else if diff > 3 then
    (print_endline "The difficulty must be at most 3. Please try again.";
    get_difficulty 0)
  else diff)
  with Failure "int_of_string" -> (let () = print_endline ("The difficulty must be "^
                                 "an integer between 1 and 3. Please try again.") in
                                 get_difficulty 0)

exception Invalidplayers
exception Invaliddifficulty

let () =
  print_endline ("Welcome to Liar's Poker!");
               (*  ^"Please enter the number of players.\n");
  print_string  "> "; *)
  let num_players = get_number_players 0 in
  let diff = get_difficulty 0 in
  Game.main num_players diff
module MyRound = Modules.GameRound

let rec play info =
  let loser = MyRound.play_round info in
  let new_info = MyRound.update_state loser info in
  (* if over new_info then winner new_info
  else play new_info *)
  1

let main n =
  let state = MyRound.init_state n in
  print_endline ("Player "^(string_of_int (play state))^" wins!")

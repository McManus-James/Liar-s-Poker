module MyRound = Pokergame.GameRound

let rec play info =
  let loser = MyRound.play_round info in
  let new_info = MyRound.update_state loser info in
  match MyRound.winner new_info with
  | Some p -> p
  | None -> play new_info

let main n =
  let state = MyRound.init_state n in
  print_endline ("Player "^(string_of_int (play state))^" wins!")


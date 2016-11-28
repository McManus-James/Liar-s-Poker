module MyRound = Modules.GameRound

let rec play info =
  let loser = MyRound.play_round info in
  let new_info = MyRound.update_state loser info in
  if List.length new_info.players = 1 then
  new_info.cur_player
  else play new_info

let main n =
  let state = MyRound.init_state n in
  print_endline ("Player "^(string_of_int (play state))^" wins!")
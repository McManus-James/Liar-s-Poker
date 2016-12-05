module MyRound = Pokergame.GameRound (Data.CardGame)
open MyRound

let rec play info =
  let loser = play_round info in
  let new_info = update_state loser info in
  match winner new_info with
  | Some p -> p
  | None -> play new_info

let main n d =
  let state = init_state n d in
  print_endline ("Player "^(string_of_int (play state))^" wins!")


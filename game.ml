open Round

let rec index_of x lst i =
  match lst with
  | [] -> failwith "Not Found"
  | h::tl -> if h = x then i else index_of x tl (i+1)

let next_player cur_player hands =
  failwith "unimplemented"

let rec play_round info =
  let cur_player = info.cur_player in
  let prev_player = info.prev_player in
  let prev_move =
    if prev_player = 0 then None
    else Some info.prev_move
  in
  let cur_hand = List.assoc cur_player info.hands in
  let move =
    if cur_player = 1 then
      human_turn cur_hand prev_move
    else ai_turn cur_hand prev_move
  in
  let cur_p = "Player "^(string_of_int cur_player) in
  let prev_p = "Player "^(string_of_int prev_player) in
  match move with
  | BS p ->
    print_endline cur_p^" called BS!"
                        ^" Let's check if the previous hand is there...";
    if (hand_exists info.cards p) then
      let () = print_endline (string_of_pokerhand p)^" is here. "
                    ^cur_p^" loses this round." in
      cur_player
    else
      print_endline (string_of_pokerhand p)^"is not here."
                    ^prev_p^" loses this round.";
      prev_player
  | Raise p -> print_endline cur_p^" raised to "^(string_of_pokerhand p)^".";
    let new_info =
      { info with
        cur_player = next_player cur_player hands;
        prev_player = cur_player;
        hands_called = p::info.hands_called;
        prev_move = Raise p;
      }
    in
    play_round new_info

let rec play info =
  let loser = play_round info in
  let state = update_state loser info in
  if List.length state.players = 1 then fst (List.hd state.players)
  else play new_players


let main p_num =
  let state = init_state p_num in
  let winner = string_of_int (play state) in
  print_endling "Player "^winner^ " wins!"


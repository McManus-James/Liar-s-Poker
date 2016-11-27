open Deck
open Round
module R = GameRound(Deck)
open R

type round_info = {
    players : pid * int list; (* association list mapping the pid to the number of cards that player has *)
    cur_player : pid; (* the pid of the player who's turn it currently is *)
    prev_player : pid; (* the previous player who made a turn. Stored for easy access if BS is called and the hand they called is not in the collective_cards *)
    hands_called : pokerhand list; (* list of pokerhands called so far in the round *)
    prev_move : move; (* previous move called in round *)
    hands : pid * hand list (* association list mapping pids to their respective hands *)
    (* cards : card list; *) (* list of all the cards in play; giant list of everyone's hands *)
  }

let rec index_of x lst i =
  match lst with
  | [] -> failwith "Not Found"
  | h::t -> if h = x then i else index_of x tl (i+1)

(* initializes the [num_cards] field in a [round_info]
 * [p_num] is the number of players to initialize
 * [players] is the association list mapping pids to numbers of cards they have *)
let rec init_players p_num players =
  if p_num = 0 then players
  else init_players (p_num - 1) (p_num,4)::players

(* generates a list of all the cards in play *)
let rec all_cards hands cards =
  match hands with
  | [] -> cards
  | (_,hand)::t -> all_cards t (cards@hand)

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


let rec update_players loser players accu =
  match players with
  | [] -> accu
  | (pid,num_cards)::tl ->
      if loser = pid then
        if num_cards = 1 then
        let () = print_endline "Player "^(string_of_int loser)^" is out!" in
        update_players loser tl accu
        else update_players loser tl ((pid,num_cards-1)::accu)
      else update_players loser tl ((pid, num_cards)::accu)

let rec play players =
  let hands = deal_hands players in
  let cards = all_cards hands in
  let info =
    { cur_player = 1;
      prev_player = 0;
      hands_called = [];
      hands = hands;
      prev_move = BS;
      cards = cards;
    } in
  let loser = play_round info in
  let state = update_state loser info in
  if List.length state.players = 1 then fst (List.hd state.players)
  else play new_players


let main p_num =
  let players = init_players p_num in
  (* let winner =  *)play players

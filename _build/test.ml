open String
open Pervasives
open List

exception InvalidMove

module type Round = sig
  type pid
  type state
  val init_state : pid -> state
  val update_state : pid -> state -> state
  val play_round : state -> pid
end

module GameRound = struct
  open GameDeck

  type pid = int

  type pokerhand =
    | FourOfAKind of int
    | FullHouse of int * int (* first int is rank of the three of
                               a kind; second int is the rank of
                               the pair *)
    | Straight of int (* the int is the high card in the straight *)
    | ThreeOfAKind of int
    | TwoPair of int * int (* first int is rank of the higher
                            pair; second int is the rank of the
                            lower pair *)
    | Pair of int
    | HighCard of int


  type move =
    | BS of pokerhand (* The hand which BS was called on *)
    | Raise of pokerhand (* The hand that was Raised *)

  type state = {
    players : (pid * int) list; (* association list mapping the pid to the number of cards that player has *)
    cur_player : pid; (* the pid of the player who's turn it currently is *)
    prev_player : pid; (* the previous player who made a turn. Stored for easy access if BS is called and the hand they called is not in the collective_cards *)
    hands_called : pokerhand list; (* list of pokerhands called so far in the round *)
    raised_hand : pokerhand option; (* previous move called in round *)
    hands : (pid * hand) list; (* association list mapping pids to their respective hands *)
    cards : hand (* list of all the cards in play; giant list of everyone's hands *)
  }

  (* initializes the [num_cards] field in a [round_info]
   * [p_num] is the number of players to initialize
   * [players] is the association list mapping pids to
   * the numbers of cards they have *)
  let rec init_players n players =
    if n = 0 then players
    else init_players (n - 1) ((n,4)::players)

  let rec deal_hands players accum =
    let d = shuffle_deck (new_deck empty) in
    match players with
      | [] -> accum
      | (p, n)::t -> deal_hands t ((p, (deal n d))::accum)

  let init_state n =
    let players = init_players n [] in
    let hands = deal_hands players [] in
    let cards = split hands |> snd |> flatten in
    { players = players;
      cur_player = 1;
      prev_player = 0;
      hands_called = [];
      hands = hands;
      raised_hand = None;
      cards = cards
    }

  let rec index_of x lst i =
    match lst with
    | [] -> failwith "Not Found"
    | h::tl -> if h = x then i else index_of x tl (i+1)

  let next_player p players =
    let pids = fst (split players) in
    let i = index_of p pids 0 in
    if i = (length pids -1) then hd pids
    else nth pids (i+1)

  let rec update_players loser players accu =
  match players with
  | [] -> accu
  | (pid,num_cards)::tl ->
      if loser = pid then
        if num_cards = 1 then
        (print_endline ("Player "^(string_of_int loser)^" is out!");
        update_players loser tl accu)
        else update_players loser tl ((pid,num_cards-1)::accu)
      else update_players loser tl ((pid, num_cards)::accu)

  let update_state l s =
    let players = update_players l s.players [] in
    let hands = deal_hands players [] in
    let cards = split hands |> snd |> flatten in
    { s with
      players = players;
      hands = hands;
      cards = cards
    }

  (* Helper method for modifying the deck returns all but the first [n] elements
   * of [lst]. If [lst] has fewer than [n] elements, return the empty list. Code
   * taken from Recitation: Lists, and Testing with OUnit *)
  let rec drop n h =
    if n =0 then h else match h with
      | []->[]
      | x::xs -> drop (n-1) xs

  (* checks if one of the card ranks is in the collective cards. THIS ISN'T
   * QUITE RIGHT THOUGH BECAUSE NEED TO REMOVE THIS CARD FROM THE LIST AFTER
   * IT'S FOUND *)
  let rec check_individual_card_rank rank rank_lst counter =
    (* failwith "unimplemented" *)
    match !rank_lst with
    | [] -> false
    | h::t -> if h = rank then
                let () = rank_lst := drop counter !rank_lst in
                true
              else check_individual_card_rank rank (ref t) (counter + 1)

  (* returns true if every card rank in [called_ranks] is in [rank_lst]. Returns
   * false otherwise *)
  let rec check called_ranks rank_lst =
    match called_ranks with
    | [] -> true
    | h::t -> (check_individual_card_rank h rank_lst 1) && check t rank_lst

  (* takes a pokerhand [phand] and converts it into a rank list of the ranks of
   * the cards in the hand that's called for checking purposes.
   * Does not return a card list, because the suits of the cards are not
   * considered when checking to see if the
   * hand is in the collective set of cards in play *)
  let convert_phand_to_hand phand =
    match phand with
    | FourOfAKind p -> [p; p; p; p]
    | FullHouse (p, t) -> List.sort compare [p; p; p; t; t]
    | Straight p -> [p - 4; p - 3; p - 2; p - 1; p]
    | ThreeOfAKind p -> [p; p; p]
    | TwoPair (p, t) -> List.sort compare [p; p; t; t]
    | Pair p -> [p; p]
    | HighCard p -> [p]


let convert_hand_to_phand hand = match hand with
  | a::[] -> HighCard a
  | a::b::[] -> Pair a
  | a::b::c::[] -> ThreeOfAKind a
  | a::b::c::d::[] -> if a = c then FourOfAKind a else TwoPair (a, c)
  | a::b::c::d::e::[] -> if a = b then FullHouse (a, d) else Straight e
  | _ -> raise InvalidMove


let rec matchOneCard card hand ret_hand = match hand with
  | [] -> None
  | h::t -> if card = fst h then Some (ret_hand@t)
    else let ret_hand_update = h::ret_hand in matchOneCard card t ret_hand_update

let rec countOneHand next_hand player_hand ret = match next_hand with
  | [] -> ret
  | h::t -> let match_one_card_return = matchOneCard h player_hand [] in (match match_one_card_return with
    | None -> countOneHand t player_hand ret
    | Some l -> countOneHand t l (fst ret + 1, snd ret))

let compareHand cur_hand player_hand next_hand = let next = countOneHand (convert_phand_to_hand next_hand) player_hand (0, convert_phand_to_hand next_hand) in
(if fst next > fst cur_hand then (fst next, convert_hand_to_phand (snd next)) else  cur_hand)

(*
cur_hand
*)
let rec chooseHand1 cur_hand player_hand(p_hands : pokerhand list) : pokerhand =
  match p_hands with
    | [] -> snd cur_hand
    | h::t -> chooseHand1 (compareHand cur_hand player_hand h) player_hand t



  (* [hand_exists hands handrank] returns true if [handrank] exists within all
   * the cards in [hands] *)
  let hand_exists hands handrank =
    let ranks = ref (List.sort compare (fst (List.split hands))) in
    let hand_rank_lst = convert_phand_to_hand handrank in
    check hand_rank_lst ranks

  (* [string_of_rank r] returns the string representation of rank [r] *)
  let string_of_rank r =
    if r = 11 then "Jack"
    else if r = 12 then "Queen"
    else if r = 13 then "King"
    else if r = 1 then "Ace"
    else string_of_int r

  let string_of_pokerhand phand =
    match phand with
    | FourOfAKind p -> "Four " ^ string_of_rank p ^ "s"
    | FullHouse (p, t) -> "Full house with three " ^ string_of_rank p ^
                          "s and two " ^ string_of_rank t ^ "s"
    | Straight p -> "Straight to " ^ string_of_rank p
    | ThreeOfAKind p -> "Three " ^ string_of_rank p ^ "s"
    | TwoPair (p, t) -> "Two " ^ string_of_rank p ^ "s and two "
                        ^ string_of_rank t ^ "s"
    | Pair p -> "Two " ^ string_of_rank p ^ "s"
    | HighCard p -> "Highcard of " ^ string_of_rank p

  (* returns the pokerhand called inside the Raise *)
  let unwrap_move move = match move with
    | Raise p -> p
    | _ -> raise InvalidMove

  (* beats returns true if [p_hand] is a higher ranked pokerhand than
   * [prev_hand], and false otherwise *)
  let beats p_hand prev_hand =
    match p_hand, prev_hand with
      | FourOfAKind p, FourOfAKind t -> p > t
      | FourOfAKind _, _ -> true
      | FullHouse (p1, p2), FullHouse (t1, t2) -> if p1 > t1 then true
                                                  else if p1 = t1 then p2 > t2
                                                  else false
      | FullHouse (_, _), FourOfAKind _ -> false
      | FullHouse (_, _), _ -> true
      | Straight p, Straight t -> p > t
      | Straight _, FourOfAKind _ -> false
      | Straight _, FullHouse (_, _) -> false
      | Straight _, _ -> true
      | ThreeOfAKind p, ThreeOfAKind t -> p > t
      | ThreeOfAKind _, FourOfAKind _ -> false
      | ThreeOfAKind _, FullHouse (_, _) -> false
      | ThreeOfAKind _, Straight _ -> false
      | ThreeOfAKind _, _ -> true
      | TwoPair (p1, p2), TwoPair (t1, t2) -> if p1 > t1 then true
                                              else if p1 = t1 then p2 > t2
                                              else false
      | TwoPair (_, _), Pair _ -> true
      | TwoPair (_, _), HighCard _ -> true
      | TwoPair (_, _), _ -> false
      | Pair p, Pair t -> p > t
      | Pair _, HighCard _ -> true
      | Pair _, _ -> false
      | HighCard p, HighCard t -> p > t
      | HighCard _, _ -> false

  (* returns true if [p] is a valid rank for a straight; ie [p] must be
   * between 6 and 14 but because a straight must have 5 cards in it so the
   * lowest possible stairght is 2, 3, 4, 5, 6 and the highest is 10, Jack,
   * Queen, King, Ace *)

  let valid_straight p =
    if (p < 6 || p > 14) then false
    else true

  (* [valid_call p_hand prev_hand] returns true if the pokerhand [p_hand] the
   * player calls is a valid call; ie it has a higher hand rank than the
   * previous hand [prev_hand]. Returns false if [p_hand] is an equal or lower
   * rank than [prev_hand] *)
  let valid_call p_hand prev_hand =
    match p_hand with
      | Straight p -> (valid_straight p) && beats p_hand prev_hand
      | FullHouse (p1, p2) -> if p1 = p2 then false
                              else beats p_hand prev_hand
      | TwoPair (p1, p2) -> if p1 = p2 then false
                            else beats p_hand prev_hand
      | _ -> beats p_hand prev_hand

  (* takes input from the user and parses it into a pokerhand type *)
  let rec parse_input h r =
    print_endline ("Player 1, your turn! " ^ "Here is your hand: ");
    print_hand h;
    print_endline ("And this is the previous hand called: " ^ string_of_pokerhand r);
    print_endline "What is your move?";
    print_endline ">";
    let call = trim (lowercase_ascii (read_line ())) in
    let length_call = String.length call in
    try (
      let space = index call ' ' in
      let type_of_hand = sub call 0 space in
      match type_of_hand with
        | "four" -> let i = String.sub call (space + 1) (length_call - (space + 1)) in
                    Raise (FourOfAKind (int_of_string i))
        | "fh" -> let number_part = trim (sub call (space + 1) (length_call - (space + 1))) in
                  let length_number_part = String.length number_part in
                  let space_number_part = index number_part ' ' in
                  let first_i = sub number_part 0 (space_number_part) in
                  let second_i = sub number_part (space_number_part + 1) (length_number_part - (space_number_part + 1)) in
                  Raise (FullHouse (int_of_string first_i,int_of_string second_i))
        | "straight" -> let i = sub call (space + 1) (length_call - (space + 1)) in
                        Raise (Straight (int_of_string i))
        | "three" -> let i = sub call (space + 1) (length_call - (space + 1)) in
                    Raise (ThreeOfAKind (int_of_string i))
        | "tp" -> let first_i = sub call (space + 1) 1 in
                  let second_space = rindex call ' ' in
                  let second_i = sub call (second_space + 1) 1 in
                  Raise (TwoPair (int_of_string first_i,int_of_string second_i))
        | "pair" -> let i = sub call (space + 1) (length_call - (space + 1)) in
                    Raise (Pair (int_of_string i))
        | "hc" -> let i = sub call (space + 1) (length_call - (space + 1)) in
                  Raise (HighCard (int_of_string i))
        | "bs" -> BS (r)
        | _ -> raise InvalidMove

    )
    with
      | Not_found  ->
        print_endline "That is not a valid hand type. Please try again.";
        parse_input h r
      | Invalid_argument _->
        print_endline "That is not a valid hand. Please try again.";
        parse_input h r
      | InvalidMove ->
        print_endline ("That is not a valid move. The kinds you can call are "
        ^"four, fh, straight, three, tp, pair, hc, and bs. Please try again.");
        parse_input h r

  let rec human_turn (h: hand) (r : pokerhand option) : move =
    failwith "commented out until it handles a pokerhand option"
    (* let move = parse_input h r in
    try (
      match move with
      | BS _ -> move
      | Raise p ->
        if (valid_call p r) = false then raise InvalidMove
        else move
    )
  with
    | InvalidMove ->
      print_endline ("That hand is not a higher call than the previous hand. "
                    ^"Please try again.");
                    human_turn h r *)

  let ai_turn id h ph cards =
    failwith "unimplemented"

  let rec play_round s =
    let cur_hand = List.assoc s.cur_player s.hands in
    let move =
      if s.cur_player = 1 then
        human_turn cur_hand s.raised_hand
      else ai_turn s.cur_player cur_hand s.raised_hand s.cards
    in
    let cur_p = "Player "^(string_of_int s.cur_player) in
    let prev_p = "Player "^(string_of_int s.prev_player) in
    match move with
    | BS p ->
      print_endline (cur_p^" called BS!"
                          ^" Let's check if the previous hand is there...");
      if (hand_exists s.cards p) then
        (print_endline ((string_of_pokerhand p)^" is here. "
                      ^cur_p^" loses this round.");
        s.cur_player)
      else
        (print_endline ((string_of_pokerhand p)^"is not here."
                      ^prev_p^" loses this round.");
        s.prev_player)
    | Raise p -> print_endline (cur_p^" raised to "
                              ^(string_of_pokerhand p)^".");
      let new_info =
        { s with
          cur_player = next_player s.cur_player s.players;
          prev_player = s.cur_player;
          hands_called = p::s.hands_called;
          raised_hand = Some p;
        }
      in
    play_round new_info

end


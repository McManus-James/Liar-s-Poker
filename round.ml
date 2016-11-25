open List
open Deck
open Pervasives
open String

exception InvalidMove
module Round (D: Deck) = struct

  type rank = int

  type suit = Hearts | Clubs | Diamonds | Spades

  type card = (rank * suit)

  type hand = card list

  type deck = D.deck

  type pokerhand = | FourOfAKind of int
                   | FullHouse of int * int (* first int is rank of the three of
                                             a kind; second int is the rank of
                                             the pair *)
                   | Straight of int
                   | ThreeOfAKind of int
                   | TwoPair of int * int (* first int is rank of the higher
                                          pair; second int is the rank of the
                                          lower pair *)
                   | Pair of int
                   | HighCard of int

  type pid = int

  (* The type of a move*)
  type move = | BS of pokerhand (* BS carries the last hand called from the previous round that needs to be checked *)
              | Raise of pokerhand (* Hand that the player is calling that is higher
                               than the previous hand *)

  type round_info = {
    pid_list : pid list; (* list of player ids in the order they take turns *)
    current_player : pid; (* the pid of the player who's turn it currently is *)
    prev_player : pid; (* the previous player who made a turn. Stored for easy access if BS is called and the hand they called is not in the collective_cards *)
    hands_called : pokerhand list; (* list of pokerhands called so far in the round *)
    prev_move : move; (* previous move called in round *)
    pid_hands : pid * hand list; (* association list mapping pids to their respective hands *)
    collective_cards : card list; (* list of all the cards in play; giant list of everyone's hands *)
  }

  (* formats a pokerhand into a string for printing. Called to format the calls of the AIs for printing *)
  let string_of_pokerhand (phand: pokerhand) : string = match phand with
    | FourOfAKind p -> "Four " ^ D.string_of_rank p ^ "s"
    | FullHouse (p, t) -> "Full house with three " ^ D.string_of_rank p ^ "s and two " ^ D.string_of_rank t ^ "s"
    | Straight p -> "Straight to " ^ D.string_of_rank p
    | ThreeOfAKind p -> "Three " ^ D.string_of_rank p ^ "s"
    | TwoPair (p, t) -> "Two " ^ D.string_of_rank p ^ "s and two " ^ D.string_of_rank t ^ "s"
    | Pair p -> "Two " ^ D.string_of_rank p ^ "s"
    | HighCard p -> "Highcard of " ^ D.string_of_rank p

  (* [deal_hands n] returns an associative list that maps [pid]s to hands of
   * [n] cards *)
  let rec deal_hands (players: pid * int list) (accum: pid * hand list) : pid * hand list =
    let d = D.shuffle_deck (D.new_deck D.empty) in
    match players with
      | [] -> accum
      | (p, n)::t -> deal_hands t ((p, (D.deal n d))::accum)

  (* returns the pokerhand called inside the Raise *)
  let unwrap_move move = match move with
    | Raise p -> p
    | _ -> raise InvalidMove

  (* returns: pid of the losing player of the round *)
  let rec run_turn r_info =
    (* match r_info.prev_move with
      | BS p -> if (hand_exists r_info.collective_cards p) then current_player
                else prev_player *)
    if current_player = 1 then
      let player_one_hand = assoc 1 pid_hands in
      let human_move = human_turn player_one_hand (unwrap_move r_info.prev_move) in
      match human_move with
        | BS p -> print_endline "You called BS! Let's check if the previous hand is there...";
                  if (hand_exists r_info.collective_cards p) then
                    print_endline "Sorry, " ^ (string_of_pokerhand p) ^ "is here. You lose this round.";
                    current_player
                  else
                    print_endline "Congrats! " ^ (string_of_pokerhand p) ^ "is not here.";
                    prev_player
        | Raise p -> print_endline "You called " ^ (string_of_pokerhand p) ^ ".";
                     let new_pid_list = (tl r_info.pid_list)@[current_player] in
                     let new_r_info = {r_info with
                                      pid_list = new_pid_list;
                                      current_player = }



  (* Helper method for modifying the deck returns all but the first [n] elements of [lst]. If [lst] has fewer than
   * [n] elements, return the empty list. Code taken from Recitation: Lists,
   * and Testing with OUnit *)
  let rec drop n h =
    if n =0 then h else match h with
      | []->[]
      | x::xs -> drop (n-1) xs

  (* checks if one of the card ranks is in the collective cards. THIS ISN'T
   * QUITE RIGHT THOUGH BECAUSE NEED TO REMOVE THIS CARD FROM THE LIST AFTER
   * IT'S FOUND *)
  let rec check_individual_card_rank (rank : rank) (rank_lst : rank list ref) (counter : int) : bool = match rank_lst with
    | [] -> false
    | h::t -> if h = rank then
                rank_lst := drop counter !rank_lst;
                true
              else check_individual_card_rank rank t (counter + 1)

  (* returns true if every card rank in [called_ranks] is in [rank_lst]. Returns
   * false otherwise *)
  let rec check (called_ranks: rank list) (rank_lst: rank list ref) = match called_ranks with
    | [] -> true
    | h::t -> fst (check_individual_card_rank h rank_lst 1) && check t rank_lst

  (* takes a pokerhand [phand] and converts it into a rank list of the ranks of the cards in the hand that's called for checking purposes.
   * Does not return a card list, because the suits of the cards are not considered when checking to see if the
   * hand is in the collective set of cards in play *)
  let convert_phand_to_hand (phand: pokerhand) : rank list = match phand with
    | FourOfAKind p -> [p; p; p; p]
    | FullHouse (p, t) -> sort compare [p; p; p; t; t]
    | Straight p -> [p - 4; p - 3; p - 2; p - 1; p]
    | ThreeOfAKind p -> [p; p; p]
    | TwoPair (p, t) -> sort compare [p; p; t; t]
    | Pair p -> [p; p]
    | HighCard p -> [p]

  (* [hand_exists hands handrank] returns true if [handrank] exists within all
   * the cards in [hands] *)
  let hand_exists (hands : card list) (handrank : pokerhand) : bool =
    let ranks = ref (sort compare (fst (split hands))) in
    let hand_rank_lst = convert_phand_to_hand handrank in
    check hand_rank_lst ranks

  (* takes input from the user and parses it into a pokerhand type *)
  let parse_input (h : hand) (r : pokerhand) : move =
    print_endline ("Player 1, your turn! " ^ "Here is your hand: ");
    D.print_hand h;
    print_endline ("And this is the previous hand called: " ^ string_of_pokerhand r);
    print_endline "What is your move?";
    print_endline ">";
    let call = trim (lowercase_ascii (read_line ())) in
    try (
      let space = index call ' ' in
      let type_of_hand = sub call 0 space in
      match type_of_hand with
        | "four" -> let i = sub call (space + 1) 1 in
                    Raise (FourOfAKind (int_of_string i))
        | "fh" -> let first_i = sub call (space + 1) 1 in
                  let second_space = rindex call ' ' in
                  let second_i = sub call (second_space + 1) 1 in
                  Raise (FullHouse (int_of_string first_i) (int_of_string second_i))
        | "straight" -> let i = sub call (space + 1) 1 in
                        Straight (int_of_string i)
        | "three" -> let i = sub call (space + 1) 1 in
                    Raise (ThreeOfAKind (int_of_string i))
        | "tp" -> let first_i = sub call (space + 1) 1 in
                  let second_space = rindex call ' ' in
                  let second_i = sub call (second_space + 1) 1 in
                  Raise (TwoPair (int_of_string first_i) (int_of_string second_i))
        | "pair" -> let i = sub call (space + 1) 1 in
                    Raise (Pair (int_of_string i))
        | "hc" -> let i = sub call (space + 1) 1 in
                  Raise (HighCard (int_of_string i))
        | "bs" -> BS (r)
        | _ -> raise InvalidMove

    )
    with
      | Not_found _ -> print_endline "That is not a valid hand type. Please try again.";
                       parse_input h r
      | Invalid_argument _ -> print_endline "That is not a valid hand. Please try again.";
                              parse_input h r
      | InvalidMove -> print_endline "That is not a valid move. The kinds you can call are
                                      four, fh, straight, three, tp, pair, hc, and bs. Please try again.";
                       parse_input h r


  (* [human_turn h r] returns the move that the human player decides to make
   * based on his or her hand [h] and the previous hand called, [r] *)
  let human_turn (h: hand) (r : pokerhand) : move =
    let parsed_hand = parse_input h r in
    try (
      if (valid_call parsed_hand r = false) then raise InvalidMove
      else parsed_hand
    )
  with
    | InvalidMove -> print_endline "That hand is not a higher call than the previous hand. Please try again.";
                     parse_input h r


  (* returns true if [p] is a valid rank for a straight; ie [p] must be
   * between 6 and 10 but because a straight must have 5 cards in it so the
   * lowest possible stairght is 2, 3, 4, 5, 6 and the highest is 10, Jack,
   * Queen, King, Ace *)
  let valid_straight p =
    if (p < 6 || p > 10) then false
    else true


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
      | _ -> false

  (* [valid_call p_hand prev_hand] returns true if the pokerhand [p_hand] the
   * player calls is a valid call; ie it has a higher hand rank than the
   * previous hand [prev_hand]. Returns false if [p_hand] is an equal or lower
   * rank than [prev_hand] *)
  let valid_call (p_hand : pokerhand) (prev_hand : pokerhand) : bool =
    match p_hand with
      | Straight p -> (valid_straight p) && beats p_hand prev_hand
      | _ -> beats p_hand prev_hand

  (* [ai_turn id hand raisedhand raised] returns the move that the ai player
   * decides to make based on its [hand], its personality determined by its
   * [id], the last raised hand [raisedhand], and all the the cards, [raised]
   * that would be present if no player had yet lied *)
  val ai_turn  : pid -> hand -> pokerhand -> card list -> move

  (* [cards_of_pokerhand raised] is a card hand that would be ranked as the
   * pokerhand [raised] *)
  val cards_of_pokerhand : pokerhand -> hand

end
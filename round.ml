open List
open Deck
open Pervasives

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
  type move = | BS of hand (* BS carries the last hand called from the previous round that needs to be checked *)
              | Raise of hand (* Hand that the player is calling that is higher
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

  (* formats a pokerhand into a string for printing *)
  let string_of_pokerhand (phand: pokerhand) : string = match phand with
    | FourOfAKind p -> "Four " ^ D.string_of_rank p ^ "s"
    | FullHouse (p, t) -> "Three " ^ D.string_of_rank p ^ "s and two " ^ D.string_of_rank t ^ "s"
    | Straight p -> "Straight to " ^ D.string_of_rank p
    | ThreeOfAKind p -> "Three " ^ D.string_of_rank p ^ "s"
    | TwoPair (p, t) -> "Two " ^ D.string_of_rank p ^ "s and two " ^ D.string_of_rank t ^ "s"
    | Pair p -> "Two " ^ D.string_of_rank p ^ "s"
    | HighCard p -> "One " ^ D.string_of_rank p

  (* [deal_hands n] returns an associative list that maps [pid]s to hands of
   * [n] cards *)
  let rec deal_hands (players: pid * int list) (accum: pid * hand list) : pid * hand list =
    let d = D.shuffle_deck (D.new_deck D.empty) in
    match players with
      | [] -> accum
      | (p, n)::t -> deal_hands t ((p, (D.deal n d))::accum)

  (* returns: pid of the losing player of the round *)
  let rec run_turn r_info =
    match r_info.prev_move with
      | BS p -> if (hand_exists r_info.collective_cards p) then current_player
                else prev_player
    else if current_player = 1 then
      let move = human_turn in


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

  (* takes a pokerhand [phand] and converts it into a rank list of the ranks of the cards in the hand.
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
  let hand_exists (hands: card list) (handrank: pokerhand) : bool =
    let ranks = ref (sort compare (fst (split hands))) in
    let hand_rank_lst = convert_phand_to_hand handrank in
    check hand_rank_lst ranks

  (* [human_turn h r] returns the move that the human player decides to make
   * based on his or her hand [h] and the previous hand called, [r] *)
  let human_turn (h: hand) (r : pokerhand) : move =
    print_endline ("Player 1, your turn! " ^ "Here is your hand: ");
    D.print_hand;

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
      | FullHouse (p1, p2), FullHouse (t1, t2) ->
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
      | TwoPair (p1, p2), TwoPair (t1, t2) ->
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
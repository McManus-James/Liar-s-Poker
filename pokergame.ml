module type Poker = sig
  include Data.Cards
  type pid
  type pokerhand
  type move
end

module LiarsPoker = struct

  include Data.CardGame

  type pid = int

  type pokerhand =
    | FourOfAKind of int
    | FullHouse of (int * int) (* first int is rank of the three of a kind;
                                second int is the rank of the pair *)
    | Straight of int (* the int is the high card in the straight *)
    | ThreeOfAKind of int
    | TwoPair of (int * int) (* first int is rank of one of the pairs; second
                              int is the rank of the other pair *)
    | Pair of int
    | HighCard of int

  type move =
    | BS of pokerhand (* The hand which BS was called on *)
    | Raise of pokerhand (* The hand that was Raised *)

end

module type Round  = sig
  include Poker
  type state
  val init_state : pid -> state
  val update_state : pid -> state -> state
  val play_round : state -> pid
  val winner : state -> pid option
end

module GameRound = struct
  open List
  open Data.GameDeck
  include LiarsPoker
  exception InvalidMove
  exception InvalidRaise of pokerhand
  exception InvalidBS
  exception InvalidRank

  type state = {
    difficulty : int; (*difficulty of game. Between 1 and 3 incl. 1 = easy, 2 = medium, 3 = hard*)
    players : (pid * int) list; (* association list mapping the pid to the number of cards that player has *)
    cur_player : pid; (* the pid of the player who's turn it currently is *)
    prev_player : pid; (* the pid of the previous player who made a turn. *)
    hands_called : pokerhand list; (* list of pokerhands called so far in the
                                    round *)
    raised_hand : pokerhand option; (* previous move called in round *)
    hands : (pid * hand) list; (* association list mapping pids to their
                               respective hands *)
    cards : card list (* list of all the cards in play *)
  }

  (* initializes the [players] field of a round's state
   * [n] is the number of players to initialize
   * [players] is the association list mapping pids to
   * the numbers of cards they have *)
  let rec init_players n players d =
    if n = 0 then players
    else if n = 1 then init_players (n-1) ((n,4)::players) d
    else match d with
      |1 -> init_players (n - 1) ((n,4)::players) d
      |2 -> init_players (n - 1) (((n+10),4)::players) d
      |_ -> init_players (n - 1) (((n+20),4)::players) d


  (* [deal_hands] is an association list mapping pid's to the number
   * of cards they should receive  *)
  let rec deal_hands players accum d =
    match players with
      | [] -> accum
      | (p, n)::t -> deal_hands t ((p, (deal n d))::accum) d

  let init_state n d =
    let players = init_players n [] d in
    let hands = deal_hands players [] (shuffle_deck (new_deck empty)) in
    let cards = List.split hands |> snd |> List.flatten in
    { difficulty = d;
      players = players;
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
    if i = (List.length pids -1) then hd pids
    else nth pids (i+1)

  let rec update_players loser players accu =
  match players with
  | [] -> accu
  | (pid,num_cards)::tl ->
      if loser = pid then
        if num_cards = 1 then
        (print_endline ("Player "^(string_of_int loser)^" is out!");
        update_players loser tl accu)
        else update_players loser tl (accu@[(pid,num_cards-1)])
      else update_players loser tl (accu@[(pid, num_cards)])


  let update_state l s =
    let players = update_players l s.players [] in
    let next =
      if List.length players <> length s.players then
        next_player l s.players
      else l
    in
    let hands = deal_hands players [] (shuffle_deck (new_deck empty)) in
    let cards = split hands |> snd |> flatten in
    { s with
      cur_player = next;
      players = players;
      hands = hands;
      cards = cards
    }

  (* takes a pokerhand [phand] and converts it into a rank list of the ranks of
   * the cards in the hand that's called for checking purposes.
   * Does not return a card list, because the suits of the cards are not
   * considered when checking to see if the
   * hand is in the collective set of cards in play *)
  let convert_phand_to_rank phand =
    match phand with
    | FourOfAKind p -> [p; p; p; p]
    | FullHouse (p, t) -> sort compare [p; p; p; t; t]
    | Straight p -> [p - 4; p - 3; p - 2; p - 1; p]
    | ThreeOfAKind p -> [p; p; p]
    | TwoPair (p, t) -> sort compare [p; p; t; t]
    | Pair p -> [p; p]
    | HighCard p -> [p]


  let convert_rank_to_phand hand = match hand with
    | a::[] -> HighCard a
    | a::b::[] -> Pair a
    | a::b::c::[] -> ThreeOfAKind a
    | a::b::c::d::[] -> if a = c then FourOfAKind a else TwoPair (a, c)
    | a::b::c::d::e::[] -> if a = b then FullHouse (a, d) else Straight e
    | _ -> raise InvalidMove


  (* returns true if every card rank in [called_ranks] is in [rank_lst]. Returns
   * false otherwise *)
  let rec check_straight called_ranks rank_lst =
    match called_ranks with
      | [] -> true
      | h::t -> (exists (fun x -> x = h) rank_lst) && check_straight t rank_lst

  let hand_exists hands handrank =
    let ranks = (List.sort compare (fst (List.split hands))) in
    let hand_rank_lst = convert_phand_to_rank handrank in
    match handrank with
      | HighCard p -> exists (fun x -> x = p) ranks
      | Pair p -> let lst = filter (fun x -> x = p) ranks in
                  if (List.length lst > 1) then true
                  else false
      | TwoPair (p, t) -> let lst = filter (fun x -> x = p) ranks in
                          let lst2 = filter (fun x -> x = t) ranks in
                          if (List.length lst > 1) && (List.length lst2 > 1)
                            then true
                          else false
      | ThreeOfAKind p -> let lst = filter (fun x -> x = p) ranks in
                          if (List.length lst > 2) then true
                          else false
      | Straight p -> check_straight hand_rank_lst ranks
      | FullHouse (p, t) -> let lst = filter (fun x -> x = p) ranks in
                            let lst2 = filter (fun x -> x = t) ranks in
                            if (List.length lst > 2) && (List.length lst2 > 1)
                              then true
                            else false
      | FourOfAKind p -> let lst = filter (fun x -> x = p) ranks in
                         if (List.length lst > 3) then true
                         else false


  (* [string_of_pokerhand phand] returns the string representation of [phand]
   * requires: [phand] is a valid pokerhand
   *)
  let string_of_pokerhand phand =
    match phand with
    | FourOfAKind p -> "four " ^ string_of_rank p ^ "s"
    | FullHouse (p, t) -> "full house with three " ^ string_of_rank p ^
                          "s and two " ^ string_of_rank t ^ "s"
    | Straight p -> "straight to " ^ string_of_rank p
    | ThreeOfAKind p -> "three " ^ string_of_rank p ^ "s"
    | TwoPair (p, t) -> "two " ^ string_of_rank p ^ "s and two "
                        ^ string_of_rank t ^ "s"
    | Pair p -> "a pair of " ^ string_of_rank p ^ "s"
    | HighCard p -> "highcard of " ^ string_of_rank p


  (* [compare_phands p1 p2] is 1 if p1 is greater than p2, 0 if they are of the
   * same rank, and -1 if p2 is greater than p1 *)
  let compare_phands p1 p2 =
    match p1,p2 with
    | FourOfAKind x, FourOfAKind y -> compare x y
    | FourOfAKind _, _ -> 1
    | _, FourOfAKind _ -> -1
    | FullHouse (t1,p1), FullHouse (t2,p2) ->
      if compare t1 t2 = 0 then compare p1 p2
      else compare t1 t2
    | FullHouse _, _ -> 1
    | _, FullHouse _ -> -1
    | Straight x, Straight y -> compare x y
    | Straight _, _ -> 1
    | _, Straight _ -> -1
    | ThreeOfAKind x, ThreeOfAKind y -> compare x y
    | ThreeOfAKind _, _ -> 1
    | _, ThreeOfAKind _ -> -1
    | TwoPair (x1,y1), TwoPair (x2,y2) ->
      if compare x1 x2 = 0 then compare y1 y2
      else compare x1 x2
    | TwoPair _, _ -> 1
    | _, TwoPair _ -> -1
    | Pair x, Pair y -> compare x y
    | Pair _, _ -> 1
    | _, Pair _ -> -1
    | HighCard x, HighCard y -> compare x y

  let int_of_rank rank =
    match rank with
    | ("ace" | "aces") -> 14
    | ("king" | "kings") -> 13
    | ("queen" | "queens") -> 12
    | ("jack" | "jacks") -> 11
    | ("10s" | "10") -> 10
    | _ ->
        try (
          let i =
            if String.length rank = 2 && String.get rank 1 = 's' then
              (int_of_string (String.sub rank 0 1))
            else (int_of_string rank)
          in
          if (i > 1) && (i < 15) then i
          else raise InvalidRank) with
        | Failure _ -> raise InvalidMove

  let parse_single_rank rank =
    if List.length rank <> 1 then
      raise InvalidMove
    else int_of_rank (hd rank)

  let parse_double_rank ranks =
    if List.length ranks <> 2 then
      raise InvalidMove
    else (int_of_rank (hd ranks), int_of_rank (nth ranks 1))

   let parse_straight_rank rank =
    if List.length rank <> 1 then
      raise InvalidMove
    else
      let r = int_of_rank (hd rank) in
      if (r < 6 || r > 14) then raise InvalidMove
      else r

  let parse_raised raised =
    let hand_type = hd raised in
    let hand_rank = tl raised in
    match hand_type with
      | "four" -> FourOfAKind (parse_single_rank hand_rank)
      | "fh" -> FullHouse (parse_double_rank hand_rank)
      | "straight" -> Straight (parse_straight_rank hand_rank)
      | "three" -> ThreeOfAKind (parse_single_rank hand_rank)
      | "tp" -> TwoPair (parse_double_rank hand_rank)
      | "pair" -> Pair (parse_single_rank hand_rank)
      | "hc" -> HighCard (parse_single_rank hand_rank)
      | _ -> raise InvalidMove

  let handle_acronyms c =
    let rep = Str.global_replace in
    let reg = Str.regexp in
    rep (reg "four of a kind") "four" c
    |> rep (reg "full house") "fh"
    |> rep (reg "three of a kind") "three"
    |> rep (reg "two pair") "tp"
    |> rep (reg "high card") "hc"

  (* takes input from the user and parses it into a pokerhand type *)
  let parse_move move ph =
    let r = Str.regexp "[^a-zA-Z0-9]+" in
    let words =  handle_acronyms move |> Str.split r in
    let num_words = List.length words in
    if num_words = 0 then raise InvalidMove
    else
      let move_type = List.hd words in
      if (num_words = 1 && move_type = "bs" && ph = HighCard 1) then
        raise InvalidBS
      else if (num_words = 1 && move_type = "bs") then BS ph
      else if num_words > 1 && move_type = "raise" then
        let raise_type = tl words in
        let raised = parse_raised raise_type in
        if compare_phands raised ph = 1 then
          Raise (parse_raised raise_type)
        else raise (InvalidRaise raised)
      else raise InvalidMove

  let print_valid_moves ()=
    print_endline ("\nValid moves consist of calling just BS or calling RAISE "
                   ^"followed by a valid pokerhand \nthat is higher than the "
                   ^"previously raised pokerhand. \nValid pokerhands include:");
    print_endline ("FOUR OF A KIND (four) followed by the rank. \nEx: four 4");
    print_endline ("FULL HOUSE (fh) followed by the rank of the three of a kind"
                   ^" and the rank of the pair \nEx: fh 3 aces");
    print_endline ("STRAIGHT followed by the highest card in the straight\n"
                   ^"Ex: Straight 9 is 5, 6, 7, 8, 9.");
    print_endline ("THREE OF A KIND (three) followed by the rank");
    print_endline ("TWO PAIR (tp) followed by the rank of the first pair, "
                   ^"then the rank of the second pair");
    print_endline ("PAIR followed by the rank");
    print_endline ("HIGH CARD (hc) followed by the rank");
    print_endline ("Valid ranks include the numbers 2-14(s), jack(s), queen(s),"
                  ^" king(s) and ace(s)");
    print_endline ("Valid ranks for a STRAIGHT do not include the numbers"
                  ^" 2-5, because\nthe lowest possible straight is:"
                  ^" 2,3,4,5,6")

  let print_numcards (p,n) =
    print_endline ("Player "^string_of_int (p mod 10)^" has "^(string_of_int n)
                  ^" cards.")

  let rec print_number_cards p = match p with
    | [] -> ()
    | (pid, n)::t -> print_numcards (pid, n);
                     print_number_cards t

  let print_total_num_cards (p : (pid * int) list) =
    let num_cards = snd (split p) in
    let total_num_cards = fold_left (+) 0 num_cards in
    print_endline ("The total number of cards in play is "^
                  string_of_int total_num_cards^".")


 let rec human_turn h (ph:pokerhand option) (pl : (pid * int) list) =
    print_endline ("\nPlayer 1, your turn! Here is your hand: \n");
    print_hand h;
    let prev =
      match ph with
      |None -> HighCard 1
      |Some p -> p
    in
    let () =
      if ph <> None then
        (print_endline ("\nThe previous call was: "^
          (string_of_pokerhand prev));)
      else ()
   in
    print_endline ("\nWhat is your move? (Type \"help\" to display valid "^
      "moves\n or \"numcards\" to display the total number of cards in play \n"^
      "and how many cards each player currently has)");
    print_string "> ";
    let move = read_line ()
              |> String.trim
              |> String.lowercase_ascii
    in
    if move = "help" then
      (print_valid_moves ();
      human_turn h ph pl)
    else if move = "numcards" then
      (print_endline "";
      print_total_num_cards pl;
      print_number_cards pl;
      human_turn h ph pl)
    else
      try (parse_move move prev) with
      | InvalidMove ->
          print_endline "I'm sorry, but that is not a valid move.";
          human_turn h ph pl
      | InvalidRaise phand ->
          print_endline ("I'm sorry, but "^(string_of_pokerhand phand)
                        ^" is not a higher hand than the previously raised hand"
                        ^":\n"^(string_of_pokerhand prev));
          human_turn h ph pl
      | InvalidBS ->
          print_endline ("You can't call BS on the first turn of a"
                        ^" round.");
          human_turn h ph pl
      | InvalidRank -> print_endline ("I'm sorry, but that is not a valid "^
                       "rank. Please try again.");
                       human_turn h ph pl


(******************AI*********************)

(*[match_one_card] is none if card is not found in [cards] and Some list which
 *is [cards] with [card] removed
 *[cards] is potential cards in play
 *[card] is single card from a pokerhand
 *[ret_hand] is cards that won't contain [card]
*)
let rec match_one_card card cards ret_hand = match cards with
  | [] -> None
  | h::t -> if card = h then Some (ret_hand@t)
    else let ret_hand_update = h::ret_hand in
    match_one_card card t ret_hand_update

(*[count_one_hand] is tuple with number of cards in common between [next_hand]
 *and [cards_in_play] and the hand itself
 *[next_hand] is next potential pokerhand
 *[cards_in_play] is potential cards in play
*)
let rec count_one_hand next_hand cards_in_play ret = match next_hand with
  | [] -> ret
  | h::t -> let match_one_card_return = match_one_card h cards_in_play [] in
    (match match_one_card_return with
      | None -> count_one_hand t cards_in_play ret
      | Some l -> count_one_hand t l (fst ret + 1, snd ret))

(*[compare_hand2] returns hand with most cards in common with cards in play
 *between cur_hand and next_hand.
 *[cur_hand] is tuple with current pokerhand that shares most cards with
 *cards in play and that number
 *[cards_in_play] is list of ranks of player's hand/previously called hands
 *[next_round] is next potential pokerhand
*)
let compare_hand cur_hand cards_in_play next_hand =
  let next_hand_rank = convert_phand_to_rank next_hand in
  let next = count_one_hand (next_hand_rank) cards_in_play
    (0, next_hand_rank) in
  (if fst next > fst cur_hand then
    (fst next, convert_rank_to_phand (snd next)) else  cur_hand)

(*[compare_hand2] returns hand with most cards in common with cards in play
 *between cur_hand and next_hand. [compare_hand2] returns hand such that every
 *card is found in the cards in play
 *[cur_hand] is tuple with current pokerhand that shares most cards with
 *cards in play and that number
 *[cards_in_play] is list of ranks of player's hand/previously called hands
 *[next_round] is next potential pokerhand
*)
let compare_hand2 cur_hand cards_in_play next_hand =
  let next_hand_rank = convert_phand_to_rank next_hand in
  let next = count_one_hand (next_hand_rank) cards_in_play
    (0, next_hand_rank) in
  if fst next > 0 && (fst next) - (List.length next_hand_rank) = 0
  then (fst next, convert_rank_to_phand (snd next))
  else cur_hand

(*[choose_hand1] is the lowest ranking pokerhand that has the most
 *cards in common with bothe the previously called hands and the players
 *current hand.
 *[cur_hand] is the current hand with most cards in common
 *[hand_ranks] is list of card-ranks in players hand/previously called hands
 *[p_hands] is list if previously called pokerhands
 *)
let rec choose_hand1 cur_hand hand_ranks p_hands =
  match p_hands with
    | [] -> snd cur_hand
    | h::t -> let x = (compare_hand2 cur_hand hand_ranks h) in
      if snd x = (HighCard 2) then choose_hand1 (compare_hand cur_hand hand_ranks h) hand_ranks t
      else choose_hand1 x hand_ranks t



let rec get_higher_three n lst =
  if n > 14 then List.rev lst else
  get_higher_three (n + 1) ((ThreeOfAKind n)::lst)

let rec get_higher_pair n lst =
  if n > 14 then List.rev lst else
  get_higher_pair (n + 1) ((Pair n)::lst)

let rec get_higher_four n lst =
  if n > 14 then List.rev lst else
  get_higher_four (n + 1) ((FourOfAKind n)::lst)

let rec get_higher_straight n lst =
  if n > 14 then List.rev lst
  else if n < 6 then get_higher_straight (n + 1) lst
  else get_higher_straight (n + 1) ((Straight n)::lst)

let rec get_higher_high_card n lst =
  if n > 14 then List.rev lst else
  get_higher_high_card (n + 1) ((HighCard n)::lst)

let rec get_higher_two_pair_help2 low high lst =
    if low >= high then lst else
  get_higher_two_pair_help2 (low + 1) high ((TwoPair (low, high))::lst)

let rec get_higher_two_pair_help1 high lst =
  if high > 14 then lst
else get_higher_two_pair_help1 (high + 1) ((get_higher_two_pair_help2 2 high [])@lst)

let get_higher_two_pair low high =
  List.rev ((get_higher_two_pair_help1 (high + 1) [])@(get_higher_two_pair_help2 (low + 1) high []))

let rec get_higher_full_house_help2 low high lst =
    if low >= high then lst else
  get_higher_full_house_help2 (low + 1) high ((FullHouse (low, high))::lst)

let rec get_higher_full_house_help1 high lst =
  if high > 14 then lst
else get_higher_full_house_help1 (high + 1) ((get_higher_full_house_help2 2 high [])@lst)

let get_higher_full_house low high =
  List.rev ((get_higher_full_house_help1 (high + 1) [])@(get_higher_full_house_help2 (low + 1) high []))



let get_higher_hands hand = match hand with
  | HighCard h -> (get_higher_high_card (h + 1) [])@(get_higher_pair 2 [])
    @(get_higher_two_pair 2 2)@(get_higher_three 2 [])
    @(get_higher_straight 2 [])@(get_higher_full_house 2 2)
    @(get_higher_four 2 [])
  | Pair h -> (get_higher_pair (h + 1) [])@(get_higher_two_pair 2 2)
    @(get_higher_three 2 [])@(get_higher_straight 2 [])
    @(get_higher_full_house 2 2)@(get_higher_four 2 [])
  | TwoPair (a,b) -> let h = (if a > b then a else b) in
    let l = (if a < b then a else b) in
    (get_higher_two_pair l h)@(get_higher_three 2 [])
    @(get_higher_straight 2 [])@(get_higher_full_house 2 2)
    @(get_higher_four 2 [])
  | ThreeOfAKind h -> (get_higher_three (h + 1) [])@(get_higher_straight 2 [])
    @(get_higher_full_house 2 2)@(get_higher_four 2 [])
  | Straight h -> (get_higher_straight (h + 1) [])@(get_higher_full_house 2 2)
    @(get_higher_four 2 [])
  | FullHouse (a,b) -> let h = (if a > b then a else b) in
    let l = (if a < b then a else b) in
    (get_higher_full_house l h)@(get_higher_four 2 [])
  | FourOfAKind h -> get_higher_four (h + 1) []


type hand_lists = {
  high : pokerhand list;
  pair : pokerhand list;
  two_pair : pokerhand list;
  three : pokerhand list;
  straight : pokerhand list;
  full_house : pokerhand list;
  four : pokerhand list
  }

let rec split_pokerhand_list  prev_hands hand_lists = match prev_hands with
  | [] -> hand_lists
  | h::t -> (match h with
    | HighCard a -> split_pokerhand_list t {hand_lists with high = (HighCard a)::hand_lists.high}
    | Pair a -> split_pokerhand_list t {hand_lists with pair = (Pair a)::hand_lists.pair}
    | TwoPair (a,b) -> split_pokerhand_list t {hand_lists with two_pair = (TwoPair (a,b))::hand_lists.two_pair}
    | ThreeOfAKind a -> split_pokerhand_list t {hand_lists with three = (ThreeOfAKind a)::hand_lists.three}
    | Straight a -> split_pokerhand_list t {hand_lists with straight = (Straight a)::hand_lists.straight}
    | FullHouse (a,b) -> split_pokerhand_list t {hand_lists with full_house = (FullHouse (a,b))::hand_lists.full_house}
    | FourOfAKind a -> split_pokerhand_list t {hand_lists with four = (FourOfAKind a)::hand_lists.four}
  )

let rec remove_three n lst ret = match lst with
  | [] -> ret
  | h::t -> (match h with
    | ThreeOfAKind a -> if a = n then remove_three n t ret else remove_three n t (h::ret)
    | _ -> failwith "somethin dun fuck up")


let rec remove_pair n lst ret = match lst with
  | [] -> ret
  | h::t -> (match h with
    | Pair a -> if a = n then remove_pair n t ret else remove_pair n t (h::ret)
    | _ -> failwith "somethin dun fuck up")

let rec remove_high n lst ret = match lst with
  | [] -> ret
  | h::t -> (match h with
    | HighCard a -> if a = n then remove_high n t ret else remove_high n t (h::ret)
    | _ -> failwith "somethin dun fuck up")

let rec remove_two_pair n1 n2 lst ret = match lst with
  | [] -> ret
  | h::t -> (match h with
    | TwoPair (a,b) -> if (a = n1 && b = n2) || (a = n2 && b = n1) then remove_two_pair n1 n2 t ret else remove_two_pair n1 n2 t (h::ret)
    | _ -> failwith "somethin dun fuck up")

let rec add_fours hand_lists potential_cards four_lst = match four_lst with
  | [] -> (hand_lists, potential_cards)
  | h::t -> (match h with
    | FourOfAKind a -> add_fours {hand_lists with
      three = remove_three a hand_lists.three [];
      pair = remove_pair a hand_lists.pair [];
      high = remove_high a hand_lists.high []}
      ((convert_phand_to_rank (h))@potential_cards) t
    | _ -> failwith "somethin dun fuck up"
  )

let rec add_full_houses hand_lists potential_cards full_house_lst = match full_house_lst with
  | [] -> (hand_lists, potential_cards)
  | h::t -> (match h with
    | FullHouse (a,b) -> add_full_houses {hand_lists with
      three = remove_three a hand_lists.three [];
      pair = remove_pair b hand_lists.pair [];
      high = remove_high b (remove_high a hand_lists.high []) []}
      ((convert_phand_to_rank (h))@potential_cards) t
    | _ -> failwith "somethin dun fuck up"
  )

let rec add_threes hand_lists potential_cards three_lst = match three_lst with
  | [] -> (hand_lists, potential_cards)
  | h::t -> (match h with
    | ThreeOfAKind a -> add_threes {hand_lists with
      pair = remove_pair a hand_lists.pair [];
      high = remove_high a hand_lists.high []}
      ((convert_phand_to_rank (h))@potential_cards) t
    | _ -> failwith "somethin dun fuck up"
)

let rec add_two_pairs hand_lists potential_cards two_pair_lst = match two_pair_lst with
  | [] -> (hand_lists, potential_cards)
  | h::t -> (match h with
    | TwoPair (a,b) -> add_two_pairs {hand_lists with
      pair = remove_pair b (remove_pair a hand_lists.pair []) [];
      high = remove_high b (remove_high a hand_lists.high []) []}
      ((convert_phand_to_rank (h))@potential_cards) t
    | _ -> failwith "somethin dun fuck up"
)

let rec add_pairs hand_lists potential_cards two_lst = match two_lst with
  | [] -> (hand_lists, potential_cards)
  | h::t -> (match h with
    | Pair a -> add_pairs {hand_lists with
      high = remove_high a hand_lists.high []}
      ((convert_phand_to_rank (h))@potential_cards) t
    | _ -> failwith "somethin dun fuck up"
)

let rec add_high potential_cards two_lst = match two_lst with
  | [] -> potential_cards
  | h::t -> add_high ((convert_phand_to_rank (h))@potential_cards) t
  (*   | _ -> failwith "somethin dun fuck up" *)


let rec add_straights_help potential_cards straight_lst = match straight_lst with
  | [] -> potential_cards
  | h::t -> if List.mem h potential_cards then add_straights_help potential_cards t else add_straights_help (h::potential_cards) t

let rec add_straights potential_cards straight_lst =
  let rank_straight_lst = List.flatten (List.map convert_phand_to_rank straight_lst) in
  let rank_st_lst_no_dups = List.sort_uniq Pervasives.compare rank_straight_lst in
  add_straights_help potential_cards rank_st_lst_no_dups

let get_potential_cards pokerhands =
  let split_hands = split_pokerhand_list pokerhands {high = []; pair = [];
  two_pair = []; three = []; straight = []; full_house= []; four = []} in
  let fours_added = add_fours split_hands [] split_hands.four in
  let full_houses_addded = add_full_houses (fst fours_added) (snd fours_added) ((fst fours_added).full_house) in
  let three_added = add_threes (fst full_houses_addded) (snd full_houses_addded) ((fst full_houses_addded).three) in
  let two_pair_added = add_two_pairs (fst three_added) (snd three_added) ((fst three_added).two_pair) in
  let pairs_added = add_pairs (fst two_pair_added) (snd two_pair_added) ((fst two_pair_added).pair) in
  let high_added = add_high (snd pairs_added) ((fst pairs_added).high) in
  add_straights high_added (fst pairs_added).straight


let choose_hand2 player_hand prev_hands prev_hand =
  let higher_hands = get_higher_hands prev_hand in
  let player_hand_ranks = fst (List.split player_hand) in
  let prev_hand_ranks = get_potential_cards prev_hands in
  choose_hand1 ((-1), HighCard 2) (player_hand_ranks@prev_hand_ranks) higher_hands

let rec get_num hands prev_hand accum = match prev_hand with
  | [] -> accum
  | h::t -> if List.mem h hands then get_num hands t (accum + 1)
    else get_num hands t accum

let bs hands prev_hand diff =
  let t1 = if diff = 1 then [94;85;80;55;45;25] else if diff = 2 then [98;93;85;70;75;60;60] else [98;93;85;70;75;60;60] in
  let t2 = if diff = 1 then [93;86;55;35;30] else if diff = 2 then [75;60;45;15;10] else [75;60;45;15;10] in
  Random.self_init ();
  let random = Random.int 100 in
  let cards = fst (List.split hands) in
  let hand_ranks = convert_phand_to_rank prev_hand in
  let len = List.length hand_ranks in
  let num = get_num cards hand_ranks 0 in
  let dif = len - num in
  if dif = 0 then
    (match prev_hand with
      | HighCard _ -> if random > List.nth t1 0 then true else false
      | Pair _ -> if random > List.nth t1 1 then true else false
      | TwoPair _ -> if random > List.nth t1 2 then true else false
      | ThreeOfAKind _ -> if random > List.nth t1 3 then true else false
      | Straight _ -> if random > List.nth t1 4 then true else false
      | FullHouse _ -> if random > List.nth t1 5 then true else false
      | FourOfAKind _ -> if random > List.nth t1 6 then true else false
    )
  else if dif = 1 && random > List.nth t2 0 then true
  else if dif = 2 && random > List.nth t2 1 then true
  else if dif = 3 && random > List.nth t2 2 then true
  else if dif = 4 && random > List.nth t2 3 then true
  else if dif = 5 && random > List.nth t2 4 then true
  else false

let rec get_num l u =
  Random.self_init ();
  let num = Random.int u in
  if num >= l then num else get_num l u

let lie hand diff =
  Random.self_init ();
  let lie = Random.int 11 in
  let c1 = (get_num 2 15, Hearts) in
  let c2 = (get_num 2 15, Hearts) in
  let c3 = (get_num 2 15, Hearts) in
  if diff = 1 then hand else
    let new_hand =
    if lie > 8 then (
      match hand with
        | h1::h2::h3::t -> (c1::c2::c3::t)
        | h1::h2::t -> (c1::c2::t)
        | h::t -> (c1::t)
        | _ -> hand)
    else if lie > 6 then (
      match hand with
        | h1::h2::t -> (c1::c2::t)
        | h::t -> (c1::t)
        | _ -> hand)
    else if lie > 4 then (
      match hand with
        | h::t -> (c1::t)
        | _ -> hand)
    else hand
    in new_hand


let choose_hand3 hand all_hands prev_hands prev_hand first_hand diff =
  Random.self_init ();
  let automatic_bs = Random.int 11 in
  let new_hand = lie hand diff in
  let next_hand = if List.length prev_hands = 0 then choose_hand2 new_hand prev_hands (HighCard 2)
  else choose_hand2 new_hand prev_hands prev_hand in
  let is_bs = if first_hand then false else
  (match prev_hand with
  | FourOfAKind a -> if a = 14 then true else bs all_hands prev_hand diff
  | _ -> bs all_hands prev_hand diff) in
  let len = List.length (convert_phand_to_rank next_hand) in
  let cards_present = count_one_hand (convert_phand_to_rank next_hand) (fst (List.split hand)) (0, convert_phand_to_rank next_hand) in
  let dif = len - fst cards_present in
  if is_bs then BS prev_hand
  else if dif >= 2 && automatic_bs > 7 then BS prev_hand
  else if dif >= 3 && automatic_bs > 4 then BS prev_hand
  else if dif >= 4 && automatic_bs > 3 then BS prev_hand
  else Raise next_hand

let new_bs cards prev_hand diff =
  Random.self_init ();
  let random = Random.int 100 in
  match prev_hand with
  |FourOfAKind 14 -> true
  |_ ->
    let b = hand_exists cards prev_hand in
    match (diff/10) with
    |0 -> if random > 75 then not b else b
    |1 -> if random > 50 then not b else b
    |_ -> if random > 25 then not b else b


let rec nh_helper ph cards hl p =
Random.self_init ();
let random = Random.int 100 in
match hl with
|[] -> BS ph
|hd::tl -> match (p/10) with
  |0-> if random > 50 then
          Raise hd
        else (
          if hand_exists cards hd then Raise hd
          else nh_helper ph cards tl p)
  |1-> if random > 75 then
          Raise hd
        else (
          if hand_exists cards hd then Raise hd
          else nh_helper ph cards tl p)
  |_-> if random > 95 then
          Raise hd
        else (
          if hand_exists cards hd then Raise hd
          else nh_helper ph cards tl p)

let trusting_ai id h ph cards hands_called diff =
  match ph with
  | Some h2 -> choose_hand3 h cards hands_called h2 false diff
  | None -> choose_hand3 h cards hands_called (HighCard 1) true diff

let cheating_ai id ph cards =
  match ph with
  |Some ha ->
    if (new_bs cards ha id) then BS ha
    else nh_helper ha cards (get_higher_hands ha) id
  |None -> nh_helper (HighCard 1) cards (get_higher_hands (HighCard 1)) id

let ai_turn id h ph cards ph_lst diff =
  if (id mod 10 = 3) then cheating_ai id ph cards
  else trusting_ai id h ph cards ph_lst diff




(********************** END AI **************************)

(* prints the hand of each player *)
  let rec print_player_hands (hands : (pid * hand) list) =
    match hands with
    | [] -> ()
    | (pid, hand)::t -> print_endline ("Player " ^ (string_of_int (pid mod 10))
                        ^ "'s hand is:");
                        print_hand hand;
                        print_endline "";
                        print_player_hands t


  let print_pokerhand ph =
    print_endline (string_of_pokerhand ph)


  let print_raised ph =
    match ph with
    | None -> print_endline "NONE"
    | Some p -> print_pokerhand p


  (* [print_state s] prints all of the fields of [s] for debugging purposes *)
  let print_state s =
    List.iter print_numcards s.players;
    print_endline
      ("The current player is Player "^(string_of_int s.cur_player));
    print_endline
      ("The previous player was Player "^(string_of_int s.prev_player));
    print_endline "The previous hand's called are: ";
    List.iter print_pokerhand s.hands_called;
    print_raised s.raised_hand;
    print_player_hands s.hands;
    print_endline "All of the cards in play are:";
    print_hand s.cards

  let rec play_round s =
    let cur_hand = List.assoc s.cur_player s.hands in
    let move =
      if s.cur_player = 1 then human_turn cur_hand s.raised_hand s.players
      else ai_turn s.cur_player (List.assoc s.cur_player s.hands) s.raised_hand s.cards s.hands_called s.difficulty
    in
    let cur_p = "Player "^(string_of_int (s.cur_player mod 10)) in
    let prev_p = "Player "^(string_of_int (s.prev_player mod 10)) in
    match move with
    | BS p ->
      print_endline (cur_p^" called BS!"
                          ^ " Let's check if the previous hand is there...\n");
      print_player_hands s.hands;
      if (hand_exists s.cards p) then
        (print_endline ("The hand "^(string_of_pokerhand p)^" is here. "
                      ^cur_p^" loses this round.");
        print_endline ("\n*********************************** END OF ROUND "^
                      "***********************************\n");
        s.cur_player)
      else
        (print_endline ("The hand "^(string_of_pokerhand p)^" is not here. "
                      ^prev_p^" loses this round.");
        print_endline ("\n*********************************** END OF ROUND "^
                      "***********************************\n");
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

 let winner s =
  if List.length s.players = 1 then
    let winning_pid = (fst (List.hd s.players)) in
    Some (winning_pid mod 10)
  else None

end
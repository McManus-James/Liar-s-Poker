open String
open Pervasives
open List

module type Deck = sig
  type card
  type hand = card list
  type deck
  val empty : deck
  val new_deck : deck -> deck
  val shuffle_deck: deck -> deck
  val deal : int -> deck -> hand
  val print_hand : hand -> unit
end

module GameDeck = struct

  type rank = int

  type suit = Hearts | Clubs | Diamonds | Spades

  type card = (rank * suit)

  type hand = card list

  type deck = card list ref

  let empty = ref []

  (* helper function taken from Recitation 3: Lists, and Testing with OUnit
   * that creates an infix operator that makes a list of all integers from i
   * through j inclusive *)
  let (--) i j =
    let rec from i j l =
      if i>j then l
      else from i (j-1) (j::l)
      in from i j []

  (* [string_of_suit s] returns the string representation of suit [s] *)
  let string_of_suit (s:suit) = match s with
    | Spades -> "Spades"
    | Clubs -> "Clubs"
    | Diamonds -> "Diamonds"
    | Hearts -> "Hearts"

  (* [string_of_rank r] returns the string representation of rank [r] *)
  let string_of_rank r =
    if r = 11 then "Jack"
    else if r = 12 then "Queen"
    else if r = 13 then "King"
    else if r = 14 then "Ace"
    else string_of_int r

  (* [string_of_card c] formats a  *)
  let string_of_card (c:card) : string =
    string_of_rank (fst c) ^ " of " ^ string_of_suit (snd c)

  (* print a hand. h is the hand to print. *)
  let rec print_hand h = match h with
    | [] -> ()
    | h::t -> print_endline (string_of_card h);
              print_hand t

  (* helper function to instantiate a 52 card deck.
   * [suit] is a suit, and [rank_list] is a list of
   * all possible ranks
   * [accum] is an accumulator holding the deck created so far
   * function returns all the cards for a particular suit, from 2 through Ace
   * (represented as rank 14) *)
  let rec deck_helper suit rank_list accum = match rank_list with
    | [] -> accum
    | h::t -> deck_helper suit t ((h, suit)::accum)

  (* helper function that matches a list of suits and for each suit calls
   * [deck_helper] to instantiate cards 1-14 for each suit *)
  let rec suit_helper suit_list accum = match suit_list with
    | [] -> accum
    | h::t -> suit_helper t ((deck_helper h (2--14) [])@accum)


  let rec new_deck (e : deck) : deck = ref (suit_helper [Hearts; Spades; Clubs; Diamonds] (!e))

  (* Helper method for modifying the deck returns all but the first [n] elements of [lst]. If [lst] has fewer than
   * [n] elements, return the empty list. Code taken from Recitation: Lists,
   * and Testing with OUnit *)
  let rec drop n h =
    if n =0 then h else match h with
      | []->[]
      | x::xs -> drop (n-1) xs

  (* returns the first [n] elements of [lst]. If [lst] has fewer than [n] elements, return all of them.
   * Code taken from Recitation: Lists, and Testing with OUnit *)
  let rec take n l =
    if n = 0 then [] else match l with
      | [] -> []
      | (x::xs) -> x :: (take (n-1) xs)

  let shuffle_deck (d : deck) : deck =
    let deck = !d in
    let array_deck = Array.of_list deck in
    Random.self_init ();
    for i=0 to 10000 do
      let first = Random.int 52 in
      let second = Random.int 52 in
      let old_card_one = array_deck.(first) in
      let old_card_two = array_deck.(second) in
      array_deck.(second) <- old_card_one;
      array_deck.(first) <- old_card_two;
    done;
    d := (Array.to_list array_deck);
    d

  let deal (n : int) (d : deck) : hand =
    let hand = take n !d in
    d := drop n !d;
    hand

end

exception InvalidMove
exception InvalidRank

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

  let rec deal_hands players accum d =
    match players with
      | [] -> accum
      | (p, n)::t -> deal_hands t ((p, (deal n d))::accum) d

  let init_state n =
    let players = init_players n [] in
    let hands = deal_hands players [] (shuffle_deck (new_deck empty)) in
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
    let hands = deal_hands players [] (shuffle_deck (new_deck empty)) in
    let cards = split hands |> snd |> flatten in
    { s with
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
    | FullHouse (p, t) -> List.sort compare [p; p; p; t; t]
    | Straight p -> [p - 4; p - 3; p - 2; p - 1; p]
    | ThreeOfAKind p -> [p; p; p]
    | TwoPair (p, t) -> List.sort compare [p; p; t; t]
    | Pair p -> [p; p]
    | HighCard p -> [p]


let convert_rank_to_phand hand = match hand with
  | a::[] -> HighCard a
  | a::b::[] -> Pair a
  | a::b::c::[] -> ThreeOfAKind a
  | a::b::c::d::[] -> if a = c then FourOfAKind a else TwoPair (a, c)
  | a::b::c::d::e::[] -> if a = b then FullHouse (a, d) else Straight e
  | _ -> raise InvalidMove

let rec match_one_card card hand ret_hand = match hand with
  | [] -> None
  | h::t -> if card = h then Some (ret_hand@t)
    else let ret_hand_update = h::ret_hand in match_one_card card t ret_hand_update

let rec count_one_hand next_hand player_hand ret = match next_hand with
  | [] -> ret
  | h::t -> let match_one_card_return = match_one_card h player_hand [] in (match match_one_card_return with
    | None -> count_one_hand t player_hand ret
    | Some l -> count_one_hand t l (fst ret + 1, snd ret))

let compare_hand cur_hand player_hand next_hand = let next = count_one_hand (convert_phand_to_rank next_hand) player_hand (0, convert_phand_to_rank next_hand) in
(if fst next > fst cur_hand then (fst next, convert_rank_to_phand (snd next)) else  cur_hand)

(*
cur_hand
*)
let rec choose_hand1 cur_hand player_hand_ranks (p_hands : pokerhand list) : pokerhand =
  match p_hands with
    | [] -> snd cur_hand
    | h::t -> choose_hand1 (compare_hand cur_hand player_hand_ranks h) player_hand_ranks t



let rec get_higher_three n lst =
  if n > 14 then lst else
  get_higher_three (n + 1) ((ThreeOfAKind n)::lst)

let rec get_higher_pair n lst =
  if n > 14 then lst else
  get_higher_pair (n + 1) ((Pair n)::lst)

let rec get_higher_four n lst =
  if n > 14 then lst else
  get_higher_four (n + 1) ((FourOfAKind n)::lst)

let rec get_higher_straight n lst =
  if n > 14 then lst else
  get_higher_straight (n + 1) ((Straight n)::lst)

let rec get_higher_high_card n lst =
  if n > 14 then lst else
  get_higher_high_card (n + 1) ((Straight n)::lst)

let rec get_higher_two_pair_help2 low high lst =
    if low >= high then lst else
  get_higher_two_pair_help2 (low + 1) high ((TwoPair (low, high))::lst)

let rec get_higher_two_pair_help1 high lst =
  if high > 14 then lst
else get_higher_two_pair_help1 (high + 1) ((get_higher_two_pair_help2 2 high [])@lst)

let get_higher_two_pair low high =
  (get_higher_two_pair_help1 (high + 1) [])@(get_higher_two_pair_help2 (low + 1) high [])

let rec get_higher_full_house_help2 low high lst =
    if low >= high then lst else
  get_higher_full_house_help2 (low + 1) high ((FullHouse (low, high))::lst)

let rec get_higher_full_house_help1 high lst =
  if high > 14 then lst
else get_higher_full_house_help1 (high + 1) ((get_higher_full_house_help2 2 high [])@lst)

let get_higher_full_house low high =
  (get_higher_full_house_help1 (high + 1) [])@(get_higher_full_house_help2 (low + 1) high [])



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


type hand_lists = {high : pokerhand list; pair : pokerhand list;
two_pair : pokerhand list; three : pokerhand list; straight : pokerhand list;
full_house : pokerhand list; four : pokerhand list}

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
    | _ -> failwith "somethin dun fuck up"


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

let bs hands prev_hand =
  let random = Random.int 100 in
  let cards = fst (List.split hands) in
  let hand_ranks = convert_phand_to_rank prev_hand in
  let len = List.length hand_ranks in
  let num = get_num cards hand_ranks 0 in
  let dif = len - num in
  if dif = 0 then
    if len = 1 && random > 90 then true
    else if len = 2 && random > 80 then true
    else if len = 3 && random > 70 then true
    else if len = 4 && random > 50 then true
    else if len = 5 && random > 40 then true
    else false
  else if dif = 1 && random > 90 then true
  else if dif = 2 && random > 85 then true
  else if dif = 3 && random > 50 then true
  else if dif = 4 && random > 30 then true
  else if dif = 5 && random > 20 then true
  else false

let choose_hand3 hand all_hands prev_hands prev_hand =
  let next_hand = if List.length prev_hands = 0 then choose_hand2 hand prev_hands (HighCard 2)
  else choose_hand2 hand prev_hands prev_hand in
  let is_bs = bs all_hands prev_hand in
  if is_bs then BS prev_hand
  else Raise next_hand


  (* returns true if every card rank in [called_ranks] is in [rank_lst]. Returns
   * false otherwise *)
  let rec check_straight called_ranks rank_lst =
    match called_ranks with
      | [] -> true
      | h::t -> (List.exists (fun x -> x = h) rank_lst) && check_straight t rank_lst

  let hand_exists hands handrank =
    let ranks = (List.sort compare (fst (List.split hands))) in
    let hand_rank_lst = convert_phand_to_rank handrank in
    match handrank with
      | HighCard p -> List.exists (fun x -> x = p) ranks
      | Pair p -> let lst = List.filter (fun x -> x = p) ranks in
                  if (length lst > 1) then true
                  else false
      | TwoPair (p, t) -> let lst = List.filter (fun x -> x = p) ranks in
                          let lst2 = List.filter (fun x -> x = t) ranks in
                          if (length lst > 1) && (length lst2 > 1) then true
                          else false
      | ThreeOfAKind p -> let lst = List.filter (fun x -> x = p) ranks in
                          if (length lst > 2) then true
                          else false
      | Straight p -> check_straight hand_rank_lst ranks
      | FullHouse (p, t) -> let lst = List.filter (fun x -> x = p) ranks in
                            let lst2 = List.filter (fun x -> x = t) ranks in
                            if (length lst > 2) && (length lst2 > 1) then true
                            else false
      | FourOfAKind p -> let lst = List.filter (fun x -> x = p) ranks in
                         if (length lst > 3) then true
                         else false


  (* [string_of_rank r] returns the string representation of rank [r] *)
  let string_of_rank r =
    if r = 11 then "Jack"
    else if r = 12 then "Queen"
    else if r = 13 then "King"
    else if r = 14 then "Ace"
    else string_of_int r

  let string_of_pokerhand phand = match phand with
    | FourOfAKind p -> "four " ^ string_of_rank p ^ "s"
    | FullHouse (p, t) -> "full house with three " ^ string_of_rank p ^
                          "s and two " ^ string_of_rank t ^ "s"
    | Straight p -> "straight to " ^ string_of_rank p
    | ThreeOfAKind p -> "three " ^ string_of_rank p ^ "s"
    | TwoPair (p, t) -> "two " ^ string_of_rank p ^ "s and two "
                        ^ string_of_rank t ^ "s"
    | Pair p -> "a pair of " ^ string_of_rank p ^ "s"
    | HighCard p -> "highcard of " ^ string_of_rank p


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

  let print_prev_call r = match r with
    | HighCard 1 -> ()
    | _ -> print_endline ("And this is the previous hand called: " ^ string_of_pokerhand r)


  let convert_input_rank_to_int rank = match rank with
    | ("ace" | "aces") -> 14
    | ("king" | "kings") -> 13
    | ("queen" | "queens") -> 12
    | ("jack" | "jacks") -> 11
    | _ -> try (let i = (int_of_string rank) in
           if (i > 1) && (i < 15) then i
           else raise InvalidRank)
    with
      | Failure "int_of_string" -> raise InvalidRank


  (* takes input from the user and parses it into a pokerhand type *)
  let rec parse_input h r =
    print_endline ("Player 1, your turn! " ^ "Here is your hand: ");
    print_hand h;
    print_prev_call r;
    print_endline "What is your move?";
    print_string ">";
    let call = trim (lowercase_ascii (read_line ())) in
    let length_call = String.length call in
    try (
    let space = index call ' ' in
    let type_of_hand = sub call 0 space in
    match type_of_hand with
      | "four" -> let i = String.sub call (space + 1) (length_call - (space + 1)) in
                  Raise (FourOfAKind (convert_input_rank_to_int i))
      | "fh" -> let number_part = trim (sub call (space + 1) (length_call - (space + 1))) in
                let length_number_part = String.length number_part in
                let space_number_part = index number_part ' ' in
                let first_i = sub number_part 0 (space_number_part) in
                let second_i = sub number_part (space_number_part + 1) (length_number_part - (space_number_part + 1)) in
                Raise (FullHouse (convert_input_rank_to_int first_i, convert_input_rank_to_int second_i))
      | "straight" -> let i = sub call (space + 1) (length_call - (space + 1)) in
                      Raise (Straight (convert_input_rank_to_int i))
      | "three" -> let i = sub call (space + 1) (length_call - (space + 1)) in
                  Raise (ThreeOfAKind (convert_input_rank_to_int i))
      | "tp" -> let number_part = trim (sub call (space + 1) (length_call - (space + 1))) in
                let length_number_part = String.length number_part in
                let space_number_part = index number_part ' ' in
                let first_i = sub number_part 0 (space_number_part) in
                let second_i = sub number_part (space_number_part + 1) (length_number_part - (space_number_part + 1)) in
                Raise (TwoPair (convert_input_rank_to_int first_i, convert_input_rank_to_int second_i))
      | "pair" -> let i = sub call (space + 1) (length_call - (space + 1)) in
                  Raise (Pair (convert_input_rank_to_int i))
      | "hc" -> let i = sub call (space + 1) (length_call - (space + 1)) in
                Raise (HighCard (convert_input_rank_to_int i))
      | "bs" -> BS (r)
      | _ -> raise InvalidMove

    )
    with
      | Not_found ->
        print_endline "That is not a valid hand type. Please try again.";
        parse_input h r
      | Invalid_argument _->
        print_endline "That is not a valid hand. Please try again.";
        parse_input h r
      | InvalidMove ->
        print_endline ("That is not a valid move. The kinds you can call are "
        ^"four, fh, straight, three, tp, pair, hc, and bs. Please try again.");
        parse_input h r
      | InvalidRank -> print_endline ("That is not a valid rank to call. Please try again.");
                       parse_input h r
      (* | Failure "int_of_string" ->  *)

  let rec human_turn (h: hand) (r : pokerhand option) : move =
    match r with
      | None -> parse_input h (HighCard 1) (* default dummy value *)
      | Some p -> let move = parse_input h p in
                  try (
                    match move with
                      | BS _ -> move
                      | Raise h -> if (valid_call h p) = false
                                    then raise InvalidMove
                                   else move
                  )
    with
      | InvalidMove -> print_endline ("That hand is not a higher call than the previous hand. "
                       ^"Please try again.");
                       human_turn h r


  let ai_turn id h ph cards hands_called = match ph with
    | Some h2 -> choose_hand3 h cards hands_called h2
    | None -> failwith "asdf"

(* prints the hand of each player *)
  let rec print_player_hands (hands : (pid * hand) list) = match hands with
    | [] -> ()
    | (pid, hand)::t -> print_endline ("Player " ^ (string_of_int pid) ^ "'s hand is:");
                        print_hand hand;
                        print_endline "";
                        print_player_hands t

  let rec play_round s =
    let cur_hand = List.assoc s.cur_player s.hands in
    let move =
      if s.cur_player = 1 then
        human_turn cur_hand s.raised_hand
      else ai_turn s.cur_player cur_hand s.raised_hand s.cards s.hands_called
    in
    let cur_p = "Player "^(string_of_int s.cur_player) in
    let prev_p = "Player "^(string_of_int s.prev_player) in
    match move with
    | BS p ->
      print_endline (cur_p^" called BS!"
                          ^ " Let's check if the previous hand is there...");
      print_endline "";
      print_player_hands s.hands;
      if (hand_exists s.cards p) then
        (print_endline ((string_of_pokerhand p)^" is here. "
                      ^cur_p^" loses this round.");
        s.cur_player)
      else
        (print_endline ((string_of_pokerhand p)^" is not here. "
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


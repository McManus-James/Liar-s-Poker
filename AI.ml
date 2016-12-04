let rec match_one_card card hand ret_hand = match hand with
  | [] -> None
  | h::t -> if card = h then Some (ret_hand@t)
    else let ret_hand_update = h::ret_hand in match_one_card card t ret_hand_update

let rec count_one_hand next_hand player_hand ret = match next_hand with
  | [] -> ret
  | h::t -> let match_one_card_return = match_one_card h player_hand [] in (match match_one_card_return with
    | None -> count_one_hand t player_hand ret
    | Some l -> count_one_hand t l (fst ret + 1, snd ret))

let compare_hand cur_hand player_hand next_hand =
  let next_hand_rank = convert_phand_to_rank next_hand in
  let next = count_one_hand (next_hand_rank) player_hand
    (0, next_hand_rank) in
  (if fst next > fst cur_hand then
    (fst next, convert_rank_to_phand (snd next)) else  cur_hand)

let compare_hand2 cur_hand player_hand next_hand =
  let next_hand_rank = convert_phand_to_rank next_hand in
  let next = count_one_hand (next_hand_rank) player_hand
    (0, next_hand_rank) in
  if fst next > 0 && (fst next) - (List.length next_hand_rank) = 0 then (fst next, convert_rank_to_phand (snd next))
else cur_hand

(*
cur_hand
*)
let rec choose_hand1 cur_hand player_hand_ranks (p_hands : pokerhand list) : pokerhand =
  match p_hands with
    | [] -> snd cur_hand
    | h::t -> let x = (compare_hand2 cur_hand player_hand_ranks h) in
      if snd x = (HighCard 2) then choose_hand1 (compare_hand cur_hand player_hand_ranks h) player_hand_ranks t
      else choose_hand1 x player_hand_ranks t

type pokerhand =
    | FourOfAKind of int
    | FullHouse of (int * int) (* first int is rank of the three of
                               a kind; second int is the rank of
                               the pair *)
    | Straight of int (* the int is the high card in the straight *)
    | ThreeOfAKind of int
    | TwoPair of (int * int) (* first int is rank of the higher
                            pair; second int is the rank of the
                            lower pair *)
    | Pair of int
    | HighCard of int

  type move =
    | BS of pokerhand (* The hand which BS was called on *)
    | Raise of pokerhand (* The hand that was Raised *)


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



let rec h_cards_creator_helper l handl =
  match handl with
  |[] -> List.rev l
  |hd::tl -> match hd with
    |HighCard h -> h_cards_creator_helper ([h]::l) tl
    |Pair h -> h_cards_creator_helper ([h;h]::l) tl
    |TwoPair (a,b) -> h_cards_creator_helper ([a;a;b;b]::l) tl
    |ThreeOfAKind h -> h_cards_creator_helper ([h;h;h]::l) tl
    |Straight h -> h_cards_creator_helper ([h-3;h-2;h-1;h]::l) tl
    |FullHouse (a,b) -> h_cards_creator_helper ([a;a;a;b;b]::l) tl
    |FourOfAKind h -> h_cards_creator_helper ([h;h;h;h]::l) tl

let h_cardsl_creator hand =
  let hl = get_higher_hands hand in
  h_cards_creator_helper [] hl


let hand_exists_2 hands handrank =
  let ranks = (List.sort compare hands) in
  let hand_rank_lst = convert_phand_to_rank handrank in
  match handrank with
    | HighCard p -> List.exists (fun x -> x = p) ranks
    | Pair p -> let lst = List.filter (fun x -> x = p) ranks in
                if (List.length lst > 1) then true
                else false
    | TwoPair (p, t) -> let lst = List.filter (fun x -> x = p) ranks in
                        let lst2 = List.filter (fun x -> x = t) ranks in
                        if (List.length lst > 1) && (List.length lst2 > 1) then true
                        else false
    | ThreeOfAKind p -> let lst = List.filter (fun x -> x = p) ranks in
                        if (List.length lst > 2) then true
                        else false
    | Straight p -> check_straight hand_rank_lst ranks
    | FullHouse (p, t) -> let lst = List.filter (fun x -> x = p) ranks in
                          let lst2 = List.filter (fun x -> x = t) ranks in
                          if (List.length lst > 2) && (List.length lst2 > 1) then true
                          else false
    | FourOfAKind p -> let lst = List.filter (fun x -> x = p) ranks in
                       if (List.length lst > 3) then true
                       else false

let new_bs cards prev_hand diff =
  Random.self_init ();
  let random = Random.int 100 in
  match prev_hand with
  |FourOfAkind 14 -> true
  |_ ->
    let b = hand_exists_2 cards prev_hand in
    match (diff/10) with
    |0-> if random > 50 then not b else b
    |1-> if random > 25 then not b else b
    |2-> if random > 5 then not b else b


let rec nh_helper cards hl p =
Random.self_init ();
let random = Random.int 100 in
match hl with
|[] -> None
|hd::tl -> match (p/10) with
  |0-> if random > 50 then
          Some hd
        else (
          if hand_exists_2 cards hd then Some hd
          else nh_helper cards tl p)
  |1-> if random > 75 then
          Some hd
        else (
          if hand_exists_2 cards hd then Some hd
          else nh_helper cards tl p)
  |2-> if random > 95 then
          Some hd
        else (
          if hand_exists_2 cards hd then Some hd
          else nh_helper cards tl p)

let ai_turn id h ph cards =
  match ph with
  |Some ha ->
    if new_bs cards ha id then BS ha
    else match nh_helper cards (get_higher_hands h) id with
    |None -> BS ha
    |Some hand -> Raise hand
  |None -> Raise nh_helper cards (get_higher_hands h) id




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

let bs hands prev_hand =
  Random.self_init ();
  let random = Random.int 100 in
  let cards = fst (List.split hands) in
  let hand_ranks = convert_phand_to_rank prev_hand in
  let len = List.length hand_ranks in
  let num = get_num cards hand_ranks 0 in
  let dif = len - num in
  if dif = 0 then
    (match prev_hand with
      | HighCard _ -> if random > 95 then true else false
      | Pair _ -> if random > 90 then true else false
      | TwoPair _ -> if random > 80 then true else false
      | ThreeOfAKind _ -> if random > 60 then true else false
      | Straight _ -> if random > 60 then true else false
      | FullHouse _ -> if random > 40 then true else false
      | FourOfAKind _ -> if random > 30 then true else false
    )
  else if dif = 1 && random > 90 then true
  else if dif = 2 && random > 85 then true
  else if dif = 3 && random > 50 then true
  else if dif = 4 && random > 30 then true
  else if dif = 5 && random > 20 then true
  else false

let choose_hand3 hand all_hands prev_hands prev_hand first_hand =
  Random.self_init ();
  let lie = Random.int 6 in
  Random.self_init ();
  let automatic_bs = Random.int 11 in
  let new_hand = if lie > 3 then (
    match hand with
      | h::t -> ((take 1 [])@t)
      | _ -> hand)
  else if lie > 4 then (
    match hand with
      | h1::h2::t -> ((take 2 [])@t)
      | _ -> hand)
  else hand in
  let next_hand = if List.length prev_hands = 0 then choose_hand2 new_hand prev_hands (HighCard 2)
  else choose_hand2 new_hand prev_hands prev_hand in
  let is_bs = if first_hand then false else
  (match prev_hand with
  | FourOfAKind a -> if a = 14 then true else bs all_hands prev_hand
  | _ -> bs all_hands prev_hand) in
  let len = List.length (convert_phand_to_rank next_hand) in
  let cards_present = count_one_hand (convert_phand_to_rank next_hand) (fst (List.split hand)) (0, convert_phand_to_rank next_hand) in
  let dif = len - fst cards_present in
  if is_bs then BS prev_hand
  else if dif >= 2 && automatic_bs > 7 then BS prev_hand
  else if dif >= 3 && automatic_bs > 4 then BS prev_hand
  else if dif >= 4 && automatic_bs > 3 then BS prev_hand
  else Raise next_hand

  let ai_turn id h ph cards hands_called =
    match ph with
    | Some h2 -> choose_hand3 h cards hands_called h2 false
    | None -> choose_hand3 h cards hands_called (HighCard 1) true

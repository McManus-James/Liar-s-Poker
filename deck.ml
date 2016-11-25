module type Deck = sig
  type hand
  type deck
  val empty : deck
  val new_deck : deck -> deck
  val shuffle_deck: deck -> deck
  val deal : int -> deck -> hand
  val print_hand : hand -> unit
end

module Deck = struct

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
  let string_of_suit (s:suit) =
    if s = Spades then "Spades"
    else if s = Clubs then "Clubs"
    else if s = Diamonds then "Diamonds"
    else "Hearts"

  (* [string_of_rank r] returns the string representation of rank [r] *)
  let string_of_rank r =
    if r = 11 then "Jack"
    else if r = 12 then "Queen"
    else if r = 13 then "King"
    else if r = 1 then "Ace"
    else string_of_int r

  (* [string_of_card c] formats a  *)
  let string_of_card (c:card) : string =
    string_of_rank (fst c) ^ " of " ^ string_of_suit (snd c)

  (* print a hand. h is the hand to print. !!!!!!!!!!Do we want this as a visible interface in the
   * deck sig???? *)
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
    for i=0 to 10000 do
      let first = Random.int 52 in
      let second = Random.int 52 in
      let old_card_one = array_deck.(first) in
      let old_card_two = array_deck.(second) in
      array_deck.(second) <- old_card_one;
      array_deck.(first) <- old_card_two;
      d := (Array.to_list array_deck);
    done;
    d

  let deal (n : int) (d : deck) : hand =
    let hand = take n !d in
    d := drop n !d;
    hand

end
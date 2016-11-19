open Pervasives
open List
(* A [Deck] is an OCaml representation of a standard 52 playing card deck *)
module type Deck = struct

  type rank = int

  type suit = Hearts | Clubs | Diamonds | Spades

  type card = (rank * suit)

  type hand = card list

  type deck = card list

  let empty = []

  (* helper function taken from Recitation 3: Lists, and Testing with OUnit
   * that creates an infix operator that makes a list of all integers from i through j inclusive *)
  let (--) i j =
    let rec from i j l =
      if i>j then l
      else from i (j-1) (j::l)
      in from i j []


  let string_of_rank (r:int) =
    if r = 11 then "Jack"
    else if r = 12 then "Queen"
    else if r = 13 then "King"
    else if r = 14 then "Ace"
    else string_of_int r

  let string_of_suit (s:suit) =
    if s = Spades then "Spades"
    else if s = Clubs then "Clubs"
    else if s = Diamonds then "Diamonds"
    else "Hearts"

  (* print function to print cards *)
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
    | h::t -> suit_helper t ((deck_helper h (1--14) [])@accum)


  let rec new_deck () = ref (suit_helper [Hearts; Spades; Clubs; Diamonds] [])

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


  (* [shuffle_deck d] takes in a Deck [d] and returns a shuffled copy of the
   * original deck *)
  val shuffle_deck: deck -> deck

  (* [deal n d] creates a hand of the first [n] cards off of the top of deck d,
   * returning a hand with [n] cards. It also modifies the deck to refelct the
   * fact that the top [n] cards are now gone from the deck *)
  let deal n d =
    let hand = take n !d in
    d := drop n !d;
    hand

end
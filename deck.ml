(* A [Deck] is an OCaml representation of a standard 52 playing card deck *)
module type Deck = struct

  type rank = int

  type suit = Hearts | Clubs | Diamonds | Spades

  type card = (rank * suit)

  type hand = card list

  type deck = card list

  let empty = []

  (* helper function to instantiate a 52 card deck.
   * [suit] is a suit, and [rank_list] is a list of
   * all possible ranks
   * [accum] is an accumulator holding the deck created so far
   * function returns all the cards for a particular suit, from 2 through Ace
   * (represented as rank 14) *)
  let rec deck_helper suit rank_list accum = match rank_list with
    | [] -> accum
    | h::t -> deck_helper suit t ((h, suit)::accum)

  let rec new_deck suit_list accum = match suit_list with
    | [] -> accum
    | h::t -> new_deck t ((deck_helper h [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14] [])@accum)

  (* [shuffle_deck d] takes in a Deck [d] and returns a shuffled copy of the
   * original deck *)
  val shuffle_deck: deck -> deck

  (* [deal n d] creates a hand of the first [n] cards off of the top of deck d,
   * returning a hand with [n] cards *)
  val deal: int -> deck -> hand

end
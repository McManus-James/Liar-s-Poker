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


  (* [shuffle_deck d] takes in a Deck [d] and returns a shuffled copy of the
   * original deck *)
  val shuffle_deck: deck -> deck

  (* [deal n d] creates a hand of the first [n] cards off of the top of deck d,
   * returning a hand with [n] cards *)
  val deal: int -> deck -> hand

end
(* A [Deck] is an OCaml representation of a standard 52 playing card deck *)
module type Deck = struct

  type rank = int

  type suit = Hearts | Clubs | Diamonds | Spades

  type card = (rank * suit)

  type hand = card list

  type deck = card list

  let empty = []

  let new_deck e =
  (* [shuffle_deck d] takes in a Deck [d] and returns a shuffled copy of the
   * original deck *)
  val shuffle_deck: deck -> deck

  (* [deal n d] creates a hand of the first [n] cards off of the top of deck d,
   * returning a hand with [n] cards *)
  val deal: int -> deck -> hand

end
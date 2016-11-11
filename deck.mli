(* A [Deck] is an OCaml representation of a standard 52 playing card deck *)
module type Deck = sig

  (* The type of an card rank *)
  type rank

  (* The type of an card suit *)
  type suit

  (* The type of a card *)
  type card

  (* The type of a hand *)
  type hand

  (* the type of a deck *)
  type deck

  (* [empty] is the empty deck *)
  val empty : deck

  (* [new_deck e] takes in an empty deck [e] and returns a new, unshuffled 52
   * card deck with the cards in order *)
  val new_deck : deck -> deck

  (* [shuffle_deck d] takes in a Deck [d] and returns a shuffled copy of the
   * original deck *)
  val shuffle_deck: deck -> deck

  (* [deal n d] creates a hand of the first [n] cards off of the top of deck d,
   * returning a hand with [n] cards *)
  val deal: int -> deck -> hand

end
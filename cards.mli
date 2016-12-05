module type Cards = sig
  (* The rank of a card *)
  type rank = int

  (* The suit of a card *)
  type suit = Hearts | Clubs | Diamonds | Spades

  (* The type of a card *)
  type card = (rank * suit)

  (* The type of a hand *)
  type hand = card list

  (* The type of a deck *)
  type deck

  (* [empty] is the empty deck *)
  val empty : deck

  (* [print_hand h] prints a string representation of [h] to the terminal *)
  val print_hand : hand -> unit

  (* [new_deck e] takes in an empty deck [e] and returns a new, unshuffled 52
   * card deck with the cards in order *)
  val new_deck : deck -> deck

  (* [shuffle_deck d] takes in a Deck [d] and returns a shuffled copy of the
   * original deck *)
  val shuffle_deck: deck -> deck

  (* [deal n d] creates a hand of the first [n] cards off of the top of deck d,
   * returning a hand with [n] cards *)
  val deal : int -> deck -> hand

  (* [string_of_rank r] is a string representation of [r] *)
  val string_of_rank : rank -> string

  (* [in_play hands] is the hand made up of all [hands] in play *)
  val in_play : hand list -> hand

  (* [random_card] is a random card of suit Hearts *)
  val random_card : card

  (* [num_cards] is the number of cards in a hand *)
  val num_cards : hand -> int

end
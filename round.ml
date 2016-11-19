module type Round = struct

  type rank = int

  type suit = Hearts | Clubs | Diamonds | Spades

  type card = (rank * suit)

  type hand = card list

  type deck = ????

  type pokerhand = | FourOfAKind of int
                   | FullHouse of int * int
                   | Straight of int
                   | ThreeOfAKind of int
                   | TwoPair of int * int
                   | Pair of int * int (* where first int is the pair rank and the second is the
                                        non-paired card *)
                   | HighCard of int list
                   | No (* type returned by helper function to indicate that a
                          given type of pokerhand is not exhibited by the hand *)

  type pid = int

  (* The type of a move*)
  type move

  (* [deal_hands n] returns an associative list that maps [pid]s to hands of
   * [n] cards *)
  val deal_hands : (pid * int) list -> (pid * hand) list

  (* [hand_exists hands handrank] returns true if [handrank] exists within all
   * the cards in [hands] *)
  val hand_exists : hand list -> pokerhand -> bool

  (* [human_turn h r] returns the move that the human player decides to make
   * based on his or her hand [h] and the previous hand called, [r] *)
  val human_turn  : hand -> pokerhand -> move

  (* [valid_call p_hand prev_hand] returns true if the pokerhand [p_hand] the
   * player calls is a valid call; ie it has a higher hand rank than the
   * previous hand [prev_hand]. Returns false if [p_hand] is an equal or lower
   * rank than [prev_hand] *)
  val valid_call : pokerhand -> pokerhand -> bool

  (* [ai_turn id hand raisedhand raised] returns the move that the ai player
   * decides to make based on its [hand], its personality determined by its
   * [id], the last raised hand [raisedhand], and all the the cards, [raised]
   * that would be present if no player had yet lied *)
  val ai_turn  : pid -> hand -> pokerhand -> card list -> move

  (* [cards_of_pokerhand raised] is a card hand that would be ranked as the
   * pokerhand [raised] *)
  val cards_of_pokerhand : pokerhand -> hand

end
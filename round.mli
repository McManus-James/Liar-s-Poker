open Deck

(* A [Round] handles a single round of play in the game. It deals each player's
 * hand, gets each player's move on their turn, and checks if a pokerhand exists
 * within all the cards in play the round *)
module type Round = sig

  type suit

  type rank

  (* the type of a card *)
  type card

  (* the type of a player id *)
  type pid

  (* The type of a hand *)
  type hand

  (* The different types of hands in Liar's poker *)
  type pokerhand

  (* The type of a move *)
  type move

  (* [deal_hands n accum] returns an associative list that maps [pid]s to hands of
   * [n] cards. [accum] is what holds all the new mappings as they are being formed *)
  val deal_hands : (pid * int) list -> (pid * hand) list -> (pid * hand) list

  (* [hand_exists hands handrank] returns true if [handrank] exists within all
   * the cards in [hands] *)
  val hand_exists : card list -> pokerhand -> bool

  (* [human_turn h r] returns the move that the human player decides to make
   * based on his or her hand [h] and the previous hand called, [r] *)
  val human_turn  : hand -> pokerhand option -> move

  (* [ai_turn id hand raisedhand raised] returns the move that the ai player
   * decides to make based on its [hand], its personality determined by its
   * [id], the last raised hand [raisedhand], and all the the cards, [raised]
   * that would be present if no player had yet lied *)
  val ai_turn  : pid -> hand -> pokerhand option -> hand -> move

  (* Formats a pokerhand into a string for printing *)
  val string_of_pokerhand : pokerhand -> string

end

(* A [DictionaryMaker] is a functor that makes a [Dictionary]
 * out of a [Comparable]. *)
module type RoundMaker =
  functor (D : Deck) -> Round

(* [GameRound] makes a [Round] *)
module GameRound : RoundMaker
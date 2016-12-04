(* A [Round] handles a single round of play in the game. It deals each player's
 * hand, gets each player's move on their turn, and checks if a pokerhand exists
 * within all the cards in play the round *)
module type Round = sig
  include Poker
  (* The type of a player id *)
  type pid

  (* The state of the round *)
  type state

  (* [init_state n] is the state of the first round of Liars Poker with [n]
   * players *)
  val init_state : pid -> state

  (* [update_state l s] is the state the next round after player [l] loses
   * round [s] *)
  val update_state : pid ->  state -> state

  (* [play_round s] returns the id of the loser of round s *)
  val play_round  : state -> pid

  (* [winner s] is returns the winner of the round if the round is over
   * other wise returns None *)
   val winner : state -> pid option

end
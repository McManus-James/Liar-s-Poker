module type Poker = sig
  include CardGame
  type pid = int
  type pokerhand
  type move
end
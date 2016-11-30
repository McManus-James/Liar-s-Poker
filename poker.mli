module type Poker = sig
  include CardGame
  type pid
  type pokerhand
  type move
end
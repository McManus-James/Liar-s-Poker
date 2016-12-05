module type Poker = sig
  include CardGame
  type pokerhand
  type move
end
module type AI = sig

  val ai_turn : int -> card list -> pokerhand -> card list -> pokerhand list -> move


end


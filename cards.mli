module type Cards = sig
  type suit
  type card
  type hand
  val print_hand : hand -> unit
end

module CardGame
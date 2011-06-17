open State
open Play

type strategy = unit -> (game -> play)

let dummy () =
  fun game -> {
    left_or_right = Apply_card_to_slot;
    slot_number = 0;
    card_symbol = Identity;
  }

let all_strategies : (string * strategy) list = [
  "dummy", dummy;
]

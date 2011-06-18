open State
open Play

type strategy = unit -> (game -> play)

let dummy () =
  fun game -> {
    left_or_right = Apply_card_to_slot;
    slot_number = 0;
    card = I;
  }

(* Strategy used in the official submission *)
let default = dummy

let all_strategies : (string * strategy) list = [
  "default", default; (* strategy used in the official submission! *)
  "dummy", dummy;
  "variety", Strat_variety.variety;
]

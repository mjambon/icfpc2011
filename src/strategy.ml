open State
open Play

let dummy () =
  fun game -> {
      left_or_right = Apply_card_to_slot;
      slot_number = 0;
      card = I;
  }

(* Strategy used in the official submission *)
let default = Strat_choice.choice

let top_strategies : (string * (unit -> (game -> play))) list = [
  "default", default; (* strategy used in the official submission! *)
  "dummy", dummy;
  "variety", Strat_variety.variety;
  "choice", Strat_choice.choice;
]

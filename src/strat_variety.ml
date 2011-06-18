open State
open Play

let random_card () =
  all_cards.(Random.int (Array.length all_cards))

let random_slot () =
  Random.int slots_len

let random_lr () =
  match Random.int 2 with
      0 -> Apply_card_to_slot
    | 1 -> Apply_slot_to_card
    | _ -> assert false

let random_play () =
  {
    left_or_right = random_lr ();
    slot_number = random_slot ();
    card_symbol = random_card ();
  }

let rec legal_random_play game =
  let x = random_play () in
  if is_legal game x then x
  else legal_random_play game

let variety () = legal_random_play

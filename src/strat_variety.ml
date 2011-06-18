open State
open Play
open View

let init_rng () =
  let state = Random.State.make [| |] in
  Random.State.int state

let random_card rand =
  all_cards.(rand (Array.length all_cards))

let random_slot rand =
  rand slots_len

let random_lr rand =
  match rand 2 with
      0 -> Apply_card_to_slot
    | 1 -> Apply_slot_to_card
    | _ -> assert false

let random_play rand =
  {
    left_or_right = random_lr rand;
    slot_number = random_slot rand;
    card = random_card rand;
  }

let rec legal_random_play rand game =
  let x = random_play rand in
  if is_legal game x then x
  else legal_random_play rand game

let variety () =
  let rand = init_rng () in
  legal_random_play rand

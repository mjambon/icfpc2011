(* Playing *)

open Printf

open State

type card_symbol =
    Identity
  | Zero
  | Succ
  | Dbl
  | Get
  | Put
  | Scomb
  | Kcomb
  | Inc
  | Dec
  | Attack
  | Help
  | Copy
  | Revive
  | Zombie

type left_or_right =
    Apply_card_to_slot (* left *)
  | Apply_slot_to_card (* right *)

type play = {
  left_or_right : left_or_right;
  slot_number : int;
  card_symbol : card_symbol;
}

let left_or_right_of_string = function
    "1" -> Apply_card_to_slot
  | "2" -> Apply_slot_to_card
  | _ -> invalid_arg "left_or_right_of_string"

let string_of_left_or_right = function
    Apply_card_to_slot -> "1"
  | Apply_slot_to_card -> "2"

let slot_number_of_string s =
  let n = int_of_string s in
  if n < 0 || n >= slots_len then
    invalid_arg "slot_number_of_string"
  else 
    n

let all_cards = [|
  Identity;
  Zero;
  Succ;
  Dbl;
  Get;
  Put;
  Scomb;
  Kcomb;
  Inc;
  Dec;
  Attack;
  Help;
  Copy;
  Revive;
  Zombie;
|]

let card_symbol_of_string = function
    "I" -> Identity
  | "zero" -> Zero
  | "succ" -> Succ
  | "dbl" -> Dbl
  | "get" -> Get
  | "put" -> Put
  | "S" -> Scomb
  | "K" -> Kcomb
  | "inc" -> Inc
  | "dec" -> Dec
  | "attack" -> Attack
  | "help" -> Help
  | "copy" -> Copy
  | "revive" -> Revive
  | "zombie" -> Zombie
  | s -> invalid_arg ("card_symbol_of_string: " ^ s)

let string_of_card_symbol = function
    Identity -> "I"
  | Zero -> "zero"
  | Succ -> "succ"
  | Dbl -> "dbl"
  | Get -> "get"
  | Put -> "put"
  | Scomb -> "S"
  | Kcomb -> "K"
  | Inc -> "inc"
  | Dec -> "dec"
  | Attack -> "attack"
  | Help -> "help"
  | Copy -> "copy"
  | Revive -> "revive"
  | Zombie -> "zombie"

let card_value_of_symbol = function
    Identity -> identity
  | Zero -> zero
  | Succ -> succ
  | Dbl -> dbl
  | Get -> get
  | Put -> put
  | Scomb -> scomb
  | Kcomb -> kcomb
  | Inc -> inc
  | Dec -> dec
  | Attack -> attack
  | Help -> help
  | Copy -> copy
  | Revive -> revive
  | Zombie -> zombie


let input_play interactive ic =
  if interactive then
    printf "(1) apply card to slot, or (2) apply slot to card?\n%!";
  let lr = left_or_right_of_string (input_line ic) in
  let card () =
    if interactive then
      printf "card name?\n%!";
    card_symbol_of_string (input_line ic)
  in
  let n () =
    if interactive then
      printf "slot number?\n%!";
    slot_number_of_string (input_line ic)
  in
  let n, card =
    match lr with
        Apply_card_to_slot ->
          let card = card () in
          let n = n () in
          n, card
      | Apply_slot_to_card ->
          let n = n () in
          let card = card () in
          n, card
  in
  {
    left_or_right = lr;
    slot_number = n;
    card_symbol = card;
  }

let output_play oc x =
  if x.slot_number < 0 || x.slot_number >= slots_len then
    invalid_arg "output_play";
  fprintf oc "%s\n%!" (string_of_left_or_right x.left_or_right);
  let a () = fprintf oc "%s\n%!" (string_of_card_symbol x.card_symbol) in
  let b () = fprintf oc "%s\n%!" (string_of_int x.slot_number) in
  match x.left_or_right with
      Apply_card_to_slot -> a (); b ()
    | Apply_slot_to_card -> b (); a ()

let slots_alive slots =
  Array.fold_left (fun acc x -> if is_alive x then acc + 1 else acc) 0 slots

let all_dead slots =
  slots_alive slots = 0

let update game play =
  let a = card_value_of_symbol play.card_symbol in
  let slots = proponent game in

  game.auto <- true;
  Array.iter (
    fun slot ->
      if slot.vitality = -1 then (
        (try
           ignore (top_apply game slot.field identity)
         with Invalid_play ->
           ()
        );
        slot.field <- identity;
        slot.vitality <- 0;
      )
  ) slots;

  game.auto <- false;
  let slot = get_slot slots play.slot_number in
  try
    let b = slot.field in
    let f, x =
      match play.left_or_right with
          Apply_card_to_slot -> a, b
        | Apply_slot_to_card -> b, a
    in
    slot.field <- top_apply game f x

  with Invalid_play ->
    slot.field <- identity

let next_player game =
  game.app_counter <- 0;
  (match game.current_player with
       Player0 ->
         game.current_player <- Player1
     | Player1 ->
         game.current_player <- Player0;
         game.turn_counter <- game.turn_counter + 1
  )

let rec play_game game interactive get0 print0 get1 print1 =
  if game.turn_counter > 100_000
    || all_dead game.player0
    || all_dead game.player1 then
      let score0 = slots_alive game.player0 in
      let score1 = slots_alive game.player1 in
      score0, score1
  else (
    if interactive then (
      printf "### turn %i ###\n" game.turn_counter;
      printf "*** player %i's turn, with slots:\n"
        (int_of_player game.current_player); 
      State.print_slots (proponent game);
      printf "(slots {10000,I} are omitted)\n%!";
    );
    (match game.current_player with
         Player0 ->
           let play0 = get0 game in
           update game play0;
           print0 play0
       | Player1 ->
           let play1 = get1 game in
           update game play1;
           print1 play1
    );
    next_player game;
    play_game game interactive get0 print0 get1 print1
  )

let init_slots () =
  Array.init slots_len (
    fun i -> 
      { vitality = 10_000;
        field = identity }
  )

let init_game () =
  {
    player0 = init_slots ();
    player1 = init_slots ();
    turn_counter = 1;
    current_player = Player0;
    app_counter = 0;
    auto = true;
  }


let is_legal game play =
  (* TODO *)
  true

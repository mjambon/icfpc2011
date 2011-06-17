exception Invalid_play

let invalid_play () = raise Invalid_play

let slots_len = 256

type value =
    Int of int
  | Fun of (game -> value -> value)

and slot = {
  mutable vitality : int; (* -1 .. 65535 *)
  mutable field : value;
}

and slots = slot array

and player_id = Player0 | Player1

and game = {
  player0 : slots;
  player1 : slots;
  mutable turn_counter : int;
  mutable current_player : player_id;
  mutable app_counter : int;
  mutable auto : bool; (* auto-application (modifies behavior of some cards) *)
}

(* Access to player's data *)

let other_player = function
    Player0 -> Player1
  | Player1 -> Player0

let proponent game =
  match game.current_player with
      Player0 -> game.player0
    | Player1 -> game.player1

let opponent game =
  match game.current_player with
      Player0 -> game.player1
    | Player1 -> game.player0

let is_alive x =
  match x.vitality with
      -1 | 0 -> false
    | _ -> true

let is_dead x = not (is_alive x)

(* Access functions that may raise Invalid_play *)

let int = function
    Int x -> x
  | Fun _ -> invalid_play ()

let func = function
    Fun x -> x
  | Int _ -> invalid_play ()

let get_slot slots i =
  if i < 0 || i >= slots_len then
    invalid_play ()
  else
    slots.(i)

let apply game x y =
  game.app_counter <- game.app_counter + 1;
  if game.app_counter > 1000 then
    invalid_play ()
  else
    match x with
        Fun f -> f game y
      | _ -> invalid_play ()

let top_apply game x y =
  game.app_counter <- 0;
  apply game x y


(* Card values *)

let identity = Fun (fun game x -> x)

let zero = Int 0

let succ = Fun (
  fun game x -> 
    Int (min (int x + 1) 65535)
)

let dbl = Fun (
  fun game x ->
    Int (min (2 * int x) 65535)
)

let get = Fun (
  fun game x ->
    let i = int x in
    let slot = get_slot (proponent game) i in
    if is_alive slot then
      slot.field
    else
      invalid_play ()
)

let put = Fun (
  fun game x -> identity
)

let scomb = Fun (
  fun game f ->
    Fun (
      fun game g ->
        Fun (
          fun game x ->
            let h = apply game f x in
            let y = apply game g x in
            apply game h y
        )
    )
)

let kcomb = Fun (
  fun game x ->
    Fun (fun game y -> x)
)

let inc = Fun (
  fun game x ->
    let i = int x in
    let slot = get_slot (proponent game) i in
    if slot.vitality > 0 then
      if game.auto then
        slot.vitality <- slot.vitality - 1
      else
        slot.vitality <- min (slot.vitality + 1) 65535;
    identity
)

let dec = Fun (
  fun game x ->
    let i = int x in
    let slot = get_slot (opponent game) (255 - i) in
    if slot.vitality > 0 then
      if game.auto then
        slot.vitality <- min (slot.vitality + 1) 65535
      else
        slot.vitality <- slot.vitality - 1;
    identity
)

let attack = Fun (
  fun game i ->
    Fun (
      fun game j ->
        Fun (
          fun game n ->
            let pslot = get_slot (proponent game) (int i) in
            let n = int n in
            let v = pslot.vitality in
            if n > v then
              invalid_play ()
            else
              pslot.vitality <- v - n;

            (* and then *)

            if is_alive pslot then (
              let oslot = get_slot (opponent game) (255 - int j) in
              let w = oslot.vitality in
              if game.auto then (
                if w > 0 then
                  oslot.vitality <- min (w + n * 9 / 10) 65535
              )
              else
                oslot.vitality <- max 0 (w - n * 9 / 10);
            );

            identity
        )
    )
)

let help = Fun (
  fun game i ->
    Fun (
      fun game j ->
        Fun (
          fun game n ->
            let pslot = get_slot (proponent game) (int i) in
            let n = int n in
            let v = pslot.vitality in
            if n > v then
              invalid_play ()
            else
              pslot.vitality <- v - n;

            (* and then *)

            if is_alive pslot then (
              let oslot = get_slot (opponent game) (int j) in
              let w = oslot.vitality in
              if game.auto then (
                if w > 0 then
                  oslot.vitality <- max 0 (w - n * 11 / 10)
              )
              else
                oslot.vitality <- min (w + n * 11 / 10) 65535
            );
            
            identity
        )
    )
)

let copy = Fun (
  fun game i ->
    (get_slot (opponent game) (int i)).field
)

let revive = Fun (
  fun game i ->
    let slot = get_slot (proponent game) (int i) in
    let v = slot.vitality in
    if v <= 0 then
      slot.vitality <- 1;
    identity
)

let zombie = Fun (
  fun game i ->
    Fun (
      fun game x ->
        let oslot = get_slot (opponent game) (255 - int i) in
        if is_dead oslot then
          oslot.field <- x
        else
          invalid_play ();
        oslot.vitality <- -1;
        identity
    )
)

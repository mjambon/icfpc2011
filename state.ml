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
  match x with
      Fun f -> f game y
    | _ -> invalid_play ()

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
      slot.vitality <- min (slot.vitality + 1) 65535;
    identity
)

let dec = Fun (
  fun game x ->
    let i = int x in
    let slot = get_slot (opponent game) (255 - i) in
    if slot.vitality > 0 then
      slot.vitality <- min (slot.vitality - 1) 65535;
    identity
)


(*************************************************)

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
  }

let next_player game =
  (match game.current_player with
       Player0 ->
         game.current_player <- Player1
     | Player1 ->
         game.current_player <- Player0;
         game.turn_counter <- game.turn_counter + 1
  )

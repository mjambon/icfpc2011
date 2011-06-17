open Printf

exception Invalid_play

let invalid_play () = raise Invalid_play

let slots_len = 256

(* Name used for closure tracking (printing and debugging) *)
type name =
    Int_name of int
  | String_name of string
  | App_name of name * name

type value =
    Int of int
  | Fun of name * (game -> value -> value)

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

let int_of_player = function
    Player0 -> 0
  | Player1 -> 1

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
  | Fun (_, _) -> invalid_play ()

let func = function
    Fun (_, x) -> x
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
        Fun (_, f) -> f game y
      | _ -> invalid_play ()

let top_apply game x y =
  game.app_counter <- 0;
  apply game x y


(* Closure naming *)

let name_of_value = function
    Fun (x, _) -> x
  | Int n -> Int_name n

let app a b =
  App_name (a, name_of_value b)

let string_of_value x =
  let rec print buf = function
      Int_name n -> bprintf buf "%i" n
    | String_name s -> bprintf buf "%s" s
    | App_name (a, b) ->
        bprintf buf "%a(%a)" print a print b
  in
  let buf = Buffer.create 100 in
  print buf (name_of_value x);
  Buffer.contents buf


(* Card values *)

let identity = Fun (String_name "I", fun game x -> x)

let zero = Int 0

let succ = Fun (String_name "succ",
  fun game x -> 
    Int (min (int x + 1) 65535)
)

let dbl = Fun (String_name "dbl",
  fun game x ->
    Int (min (2 * int x) 65535)
)

let get = Fun (String_name "get",
  fun game x ->
    let i = int x in
    let slot = get_slot (proponent game) i in
    if is_alive slot then
      slot.field
    else
      invalid_play ()
)

let put = Fun (String_name "put",
  fun game x -> identity
)

let scomb =
  let name = String_name "S" in
  Fun (
    name,
    fun game f ->
      let name = app name f in
      Fun (
        name,
        fun game g ->
          let name = app name g in
          Fun (
            name,
            fun game x ->
              let h = apply game f x in
              let y = apply game g x in
              apply game h y
          )
      )
  )

let kcomb =
  let name = String_name "K" in
  Fun (
    name,
    fun game x ->
      let name = app name x in
      Fun (name, fun game y -> x)
)

let inc = Fun (String_name "inc",
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

let dec = Fun (String_name "dec",
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

let attack =
  let name = String_name "attack" in
  Fun (
    name,
    fun game i ->
      let name = app name i in
      Fun (
        name,
        fun game j ->
          let name = app name j in
          Fun (
            name,
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
    

let help =
  let name = String_name "help" in
  Fun (
    name,
    fun game i ->
      let name = app name i in
      Fun (
        name,
        fun game j ->
          let name = app name j in
          Fun (
            name,
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

let copy = Fun (String_name "copy",
  fun game i ->
    (get_slot (opponent game) (int i)).field
)

let revive = Fun (String_name "revive",
  fun game i ->
    let slot = get_slot (proponent game) (int i) in
    let v = slot.vitality in
    if v <= 0 then
      slot.vitality <- 1;
    identity
)

let zombie =
  let name = String_name "zombie" in
  Fun (
    name,
    fun game i ->
      let name = app name i in
      Fun (
        name,
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

(* Printing and debugging *)

let print_slots slots =
  Array.iteri (
    fun i slot ->
      if slot.field != identity || slot.vitality <> 10_000 then
        printf "%i={%i,%s}\n" i slot.vitality (string_of_value slot.field)
  ) slots


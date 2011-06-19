(* Utilities for analyzing the game *)

open State
open Play

type indicators = {
  pro_vitality_sum : int;
  opp_vitality_sum : int;

  pro_alive_count : int;
  opp_alive_count : int;

  pro_non_identity_count : int;
  opp_non_identity_count : int;
}

let sum_vitality a =
  Array.fold_left
    (fun acc x -> acc + x.vitality) 0 a

let count_alive a =
  Array.fold_left
    (fun acc x -> if is_alive x then acc + 1 else acc) 0 a

let count_non_identity a =
  Array.fold_left
    (fun acc x -> if x.field != identity then acc + 1 else acc) 0 a

let get_indicators game =
  let pro = proponent game in
  let opp = opponent game in
  {
    pro_vitality_sum = sum_vitality pro;
    opp_vitality_sum = sum_vitality opp;
    
    pro_alive_count = count_alive pro;
    opp_alive_count = count_alive opp;
    
    pro_non_identity_count = count_non_identity pro;
    opp_non_identity_count = count_non_identity opp;
  }


let is_int = function
    Int _ -> true
  | Fun _ -> false

let is_function = function
    Int _ -> false
  | Fun (desc, _) ->
      match desc with
          Val _ -> assert false
        | Prim Zero -> assert false
        | Prim _ -> true
        | Clo _ -> true

let get_fun_desc = function
    Int _ -> invalid_arg "get_fun_desc"
  | Fun (desc, _) -> desc

let get_card_arity = function
    I -> 1
  | Zero -> 0
  | Succ -> 1
  | Dbl -> 1
  | Get -> 1
  | Put -> 1
  | S -> 3
  | K -> 2
  | Inc -> 1
  | Dec -> 1
  | Attack -> 3
  | Help -> 3
  | Copy -> 1
  | Revive -> 1
  | Zombie -> 2

let rec get_fun_arity = function
    Val _
  | Prim Zero -> 0
  | Prim prim -> get_card_arity prim
  | Clo (f, x) -> get_fun_arity f - 1
      
(* Determines the number of arguments required before performing
   some evaluation other than just building a closure. *)
let rec get_arity = function
    Int _ -> 0
  | Fun (desc, _) -> get_fun_arity desc

let is_function x = get_arity x >= 1

exception Illegal
let check b =
  if not b then
    raise Illegal

(*
  Inspect a function and determine the primitive that will eventually
  be called and the position of the next argument to apply.

  Argument numbering starts from 0.
*)
let rec get_prim_and_argn pos = function
    Val _ -> assert false
  | Prim card -> card, pos
  | Clo (f, x) -> get_prim_and_argn (pos + 1) f


let check_slot ?(min_vit = -1) ?(max_vit = 65535) ?(compl = false) slots x =
  match x with
      Int i ->
        let i = if compl then 255 - i else i in
        if i < 0 || i > 255 then false
        else
          let v = slots.(i).vitality in
          v >= min_vit && v <= max_vit
    | Fun _ ->
        false

let check_proponent_slot ?min_vit ?max_vit game x =
  check_slot ?min_vit ?max_vit (proponent game) x

let check_opponent_slot ?min_vit ?max_vit game x =
  check_slot ?min_vit ?max_vit (opponent game) x

let check_opponent_compl_slot ?min_vit ?max_vit game x =
  check_slot ~compl:true ?min_vit ?max_vit (opponent game) x

let check_prim_arg game prim argn arg =
  match prim, argn with
      I, 0 -> true
    | Succ, 0 -> is_int arg
    | Dbl, 0 -> is_int arg
    | Get, 0 -> check_proponent_slot game arg ~min_vit:1
    | Put, 0 -> true
    | S, 0 -> is_function arg (* should return a function *)
    | S, 1 -> is_function arg
    | S, 2 -> true
    | K, 0 -> true
    | K, 1 -> true
    | Inc, 0 -> check_proponent_slot game arg ~min_vit:1 ~max_vit:35534
    | Dec, 0 -> check_opponent_slot game arg ~min_vit:1
    | Attack, 0 -> check_proponent_slot game arg
    | Attack, 1 -> check_opponent_compl_slot game arg ~min_vit:1
    | Attack, 2 -> is_int arg (* TODO: check v >= n *)
    | Help, 0 -> check_proponent_slot game arg
    | Help, 1 ->  check_proponent_slot game arg
    | Help, 2 -> check_proponent_slot game arg ~min_vit:1 (* TODO: check v>=n *)
    | Copy, 0 -> check_opponent_slot game arg
    | Revive, 0 -> check_proponent_slot game arg ~max_vit:0
    | Zombie, 0 -> check_opponent_compl_slot game arg
    | Zombie, 1 -> true (* TODO: check v<=0 *)
    | _ -> false


let is_legal game play =
  try
    let p = proponent game in
    let i = play.slot_number in
    let f, x =
      match play.left_or_right with
          Apply_card_to_slot -> value_of_card play.card, p.(i).field
        | Apply_slot_to_card -> p.(i).field, value_of_card play.card
    in
    check (is_function f);
    let f_arity = get_arity f in
    check (f_arity >= 1);
    let f_desc = get_fun_desc f in
    let prim, argn = get_prim_and_argn 0 f_desc in
    check_prim_arg game prim argn x

  with Illegal ->
    false

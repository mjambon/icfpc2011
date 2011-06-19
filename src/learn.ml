open State
open Play
open View

type strategy = {
  sub_strategy_name : string;
  mutable success : float;
  strat_fun : (game -> play);
}

let create_strategy name f = {
  sub_strategy_name = name;
  success = 1.;
  strat_fun = f;
}

type general_strategy = {
  general_strategy_name : string;
  sub_strategies : strategy list;
  get_health : (game -> indicators -> float);
  measure_success_after : int; (* number of turns *)
}

type learner = {
  turn1 : int;
  turn2 : int; (* key in the table *)
  health1 : float;
  gen_strat : general_strategy; (* provides get_health *)
  sub_strat : strategy; (* contains mutable success field *)
}

type learning_table = (int, learner) Hashtbl.t
    (* key = turn number at which we will measure the health
       for the second time and compute the success as the difference *)

let create_learning_table () = Hashtbl.create 1000

(* give more or less weight to newer data by changing the contribution ratio *)
let update_success average new_data =
  let contribution = 1./.50. in
  assert (contribution < 1. && contribution > 0.);
  (1. -. contribution) *. average +. contribution *. new_data

let learn (tbl : learning_table) game indicators =
  try
    let turn = game.turn_counter in
    assert (not (Hashtbl.mem tbl (turn-1)));
    let learners = Hashtbl.find_all tbl turn in
    List.iter (
      fun x ->
        let health2 = x.gen_strat.get_health game indicators in
        let local_success = health2 -. x.health1 in
        let strat = x.sub_strat in
        strat.success <- update_success strat.success local_success
    ) learners;
    Hashtbl.remove tbl turn

  with Not_found -> ()

let register tbl gen_strat sub_strat game indicators =
  let health1 = gen_strat.get_health game indicators in
  let turn1 = game.turn_counter in
  let turn2 = turn1 + gen_strat.measure_success_after in
  assert (turn2 > turn1);
  let learner = {
    turn1;
    turn2;
    health1;
    gen_strat;
    sub_strat;
  }
  in
  Hashtbl.add tbl turn2 learner
  
let find_best get_score l =
  match l with
      [] -> invalid_arg "find_best"
    | x :: l ->
        let best_score, best =
          List.fold_left (
            fun ((best_score, best) as acc) x ->
              let score = get_score x in
              if score > best_score then (score, x)
              else acc
          ) (get_score x, x) l
        in
        best

let get_best_sub_strategy gen_strat =
  find_best (fun x -> x.success) gen_strat.sub_strategies

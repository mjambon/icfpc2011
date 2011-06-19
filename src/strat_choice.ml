open State
open Play
open Learn
open View

type gen = {
  gen_build : general_strategy;
  gen_kill : general_strategy;
}

let init_rng () =
  let state = Random.State.make [| |] in
  Random.State.int state

(* Various sub_strategies *)

let trick1 rand =
  Strat_variety.variety ()

let trick2 rand game =
  trick1 rand game


(* Recipe to choose the general strategy *)

let choose_gen_strat gen game (x : indicators) =
  if x.pro_alive_count >= x.opp_alive_count then
    (* if winning then take the time to win even more in the long term *)
    gen.gen_build
  else
    (* if losing then destroy the enemy as fast as possible *)
    gen.gen_kill

let get_health_build game (x : indicators) =
  float (x.pro_non_identity_count - x.opp_non_identity_count)

let get_health_kill game (x : indicators) =
  float (x.pro_vitality_sum - x.opp_vitality_sum)


(* Learning/choosing machinery *)

let get_play rand learning_table gen game =
  let indicators = View.get_indicators game in
  Learn.learn learning_table game indicators;
  let gen_strat = choose_gen_strat gen game indicators in
  let sub_strat = Learn.get_best_sub_strategy gen_strat in
  Learn.register learning_table gen_strat sub_strat game indicators;
  sub_strat.strat_fun game


(* Initialization of match data *)

let choice () : game -> play =
  let rand = init_rng () in
  let gen_build = {
    general_strategy_name = "build";
    get_health = get_health_build;
    measure_success_after = 10;
    sub_strategies = [
      create_strategy "test123" (Trick.test123 rand);
      (*create_strategy "random" (Strat_variety.variety ());*)
    ]
  }
  in
  let gen_kill = {
    general_strategy_name = "kill";
    get_health = get_health_kill;
    measure_success_after = 10;
    sub_strategies = [
      create_strategy "test123" (Trick.test123 rand);
      (*create_strategy "random" (Strat_variety.variety ());*)
      (*create_strategy "helper" Trick.helper;*)
    ];
  }
  in
  let gen = {
    gen_kill;
    gen_build;
  }
  in
  let learning_table = Learn.create_learning_table () in
  get_play rand learning_table gen

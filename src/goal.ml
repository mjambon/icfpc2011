(*
  A goal is an expression that we eventually want to obtain.

  Because it takes several turns and various unexpected accidents,
  we specify the expression as a goal.

  Ways of achieving the goal include:
  - using partial applications and values in the proponent slot
  - copying and using values from the opponent
*)

open State

(*
  A goal is an expression that we would like to build.
*)
type goal =
    Left of card * goal (* apply card to slot *)
  | Right of goal * card (* apply slot to card *)
  | Range of range
  | Card of card
  | Alt of goal * goal (* alternative, required for expressing
                          a value before and after computation,
                          e.g. Alt (Left (Card Succ, exact 0), 
                                    exact 1)
                       *)

and high_low = High | Low

and range = (high_low option * int option * int option)


let exact x = Range (None, Some x, Some x)
let high mini maxi = Range (Some High, Some mini, Some maxi)
let low mini maxi = Range (Some Low, Some mini, Some maxi)
let pos = Range (None, Some 0, Some 255)

let left a b = Left (a, b)
let right a b = Right (a, b)

let in_range (_, mini, maxi) x =
  (match mini with
       None -> true
     | Some m -> m <= x)
  &&
    (match maxi with
         None -> true
       | Some m -> x <= m)

let rec matches goal value_desc =
  match goal, value_desc with
      Range r, Val x -> in_range r x
    | Range r, Prim Zero -> in_range r 0
    | Card Zero, Val 0 -> true
    | Card a, Prim b -> a = b
    | Left (a1, a2), Clo (b1, b2) -> matches (Card a1) b1 && matches a2 b2
    | Right (a1, a2), Clo (b1, b2) -> matches a1 b1 && matches (Card a2) b2
    | Alt (a, b), x -> matches a x || matches b x
    | _ -> false


(* Find a slot whose field values match the pattern *)
let find_slot rand goal game return : 'a option =
  let slots = proponent game in
  let len = Array.length slots in
  let offset = rand len in
  let acc = ref None in
  try
    for k = 0 to len - 1 do
      let i = (k + offset) mod len in
      let value = slots.(i).field in
      if matches goal (name_of_value value) then
        match return game i with
            None -> ()
          | Some _ as r ->
              acc := r;
              raise Exit
    done;
    !acc
  with Exit ->
    !acc

let validate make_play =
  fun game i ->
    let play = make_play i in
    if View.is_legal game play then Some play
    else None

(* Try to find a play that creates a value closer to the goal *)
let achieve_goal rand goal game : Play.play option =
  let rec is_buildable goal =
    match goal with
        Left (card, goal) ->
          (match
             find_slot rand goal game
               (validate (Play.left card))
           with
               None -> (* top goal not buildable *) is_buildable goal
             | Some play -> Some play
          )
      | Right (goal, card) ->
          (match
             find_slot rand goal game
               (validate (fun i -> Play.right i card))
           with
               None -> (* top goal not buildable *) is_buildable goal
             | Some play -> Some play
          )
      | Range _ -> None
      | Card _ -> None
      | Alt (a, b) ->
          match is_buildable a with
              None -> is_buildable b
            | Some _ as x -> x
  in
  is_buildable goal


(* Try to progress toward one of the goals, in that order of preference *)
let rec achieve rand (goals : goal list) game : Play.play option =
  match goals with
      [] -> None
    | goal :: l ->
        match achieve_goal rand goal game with
            Some play -> Some play
          | None -> achieve rand l game

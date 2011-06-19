(* Command-line interface *)

open Printf

let parse_player interactive = function
    "." ->
      let get game = Play.input_play interactive stdin in
      let print x = () in
      get, print
  | s ->
      let strategy =
        try List.assoc s Strategy.top_strategies
        with Not_found -> failwith ("Unknown strategy " ^ s)
      in
      let get = strategy () in
      let print x = Play.output_play stdout x in
      get, print

let main () =
  let args = ref [] in
  let options = [
    "-d", Arg.Set Debug.debug,
    "debugging mode";
  ] in
  let anon_fun = (fun s -> args := s :: !args) in
  let usage_msg =
    sprintf "\
Usage:
  %s play PLAYER0 PLAYER1
  %s compete {0|1} STRATEGY
  %s judge PROG0 PROG1

Where a player is either '.' for a human player or one of the following
built-in strategies:
%s
"
      Sys.argv.(0) Sys.argv.(0) Sys.argv.(0)
      (String.concat "\n" (List.map fst Strategy.top_strategies))
  in

  Arg.parse options anon_fun usage_msg;

  let usage () =
    prerr_string usage_msg;
    flush stderr;
    exit 1
  in

  match List.rev !args with
      [ "play"; x0; x1 ] ->
        let get0, print0 = parse_player true x0 in
        let get1, print1 = parse_player true x1 in
        let score0, score1 =
          Play.play_game (Play.init_game ()) true get0 print0 get1 print1
        in
        printf "player 0: %i\n" score0;
        printf "player 1: %i\n" score1

    | [ "compete"; i; x ] ->
        let x0, x1 =
          match i with
              "0" -> x, "."
            | "1" -> ".", x
            | _ -> usage ()
        in
        let get0, print0 = parse_player false x0 in
        let get1, print1 = parse_player false x1 in
        ignore
          (Play.play_game (Play.init_game ()) false get0 print0 get1 print1)

    | [ "judge"; x0; x1 ] ->
        failwith "judge: not implemented"
    | _ ->
        usage ()

let () = main ()

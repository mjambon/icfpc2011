(* Various micro-strategies *)

open State
open Play
open Goal

let example1 =
  left Succ (exact 0)

let example2 =
  left Dbl (high 1 65535)

let example3 =
  left K (left Get pos)

let example4 f x =
  right (left S (Card Get)) 


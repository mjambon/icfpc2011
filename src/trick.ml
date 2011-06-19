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

let make_int n =
  alt [
    exact n;
    left Succ (high (n/2+1) (n-1));
    left Dbl (high 1 (n/2)); (* find slot with less than m and double it *)
    left Succ (exact 0); (* find slot with 0 and add 1 *)
    right (Card I) Zero; (* find slot with identity function and place 0 *)
    left Put Any; (* clear any slot and put the identity function *)
  ]


let test123 rand =
  fun game ->
    achieve rand game [
      100, make_int 123; (* make 100 copies of the number 123 *)
    ]

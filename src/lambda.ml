(* OCaml exercises *)

(* Slots *)

type slot = {
  mutable v : int;
  mutable f : Obj.t;
}

let pro = Array.init 256 (fun i -> { v = 10_000; f = Obj.repr (fun x -> x) })
let opp = Array.init 256 (fun i -> { v = 10_000; f = Obj.repr (fun x -> x) })

(* Primitives (cards) *)

let id x = x

let zero = 0

let succ n = min (n + 1) 65535

let dbl n = min (2 * n) 65535

let get (i : int) = pro.(i).f

let put x = id

let scomb f g x =
  (f x) (g x)

let kcomb x y = x

let inc (i : int) = id

let dec (i : int) = id

let attack (i : int) (j : int) (n : int) = id

let help (i : int) (j : int) (n : int) = id

let copy (i : int) = opp.(i).f

let revive (i : int) = id

let zombie i x =
  opp.(i).f <- Obj.repr x


(* Examples *)

let x1 =
  dbl (dbl (succ zero))

let x2 =
  kcomb zero

(*
  S(Kf)Ix = (Kfx)(Ix)
          = f x
*)
let apply f x =
  scomb (kcomb f) id x

(* Helper library: https://bitbucket.org/cakeplus/solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


let pi = 4.0 *. atan 1.0

Solution (r, t: int64) : int64 =
  let rec iter ct sum r =
    let sum = sum + (pi *. (Int64.mul r ct)) in
    if Int64.compare sum t = -1 then
      iter (Int64.succ ct) (Int64.add r 2L)
    else
      ct
  in
  iter 0 0L r

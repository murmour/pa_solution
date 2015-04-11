(* Helper library: https://bitbucket.org/cakeplus/solution *)

open Batteries
  (* https://github.com/ocaml-batteries-team/batteries-included *)


let modulo = 1_000_000_007
let modulo64 = 1_000_000_007L


let combs n k =
  let arr = Array.make n 0 in
  arr.(k) <- 1;
  for i = k+1 to (n-1) do
    arr.(i) <- (arr.(i-1) * i / (i - k)) mod modulo
  done;
  arr


Solution (n: int) (k: int) (arr: array[n] of int) : int64 =
  Array.sort compare arr;
  let c = combs n (k-1) in

  let rec iter i acc =
    if i = n then
      acc
    else
      let acc = Int64.add acc (Int64.of_int (arr.(i) * c.(i))) in
      iter (i+1) (Int64.modulo acc modulo64)
  in

  iter k (Int64.of_int arr.(k-1))

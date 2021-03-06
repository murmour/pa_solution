(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries
  (* https://github.com/ocaml-batteries-team/batteries-included *)


let modulo = 1_000_000_007


let combs n k =
  let arr = Array.make n 0 in
  arr.(k) <- 1;
  for i = k+1 to (n-1) do
    arr.(i) <- (arr.(i-1) * i / (i - k)) mod modulo
  done;
  arr


Solution (n: "%d ") (k: "%d ") (arr: array[n] of "%d ") : "%d" =
  Array.sort compare arr;
  let c = combs n (k-1) in
  let rec iter i acc =
    if i = n then
      acc
    else
      let acc = acc + (arr.(i) * c.(i)) in
      iter (i+1) (acc mod modulo)
  in
  iter k arr.(k-1)

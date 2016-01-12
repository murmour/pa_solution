(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries
  (* https://github.com/ocaml-batteries-team/batteries-included *)


Solution (n: int) (a, b: float) (stages: array[n] of float) : string =
  let expectation = ref 0.0 in
  let calc_expectation a' b' = ((b'-.a') /. (b-.a)) *. ((b'+.a') /. 2.0) in

  let rec iter ci a b =
    let ci = if ci >= n then 0 else ci in
    let c = stages.(ci) in
    if c <= a then
      iter (ci+1) (a-.c) (b-.c)
    else if c > b then
      expectation := !expectation +. calc_expectation a b
    else
      (expectation := !expectation +. calc_expectation a c;
       iter (ci+1) 0.0 (b-.c))
  in
  iter 0 a b;

  Printf.sprintf "%.7f" !expectation

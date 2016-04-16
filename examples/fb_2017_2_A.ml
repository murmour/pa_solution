(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries
  (* https://github.com/ocaml-batteries-team/batteries-included *)


let solve1 h w k =
  if w < k*2 + 3 then
    -1
  else if h < k+1 then
    -1
  else if h < 2*k+1 then
    2
  else if k = 1 then
    5
  else
    4

let solve2 h w k =
  if w < k*2 + 3 then
    -1
  else if h < k+1 then
    -1
  else if h mod k = 0 then
    h / k
  else
    h / k + 1


Solution (n, m, k: "%d ") : "%d" =
  let best s1 s2 = if s1 = -1 then s2 else if s2 = -1 then s1 else min s1 s2 in
  List.reduce best [ solve1 n m k; solve1 m n k; solve2 n m k; solve2 m n k ]

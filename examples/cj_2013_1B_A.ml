(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


Solution (a, n: "%d ") (arr: array[n] of "%d ") : "%d" =
  Array.sort compare arr;
  let len = Array.length arr in

  let rec opt a i =
    if i = len then
      0
    else if arr.(i) < a then
      opt (a + arr.(i)) (i+1)
    else
      min (len - i) (1 + opt (a + (a-1)) i)
  in

  if a = 1 then
    len
  else
    opt a 0

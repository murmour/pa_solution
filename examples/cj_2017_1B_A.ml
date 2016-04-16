(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


Solution (d, n: "%d ") (horses: array[n] of "%d %d ") : "%.7f" =
  let time_left (k, s) = float_of_int (d - k) /. float_of_int s in
  let slower h1 h2 = if time_left h1 > time_left h2 then h1 else h2 in
  let slowest = Array.reduce slower horses in
  float_of_int d /. (time_left slowest)

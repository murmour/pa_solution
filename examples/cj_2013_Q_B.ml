(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


let any = reduce (||)

Solution (n, m: int) (arr: array[n,m] of int) : string =
  (function | true -> "YES" | false -> "NO")
    (not (any [? any [? arr.(x).(j) > arr.(i).(j) | x <- 0--(n-1) ?] &&
                 any [? arr.(i).(y) > arr.(i).(j) | y <- 0--(m-1) ?]
              | i <- 0--(n-1); j <- 0--(m-1) ?]))

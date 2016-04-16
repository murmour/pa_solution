(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


let all = reduce (&&)
let any = reduce (||)

let check arr fn =
  List.reduce (||)
    [
      any [? all [? fn arr.(i).[j] | j <- 0--3 ?] | i <- 0--3 ?];
      any [? all [? fn arr.(j).[i] | j <- 0--3 ?] | i <- 0--3 ?];
      all [? fn arr.(i).[i] | i <- 0--3 ?];
      all [? fn arr.(i).[3-i] | i <- 0--3 ?];
    ]


Solution (arr: array[4] of "%s ") : "%s" =
  if check arr (function 'X' | 'T' -> true | _ -> false) then
    "X won"
  else if check arr (function 'O' | 'T' -> true | _ -> false) then
    "O won"
  else if any [? arr.(i).[j] = '.' | i <- 0--3; j <- 0--3 ?] then
    "Game has not completed"
  else
    "Draw"

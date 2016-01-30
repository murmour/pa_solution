(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


Solution (n: int64) (d, g: int) : string =
  let result =
    if g = 100 then
      d = 100
    else if g = 0 then
      d = 0
    else if d = 100 then
      g > 0
    else if d = 0 then
      g < 100
    else
      let d = Int64.of_int d in
      let hun = Int64.of_int 100 in
      let rec iter_n n =
        if n = Int64.zero then false else
        if Int64.rem (Int64.mul n d) hun = Int64.zero then
          true
        else
          iter_n (Int64.pred n)
      in
      iter_n n
  in
  if result then "Possible" else "Broken"

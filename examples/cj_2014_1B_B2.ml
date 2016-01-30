(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


let calc_diff a b  =
  Enum.cartesian_product (0--(a-1)) (0--(b-1))
  |> Enum.map (fun (a, b) -> if a land b = 0 then 1 else 0)
  |> Enum.sum


let solve a b k =
  let base = (min k a) * (min k b) in
  let a' = max 0 (a - k - 1) in
  let b' = max 0 (b - k - 1) in
  base * calc_diff a' b'


Solution (a, b, k: int) : int =
  min k a * min k b * min 1 (a - k) * min 1 (b - k)

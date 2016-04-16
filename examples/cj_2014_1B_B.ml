(* Helper library: https://bitbucket.org/cakeplus/solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


Solution (a, b, k: "%d ") : "%d" =
  Enum.cartesian_product (0--(a-1)) (0--(b-1))
  |> Enum.filter (fun (a, b) -> a land b < k)
  |> Enum.count

(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries
  (* https://github.com/ocaml-batteries-team/batteries-included *)


Solution (n, r: "%d ") (zombies: list[n] of "%d %d ") : "%d" =
  let (xs, ys) = List.split zombies in
  let squares = List.cartesian_product (List.unique xs) (List.unique ys) in
  let zsets = squares |> List.map (fun (x, y) ->
    Set.of_list (zombies |> List.filter (fun (x', y') ->
      x' >= x && x' <= x+r && y' >= y && y' <= y+r)))
  in
  List.cartesian_product zsets zsets
  |> List.map (fun (s1, s2) -> Set.cardinal (Set.union s1 s2))
  |> List.max

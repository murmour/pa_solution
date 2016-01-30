(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


let rec solve_war = function
  | ([], _) | (_, []) -> 0
  | (x :: xs, y :: ys) ->
      if x > y then
        1 + solve_war (xs, y :: ys)
      else
        solve_war (xs, ys)

let rec solve_dwar = function
  | ([], _) | (_, []) -> 0
  | (x :: xs, y :: ys) ->
      if x > y then
        1 + solve_dwar (xs, ys)
      else
        solve_dwar (x :: xs, ys)


Solution (n: int) (n_blocks, k_blocks: list[n] of float) : tuple(int, int) =
  let n_blocks = List.sort compare n_blocks |> List.rev in
  let k_blocks = List.sort compare k_blocks |> List.rev in
  (solve_dwar (n_blocks, k_blocks), solve_war (n_blocks, k_blocks))

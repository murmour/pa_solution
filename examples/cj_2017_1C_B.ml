(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


let solve_small (a1s, a1e) (a2s, a2e) : int =
  if (a2s - a1e >= 720) || (1440 - a2e + a1s >= 720) then 2 else 4


Solution (ac, aj: "%d ")
         (acs: array[ac] of "%d %d ")
         (ajs: array[aj] of "%d %d ") : "%d" =
  Array.sort compare acs;
  Array.sort compare ajs;
  match (ac, aj) with
    | ((1 | 0), (1 | 0)) ->
        2
    | (2, 0) ->
        solve_small acs.(0) acs.(1)
    | (0, 2) ->
        solve_small ajs.(0) ajs.(1)
    | _ ->
        (* large: unfinished *)
        -1

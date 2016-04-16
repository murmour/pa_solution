(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


Solution (c, f, x: "%f ") : "%.7f" =
  let max_time = x /. 2.0 in
  let max_farms = int_of_float (ceil x) in

  let rec calc farms time speed : float list =
    if not (farms < max_farms && time < max_time) then [] else
      List.cons
        (time +. (x /. speed))
        (calc (farms + 1) (time +. (c /. speed)) (speed +. f))
  in

  calc 0 0.0 2.0 |> List.min

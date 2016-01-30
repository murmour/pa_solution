(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


Solution (d: int) (diners: list[d] of int) : int =
  List.min @@ List.init 1000 (fun lev ->
    if lev = 0 then List.max diners else
      List.sum (diners |> List.map (fun i -> (i - 1) / lev)) + lev)

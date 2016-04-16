(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


Solution (k, c, s: "%d ") : "%s" =
  if c * s < k then
    "IMPOSSIBLE"
  else
    List.of_enum (0--(k-1))
    |> List.ntake c
    |> List.map (fun tiles ->
        Enum.combine (List.enum tiles, 0--(c-1))
        |> Enum.map (fun (t, rank) -> t * Int.pow k rank)
        |> Enum.sum
        |> ((+) 1)
        |> string_of_int)
    |> String.concat " "

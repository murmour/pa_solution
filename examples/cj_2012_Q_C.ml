(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


let powers =
  Array.of_enum [? Int.pow 10 i | i <- 1--9 ?]


Solution (a, b: int) : int =
  let n = String.length (string_of_int a) in

  let rotate r by =
    let tail = r mod powers.(by-1) in
    r / powers.(by-1) + (tail * powers.(n-by-1))
  in

  a--b |>
  Enum.map (fun a ->
      [? rotate a by | by <- 1--(n-1) ?]
      |> Enum.filter (fun c -> c > a && c <= b)
      |> List.of_enum
      |> List.unique
      |> List.length)
  |> Enum.sum

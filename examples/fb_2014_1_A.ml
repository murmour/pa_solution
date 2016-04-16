(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries
  (* https://github.com/ocaml-batteries-team/batteries-included *)


let count_places base n : int =
  let rec iter acc ct =
    if acc >= n then
      ct
    else
      let ct = ct + 1 in
      iter (acc + (Int.pow base ct)) ct
  in
  iter base 1


Solution (l: "%s ") (n: "%d ") : "%s" =
  let base = String.length l in
  let places = count_places base n in
  let map =
    l |> String.enum
      |> Enum.mapi (fun i c -> (i+1, c))
      |> Map.of_enum
  in
  let n = ref n in
  String.init places (fun place ->
    let place = places - place in
    let x = !n / (Int.pow base place) in
    n := !n - x;
    map |> Map.find x)

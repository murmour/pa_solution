(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries
  (* https://github.com/ocaml-batteries-team/batteries-included *)


let count_places base n : int =
  let rec iter acc ct =
    if acc >= n then
      ct
    else
      let ct = ct + 1 in
      iter (Int64.add acc (Int64.pow base (Int64.of_int ct))) ct
  in
  iter base 1


Solution (l: string) (n: int64) : string =
  let base = Int64.of_int (String.length l) in
  let places = count_places base n in
  let map =
    l |> String.enum
      |> Enum.mapi (fun i c -> (Int64.of_int (i + 1), c))
      |> Map.of_enum
  in
  let n = ref n in
  String.init places (fun place ->
    let place = places - place in
    let x = Int64.div !n (Int64.pow base (Int64.of_int place)) in
    n := Int64.sub !n x;
    map |> Map.find x)

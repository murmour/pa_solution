(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


let rec get_words keys = function
  | 0 ->
      []
  | 1 ->
      keys |> List.map List.singleton
  | n ->
      let rest = get_words keys (n-1) in
      List.concat (rest |> List.map (fun word ->
        keys |> List.map (fun k -> k :: word)))


Solution (k, l, s: int) (keys: list[k] of char) (target: list[l] of char) : "%.6f" =
  let target' = String.of_list target in
  let words = get_words keys s in
  let words' = words |> List.map String.of_list in
  let counts = words' |> List.map (fun w -> String.find_all w target' |> Enum.count) in
  let maxcount = List.max counts in
  let prob = (1.0 /. (float_of_int k)) ** float_of_int s in
  let probs = counts |> List.map (fun c -> float_of_int c *. prob) in
  let expectation = probs |> ListLabels.fold_left ~init:0.0 ~f:(+.) in
  float_of_int maxcount -. expectation

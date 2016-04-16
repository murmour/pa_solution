(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


let all = reduce (&&)

let is_palin i =
  let s = string_of_int i in
  let l = String.length s in
  all [? s.[i] = s.[l-1-i] | i <- 0--(l/2) ?]

let get_palin i =
  let s = String.of_int i in
  (int_of_string (s ^ (String.backwards s |> String.of_enum)),
   int_of_string (s ^ (String.backwards s |> Enum.skip 1 |> String.of_enum)))

let palins =
  [? get_palin i | i <- 0--100000 ?]
  |> Enum.uncombine
  |> fun (a, b) -> Enum.append a b
  |> map (fun i -> i*i)
  |> filter is_palin

let palins_arr =
  let arr = Array.of_enum palins in
  Array.sort compare arr;
  arr


Solution (a, b: "%d ") : "%d" =
  let i1 = palins_arr |> Array.findi (fun c -> c >= a) in
  let i2 = palins_arr |> Array.findi (fun c -> c > b) in
  i2 - i1

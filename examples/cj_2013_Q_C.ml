(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


let all = reduce (&&)

let is_palin i =
  let s = Int64.to_string i in
  let l = String.length s in
  all [? s.[i] = s.[l-1-i] | i <- 0--(l/2) ?]

let get_palin i =
  let s = String.of_int i in
  (Int64.of_string (s ^ (String.backwards s |> String.of_enum)),
   Int64.of_string (s ^ (String.backwards s |> Enum.skip 1 |> String.of_enum)))

let palins =
  [? get_palin i | i <- 0--100000 ?]
  |> Enum.uncombine
  |> fun (a, b) -> Enum.append a b
  |> map (fun i -> Int64.pow i 2L)
  |> filter is_palin

let palins_arr =
  let arr = Array.of_enum palins in
  Array.sort compare arr;
  arr


Solution (a, b: int64) : int =
  let i1 = palins_arr |> Array.findi (fun c -> Int64.compare c a >= 0) in
  let i2 = palins_arr |> Array.findi (fun c -> Int64.compare c b > 0) in
  i2 - i1

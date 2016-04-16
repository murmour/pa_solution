(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


let add_slices len n map =
  try
    Map.modify len ((+) n) map
  with Not_found ->
    Map.add len n map

let rec solve map k =
  let (slice, ct) = Map.max_binding map in
  if slice = 0 then
    (0, 0)
  else
    let k = k - ct in
    let ls = (slice - 1) / 2 in
    let rs = slice - 1 - ls in
    if k <= 0 then
      (max ls rs, min ls rs)
    else
      let map = Map.remove slice map in
      let map = add_slices ls ct map in
      let map = add_slices rs ct map in
      solve map k

Solution (n, k: "%d ") : "%d %d" =
  solve (Map.singleton n 1) k

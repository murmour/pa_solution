(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


let rec for_each_digit i fn =
  fn (i mod 10);
  let rest = i / 10 in
  if rest > 0 then
    for_each_digit rest fn


Solution (n: "%d ") : "%s" =
  let digits = ref [? Set: x | x <- 0--9 ?] in
  let rec iter ct =
    let n' = n * ct in
    for_each_digit n' (fun i -> digits := Set.remove i !digits);
    if Set.is_empty !digits then n' else iter (ct+1)
  in
  if n = 0 then "INSOMNIA" else string_of_int (iter 1)

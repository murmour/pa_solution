(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


let rec gcd a = function
  | 0 -> a
  | b -> gcd b (a mod b)

let simplify (a, b) =
  let g = abs (gcd a b) in
  (a / g, b / g)

let pow2 =
  let p = ref 1 in
  LazyList.from (fun () -> let this = !p in p := this * 2; this)


Solution (p: "%d/") (q: "%d\n") : string =
  let (p, q) = simplify (p, q) in
  let base = Int.pow 2 40 in
  if base mod q <> 0 then
    "impossible"
  else
    let p' = (p * (base / q)) in
    let pow = pow2 |> LazyList.findi (fun i v -> v > p') |> fst in
    string_of_int (40 - (pow - 1))

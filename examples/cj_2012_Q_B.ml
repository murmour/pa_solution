(* Helper library: https://github.com/cakeplus/solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


let possible (a, b, c) =
  let cond x y = abs (x - y) < 3 in
  cond a b && cond a c && cond b c

let possible_triplets p n =
  [? List: (a, b, c) | a <- 0--10; b <- 0--10; c <- 0--10;
     a + b + c = n;
     possible (a, b, c);
     a >= p || b >= p || c >= p ?]

let surprising (a, b, c) =
  let cond x y = abs (x - y) = 2 in
  cond a b || cond a c || cond b c

let has_unsurprising l =
  l |> List.exists (fun l -> (not (surprising l)))

let has_surprsing l =
  l |> List.exists surprising


Solution (n, s, p: int) (scores: list[n] of int) : int =
  let gaps = ref s in
  let pipl = ref 0 in

  scores |> List.iter (fun score ->
    let all = possible_triplets p score in
    if has_unsurprising all then
      incr pipl
    else
      if !gaps > 0 && has_surprsing all then
        (incr pipl; decr gaps));
  !pipl

(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries
  (* https://github.com/ocaml-batteries-team/batteries-included *)


let limit =
  10_000_000

let primacity: int -> int =
  let sieve = Array.make (limit+1) 0 in
  for i = 2 to limit do
    if sieve.(i) = 0 then
      begin
        sieve.(i) <- 1;
        let j = ref (i+i) in
        while !j <= limit do
          sieve.(!j) <- sieve.(!j) + 1;
          j := !j + i;
        done;
      end;
  done;
  fun i -> sieve.(i)


Solution (a, b, k: int) : int =
  let count = ref 0 in
  for i = a to b do
    if primacity i = k then
      incr count
  done;
  !count

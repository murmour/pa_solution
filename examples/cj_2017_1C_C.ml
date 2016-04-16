(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


Solution (n, k: "%d ") (u: "%f ") (cores: array[n] of "%f ") : "%.7f" =
  Array.sort compare cores;
  let u = ref u in
  let last = ref cores.(0) in
  for i = 1 to n-1 do
    let diff = min (cores.(i) -. !last) (!u /. float_of_int i) in
    for j = 0 to i-1 do
      cores.(j) <- cores.(j) +. diff;
    done;
    u := !u -. (diff *. float_of_int i);
    last := cores.(i)
  done;
  for i = 0 to n-1 do
    cores.(i) <- cores.(i) +. (!u /. float_of_int n)
  done;
  Array.reduce ( *. ) cores

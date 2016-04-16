(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries
  (* https://github.com/ocaml-batteries-team/batteries-included *)


Solution (n: "%d ") (difficulties: array[n] of "%d ") : "%d" =
  let count = ref 0 in
  let rec iter i pos last =
    if pos = 4 then
      iter i 0 0
    else if i >= n then
      count := !count + ((4 - pos) mod 4)
    else if pos > 0 && difficulties.(i) - last > 10 then
      (count := !count + 1;
       iter i (pos+1) (last+10))
    else if pos = 0 || difficulties.(i) > last then
      iter (i+1) (pos+1) difficulties.(i)
    else
      (count := !count + ((4 - pos) mod 4);
       iter i 0 0)
  in
  iter 0 0 0;
  !count

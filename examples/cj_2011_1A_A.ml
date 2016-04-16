(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


Solution (n, d, g: "%d ") : "%s" =
  let result =
    if g = 100 then
      d = 100
    else if g = 0 then
      d = 0
    else if d = 100 then
      g > 0
    else if d = 0 then
      g < 100
    else
      let rec iter n =
        if n = 0 then
          false
        else if (n * d) mod 100 = 0 then
          true
        else
          iter (n-1)
      in
      iter n
  in
  if result then "Possible" else "Broken"

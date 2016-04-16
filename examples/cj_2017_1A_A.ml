(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


Solution (r, c: "%d ") (cake: array[r] of "%s ") : "\n%s" =
  for row = 0 to r-1 do
    for col = 1 to c-1 do
      if cake.(row).[col] = '?' then
        cake.(row).[col] <- cake.(row).[col-1]
    done;
    for col = c-2 downto 0 do
      if cake.(row).[col] = '?' then
        cake.(row).[col] <- cake.(row).[col+1]
    done;
  done;
  for col = 0 to c-1 do
    for row = 1 to r-1 do
      if cake.(row).[col] = '?' then
        cake.(row).[col] <- cake.(row-1).[col]
    done;
    for row = r-2 downto 0 do
      if cake.(row).[col] = '?' then
        cake.(row).[col] <- cake.(row+1).[col]
    done;
  done;
  String.concat "\n" (Array.to_list cake)

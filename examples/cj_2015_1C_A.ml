(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


Solution (r, c, w: "%d ") : "%d" =
   ((c / w) * r) + (if c mod w = 0 then w - 1 else w)

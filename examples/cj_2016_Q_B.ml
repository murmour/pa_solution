(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


Solution (cakes: "%s ") : "%d" =
  let ct = ref 1 in
  let last = ref cakes.[0] in
  cakes |> String.iter (fun c -> if c <> !last then (incr ct; last := c));
  if !last = '-' then !ct else !ct - 1

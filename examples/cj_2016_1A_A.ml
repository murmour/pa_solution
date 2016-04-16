(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


Solution (s: "%s ") : "%s" =
  let word = ref "" in
  s |> String.iter (fun c ->
    let w1 = String.of_char c ^ !word in
    let w2 = !word ^ String.of_char c in
    word := if w1 > w2 then w1 else w2);
  !word

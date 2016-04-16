(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


Solution (smax: "%d ") (shy_c: array[smax+1] of "%c ") : "%d" =
  let shy_i = shy_c |> Array.map (fun c -> Char.code c - Char.code '0') in
  let standing = ref 0 in
  let friends = ref 0 in
  for i = 0 to smax do
    if i <= !standing then
      standing := !standing + shy_i.(i)
    else
      (let more_friends = i - !standing in
       friends := !friends + more_friends;
       standing := !standing + more_friends + shy_i.(i))
  done;
  !friends

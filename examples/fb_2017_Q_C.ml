(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries
  (* https://github.com/ocaml-batteries-team/batteries-included *)


let parse_spell s : (int * int * int) =
  try
    Scanf.sscanf s "%dd%d%d" (fun x y z -> (x, y, z))
  with _ ->
    Scanf.sscanf s "%dd%d" (fun x y -> (x, y, 0))

let estimate_spell h (x, y, z) : float =
  let limit = x*y in
  let dp = Array.make_matrix (x+1) (limit+1) 0.0 in
  let p = 1.0 /. float_of_int y in

  dp.(0).(0) <- 1.0;
  for i = 1 to x do
    dp.(i) <- Array.make (limit+1) 0.0;
    for j = 0 to limit do
      for k = 1 to y do
        if j+k <= limit then
          dp.(i).(j+k) <- dp.(i).(j+k) +. (p *. dp.(i-1).(j))
      done
    done
  done;

  let estim = ref 0.0 in
  for i = 0 to limit do
    if i+z >= h then
      estim := !estim +. dp.(x).(i)
  done;
  !estim


Solution (h, s: "%d ") (spells: list[s] of "%s ") : "%f" =
  spells
  |> List.map parse_spell
  |> List.map (estimate_spell h)
  |> List.reduce max

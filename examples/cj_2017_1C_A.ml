(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


let pi =
  3.1415926535897932384626433832795029


Solution (n, k: "%d ") (cakes: array[n] of "%f %f ") : "%.7f" =
  Array.sort (fun (r1, h1) (r2, h2) -> compare r1 r2) cakes;
  let best = ref 0.0 in

  for i = k-1 to n-1 do
    let (r0, h0) = cakes.(i) in
    let s0 = (2.0 *. pi *. r0 *. h0) +. (pi *. (r0 *. r0)) in
    let sub = Array.sub cakes 0 i in
    let total =
      let area = sub |> Array.map (fun (r, h) -> 2.0 *. pi *. r *. h) in
      Array.sort compare area;
      Array.fold_left (+.) 0.0 (Array.sub area (i-k+1) (k-1)) +. s0
    in
    best := max !best total
  done;

  !best

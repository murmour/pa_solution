(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)
open Printf


let count_cycle bffs i : int option =
  let rec iter seen i' ct =
    if i = i' then
      Some ct
    else if Set.mem i' seen then
      None
    else
      iter (Set.add i' seen) bffs.(i') (ct+1)
  in
  iter (Set.singleton i) bffs.(i) 1


let count_oglobli bffs n : int =
  let is_knot = bffs |> Array.mapi (fun i friend -> bffs.(friend) = i) in
  let knot_power = bffs |> Array.map (fun i -> 0) in

  let knot_path_len i : (int * int) option =
    let rec iter seen i ct =
      if is_knot.(i) then
        Some (ct, i)
      else if Set.mem i seen then
        None
      else
        iter (Set.add i seen) bffs.(i) (ct+1)
    in
    iter Set.empty i 0
  in

  for i = 0 to n-1 do
    knot_path_len i |> Option.may (fun (len, knot) ->
      knot_power.(knot) <- max knot_power.(knot) len)
  done;

  let knot_score = ref 0 in
  is_knot |> Array.iter (fun b -> if b then incr knot_score);
  Array.sum knot_power + !knot_score


Solution (n: "%d ") (bffs: array[n] of "%d ") : "%d" =
  let bffs = bffs |> Array.map (fun i -> i-1) in
  let result = ref 0 in
  for i = 0 to n-1 do
    count_cycle bffs i |> Option.may (fun ct -> result := max ct !result);
  done;
  result := max (count_oglobli bffs n) !result;
  !result

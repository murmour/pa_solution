(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


let rec get_perms = function
  | [] ->
      []
  | [ x ] ->
      [[]; [ x ]]
  | x :: xs ->
      let rest = get_perms xs in
      (rest |> List.map (fun xs' -> x :: xs')) @ rest

let rec times n l =
  match n with
    | 1 ->
        l
    | n ->
        l @ times (n-1) l

let sum l =
  l |> ListLabels.fold_left ~init:0 ~f:(+)


Solution (c, d, v: int) (dens: list[d] of int) : int =
  let new_dens = ref (Set.of_list dens) in
  for i = 1 to v do
    let sums = get_perms (times c (Set.elements !new_dens)) |> List.map sum in
    if not (List.mem i sums) then
      new_dens := Set.add i !new_dens
  done;
  List.length (Set.elements !new_dens) - d

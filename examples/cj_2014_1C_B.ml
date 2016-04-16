(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


let ( * ) =
  let limit = 1_000_000_007 in
  fun a b -> (a * b) mod limit

let rec fact = function
  | 0 -> 1
  | n -> n * fact (n-1)

let classify set =
  let first = set.[0] in
  if set |> String.enum |> Enum.for_all ((=) first) then
    `Pure first
  else
    `Impure

let try_merge s1 s2 =
  if String.right s1 1 = String.left s2 1 then
    Some (s1 ^ s2)
  else if String.left s1 1 = String.right s2 1 then
    Some (s2 ^ s1)
  else
    None

let rec inject_set ~cond prev x = function
  | [] -> None
  | y :: ys ->
      if cond y then
        match try_merge x y with
          | Some z ->
              Some (prev @ (z :: ys))
          | None ->
              inject_set ~cond (y :: prev) x ys
      else
        inject_set ~cond (y :: prev) x ys

let rec merge_chains = function
  | [] -> []
  | x :: xs ->
      match inject_set ~cond:(classify %> ((!=) `Impure)) [] x xs with
        | Some xs ->
            merge_chains xs
        | None ->
            match inject_set ~cond:(fun _ -> true) [] x xs with
              | Some xs ->
                  merge_chains xs
              | None ->
                  x :: merge_chains xs

let rec is_unique = function
  | [] ->
      true
  | x :: xs ->
      (not (List.mem x xs)) && is_unique xs

let nice_chains chains : bool =
  chains
  |> String.concat ""
  |> String.to_list
  |> List.group_consecutive (=)
  |> List.map List.hd
  |> is_unique


Solution (n: "%d ") (sets: list[n] of "%s ") : "%d" =
  let chains = merge_chains sets in
  if not (nice_chains chains) then 0 else
    sets |> List.filter_map (fun set ->
              match classify set with
                | `Pure c -> Some c
                | `Impure -> None)
         |> List.group compare
         |> List.map List.length
         |> List.map fact
         |> ListLabels.fold_left ~init:1 ~f:( * )
         |> ( * ) (fact (List.length chains))

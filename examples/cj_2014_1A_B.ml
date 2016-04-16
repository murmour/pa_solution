(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


type tree = Tree of tree list


let rec build_tree g pred node : tree =
  let adj = MultiMap.find node g |> Set.remove pred in
  Tree (adj |> Set.elements |> List.map (build_tree g node))

let rec count_nodes (Tree children) : int =
  1 + (children |> List.map count_nodes |> List.fold_left (+) 0)

let rec solve_tree (Tree children) : int =
  match children with
    | [] -> 0
    | [ t1 ] ->
        count_nodes t1
    | ts ->
        ts |> List.map (fun t -> count_nodes t - solve_tree t)
           |> List.sort compare
           |> List.rev
           |> fun (c1 :: c2 :: _) ->
                let sum = ts |> List.map count_nodes |> List.sum in
                sum - c1 - c2

let build_graph edges =
  let m = ref MultiMap.empty in
  edges |> List.iter (fun (v1, v2) ->
    m := !m |> MultiMap.add v1 v2 |> MultiMap.add v2 v1);
  !m


Solution (n: "%d ") (edges: list[n-1] of "%d %d ") : "%d" =
  let g = build_graph edges in
  (1--n)
  |> Enum.map (build_tree g 0)
  |> Enum.map solve_tree
  |> Enum.reduce min

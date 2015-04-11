(* Helper library: https://bitbucket.org/cakeplus/solution *)

open Batteries
  (* https://github.com/ocaml-batteries-team/batteries-included *)

module G = Graph.Pack.Digraph
  (* ocamlgraph.lri.fr *)


Solution (n: int) (classes: list[n] of let m: int in list[m] of int) : string =
  let g = G.create () in
  let v = Array.init n (fun i -> G.V.create (i + 1)) in

  classes |> List.iteri (fun i c ->
    c |> List.iter (fun child ->
      G.add_edge g v.(i) v.(child - 1)));

  Return.label (fun lab ->
    let rec traverse v =
      if G.Mark.get v = 1 then
        Return.return lab "Yes";
      G.Mark.set v 1;
      G.iter_succ traverse g v
    in
    g |> G.iter_vertex (fun v -> G.Mark.clear g; traverse v);
    "No")

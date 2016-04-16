(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


type player = R | P | S


let choose_tree t1 t2 =
  let l = t1 ^ t2 and r = t2 ^ t1 in
  if l < r then l else r


let rec construct_tree h t =
  if h = 0 then
    match t with
      | R -> "R"
      | P -> "P"
      | S -> "S"
  else
    let h = h - 1 in
    let r_tree = construct_tree h R in
    let p_tree = construct_tree h P in
    let s_tree = construct_tree h S in
    match t with
      | R ->
          choose_tree r_tree s_tree
      | P ->
          choose_tree p_tree r_tree
      | S ->
          choose_tree s_tree p_tree


let count_players str =
  let r = ref 0 and p = ref 0 and s = ref 0 in
  str |> String.iter (function
    | 'R' -> incr r
    | 'P' -> incr p
    | 'S' -> incr s
    | _ -> assert false);
  (!r, !p, !s)


Solution (n, r, p, s: "%d ") : "%s" =
  let sol = ref None in

  let update_sol t =
    if count_players t = (r, p, s) then
      match !sol with
        | None ->
            sol := Some t
        | Some sol' ->
            sol := Some (min sol' t)
  in

  update_sol (construct_tree n R);
  update_sol (construct_tree n P);
  update_sol (construct_tree n S);

  match !sol with
    | None ->
        "IMPOSSIBLE"
    | Some sol' ->
        sol'

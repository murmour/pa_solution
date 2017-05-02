(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries
  (* https://github.com/ocaml-batteries-team/batteries-included *)


let calc_dists n edges : int option array array =
  let dists = Array.make_matrix n n None in
  for i = 0 to n-1 do
    dists.(i).(i) <- Some 0
  done;

  (* Choosing minimal edges *)
  edges |> List.iter (fun (a, b, g) ->
    let d = match dists.(a).(b) with Some d -> min g d | None -> g in
    dists.(a).(b) <- Some d;
    dists.(b).(a) <- Some d);

  (* Warshall-Floyd *)
  for k = 0 to n-1 do
    for i = 0 to n-1 do
      for j = 0 to n-1 do
        match (dists.(i).(j), dists.(i).(k), dists.(k).(j)) with
          | (Some ij, Some ik, Some kj) when ij > ik+kj ->
              dists.(i).(j) <- Some (ik+kj)
          | (None, Some ik, Some kj) ->
              dists.(i).(j) <- Some (ik+kj)
          | _ -> ()
      done
    done
  done;

  dists


exception Impossible


Solution (n, m, k: "%d ")
         (roads: list[m] of "%d %d %d ")
         (families: array[k] of "%d %d ") : "%d" =

  let families = families |> Array.map (fun (a, b) -> (a-1, b-1)) in
  let roads = roads |> List.map (fun (a, b, g) -> (a-1, b-1, g)) in
  let dists = calc_dists n roads in

  let get_dist a b =
    match dists.(a).(b) with
      | Some d -> d
      | None -> raise Impossible
  in

  let rec iter f (sum1, c1) (sum2, c2) =
    if f = k-1 then
      let (_, last) = families.(k-1) in
      min (sum1 + get_dist c1 last) (sum2 + get_dist c2 last)
    else
      let (_, a2) = families.(f) in
      let (b1, _) = families.(f+1) in
      let sum1' =
        min (sum1 + get_dist c1 a2 + get_dist a2 b1)
            (sum2 + get_dist c2 a2 + get_dist a2 b1)
      in
      let sum2' =
        min (sum1 + get_dist c1 b1 + get_dist b1 a2)
            (sum2 + get_dist c2 b1 + get_dist b1 a2)
      in
      iter (f+1) (sum1', b1) (sum2', a2)
  in

  let (a, _) = families.(0) in
  let ad = get_dist 0 a in
  try
    iter 0 (ad, a) (ad, a)
  with Impossible ->
    -1

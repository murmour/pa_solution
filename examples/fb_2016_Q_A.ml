(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries
  (* https://github.com/ocaml-batteries-team/batteries-included *)


let distance (x1, y1) (x2, y2) : int =
  let dx = x1 - x2 and dy = y1 - y2 in
  dx*dx + dy*dy

let count_duplicates arr : (int, int) Map.t =
  let m = ref Map.empty in
  arr |> Array.iter (fun d ->
    m := Map.modify_def 0 d succ !m);
  !m


Solution (n: "%d ") (stars: array[n] of "%d %d ") : "%d" =
  let dists = Array.make_matrix n n (-1) in
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      if i <> j then
        dists.(i).(j) <- distance stars.(i) stars.(j)
    done
  done;

  let count = ref 0 in
  for i = 0 to n-1 do
    count_duplicates dists.(i) |> Map.iter (fun dist ct ->
      count := !count + ((ct * (ct-1)) / 2))
  done;
  !count

(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries
  (* https://github.com/ocaml-batteries-team/batteries-included *)


let distance (x1, y1) (x2, y2) : int =
  let dx = x1 - x2 and dy = y1 - y2 in
  dx*dx + dy*dy

let count_duplicates arr : int list =
  Array.sort compare arr;
  let last = ref arr.(0) in
  let last_count = ref 1 in
  let list = ref [] in
  for i = 1 to Array.length arr - 1 do
    if arr.(i) = !last then
      incr last_count
    else
      (list := !last_count :: !list;
       last := arr.(i);
       last_count := 1)
  done;
  list := !last_count :: !list;
  !list


Solution (n: int) (stars: array[n] of tuple(int, int)) : int =
  let dists = Array.make_matrix n n (-1) in
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      if i <> j then
        dists.(i).(j) <- distance stars.(i) stars.(j)
    done
  done;

  let count = ref 0 in
  for i = 0 to n-1 do
    count_duplicates dists.(i) |> List.iter (fun ct ->
      count := !count + ((ct * (ct-1)) / 2))
  done;
  !count

(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries
  (* https://github.com/ocaml-batteries-team/batteries-included *)


Solution (l, n, m, d: "%d ") (times: array[n] of "%d ") : "%d" =

  (* l -> wq *)
  let wq = ref Set.empty in
  for i = 0 to n-1 do
    wq := Set.add (times.(i), i) !wq
  done;

  (* wq -> ... *)
  let get_next_wtime () =
    let ((time, wid), wq') = Set.pop_min !wq in
    let time' = time + times.(wid) in
    wq := Set.add (time', wid) wq';
    time
  in

  (* wq -> dq *)
  let dq_size = min l m in
  let dq = Queue.create () in
  for i = 1 to dq_size do
    Queue.add (get_next_wtime () + d) dq
  done;

  (* wq -> dq -> ... *)
  for i = dq_size + 1 to l do
    Queue.add ((max (Queue.pop dq) (get_next_wtime ())) + d) dq
  done;

  (* dq -> ... *)
  let last = ref 0 in
  for i = 1 to dq_size do
    last := Queue.pop dq
  done;
  !last

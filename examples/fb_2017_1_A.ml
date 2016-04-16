(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries
  (* https://github.com/ocaml-batteries-team/batteries-included *)


Solution (n, m: "%d ") (pies: array[n, m] of "%d ") : "%d" =
  let heap = ref Heap.empty in
  let sum = ref 0 in
  for i = 0 to n-1 do
    Array.sort compare pies.(i);
    let last_p = ref 0 in
    for j = 0 to m-1 do
      let p = (j+1)*(j+1) in
      heap := Heap.add (pies.(i).(j) + p - !last_p) !heap;
      last_p := p;
    done;
    let pie = Heap.find_min !heap in
    sum := !sum + pie;
    heap := Heap.del_min !heap
  done;
  !sum

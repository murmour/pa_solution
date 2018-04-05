(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries
  (* https://github.com/ocaml-batteries-team/batteries-included *)


Solution (n: "%d ") (weights: list[n] of "%d ") : "%d" =
  let d = ref (weights |> List.sort compare |> Deque.of_list) in
  let trips = ref 0 in
  while not (Deque.is_empty !d) do
    Deque.rear !d |> Option.may (fun (d', top) ->
      d := d';
      let total = ref top in
      while !total < 50 && not (Deque.is_empty !d) do
        Deque.front !d |> Option.may (fun (_, d') ->
          d := d';
          total := !total + top)
      done;
      if !total >= 50 then
        incr trips)
  done;
  !trips

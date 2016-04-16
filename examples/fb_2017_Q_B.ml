(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries
  (* https://github.com/ocaml-batteries-team/batteries-included *)


Solution (n: "%d ") (weights: list[n] of "%d ") : "%d" =
  let rec iter d =
    match Deque.rear d with
      | None ->
          0
      | Some (d', x) ->
          let rec subiter d total =
            if total >= 50 then
              Some d
            else match Deque.front d with
              | None ->
                  None
              | Some (_, d') ->
                  subiter d' (total+x)
          in
          match subiter d' x with
            | None ->
                0
            | Some d'' ->
                1 + iter d''
  in
  iter (weights |> List.sort compare |> Deque.of_list)

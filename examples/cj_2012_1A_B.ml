(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries
  (* https://github.com/ocaml-batteries-team/batteries-included *)


let some_unsolved =
  Array.exists Option.is_some

let choose_s s =
  Array.findi (function | Some a -> a <= s | _ -> false)


Solution (n: "%d ") (levels: list[n] of "%d %d ") : "%s" =
  let size = 1000 in
  let l1 = Array.make size None in
  let l2 = Array.make size None in
  levels
  |> List.sort (fun (_, b1) (_, b2) -> compare b2 b1)
  |> List.iteri (fun i (a, b) ->
      l1.(i) <- Some a;
      l2.(i) <- Some b);

  let rec solve_iter d s =
    if not (some_unsolved l2) then
      Some d
    else
      try
        let i = choose_s s l2 in
        l2.(i) <- None;
        if l1.(i) = None then
          solve_iter (d + 1) (s + 1)
        else
          (l1.(i) <- None;
           solve_iter (d + 1) (s + 2))
      with Not_found ->
        try
          let i = choose_s s l1 in
          l1.(i) <- None;
          solve_iter (d + 1) (s + 1)
        with Not_found ->
          None
  in
  match (solve_iter 0 0) with
    | Some i ->
        string_of_int i
    | None ->
        "Too Bad"

(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries
  (* https://github.com/ocaml-batteries-team/batteries-included *)


let get_prob_list l =
  let p = ref 1.0 in
  l |> List.map (fun p2 -> p := !p *. p2; !p) |> List.rev


Solution (a, b: "%d ") (probs: list[a] of "%f ") : "%f" =
  let to_retype = float_of_int (b + 1) in

  let expectation n succ_p =
    let to_finish = float_of_int (b - a + 1 + (n * 2)) in
    to_finish *. succ_p +. (to_finish +. to_retype) *. (1.0 -. succ_p)
  in

  probs
  |> get_prob_list
  |> List.mapi expectation
  |> List.cons (float_of_int (b + 2)) (* give up *)
  |> List.min

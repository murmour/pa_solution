(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


let context_map f l =
  let rec iter ys rs = function
    | [] ->
        List.rev rs
    | x :: xs ->
        let r = f x (ys @ xs) in
        iter (x :: ys) (r :: rs) xs
  in
  iter [] [] l

let solve_guy f rest sum =

  let solve_p p =
    let my_p = f +. p *. sum in
    rest
    |> List.map (fun f -> (my_p -. f) /. sum)
    |> List.map (max 0.0)
    |> List.fold_left (+.) 0.0
    |> (+.) p
    |> (<=) 1.0
  in

  let rec solve_range a b iter =
    let mid = (a +. b) /. 2.0 in
    if iter = 100 then
      mid
    else if solve_p mid then
      solve_range a mid (iter + 1)
    else
      solve_range mid b (iter + 1)
  in
  solve_range 0.0 1.0 0


Solution (n: "%d ") (pts: list[n] of "%d ") : list[n] of "%.7f " =
  let sum = float_of_int (List.sum pts) in
  let pts = pts |> List.map float_of_int in
  pts
  |> context_map (fun f rest -> solve_guy f rest sum)
  |> List.map (( *.) 100.0)

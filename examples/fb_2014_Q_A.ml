(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries
  (* https://github.com/ocaml-batteries-team/batteries-included *)


let is_black arr (i, j) =
  arr.(i).[j] = '#'

let find_first arr n =
  Enum.Exceptionless.find (is_black arr)
    [? (i, j) | i <- 0--(n-1); j <- 0--(n-1) ?]

let find_last arr n =
  Enum.Exceptionless.find (is_black arr)
    [? (i, j) | i <- (n-1)---0; j <- (n-1)---0 ?]

let check_square arr n (x1, y1) (x2, y2) =
  if (x1 - x2) <> (y1 - y2) then
    false
  else
    [? (i, j) | i <- 0--(n-1); j <- 0--(n-1) ?]
    |> Enum.exists (fun (i, j) ->
         if i >= x1 && i <= x2 && j >= y1 && j <= y2 then
           not (is_black arr (i, j))
         else
           is_black arr (i, j))
    |> not


Solution (n: "%d ") (arr: array[n] of "%s ") : "%s" =
  (function | true -> "YES" | false -> "NO")
  (match (find_first arr n, find_last arr n) with
    | (Some pos1, Some pos2) ->
        check_square arr n pos1 pos2
    | _ ->
        false)

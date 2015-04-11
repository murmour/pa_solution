(* Helper library: https://bitbucket.org/cakeplus/solution *)

open Batteries (* batteries.forge.ocamlcore.org *)
open Int64.Infix


Solution (n, m: int) (x: list[n] of tuple(int64, int))
                     (y: list[m] of tuple(int64, int)) : int64 =
  let x = x |> List.mapi (fun i (v, t) -> (v, t, i)) in
  let y = y |> List.mapi (fun i (v, t) -> (v, t, i)) in
  let h = Hashtbl.create 1000 in

  let rec recurse = function
    | (_, []) | ([], _) ->
        0L
    | ((x, xt, xi) :: xs, (y, yt, yi) :: ys) ->
        match Hashtbl.find_option h (x, xt, xi, y, yt, yi) with
          | Some result ->
              result
          | None ->
              let result =
                if xt = yt then
                  if x > y then
                    y + (recurse ((x - y, xt, xi) :: xs, ys))
                  else if x < y then
                    x + (recurse (xs, (y - x, yt, yi) :: ys))
                  else
                    x + (recurse (xs, ys))
                else
                  max (recurse (xs, (y, yt, yi) :: ys))
                      (recurse ((x, xt, xi) :: xs, ys))
              in
              Hashtbl.add h (x, xt, xi, y, yt, yi) result;
              result
  in

  recurse (x, y)

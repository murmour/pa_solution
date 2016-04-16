(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


module Warshall (S: sig type t val zero: t val add: t -> t -> t end) = struct

  let calc n edges : S.t option array array =
    let dists = Array.make_matrix n n None in
    for i = 0 to n-1 do
      for j = 0 to n-1 do
        dists.(i).(j) <- if i = j then Some S.zero else edges.(i).(j)
      done
    done;
    for k = 0 to n-1 do
      for i = 0 to n-1 do
        for j = 0 to n-1 do
          match (dists.(i).(j), dists.(i).(k), dists.(k).(j)) with
            | (Some ij, Some ik, Some kj) when ij > S.add ik kj ->
                dists.(i).(j) <- Some (S.add ik kj)
            | (None, Some ik, Some kj) ->
                dists.(i).(j) <- Some (S.add ik kj)
            | _ -> ()
        done
      done
    done;
    dists

end

module WarshallI = Warshall(Int)
module WarshallF = Warshall(Float)


Solution (n, q: "%d ")
         (horses: array[n] of "%d %d ")
         (roads: array[n, n] of "%d ")
         (pairs: list[q] of "%d %d ") : list[q] of "%.7f " =
  let edges1 =
    roads |> Array.map (Array.map (function -1 -> None | d -> Some d))
  in
  let dists1 = WarshallI.calc n edges1 in
  let dists2 = Array.make_matrix n n None in
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      dists1.(i).(j) |> Option.may (fun d ->
        let (k, s) = horses.(i) in
        if d <= k then
          dists2.(i).(j) <- Some (float_of_int d /. float_of_int s));
    done;
  done;
  let dists3 = WarshallF.calc n dists2 in
  let query (u, v) = Option.get dists3.(u-1).(v-1) in
  pairs |> List.map query

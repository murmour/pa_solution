(* Helper library: https://bitbucket.org/cakeplus/solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


Solution (n: int) (vines: list[n] of tuple(int, int)) (d: int) : string =
  let vines = Array.of_list (vines @ [ (d, 0) ]) in
  let len = Array.length vines in
  let sol = Array.make len None in
  sol.(0) <- Some (fst vines.(0));

  vines |> Array.iteri (fun i (d1, l1) ->
    sol.(i) |> Option.may (fun h1 ->
      for j = i+1 to len-1 do
        let (d2, l2) = vines.(j) in
        let dist = d2 - d1 in
        if h1 >= dist then
          let new_h = min l2 dist in
          match sol.(j) with
            | Some old_h ->
                sol.(j) <- Some (max new_h old_h)
            | None ->
                sol.(j) <- Some new_h
      done));

  if sol.(len-1) <> None then
    "YES"
  else
    "NO"

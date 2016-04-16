(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


let calc_descn arr i =
  let len = Array.length arr in
  let rec iter n =
    let idx = i + n + 1 in
    if idx >= len then
      None
    else if arr.(idx) < arr.(i) then
      iter (n+1)
    else
      Some n
  in
  iter 0


Solution (e, r, n: "%d ") (arr: array[n] of "%d ") : "%d" =
  let cre = ref e in
  let gain = ref 0 in
  for i = 0 to n-1 do
    let v =
      match calc_descn arr i with
        | None ->
            !cre
        | Some d ->
            min !cre (max 0 (((d + 1) * r) + !cre - e))
    in
    gain := !gain + (arr.(i) * v);
    cre := min e (!cre - v + r);
  done;
  !gain

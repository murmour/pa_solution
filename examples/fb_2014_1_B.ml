(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries
  (* https://github.com/ocaml-batteries-team/batteries-included *)


Solution (n, k, c: "%d ") : "%d" =
  Enum.reduce min (Enum.init n (fun w ->
    let w = w + 1 in
    let pen = n - w in
    let known = k - k mod w in
    if c <= known then
      c + pen
    else
      let h = k / (w-1) in
      if h > k mod (w-1) then
        c + pen + 1
      else
        known + (w - k mod w) + (c - known) + pen))

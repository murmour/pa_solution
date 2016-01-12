(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries
  (* https://github.com/ocaml-batteries-team/batteries-included *)


let edit_distance s1 s2 =
  let len1 = String.length s1 in
  let len2 = String.length s2 in
  let rec iter i =
    if i >= len1 then
      len2 - len1
    else if i >= len2 then
      len1 - len2
    else if s1.[i] = s2.[i] then
      iter (i+1)
    else
      len1-i + len2-i
  in
  iter 0


Solution (n, k: int) (words: array[n] of string) : int =
  Array.sort compare words;

  let dp = Array.make_matrix n k max_int in
  for i = 0 to n-1 do
    dp.(i).(0) <- String.length words.(i)
  done;

  for i = 0 to n-2 do
    for j = i+1 to n-1 do
      let dist = edit_distance words.(i) words.(j) in
      for l = 1 to k-1 do
        if dp.(i).(l-1) <> max_int then
          dp.(j).(l) <- min dp.(j).(l) (dp.(i).(l-1) + dist)
      done
    done
  done;

  let result = ref max_int in
  for i = 0 to n-1 do
    if dp.(i).(k-1) <> max_int then
      result := min !result (dp.(i).(k-1) + String.length words.(i))
  done;
  !result + k

(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries
  (* https://github.com/ocaml-batteries-team/batteries-included *)


Solution (n: "%d ") (a, b: "%s ") : "%d" =
  let reps = Array.make n 0 in
  let last_ct = ref 0 in
  for i = n-1 downto 0 do
    if a.[i] = b.[i] then
      (incr last_ct;
       reps.(i) <- !last_ct)
    else
      (last_ct := 0;
       reps.(i) <- 0)
  done;

  let ct = ref 0 in

  let rec fill p_i p_j c_i c_j =
    if p_i+1 < n && b.[p_i+1] = c_i then
      fill (p_i+1) p_j c_i c_j
    else if p_j-1 >= 0 && b.[p_j-1] = c_j then
      fill p_i (p_j-1) c_i c_j
    else
      (incr ct;
       meet (p_i+1) (p_j-1))
  and meet p_i p_j =
    if p_i <= p_j && (reps.(p_i) <= p_j - p_i) then
      fill p_i p_j b.[p_i] b.[p_j]
  in
  meet 0 (n-1);

  !ct

(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries
  (* https://github.com/ocaml-batteries-team/batteries-included *)


Solution (n, k: "%d ") (p: "%f ") : "%.9f" =
  if k > n then 0.0 else begin
    let dp1 = Array.make_matrix (n+1) (n+1) 0.0 in
    for n' = 0 to n do
      for k' = 0 to n do
        if n' = 0 then
          dp1.(n').(k') <- (if k' > 0 then 0.0 else 1.0)
        else if k' = 0 then
          dp1.(n').(k') <- dp1.(n'-1).(k') *. (1.0 -. p)
        else if k' <= n' then
          dp1.(n').(k') <- (dp1.(n'-1).(k'-1) *. p) +.
                           (dp1.(n'-1).(k') *. (1.0 -. p))
      done;
    done;

    for n' = 0 to n do
      for k' = n-1 downto k do
        dp1.(n').(k') <- dp1.(n').(k') +. dp1.(n').(k'+1)
      done;
    done;

    let dp2 = Array.make (n+1) 0.0 in
    for i = 1 to n do
      for j = 0 to i-1 do
        dp2.(i) <- max dp2.(i) (dp2.(j) +. dp1.(i-j).(k))
      done;
    done;
    dp2.(n)
  end

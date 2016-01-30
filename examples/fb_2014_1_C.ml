(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries
  (* https://github.com/ocaml-batteries-team/batteries-included *)


let copy_matrix arr =
  arr |> Array.map Array.copy

let scan_input input n m =
  let arr = Array.make_matrix (n + 2) (m + 2) (-1) in
  for i = 1 to n do
    for j = 1 to m do
      if input.(i-1).[j-1] = '.' then
        arr.(i).(j) <- 0
    done
  done;
  arr.(1).(1) <- 1;
  arr

let fill mat_orig n m =
  let arr = copy_matrix mat_orig in
  let modified = ref true in
  while !modified do
    modified := false;
    for i = 1 to n do
      for j = 1 to m do
        if arr.(i).(j) >= 0 then
          let m = max arr.(i-1).(j) arr.(i).(j-1) in
          if m > arr.(i).(j) then
            (arr.(i).(j) <- m+1;
             modified := true)
      done
    done
  done;
  arr

let rec extend_left arr i j n =
  if arr.(i).(j) < 0 then
    arr.(i).(j) <- min arr.(i).(j) (1 - n)
  else
    let side = arr.(i+1).(j) in
    if side < 0 then
      extend_left arr i (j-1) (n+1)
    else if n > arr.(i).(j) && n+1 > side then
      (arr.(i+1).(j) <- n+1;
       extend_left arr i (j-1) (n+1))

let rec extend_up arr i j n =
  if arr.(i).(j) < 0 then
    arr.(i).(j) <- min arr.(i).(j) (1 - n)
  else
    let side = arr.(i).(j+1) in
    if side < 0 then
      extend_up arr (i-1) j (n+1)
    else if n > arr.(i).(j) && n+1 > side then
      (arr.(i).(j+1) <- n+1;
       extend_up arr (i-1) j (n+1))

let extend mat_orig n m =
  let arr = copy_matrix mat_orig in
  for i = 1 to n do
    for j = 1 to m do
      if arr.(i).(j) >= 0 then
        (let up = mat_orig.(i-1).(j) in
         if up > 0 then
           extend_left arr i (j-1) (up+2);

         let left = mat_orig.(i).(j-1) in
         if left > 0 then
           extend_up arr (i-1) j (left+2))
    done
  done;
  arr


Solution (n, m: int) (input: array[n] of string) : int =
  let arr = scan_input input n m in
  let filled = fill arr n m in
  let extended = extend filled n m in
  let ext_filled = fill extended n m in
  List.max [? List: abs ext_filled.(i).(j) | i <- 0--(n+1); j <- 0--(m+1) ?]

(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


let rec bisect pred ltime rtime =
  assert (ltime <= rtime);
  let pivot = (ltime + rtime) / 2 in
  match pred pivot with
    | `Found result ->
        result
    | `SolutionLeft ->
        bisect pred ltime (pivot - 1)
    | `SolutionRight ->
        bisect pred (pivot + 1) rtime


Solution (b, n: int) (barbers: array[b] of int) : int =

  let check_solution time =
    let processed =
      barbers
      |> Array.map (fun v ->
          if time mod v = 0 then time / v else time / v + 1)
      |> Array.reduce (+)
    in
    let barber_status =
      barbers |> Array.map (fun v ->
        if time mod v = 0 then 0 else v - time mod v)
    in
    let min = Array.min barber_status in
    let ready_barbers =
      barber_status
      |> Array.mapi (fun i v -> (i, v))
      |> Array.find_all (fun (i, v) -> v = min)
    in
    if (processed < n) && processed + Array.length ready_barbers >= n then
      `Found (fst ready_barbers.(n - processed - 1) + 1)
    else if processed < n then
      `SolutionRight
    else
      `SolutionLeft
  in

  let max_time = Array.max barbers * n * 2 in
  bisect check_solution 0 max_time

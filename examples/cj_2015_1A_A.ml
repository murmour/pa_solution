(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


let solve1 n plates =
  let eaten = ref 0 in
  let last = ref plates.(0) in
  for i = 1 to n-1 do
    if plates.(i) < !last then
      eaten := !eaten + !last - plates.(i);
    last := plates.(i);
  done;
  !eaten

let test_speed n plates speed =
  let eaten = ref 0 in
  let state = ref plates.(0) in
  Return.label (fun lab ->
    for i = 1 to n-1 do
      if plates.(i) >= !state - speed then
        (eaten := !eaten + min speed !state;
         state := plates.(i))
      else
        Return.return lab None
    done;
    Return.return lab (Some !eaten))

let solve2 n plates =
  0--10000
  |> List.of_enum
  |> List.filter_map (test_speed n plates)
  |> List.min


Solution (n: int) (plates: array[n] of int) : tuple(int, int) =
  (solve1 n plates, solve2 n plates)

(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)
open Stdint (* https://github.com/andrenth/ocaml-stdint *)
open Printf


let max_divisor = 7


let get_simple_divisor (n: Uint128.t) : int option =
  let open Return in
  with_label (fun l ->
    for i = 2 to max_divisor do
      let i' = Uint128.of_int i in
      if i' <> n && Uint128.(rem n i' = zero) then
        return l (Some i)
    done;
    None)


let for_each_coin n fn : unit =
  let rec iter acc n =
    if n = 0 then
      fn (1 :: acc)
    else
      (iter (0 :: acc) (n-1);
       iter (1 :: acc) (n-1));
  in
  iter [ 1 ] (n-2)


let project coin base : Uint128.t =
  let open Uint128 in
  let sum = ref zero in
  let base = of_int base in
  let multiple = ref one in
  coin |> List.iter (fun i ->
    if i = 1 then
      sum := add !sum !multiple;
    multiple := mul !multiple base);
  !sum


let print_coin coin : string =
  coin
  |> List.rev
  |> List.map (fun i -> (if i = 1 then '1' else '0'))
  |> String.of_list


Solution (n, j: "%d ") : tuple("\n", array[j] of tuple("%s ", list[9] of "%d ", "\n")) =
  let left = ref j in
  let result = DynArray.create () in

  let open Return in
  with_label (fun l ->
    for_each_coin n (fun coin ->
      let projections = [? List: project coin base | base <- 2--10 ?] in
      let divisors = projections |> List.map get_simple_divisor in
      if divisors |> List.for_all (fun d -> d <> None) then
        begin
          let divisors = divisors |> List.map Option.get in
          DynArray.add result (print_coin coin, divisors, ());
          decr left;
          if !left = 0 then return l ()
        end));

  if DynArray.length result <> j then
    printf "Not enough coins: %d\n" (DynArray.length result);
  ((), DynArray.to_array result)

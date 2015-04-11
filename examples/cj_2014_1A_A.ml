(* Helper library: https://bitbucket.org/cakeplus/solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


let count_bits (patt: int) : int =
  Enum.sum [? (patt lsr bit) land 1 | bit <- 0--40 ?]


Solution (n, _: int) (flow, req: list[n] of string) : string =
  let read_mask s = int_of_string ("0b" ^ s) in
  let flow  = flow |> List.map read_mask in
  let req   = req  |> List.map read_mask in
  let flow1 = List.hd flow in

  req |> List.map (fun r ->
           let diff = flow1 lxor r in
           let flow = flow |> List.map (fun f -> f lxor diff) in
           if Set.equal (Set.of_list flow) (Set.of_list req) then
             Some (count_bits diff)
           else
             None)

      |> List.filter_map identity
      |> function
           | [] ->
               "NOT POSSIBLE"
           | xs ->
               xs |> List.min |> string_of_int

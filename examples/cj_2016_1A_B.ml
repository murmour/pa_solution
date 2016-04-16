(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


Solution (n: "%d ") (lists: list[(n*2)-1] of list[n] of "%d ") : list[n] of "%d " =
  let ct = Array.make 2501 0 in
  lists |> List.iter (List.iter (fun i -> ct.(i) <- ct.(i) + 1));

  let result = ref [] in
  ct |> Array.iteri (fun i count ->
    if count mod 2 <> 0 then
      result := i :: !result);
  List.rev !result

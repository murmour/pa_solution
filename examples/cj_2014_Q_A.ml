(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


Solution (answer1: "%d ") (grid1: list[4, 4] of "%d ")
         (answer2: "%d ") (grid2: list[4, 4] of "%d ") : "%s" =
  let set1 = Set.of_list @@ List.nth grid1 (answer1 - 1) in
  let set2 = Set.of_list @@ List.nth grid2 (answer2 - 1) in
  match Set.elements @@ Set.intersect set1 set2 with
    | [x] -> string_of_int x
    | [ ] -> "Volunteer cheated!"
    | _   -> "Bad magician!"

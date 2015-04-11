(* Helper library: https://bitbucket.org/cakeplus/solution *)

open Batteries
  (* https://github.com/ocaml-batteries-team/batteries-included *)


Solution (input: line) : int =
  input |> String.lowercase
        |> String.to_list
        |> List.filter Char.is_letter
        |> List.sort compare
        |> List.group compare
        |> List.map List.length
        |> List.sort compare
        |> List.rev
        |> List.enum
        |> fun l -> Enum.combine (l, (26---1))
        |> Enum.map (fun (c, beauty) -> c * beauty)
        |> Enum.sum
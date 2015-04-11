(* Helper library: https://bitbucket.org/cakeplus/solution *)

open Batteries
  (* https://github.com/ocaml-batteries-team/batteries-included *)


let sample_in =
  "ejp mysljylc kd kxveddknmc re jsicpdrysi\
   rbcpc ypc rtcsra dkh wyfrepkym veddknkmkrkcd\
   de kr kd eoya kw aej tysr re ujdr lkgc jv qz"

let sample_out =
  "our language is impossible to understand\
   there are twenty six factorial possibilities\
   so it is okay if you want to just give up zq"

let mapping =
  let c1 = String.enum sample_in in
  let c2 = String.enum sample_out in
  (c1, c2) |> Enum.combine |> Map.of_enum


Solution (input: line) : string =
  input |> String.map (fun c -> Map.find c mapping)

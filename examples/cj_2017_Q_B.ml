(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


Solution (n: "%d ") : "%d" =
  let s = String.of_int n in
  for i = String.length s - 1 downto 1 do
    if s.[i] < s.[i-1] then
      (for j = i to String.length s - 1 do
         s.[j] <- '9';
       done;
       s.[i-1] <- (Char.chr (Char.code s.[i-1] - 1)));
  done;
  Int.of_string s

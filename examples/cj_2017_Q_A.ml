(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


Solution (s: "%s ") (k: "%d ") : "%s" =
  let ct = ref 0 in
  for i = 0 to String.length s - 1 - k do
    if s.[i] = '-' then
      (for j = i to i+k-1 do
         s.[j] <- if s.[j] = '-' then '+' else '-';
       done;
       incr ct)
  done;
  let last = List.of_enum ((String.length s - k)--(String.length s - 1)) in
  if last |> List.for_all (fun i -> s.[i] = '+') then
    string_of_int !ct
  else if last |> List.for_all (fun i -> s.[i] = '-') then
    string_of_int (!ct + 1)
  else
    "IMPOSSIBLE"

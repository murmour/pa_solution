(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


let rec process_string last ct = function
  | [] ->
      [ (last, ct) ]
  | x :: xs ->
      if x = last then
        process_string last (ct + 1) xs
      else
        (last, ct) :: process_string x 1 xs

let rec split_groups prev = function
  | [] -> []
  | x :: xs ->
      (x, (prev @ xs)) :: split_groups (x :: prev) xs

let solve_group ((_, hd), rest) =
  rest |> List.map (fun (_, x) -> abs (x - hd)) |> List.sum

let unify groups =
  split_groups [] groups |> List.map solve_group |> List.min


Solution (n: "%d ") (strings: list[n] of "%s ") : "%s" =
  let processed =
    strings
    |> List.map String.to_list
    |> List.map (fun s -> process_string (List.hd s) 0 s)
  in

  let rec combine l =
    if l |> List.for_all List.is_empty then 0
    else
      let heads = l |> List.map List.hd in
      let char = fst (List.hd heads) in
      if heads |> List.exists (fun (c, _) -> c <> char) then
        failwith "char"
      else
        let n = l |> List.map List.hd |> unify in
        n + combine (l |> List.map List.tl)
  in

  try
    string_of_int (combine processed)
  with Failure "tl" | Failure "hd" | Failure "char" ->
    "Fegla Won"

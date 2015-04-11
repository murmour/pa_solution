(* Helper library: https://bitbucket.org/cakeplus/solution *)

open Batteries
  (* https://github.com/ocaml-batteries-team/batteries-included *)


let scan_symbol = function
  | '(' -> `LP | ')' -> `RP | ':' -> `COLON | _   -> `REST

let rec compress = function
  | `COLON :: `LP :: xs -> `LPC :: (compress xs)
  | `COLON :: `RP :: xs -> `RPC :: (compress xs)
  | `COLON :: xs -> compress xs
  | `LP :: xs -> `LP :: (compress xs)
  | `RP :: xs -> `RP :: (compress xs)
  | x :: xs -> compress xs
  | [] -> []

let rec remove item acc = function
  | [] ->
      None
  | x :: xs when x = item ->
      Some (List.rev acc @ xs)
  | x :: xs ->
      remove item (x :: acc) xs

let rec check n = function
  | `LP :: xs ->
      (match remove `RP [] xs with
        | Some xs ->
            check n xs
        | None ->
            (match remove `RPC [] xs with
              | Some xs ->
                  check n xs
              | None ->
                  false))

  | `RP  :: xs when n > 0 -> check (n - 1) xs
  | `LPC :: xs -> check (n + 1) xs
  | `RPC :: xs -> check n xs
  | [] -> true
  | _ -> false


Solution (input: line) : string =
  let result =
    input |> String.to_list
          |> List.map scan_symbol
          |> compress
          |> check 0
  in
  if result then "YES" else "NO"

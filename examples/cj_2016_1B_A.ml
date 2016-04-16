(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


type lset = int array


let calc_lset s : lset =
  let letters = Array.make 26 0 in
  s |> String.iter (fun c ->
    let code = Char.code c - 65 in
    letters.(code) <- letters.(code) + 1);
  letters

let sub_lset set1 set2 : lset option =
  let set3 = Array.copy set1 in
  for i = 0 to 25 do
    set3.(i) <- set1.(i) - set2.(i);
  done;
  if set3 |> Array.exists (fun v -> v < 0) then
    None
  else
    Some set3

let digits =
  [
    ("ZERO", 0);
    ("TWO", 2);
    ("FOUR", 4);
    ("SIX", 6);
    ("EIGHT", 8);
    ("ONE", 1); (* depends on 0, 2, 4 (letter O) *)
    ("THREE", 3); (* depends on 0, 4 (letter R) *)
    ("FIVE", 5); (* depends on 4 (letter F) *)
    ("SEVEN", 7); (* depends on 6 (letter S) *)
    ("NINE", 9); (* depends on 1, 7 (letter N) *)
  ]


Solution (s: "%s ") : "%s" =
  let start = ref (calc_lset s) in
  let number = Array.make 10 0 in

  digits |> List.iter (fun (s, d) ->
    let dset = calc_lset s in
    let rec iter () =
      sub_lset !start dset |> Option.may (fun start' ->
        number.(d) <- number.(d) + 1;
        start := start';
        iter ())
    in
    iter ());

  let b = Buffer.create 10 in
  number |> Array.iteri (fun d ct ->
    for i = 1 to ct do
      Buffer.add_char b (Char.chr (48 + d))
    done);
  Buffer.contents b

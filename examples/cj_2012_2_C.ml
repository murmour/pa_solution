(* Helper library: https://bitbucket.org/cakeplus/solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


let base = 1_000_000_000


Solution (n: int) (peaks: array[n-1] of int) : string =
  let n = n - 1 in
  let peaks = peaks |> Array.map pred in
  let sol = Array.make (n + 1) None in

  let rec find_basis idx =
    sol.(idx) <- Some base;
    if idx <> n then
      find_basis peaks.(idx)
  in

  let rec get_streak start i =
    match sol.(i) with
      | Some s ->
          (s / (i - start), i)
      | None ->
          get_streak start peaks.(i)
  in

  let rec fill_streak i stop step =
    if sol.(i) = None then
      (sol.(i) <- Some ((Option.get sol.(stop)) - (step * (stop - i)));
       fill_streak peaks.(i) stop step)
  in

  Return.label (fun lab ->

    peaks |> Array.iteri (fun h1 h2 ->
      for i = h1 + 1 to h2 - 1 do
        if peaks.(i) > h2 then
          Return.return lab "Impossible"
      done);

    find_basis 0;

    for i = 0 to n do
      if sol.(i) = None then
        let (step, stop) = get_streak i peaks.(i) in
        fill_streak i stop step
    done;

    sol |> Array.map Option.get
        |> Array.map string_of_int
        |> Array.to_list
        |> String.concat " ")

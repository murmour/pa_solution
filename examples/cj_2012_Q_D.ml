(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


let flipped c max =
  let c2 = if c < 0 then -c - 1 else c in
  if (c2 / max) mod 2 = 1 then
    max - (c2 mod max) - 1
  else
    c2 mod max

let read_cell map h w y x =
  map.(flipped y h).(flipped x w)

let distance (y1, x1) (y2, x2) =
  sqrt (float_of_int (Int.pow (x1-x2) 2 + Int.pow (y1-y2) 2))

let rec gcd a = function
  | 0 -> a
  | b -> gcd b (a mod b)

let simplify (a, b) =
  let g = abs (gcd a b) in
  (a / g, b / g)

let slope (y1, x1) (y2, x2) =
  simplify (y1 - y2, x1 - x2)


Solution (h, w, d: int) (map: array[h] of string) : int =
  let h = h-2 and w = w-2 in
  let mat = Array.make_matrix h w `Empty in
  let center = ref (0, 0) in

  for y = 0 to h-1 do
    for x = 0 to w-1 do
      if map.(y+1).[x+1] = 'X' then
        begin
          mat.(y).(x) <- `Me;
          center := (y, x)
        end
    done
  done;

  let center = !center in
  let limit = 100 in
  let d = float_of_int d in

  [? (y, x) |
      y <- -limit--limit;
      x <- -limit--limit;
      (y, x) <> center;
      distance center (y, x) <= d;
      read_cell mat h w y x = `Me ?]
  |> Enum.map (slope center)
  |> List.of_enum
  |> List.unique
  |> List.length

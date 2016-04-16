(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries
  (* https://github.com/ocaml-batteries-team/batteries-included *)


type tree =
  { l: int; r: int; contents: node }

and node =
  | Range of int * tree * tree
  | Fill of int


let rec add_range x1 x2 tree =
  if x1 >= x2 then tree else
    let update_range mid_x t1 t2 =
      let t1' = add_range x1 (min x2 mid_x) t1 in
      let t2' = add_range (max x1 mid_x) x2 t2 in
      { tree with contents = Range (mid_x, t1', t2') }
    in
    match tree.contents with
      | Range (mid_x, t1, t2) ->
          update_range mid_x t1 t2
      | Fill ct ->
          if x1 = tree.l && x2 = tree.r then
            { tree with contents = Fill (ct + 1) }
          else
            let mid_x = tree.l + (tree.r - tree.l) / 2 in
            update_range mid_x
              { l = tree.l; r = mid_x; contents = Fill ct }
              { l = mid_x; r = tree.r; contents = Fill ct }

let rec rem_range x1 x2 tree =
  if x1 >= x2 then tree else
    let contents =
      match tree.contents with
        | Fill 0 ->
            Fill 0
        | Fill ct ->
            Fill (ct-1)
        | Range (mid_x, t1, t2) ->
            let t1' = rem_range x1 (min x2 mid_x) t1 in
            let t2' = rem_range (max x1 mid_x) x2 t2 in
            Range (mid_x, t1', t2')
    in
    { tree with contents }

let rec calc_range t =
  match t.contents with
    | Fill 0 ->
        0
    | Fill _ ->
        t.r - t.l
    | Range (_, t1, t2) ->
        calc_range t1 + calc_range t2


Solution (w, h, p, q, n, x, y, a, b, c, d: "%d ") : "%d" =
  let points = Array.make n (x, y) in
  for i = 1 to n-1 do
    let (x, y) = points.(i-1) in
    let x' = (x * a + y * b + 1) mod w in
    let y' = (x * c + y * d + 1) mod h in
    points.(i) <- (x', y')
  done;

  let events = Array.make w [] in
  let add_event i ev = events.(i) <- ev :: events.(i) in

  if q > 1 then
    (let x1 = 0 in
     add_event x1 (`Add (max 0 (h - q + 1), h)));

  if p > 1 then
    (let x2 = max 0 (w - p + 1) in
     add_event x2 (`Add (0, h)));

  points |> Array.sort compare;
  points |> Array.iter (fun (x, y) ->
    let x1 = max 0 (x + 1 - p) and x2 = x + 1 in
    let y1 = max 0 (y + 1 - q) and y2 = y + 1 in
    add_event x1 (`Add (y1, y2));
    if x2 < w then
      add_event x2 (`Rem (y1, y2)));

  let sum = ref 0 in
  let map = ref { l = 0; r = h; contents = Fill 0 } in
  events |> Array.iter (fun ev ->
    ev |> List.iter (function
      | `Add (y1, y2) ->
          map := add_range y1 y2 !map
      | `Rem (y1, y2) ->
          map := rem_range y1 y2 !map);
    sum := !sum + (calc_range !map));
  (w * h) - !sum

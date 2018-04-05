(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries
  (* https://github.com/ocaml-batteries-team/batteries-included *)


let rot (x, y) rad =
  let x' = x*.(cos rad) -. y*.(sin rad) in
  let y' = x*.(sin rad) +. y*.(cos rad) in
  (x', y')

let vec (ax, ay) (bx, by) =
  (bx -. ax, by -. ay)

let cross (ax, ay) (bx, by) : float =
  (ax*.by) -. (ay*.bx)

let distance (ax, ay) (bx, by) : float =
  let dx = ax -. bx in
  let dy = ay -. by in
  sqrt (dx*.dx +. dy*.dy)


Solution (p, x, y: "%d ") : "%s" =
  let o = (50.0, 50.0) in
  let v = (float_of_int x, float_of_int y) in
  if distance o v > 50.0 then
    "white"
  else
    let rad = ((Float.pi *. 2.0) /. 100.0) *. float_of_int p in
    let a = (50.0, 100.0) in
    let b =
      let (x, y) = rot (0.0, 50.0) (-.rad) in
      (x +. 50.0, y +. 50.0)
    in
    let c1 = cross (vec o v) (vec o a) in
    let c2 = cross (vec o b) (vec o v) in
    if ((p <= 50) && (c1 >= 0.0 && c2 >= 0.0))
    || ((p >= 50) && (not (c1 < 0.0 && c2 < 0.0))) then
      "black"
    else
      "white"

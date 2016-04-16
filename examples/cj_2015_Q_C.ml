(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


type sign = Plus | Minus
type quaternion = I | J | K | O


let mult sign q1 q2 : (quaternion * sign) =
  let (q3, sign') =
    match (q1, q2) with
      | (I, I) -> (O, Minus)
      | (I, J) -> (K, Plus)
      | (I, K) -> (J, Minus)
      | (I, O) -> (I, Plus)
      | (J, I) -> (K, Minus)
      | (J, J) -> (O, Minus)
      | (J, K) -> (I, Plus)
      | (J, O) -> (J, Plus)
      | (K, I) -> (J, Plus)
      | (K, J) -> (I, Minus)
      | (K, K) -> (O, Minus)
      | (K, O) -> (K, Plus)
      | (O, I) -> (I, Plus)
      | (O, J) -> (J, Plus)
      | (O, K) -> (K, Plus)
      | (O, O) -> (O, Plus)
  in
  (q3, if sign = sign' then Plus else Minus)

let rec reduces_to_k sign = function
  | [] ->
      false
  | q :: [] ->
      q = K && sign = Plus
  | q1 :: q2 :: rest ->
      let (q3, sign') = mult sign q1 q2 in
      reduces_to_k sign' (q3 :: rest)

let rec reduces_to_jk sign = function
  | [] ->
      false
  | _ :: [] ->
      false
  | J :: rest when sign = Plus ->
      reduces_to_k Plus rest
  | q1 :: q2 :: rest ->
      let (q3, sign') = mult sign q1 q2 in
      reduces_to_jk sign' (q3 :: rest)

let rec reduces_to_ijk sign = function
  | [] ->
      false
  | _ :: [] ->
      false
  | I :: rest when sign = Plus ->
      reduces_to_jk Plus rest
  | q1 :: q2 :: rest ->
      let (q3, sign') = mult sign q1 q2 in
      reduces_to_ijk sign' (q3 :: rest)


Solution (l, x: "%d ") (spelling: list[l] of "%c ") : "%s" =
  let quats = spelling |> List.map
    (function 'i' -> I | 'j' -> J | 'k' -> K | _ -> assert false)
  in
  let x = if x > 12 then 8 + x mod 4 else x in
  let xquats = List.concat (List.init x (fun _ -> quats)) in
  if reduces_to_ijk Plus xquats then "YES" else "NO"

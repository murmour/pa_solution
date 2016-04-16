(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


exception Impossible


let solve_small r y b : string =
  let n = r + y + b in
  let ponies =
    [ (r, `R); (y, `Y); (b, `B) ]
    |> List.sort (fun (ct1, c1) (ct2, c2) -> compare ct2 ct1)
    |> List.map (fun (ct, c) -> List.make ct c)
    |> List.concat
    |> List.enum
    |> Queue.of_enum
  in

  let stalls = Array.make n None in
  let put_pony i : unit =
    let pony = Queue.pop ponies in
    let (prev, next) =
      if i = 0 then
        (n-1, 1)
      else if i = n-1 then
        (n-2, 0)
      else
        (i-1, i+1)
    in
    if stalls.(prev) <> Some pony && stalls.(next) <> Some pony then
      stalls.(i) <- Some pony
  in

  if n = 1 then
    stalls.(0) <- Some (Queue.pop ponies)
  else begin
    for i = 0 to n-1 do
      if i mod 2 = 0 then
        put_pony i
    done;
    for i = 0 to n-1 do
      if i mod 2 <> 0 then
        put_pony i
    done;
  end;

  let print_stall = function
    | Some `R -> 'R'
    | Some `Y -> 'Y'
    | Some `B -> 'B'
    | None ->
        raise Impossible
  in
  stalls |> Array.map print_stall |> Array.to_list |> String.of_list


let solve_large n r o y g b v : string =
  let special c1 c2 tag =
    if c1 = c2 then
      String.repeat tag c1
    else
      raise Impossible
  in
  if g + r = n then special g r "GR" else
  if v + y = n then special v y "VY" else
  if o + b = n then special o b "OB" else
    let r = r - g in
    let y = y - v in
    let b = b - o in
    if r < 0 || y < 0 || b < 0 then
      raise Impossible;
    let subst var pat sub str =
      if var = 0 then
        str
      else
        let (success, res') = String.replace ~str ~sub:pat ~by:sub in
        if success then res' else raise Impossible
    in
    solve_small r y b
    |> subst g "R" ("R" ^ String.repeat "GR" g)
    |> subst v "Y" ("Y" ^ String.repeat "VY" v)
    |> subst o "B" ("B" ^ String.repeat "OB" o)


Solution (n, r, o, y, g, b, v: "%d ") : "%s" =
  try
    solve_large n r o y g b v
  with Impossible ->
    "IMPOSSIBLE"

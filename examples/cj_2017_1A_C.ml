(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


Solution (hd, ad, hk, ak, b, d: "%d ") : "%s" =
  let rec simulate b_ct d_ct hd' ad' hk' ak' ct =
    let heal () =
      if hd <= ak' * 2 then
        None
      else
        simulate b_ct d_ct (hd - ak') ad' hk' ak' (ct+1)
    in
    if d_ct > 0 then
      if hd' <= ak' - d then
        heal ()
      else
        let ak'' = max (ak' - d) 0 in
        simulate b_ct (d_ct-1) (hd' - ak'') ad' hk' ak'' (ct+1)

    else if b_ct > 0 then
      if hd' <= ak' then
        heal ()
      else
        simulate (b_ct-1) d_ct (hd' - ak') (ad' + b) hk' ak' (ct+1)

    else
      if hk' <= ad' then
        Some (ct+1)
      else if hd' <= ak' then
        heal ()
      else
        simulate b_ct d_ct (hd' - ak') ad' (hk' - ad') ak' (ct+1)
  in
  let sol =
    [? simulate b_ct d_ct hd ad hk ak 0 | b_ct <- 0--100; d_ct <- 0--100 ?]
    |> Enum.reduce (fun s1 s2 ->
         match (s1, s2) with
           | (None, s) -> s
           | (s, None) -> s
           | (Some s1, Some s2) -> Some (min s1 s2))
  in
  match sol with
    | None ->
        "IMPOSSIBLE"
    | Some s ->
        string_of_int s

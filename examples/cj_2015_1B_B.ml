(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


let solve_linear c n =
  let happy = if c mod 2 = 0 then c / 2 else c / 2 + 1 in
  let unhappy = max 0 (n - happy) in
  max 0 (unhappy * 2 - 1 + c mod 2)


Solution (r, c, n: int) : int =
  if r = 1 then
    solve_linear c n
  else if c = 1 then
    solve_linear r n
  else
    let calc_unhappy happy badcorners badsides side_cost =
      let unhappy = max 0 (n - happy) in
      if unhappy = 0 then 0 else
        let unhappy_corners = min badcorners unhappy in
        let unhappy_sides = min badsides (max 0 (unhappy - badcorners)) in
        let unhappy_centers = max 0 (unhappy - badcorners - badsides) in
        unhappy_corners * 2 + unhappy_sides * side_cost + unhappy_centers * 4
    in
    let badcorners = 2 in
    let badsides = ((c - 1) / 2) * 2 + ((r - 1) / 2) * 2 in
    match (c mod 2 = 0, r mod 2 = 0) with
      | (true, true) ->
          let happy = c / 2 * r in
          calc_unhappy happy badcorners badsides 3
      | (true, false) ->
          let happy = c / 2 * r in
          calc_unhappy happy badcorners (badsides-1) 3
      | (false, true) ->
          let happy = r / 2 * c in
          calc_unhappy happy badcorners (badsides-1) 3
      | (false, false) when c = 3 && r = 3 ->
          let badcorners = 0 in
          min
            (calc_unhappy 4 badcorners badsides 2)
            (calc_unhappy 5 badcorners badsides 3)
      | (false, false) ->
          let happy = (c - 1) / 2 * (r - 1) + (r + c - 1) / 2 + 1 in
          min
            (calc_unhappy happy 0 badsides 3)
            (calc_unhappy (happy-1) 4 (badsides-4) 3)

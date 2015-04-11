(* Helper library: https://bitbucket.org/cakeplus/solution *)

open Batteries
  (* https://github.com/ocaml-batteries-team/batteries-included *)


let pinc p i =
  let p' = p +. i in
  if p' > 1.0 then 1.0 else p'

let pdec p i =
  let p' = p -. i in
  if p' < 0.0 then 0.0 else p'

let memoize fn =
  let h = Hashtbl.create 1000 in
  let rec fn' arg =
    match Hashtbl.find_option h arg with
      | Some res -> res
      | None ->
          let res = fn fn' arg in
          Hashtbl.add h arg res;
          res
  in
  fn'

Solution (t: int) (ps, pr, pi, pu, pw, pd, pl: float) : float =
  (pi, 0, 0) |> memoize (fun calc (p, w, l) ->
     if w = t then 1.0 else
       if l = t then 0.0 else
         let sunny = p in
         let rainy = (1. -. sunny) in
         let win_sunny = sunny *. ps in
         let win_rainy = rainy *. pr in
         let lose_sunny = sunny *. (1. -. ps) in
         let lose_rainy = rainy *. (1. -. pr) in
         let pw' = 1. -. pw and pl' = 1. -. pl in
         List.fsum
           [
             win_sunny  *. pw  *. calc (pinc p pu, w + 1, l);
             win_sunny  *. pw' *. calc (p, w + 1, l);
             win_rainy  *. pw  *. calc (pinc p pu, w + 1, l);
             win_rainy  *. pw' *. calc (p, w + 1, l);
             lose_sunny *. pl  *. calc (pdec p pd, w, l + 1);
             lose_sunny *. pl' *. calc (p, w, l + 1);
             lose_rainy *. pl  *. calc (pdec p pd, w, l + 1);
             lose_rainy *. pl' *. calc (p, w, l + 1);
           ])

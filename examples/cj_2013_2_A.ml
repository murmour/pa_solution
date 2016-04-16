(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


let modulo i =
  i mod 1000002013

let calc_cost n len ct =
  modulo (ct * (modulo ((2 * n - len + 1) * len / 2)))


Solution (n, m: "%d ") (arr: list[m] of "%d %d %d ") : "%d" =
  let events = RefList.empty () in
  arr |> List.iter (fun (o, e, p) ->
    RefList.add events (`Start, o, p);
    RefList.add events (`Stop, e, p));

  RefList.sort ~cmp:(fun (c1, p1, _) (c2, p2, _) ->
    match (c1, c2) with
      | `Stop, `Start when p1 = p2 -> 1
      | _ -> compare p1 p2) events;

  let orig_cost = ref 0 in
  arr |> List.iter (fun (o, e, p) ->
    orig_cost := modulo (!orig_cost + calc_cost n (e - o) p));

  let cost = ref 0 in
  let rest = Stack.create () in
  events |> RefList.iter (function
    | (`Start, pos, ct) ->
        Stack.push (pos, ct) rest

    | (`Stop, pos, ct) ->
        let ct = ref ct in
        while !ct > 0 do
          let (top_pos, top_ct) = Stack.pop rest in
          let pos_diff = pos - top_pos in
          if !ct >= top_ct then
            (cost := modulo (!cost + calc_cost n pos_diff top_ct);
             ct := !ct - top_ct)
          else
            (cost := modulo (!cost + calc_cost n pos_diff !ct);
             Stack.push (top_pos, top_ct - !ct) rest;
             ct := 0)
        done);

  modulo (!orig_cost - !cost + 1000002013)

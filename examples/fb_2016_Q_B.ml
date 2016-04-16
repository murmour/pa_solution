(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries
  (* https://github.com/ocaml-batteries-team/batteries-included *)


let update_state s c =
  if c = 'X' then
    `Building
  else if s = `Building && c = '.' then
    `Start
  else if s = `SafeHall then
    `SafeHall
  else
    `Hall


Solution (n: "%d ") (row1: "%s ") (row2: "%s ") : "%d" =
  let count = ref 0 in
  let row1 = row1 ^ "X" in
  let row2 = row2 ^ "X" in
  let s1 = ref `Building in
  let s2 = ref `Building in

  for i = 0 to n-1 do
    s1 := update_state !s1 row1.[i];
    s2 := update_state !s2 row2.[i];

    match (!s1, !s2, row1.[i+1], row2.[i+1]) with
      | (`Hall, `Hall, 'X', 'X') ->
          count := !count + 2
      | ((`Start | `Hall), `Start, _, 'X') ->
          s1 := `SafeHall;
          incr count
      | (`Start, (`Start | `Hall), 'X', _) ->
          s2 := `SafeHall;
          incr count
      | ((`Start | `Hall), _, 'X', _) ->
          incr count
      | (_, (`Start | `Hall), _, 'X') ->
          incr count
      | _ ->
          ()
  done;

  !count

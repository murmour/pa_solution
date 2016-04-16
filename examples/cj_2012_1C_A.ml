(* Helper library: https://bitbucket.org/cakeplus/solution *)

open Batteries
  (* https://github.com/ocaml-batteries-team/batteries-included *)


Solution (n: "%d ") (g: list[n] of let m: "%d " in list[m] of "%d ") : "%s" =
  let h = Hashtbl.create 1010 in
  g |> List.iteri (fun i c ->
    c |> List.iter (fun inh ->
      Hashtbl.add h (i+1) inh));

  Return.label (fun lab ->
    g |> List.iteri (fun i c ->
      let m = Array.create 1010 false in
      let rec recurse c2 =
        if m.(c2) then
          Return.return lab "Yes"
        else
          (m.(c2) <- true;
           Hashtbl.find_all h c2 |> List.iter recurse)
      in
      recurse (i+1));
    "No")

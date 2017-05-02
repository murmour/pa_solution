(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries
  (* https://github.com/ocaml-batteries-team/batteries-included *)


type edge = int * string

type vertex =
  {
    label: string;
    mutable edges: edge list;
    mutable pair: edge option;
  }

exception Path_extended


let unify s1 s2 =
  try
    Some (Enum.combine (String.enum s1, String.enum s2)
          |> Enum.map (function
              | ('?', '?') -> 'a'
              | ('?', b) -> b
              | (a, '?') -> a
              | (a, b) -> if a = b then a else raise Not_found)
          |> String.of_enum)
  with Not_found ->
    None

let compare_edges (_, s1) (_, s2) =
  -(compare s1 s2)


Solution (m: "%d ") (k1, k2: "%s ") : "%s" =
  let n = String.length k1 in
  let l = n / m in
  let c = m * 2 in

  (* Adding vertices *)
  let g = Array.make c { label = ""; edges = []; pair = None } in
  for i = 0 to m-1 do
    g.(i) <- { label = String.sub k1 (i*l) l; edges = []; pair = None };
    g.(i + m) <- { label = String.sub k2 (i*l) l; edges = []; pair = None };
  done;

  (* Adding edges *)
  for i = 0 to m-1 do
    for j = m to c-1 do
      unify g.(i).label g.(j).label |> Option.may (fun s ->
        g.(i).edges <- (j, s) :: g.(i).edges;
        g.(j).edges <- (i, s) :: g.(j).edges)
    done
  done;

  (* Sorting edges *)
  g |> Array.iter (fun v ->
    v.edges <- List.sort compare_edges v.edges);

  let extend_path i j s =
    g.(j).pair <- Some (i, s);
    g.(i).pair <- Some (j, s);
    raise Path_extended
  in

  let rec kuhn i visited =
    if visited.(i) then
      false
    else
      begin
        visited.(i) <- true;
        try
          g.(i).edges |> List.iter (fun (j, s) ->
            (match g.(j).pair with
              | None ->
                  extend_path i j s
              | Some (j2, _) ->
                  if kuhn j2 visited then
                    extend_path i j s));
          false
        with Path_extended ->
          true
      end
  in

  (* Matching *)
  for i = 0 to m-1 do
    ignore (kuhn i (Array.make c false))
  done;

  (* Building the key *)
  try
    let b = Buffer.create 100 in
    for i = 0 to m-1 do
      match g.(i).pair with
        | Some (_, s) ->
            Buffer.add_string b s
        | None ->
            raise Not_found
    done;
    Buffer.contents b
  with Not_found ->
    "IMPOSSIBLE"

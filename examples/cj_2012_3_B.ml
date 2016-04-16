(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries
  (* https://github.com/ocaml-batteries-team/batteries-included *)


exception Found of (int * bool * bool * bool)

type info =
  {
    corners: int array;
    edges: int array;
  }


let print_result s =
  match s with
    | None ->
        "none"
    | Some (m, b, f, r) ->
        [ (b, "bridge"); (f, "fork"); (r, "ring") ]
        |> List.filter_map (function (true, s) -> Some s | _ -> None)
        |> String.concat "-"
        |> fun str -> Printf.sprintf "%s in move %d" str m

let merge_info a b =
  for i = 0 to 5 do
    a.corners.(i) <- max a.corners.(i) b.corners.(i);
    a.edges.(i) <- max a.edges.(i) b.edges.(i)
  done

let is_ring chk =
  let rec rotate rhs = function
    | [] ->
        None
    | None :: xs ->
        Some (xs @ List.rev rhs)
    | x :: xs ->
        rotate (x :: rhs) xs
  in
  let rec find_ring chk = function
    | [] ->
        false
    | None :: xs ->
        find_ring chk xs
    | Some a :: xs ->
        if List.mem a chk then true else
          find_ring (a :: chk) (List.drop_while ((=) (Some a)) xs)
  in
  match rotate [] chk with
    | None ->
        false
    | Some xs ->
        find_ring [] xs


Solution (s, m: "%d ") (stones: list[m] of "%d %d ") : "%s" =
  let bound = s*2 - 1 in
  let v = Array.make_matrix (bound+1) (bound+1) None in
  let h = Hashtbl.create 1000 in

  let is_corner = function
    | (1, 1) -> Some 0
    | (x, 1) when x = s -> Some 1
    | (1, y) when y = s -> Some 2
    | (x, y) when x = bound && y = s -> Some 3
    | (x, y) when x = bound && y = bound -> Some 4
    | (x, y) when x = s && y = bound -> Some 5
    | _ -> None
  in

  let is_edge = function
    | (1, _) -> Some 0
    | (_, 1) -> Some 1
    | (x, _) when x = bound -> Some 2
    | (_, y) when y = bound -> Some 3
    | (x, y) when x-y = s-1 -> Some 4
    | (x, y) when y-x = s-1 -> Some 5
    | _ -> None
  in

  let get_crown (x, y) =
    List.map (fun (x, y) -> try v.(x).(y) with Invalid_argument _ -> None)
      [ (x-1, y-1); (x, y-1); (x+1, y); (x+1, y+1); (x, y+1); (x-1, y) ]
  in

  let rec dereference a =
    match Hashtbl.find h a with
      | `Direct i -> a
      | `Indirect b -> dereference b
  in

  let rec set_links x a =
    if x <> a then
      let old = Hashtbl.find h a in
      Hashtbl.replace h a (`Indirect x);
      match old with
        | `Direct _ -> ()
        | `Indirect b -> set_links x b
  in

  let process_move move (x, y) =
    let info =
      {
        corners = Array.make 6 0;
        edges = Array.make 6 0;
      }
    in

    (* Собираем информацию об узле *)
    begin
      match is_corner (x, y) with
        | Some i ->
            info.corners.(i) <- 1
        | None ->
            match is_edge (x, y) with
              | Some i ->
                  info.edges.(i) <- 1
              | None -> ()
    end;

    let crown = get_crown (x, y) in
    match List.filter_map identity crown with
      | [] ->
          let idx = unique () in
          v.(x).(y) <- Some idx;
          Hashtbl.add h idx (`Direct info)

      | xs ->
          (* Дереференс ссылок *)
          let near = crown |> List.map (Option.map dereference) in

          (* Объединение информации *)
          let near' = near |> List.filter_map identity in
          near' |> List.iter (fun i ->
            match Hashtbl.find h i with
              | `Direct d -> merge_info info d | _ -> ());

          (* Обновление ссылок *)
          let a = List.hd near' in
          Hashtbl.replace h a (`Direct info);
          xs |> List.iter (set_links a);
          v.(x).(y) <- Some a;

          let bdge = Array.reduce (+) info.corners >= 2 in
          let fork = Array.reduce (+) info.edges >= 3 in
          let ring = is_ring near in

          if bdge || fork || ring then
            raise (Found (move+1, bdge, fork, ring))
  in

  print_result
    (try
       stones |> List.iteri process_move;
       None
     with Found sol ->
       Some sol)

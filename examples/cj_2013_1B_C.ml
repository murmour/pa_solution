(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries (* batteries.forge.ocamlcore.org *)


let dict =
  [? Array: lines | lines <- File.lines_of "garbled_email_dictionary.txt" ?]


Solution (s: "%s@\n") : "%d" =
  let len = String.length s in
  let dp = Array.make_matrix (len+1) 5 10000 in

  let matches i edge word =
    let wlen = String.length word in
    if len < i + wlen then None else
      let garb = ref 0 in
      let edge = ref edge in
      Return.label (fun lab ->
        for x = 0 to wlen - 1 do
          if s.[i+x] <> word.[x] then
            (if !edge <= 0 then
               (incr garb; edge := 4)
             else
               Return.return lab None)
          else
            decr edge
        done;
        Some ((max !edge 0), !garb))
  in

  let rec get_matching (i, edge, garb) =
    if dp.(i).(edge) > garb then
      begin
        dp.(i).(edge) <- garb;
        dict |> Array.iter (fun word ->
          matches i edge word |> Option.may (fun (e2, g2) ->
            let i = i + String.length word in
            let g = garb + g2 in
            if i = len then
              dp.(i).(e2) <- min (dp.(i).(e2)) g
            else
              get_matching (i, e2, g)))
      end
  in
  get_matching (0, 0, 0);

  List.min [? List: dp.(len).(i) | i <- 0--4 ?]


(* Pa_solution, a DSL for solving programming contest problems
   -----------------------------------------------------------------------------
   Copyright (C) 2013-2015, Max Mouratov (mmouratov@gmail.com)

   License:
     This library is free software; you can redistribute it and/or
     modify it under the terms of the GNU Library General Public
     License version 2.1, as published by the Free Software Foundation.

     This library is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

     See the GNU Library General Public License version 2.1 for more details
     (enclosed in the file LICENSE.txt).

   Description:
     Pa_solution is a compiled DSL for generating the IO boilerplate code
     that is necessary for solving problems in programming contests such as
     Google Code Jam and Facebook Hacker Cup.

     See README.rst for more information.
*)


open Camlp4.PreCast


type spec =
  | Int
  | Int64
  | Float
  | String
  | Char
  | Line
  | Empty
  | List of Ast.expr * spec         (* list[expr] of spec *)
  | Array of Ast.expr * spec        (* array[expr] of spec *)
  | Tuple of spec list              (* (spec, spec, ...) *)
  | Let of Ast.patt * spec * spec   (* let patt = spec in spec *)
  | Expr of Ast.expr


(* Code generation utilities
   -------------------------------------------------------------------------- *)

let _loc = Loc.ghost

let gensym =
  let id = ref 0 in
  fun () ->
    incr id;
    Printf.sprintf "_#%d" !id

let identity =
  let x = gensym () in
  <:expr< fun $lid:(x)$ -> $lid:(x)$ >>


(* Reader
   -------------------------------------------------------------------------- *)

let in_ch =
  gensym ()

let scan format =
  <:expr< Q.Scanf.bscanf $lid:(in_ch)$ $str:(format)$ $(identity)$ >>

let rec compile_reader (s: spec) : Ast.expr =
  match s with
    | Int    -> scan "%d "
    | Int64  -> scan "%Ld "
    | Float  -> scan "%f "
    | String -> scan "%s "
    | Char   -> scan "%c "
    | Line   -> scan "%[^\n]\n"

    | Empty ->
        <:expr< try $(compile_reader Line)$ with _ -> "" >>

    | List (size, s) ->
        <:expr< Q.Array.to_list $(compile_reader (Array (size, s)))$ >>

    | Array (size, s) ->
        <:expr< Q.Array.init $(size)$ (fun _ -> $(compile_reader s)$) >>

    | Tuple specs ->
        let l = specs |> List.map (fun s -> (gensym (), s)) in
        let rec build = function
          | (id, s) :: xs ->
              <:expr<
                let $lid:(id)$ = $(compile_reader s)$ in
                $(build xs)$
              >>
          | [] ->
              let es = l |> List.map (fun (id, r) -> <:expr< $lid:(id)$ >>) in
              <:expr< $tup:(Ast.exCom_of_list es)$ >>
        in
        build l

    | Let (patt, s1, s2) ->
        <:expr<
          let $(patt)$ = $(compile_reader s1)$ in
          $(compile_reader s2)$
        >>

    | Expr v -> v


(* Writer
   -------------------------------------------------------------------------- *)

let out_ch =
  gensym ()

let print (v: Ast.expr) format =
  <:expr< Q.Printf.fprintf $lid:(out_ch)$ $str:(format)$ $(v)$ >>

let rec compile_writer (s: spec) (v: Ast.expr) : Ast.expr =
  match s with
    | Int    -> print v "%d "
    | Int64  -> print v "%Ld "
    | Float  -> print v "%f "
    | String -> print v "%s "
    | Char   -> print v "%c "
    | Line   -> print v "%s\n"
    | Empty  -> print v "\n"

    | List (size, s) ->
        let id = gensym () in
        let writer = compile_writer s <:expr< $lid:(id)$ >> in
        <:expr< Q.List.iter (fun $lid:(id)$ -> $(writer)$) $(v)$ >>

    | Array (size, s) ->
        let id = gensym () in
        let writer = compile_writer s <:expr< $lid:(id)$ >> in
        <:expr< Q.Array.iter (fun $lid:(id)$ -> $(writer)$) $(v)$ >>

    | Tuple specs ->
        let l = specs |> List.map (fun r -> (gensym (), r)) in
        let item_patts =
          l |> List.map (fun (id, r) ->
            <:patt< $lid:(id)$ >>)
        in
        let item_writers =
          l |> List.map (fun (id, r) ->
            compile_writer r <:expr< $lid:(id)$ >>)
        in
        <:expr<
          let $tup:(Ast.paCom_of_list item_patts)$ = $(v)$ in
          do { $(Ast.exSem_of_list item_writers)$ }
        >>

    | Let (let_id, s1, s2) ->
        compile_writer s2 v

    | Expr _ -> v


(* The compiler
   -------------------------------------------------------------------------- *)

let compile_solution in_spec out_spec (body: Ast.expr) : Ast.str_item =

  let rec wrap_body = function
    | (patt, spec) :: xs ->
        <:expr<
          let $(patt)$ = $(compile_reader spec)$ in
          $(wrap_body xs)$
        >>
    | [] ->
        compile_writer out_spec body
  in

  <:str_item<
    let $lid:(in_ch)$ = Q.Scanf.Scanning.open_in (Q.Sys.argv.(1) ^ ".in") in
    let $lid:(out_ch)$ = Q.Pervasives.open_out (Q.Sys.argv.(1) ^ ".out") in
    do {
      for _i = 1 to Q.Scanf.bscanf $lid:(in_ch)$ "%d " $(identity)$ do
        Q.Printf.printf "Solving case %d\n%!" _i;
        Q.Printf.fprintf $lid:(out_ch)$ "%s " (Q.Printf.sprintf "Case #%d:" _i);
        $(wrap_body in_spec)$;
        Q.Printf.fprintf $lid:(out_ch)$ "\n"
      done;
    }
  >>


(* Syntax extension
   -------------------------------------------------------------------------- *)

open Syntax


EXTEND Gram
  GLOBAL: expr comma_expr comma_ipatt str_item;

  let_binding: [
    [
      patt = ipatt; ":"; t = type_ ->
        (patt, t)
    ]
  ];

  type_: [
    [
        id = a_LIDENT; "["; idx = comma_expr; "]"; "of"; t = type_ ->
          let specs = Ast.list_of_expr idx [] in
          (match id with
            | "list" ->
                List.fold_right (fun idx acc -> List (idx, acc)) specs t
            | "array" ->
                List.fold_right (fun idx acc -> Array (idx, acc)) specs t
            | _ ->
                failwith (Printf.sprintf "Unknown type: %s" id))

      | "tuple"; "("; type_list = LIST0 type_ SEP ","; ")" ->
          Tuple type_list

      | "let"; binds = LIST1 let_binding SEP ","; "in"; t = type_ ->
          List.fold_right (fun (patt, t) a -> Let (patt, t, a)) binds t

      | id = a_LIDENT ->
          (match id with
            | "int"    -> Int
            | "int64"  -> Int64
            | "float"  -> Float
            | "string" -> String
            | "char"   -> Char
            | "line"   -> Line
            | "empty"  -> Empty
            | id       -> Expr <:expr< $lid:(id)$ >>)

      | e = expr -> Expr e
    ]
  ];

  input: [
    [
      "("; patt_list = comma_ipatt; ":"; type_ = type_; ")" ->
        match patt_list with
          | <:patt< ($tup:(patt_list)$) >> ->
              Ast.list_of_patt patt_list [] |> List.map (fun p -> (p, type_))
          | _ ->
              [ (patt_list, type_) ]
    ]
  ];

  str_item: LEVEL "top" [
    [
      "Solution"; input = LIST0 input; ":"; output = type_; "="; body = expr ->
        compile_solution (List.concat input) output body
    ]
  ];

END

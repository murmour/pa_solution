
(* Pa_solution, a helper library for solving programming contest problems
   -----------------------------------------------------------------------------
   Copyright (C) 2013, Max Mouratov (mmouratov(_)gmail.com)

   License:
     This library is free software; you can redistribute it and/or
     modify it under the terms of the GNU Library General Public
     License version 2.1, as published by the Free Software Foundation.

     This library is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

     See the GNU Library General Public License version 2.1 for more details
     (enclosed in the file LICENSE.txt).
*)


open Camlp4.PreCast
open Syntax


let _loc = Loc.ghost

type spec =
  | Int
  | Int64
  | Float
  | String
  | Line
  | Empty
  | List  of (Ast.expr * spec)          (* list[expr] of spec *)
  | Array of (Ast.expr * spec)          (* array[expr] of spec *)
  | Pair  of (spec * spec)              (* (spec * spec) *)
  | Let   of (Ast.patt * spec * spec)   (* let patt = spec in spec *)
  | Expr  of Ast.expr


(* Code generation utilities
   -------------------------------------------------------------------------- *)

let gensym =
  let id = ref 0 in
  fun () ->
    incr id;
    Printf.sprintf "_%d" !id


(* Reader
   -------------------------------------------------------------------------- *)

let scan format =
  <:expr< Scanf.bscanf in_buf $str:(format)$ (fun _x_ -> _x_) >>

let rec compile_reader (s: spec) : Ast.expr =
  match s with
    | Int    -> scan "%d "
    | Int64  -> scan "%Ld "
    | Float  -> scan "%f "
    | String -> scan "%s "
    | Line   -> scan "%[^\n]\n"

    | Empty ->
        <:expr< try $(compile_reader Line)$ with _ -> "" >>

    | List (size, r) ->
        <:expr< BatList.init $(size)$ (fun _ -> $(compile_reader r)$) >>

    | Array (size, r) ->
        <:expr< BatArray.init $(size)$ (fun _ -> $(compile_reader r)$) >>

    | Pair (r1, r2) ->
        let id1 = gensym () in
        let id2 = gensym () in
        <:expr<
          let $lid:(id1)$ = $(compile_reader r1)$ in
          let $lid:(id2)$ = $(compile_reader r2)$ in
          ($lid:(id1)$, $lid:(id2)$) >>

    | Let (patt, r1, r2) ->
        <:expr<
          let $(patt)$ = $(compile_reader r1)$ in
          $(compile_reader r2)$ >>

    | Expr v -> v


(* Writer
   -------------------------------------------------------------------------- *)

let print (v: Ast.expr) format =
  <:expr< Printf.bprintf out_buf $str:(format)$ $(v)$ >>

let rec compile_writer (s: spec) (v: Ast.expr) : Ast.expr =
  match s with
    | Int    -> print v "%d "
    | Int64  -> print v "%Ld "
    | Float  -> print v "%f "
    | String -> print v "%s "
    | Line   -> print v "%s\n"
    | Empty  -> print v "\n"

    | List (size, r) ->
        let id = gensym () in
        let writer = compile_writer r <:expr< $lid:(id)$ >> in
        <:expr< BatList.iter (fun $lid:(id)$ -> $(writer)$) $(v)$ >>

    | Array (size, r) ->
        let id = gensym () in
        let writer = compile_writer r <:expr< $lid:(id)$ >> in
        <:expr< BatArray.iter (fun $lid:(id)$ -> $(writer)$) $(v)$ >>

    | Pair (r1, r2) ->
        let a = gensym () in
        let b = gensym () in
        <:expr< let ($lid:(a)$, $lid:(b)$) = $(v)$ in
                do { $(compile_writer r1 <:expr< $lid:(a)$ >>)$;
                     $(compile_writer r2 <:expr< $lid:(b)$ >>)$ } >>

    | Let (let_id, r1, r2) ->
        compile_writer r2 v

    | Expr _ -> v


(* The compiler
   -------------------------------------------------------------------------- *)

let gen_mainloop (sol: Ast.expr) : Ast.str_item =
  <:str_item<
    let file = Sys.argv.(1) in
    BatFile.with_file_in ~mode:[`text] (file ^ ".in") (fun in_ch ->
      BatFile.with_file_out ~mode:[`create] (file ^ ".out") (fun out_ch ->
        let in_buf = Scanf.Scanning.from_string (BatIO.read_all in_ch) in
        let out_buf = Buffer.create 1024 in
        do {
          for _i = 1 to (Scanf.bscanf in_buf "%d " identity) do
            Printf.printf "Solving case %d\n%!" _i;
            Printf.bprintf out_buf "%s " (Printf.sprintf "Case #%d:" _i);
            $(sol)$;
            Printf.bprintf out_buf "\n"
          done;
          BatIO.nwrite out_ch (Buffer.contents out_buf) }))
  >>

let compile_solution in_spec out_spec (body: Ast.expr) : Ast.str_item =
  gen_mainloop
    (List.fold_right (fun (patt, spec) acc ->
       <:expr< let $(patt)$ = $(compile_reader spec)$ in $(acc)$ >>)
       in_spec
       (compile_writer out_spec body))


(* Syntax extension
   -------------------------------------------------------------------------- *)

EXTEND Gram
  GLOBAL: expr comma_expr str_item;

  let_binding: [
    [ patt = ipatt; ":"; t = typ -> (patt, t) ]
  ];

  typ: [
    [ id = a_LIDENT; "["; idx = comma_expr; "]"; "of"; t = typ ->
      let idx = Ast.list_of_expr idx [] in
      (match id with
        | "list" ->
            List.fold_right (fun idx acc -> List (idx, acc)) idx t
        | "array" ->
            List.fold_right (fun idx acc -> Array (idx, acc)) idx t
        | _ ->
            failwith (Printf.sprintf "Unknown type: %s" id))

    | "let"; binds = LIST1 let_binding SEP ","; "in"; t_in = typ ->
        List.fold_right (fun (patt, t) acc -> Let (patt, t, acc)) binds t_in

    | id = a_LIDENT ->
        (match id with
          | "int"    -> Int
          | "int64"  -> Int64
          | "float"  -> Float
          | "string" -> String
          | "line"   -> Line
          | "empty"  -> Empty
          | id       -> Expr <:expr< $lid:(id)$ >>)

    | "("; t1 = typ; "*"; t2 = typ; ")" ->
        Pair (t1, t2)

    | e = expr -> Expr e ]
  ];

  input: [
    [ "("; patt = ipatt; ":"; typ = typ; ")" ->
        (patt, typ) ]
  ];

  str_item: LEVEL "top" [
    [ "Solution"; inputs = LIST0 input; ":"; output = typ; "="; body = expr ->
        compile_solution inputs output body ]
  ];

END

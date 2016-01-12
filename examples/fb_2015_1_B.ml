(* Helper library: https://github.com/cakeplus/pa_solution *)

open Batteries
  (* https://github.com/ocaml-batteries-team/batteries-included *)


module Trie = struct

  type 'a t = Node of (char, 'a t) Hashtbl.t


  let create () =
    Node (Hashtbl.create 10)

  let add_word t word =
    let count = ref None in
    let rec traverse depth chars (Node h) =
      match chars with
        | [] ->
            if !count = None then count := Some depth
        | c :: cs ->
            let t' =
              try
                Hashtbl.find h c
              with Not_found ->
                if !count = None then count := Some (depth+1);
                create ()
            in
            Hashtbl.add h c t';
            traverse (depth+1) cs t'
    in
    traverse 0 (String.explode word) t;
    Option.get !count

end


Solution (n: int) (words: array[n] of line) : int =
  let count = ref 0 in
  let trie = Trie.create () in
  for i = 0 to n-1 do
    let ct = Trie.add_word trie words.(i) in
    count := !count + ct
  done;
  !count

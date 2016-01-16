=============
 Pa_solution
=============

Pa_solution is a compiled DSL for generating the IO boilerplate code
that is necessary for solving problems in programming contests such as
Google Code Jam and Facebook Hacker Cup.

See INSTALL.txt for building and installation instructions.
See LICENSE.txt for copying conditions.

Home page: https://github.com/cakeplus/pa_solution


Tutorial
========

The syntax extension introduces a new toplevel construct -- `Solution` --
that allows to define inputs and outputs of a single test case. The construct is
compiled into boilerplate code that performs all the necessary IO, in accordance
with the official specification of Google Code Jam.

Suppose we write a solution to `problem D`_ of Code Jam's 2013 qualification
round. This could be the starting point of the implementation:

.. sourcecode:: ocaml

  Solution (k, n: int) (keys: array[k] of int)
           (chests: array[n] of
              let t: int in
              let k: int in
              let c: array[k] of int in (t, k, c)) : tuple(float, int) =

    (* Solution logic: an expression that returns (float * int) *)

Here, we have defined a solution that first reads two integers `n` and `k`, then
reads `keys` -- an array of integers of size `n` -- and `chests` -- an array of
(`t` * `k` * `c`) tuples, where `t` and `k` are integers, and `c` is an array of
integers of size `k`.

If we were implementing this IO logic by hand, that'd be a lot of stupid
boilerplate code, yet solving contest problems ought to be fun... so grab
Pa_solution and cherish the benefits of perfect abstraction.

To run the compiled solution, pass it the name of the input file sans the `.in`
suffix (which is mandatory) as the first command line argument. The name of the
resulting file will have the `.out` suffix. For example, if the input is in the
file `A-attempt0.in`, then running the solution produces `A-attempt0.out`.

See lots of complete examples in the `examples` folder.


Syntax reference
================

Complete BNF-style grammar of the `Solution` construct, where keywords are
quoted and `...` is a comma-separated list. `<topform>` is the entry point::

  <topform> ::= "Solution" <input>* ":" <output> "=" <body>
  <input> ::= "(" <patt>... ":" <type> ")"
  <output> ::= <type>
  <body> ::= <expr>
  <type> ::= <prim_type> | <let> | <tuple> | <list> | <array> | <expr>
  <prim_type> ::= "int" | "int64" | "float" | "string" | "char" | "line" | "empty"
  <let> ::= "let" (<patt> ":" <type>)... "in" <type>
  <tuple> ::= "tuple" "(" <type>... ")"
  <list> ::= "list" "[" <expr>... "]" "of" <type>
  <array> ::= "array" "[" <expr>... "]" "of" <type>
  <expr> ::= OCaml expression
  <patt> ::= OCaml pattern

`<array>` allows to read several values into an OCaml array of a specified size
(the size of each dimension is defined by an arbitrary expression). Example:

.. sourcecode:: ocaml

  array[3, 3] of int
  (* Read a 3x3 matrix of integers *)

`<list>` is the same as `<array>` but for lists:

.. sourcecode:: ocaml

  list[3] of array[4] of int
  (* Read a 3-element list of 4-element integer arrays *)

`<let>` binds a read value into a variable that can be referenced in subsequent declarations. Example:

.. sourcecode:: ocaml

  let n: int in array[n] of float
  (* Read n, then read an array of floats of size n *)

`<tuple>` reads a tuple of values:

.. sourcecode:: ocaml

  let n: int in list[n] of tuple(int, float)
  (* Read an n-sized list of (int * float) tuples *)

`<int>`, `<int64>`, `<char>` and `<float>` correspond to the primitive OCaml types.

`<string>` is a whitespace-delimited word (same as `%s` in `Scanf`).

`<line>` is a string of arbitrary characters ending with a newline character (but excluding it).

`<empty>` is a newline character.


.. _`problem D`: https://code.google.com/codejam/contest/2270488/dashboard#s=p3

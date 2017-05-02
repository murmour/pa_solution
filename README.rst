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

  Solution (k, n: "%d ") (keys: array[k] of "%d ")
           (chests: array[n] of
              let t: "%d " in
              let k: "%d " in
              let c: array[k] of "%d " in (t, k, c)) : "%f %d" =
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
  <input> ::= "(" <patt>... ":" <type> ")" | <type>
  <output> ::= <type>
  <body> ::= <expr>
  <type> ::= <format> | <let> | <tuple> | <list> | <array> | <expr>
  <let> ::= "let" (<patt> ":" <type>)... "in" <type>
  <tuple> ::= "tuple" "(" <type>... ")"
  <list> ::= "list" "[" <expr>... "]" "of" <type>
  <array> ::= "array" "[" <expr>... "]" "of" <type>
  <expr> ::= OCaml expression
  <patt> ::= OCaml pattern
  <format> ::= OCaml format specifier

`<format>` is a formatting specification that uses the same syntax as functions
from the standard `Printf` module (for outputs, it is the same as `Scanf`). If
the specification string has no "holes", then the type of the corresponding
input/output value is `unit`. If there is more than one hole, then the type is a
tuple.

.. sourcecode:: ocaml

  "%d %f "
  (* Read an (int * float) tuple *)

  "%s@\n"
  (* Read a string that is terminated by a newline (that is, a line of input) *)

`<array>` allows to read several values into an OCaml array of a specified size
(the size of each dimension is defined by an arbitrary expression). Example:

.. sourcecode:: ocaml

  array[3, 3] of "%d "
  (* Read a 3x3 matrix of integers *)

`<list>` is the same as `<array>` but for lists:

.. sourcecode:: ocaml

  list[3] of array[4] of "%d "
  (* Read a 3-element list of 4-element integer arrays *)

`<let>` binds a read value into a variable that can be referenced in subsequent
declarations. Example:

.. sourcecode:: ocaml

  let n: "%d " in array[n] of "%f "
  (* Read n, then read an array of floats of size n *)

`<tuple>` reads a tuple of values:

.. sourcecode:: ocaml

  let n: "%d " in list[n] of tuple("%d ", "%f ")
  (* Read an n-sized list of (int * float) tuples *)

For brevity, this can be rewritten as follows, without an explicit `tuple`:

.. sourcecode:: ocaml

  let n: "%d " in list[n] of "%d %f "
  (* Read an n-sized list of (int * float) tuples *)


.. _`problem D`: https://code.google.com/codejam/contest/2270488/dashboard#s=p3

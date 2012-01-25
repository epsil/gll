GLL Parsers
===========

Different versions of a parser combinator framework loosely based
on the papers *Memoization in Top-Down Parsing* by Mark Johnson
and *Generalized Parser Combinators* by Daniel Spiewak. Written
in Racket, a beautiful dialect of Scheme.

Version 1
---------

`1/parser.rkt` contains a simple top-down parser implemented in
the common way. This version does not support left recursion.

Version 2
---------

`2/parser.rkt` rewrites the parser to use continuation-passing style
and memoization. This version supports left recursion, constructing
the parse tree incrementally.

Version 3
---------

`3/parser.rkt` uses trampolined dispatch so that the parsing process
is not constrained by the stack (or dependent on call optimization).
Parse results are returned as a lazy stream.

References
----------

* [Memoization in Top-Down Parsing](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.14.3000), Mark Johnson, Brown University, 1995. Published in *Computational Linguistics*, Volume 21, Number 3.
* [Generalized Parser Combinators](http://www.cs.uwm.edu/~dspiewak/papers/generalized-parser-combinators.pdf) (draft), Daniel Spiewak, University of Wisconsin, 2010. Implemented as the [gll-combinators](https://github.com/djspiewak/gll-combinators) Scala library.

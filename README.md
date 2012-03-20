GLL Parsers
===========

We'll show how to write a general parser combinator framework which
handles left-recursive grammars.

We'll use Racket, which is a dialect of Scheme. Advantages of Racket
are ... blah, blah, blah. (Intermediate stages) In the end, we'll have
an implementation which can be easily ported to other languages.

We'll write the parser in several steps:

1. First, we'll write a simple, top-down combinator framework.
Implementing things in the common way. This won't handle
left-recursive grammars yet, but it will give us a simple syntax
for piecing together parsers.

2. Next, we'll rewrite this parser to use continuation-passing style.
That is, instead of having each parser *return* a value, we'll pass in
a continuation - a piece of code - which *receives* the current
parsing results, continuing as necessary. Although the parser doesn't
support left recursion yet, this style is more flexible and sets the
stage for implementing a general parser.

3. We'll now add support for left-recursive grammars. We'll do this by
memoizing the parse results, so that nothing is computed more than
once. In this way we prevent the parsing from entering an infinite
regress, while still allowing a left-recursive grammar to be parsed
properly. This is the most important step, and we'll study it in some
detail to develop our intuition.

4. Since our continuation-passing approach is prone to overflowing the
stack, we'll add a trampoline to handle function calls. The
trampoline, which is passed to each parser, stores both the current
parse results and the work remaining to be done.

5. Now we have to werewithall to implement a lazy parse process.
The parser will return a stream of parse results, computing the
results as they are requested.

Let's get started.

Step 1: Simple parser combinators
---------------------------------

How it'll look:

(define article
  (term "the " "a "))

(define noun
  (term "student " "professor " "cat " "class "))

(define verb
  (term "studies " "lectures " "eats " "sleeps "))

(define noun-phrase
  (seq article noun))

(define verb-phrase
  (alt (seq verb noun-phrase)
       (seq verb sentence)))

(define sentence
  (seq NP VP))

(sentence "the student studies the cat intently "))

How will we deal with evaluation?

(define sentence
  (delay
    (seq NP VP)))

Or use delay in the macro body of ... seq and alt?
Delays are memoized!


;; verb phrase: VP -> V NP | V S

(define VP
  (alt (seq V NP)
       (seq V S)))


(define verb-phrase
  (alt (seq verb noun-phrase)
       (seq verb sentence)))

Although more likely:

(define-parser verb-phrase
  (alt (seq verb noun-phrase)
       (seq verb sentence)))


























We'll now add memoization to the continuation-passing parser,
to

Instead of having the higher-level parser collect the
return values of lower-level parsers and piece their results together,
we'll pass this operation in as a continuation argument.

we'll add a continuation argument to each parser and

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

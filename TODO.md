Code cleanup:

* Simplify `term`?
* Replace `print` with `displayln`
* memo-cps args . fn
* consistent formatting wrt. `memo`
* Let `s` be a parser that matches the empty string infinitely many times and let `t` be a terminal parser, for example for the string `"foo"`. Then `(seq s t)` is a parser that will never terminate for some nonparseable input `"bar"`. The first parser produces infinitely many results for the beginning of the string and the second parser always fails for the rest of the string. `s` will never stop producing results because it cannot know that `t` cannot use them. Infinite ambiguity can be curtailed by making `result-subsumed?` inspect the results' tails, but this also affects regular ambiguity.

----

*Scratch space begins here.*

----

Used in some LISP implementations, a trampoline is a loop that iteratively invokes thunk-returning functions. A single trampoline is sufficient to express all control transfers of a program; a program so expressed is trampolined or in "trampolined style"; converting a program to trampolined style is trampolining. Trampolined functions can be used to implement tail recursive function calls in stack-oriented languages.

----

Other common combinators are `many` and `many1`, which match the same parser multiple times. We will not cover their definitions here, but instead skip ahead to a complete example. The following implements a simple linguistic grammar taken from SICP:

Let us now look at a complete example. The following implements a simple linguistic grammar taken from SICP:

We can use `foldl` to generalize their definitions:

```Scheme
(define (alt . parsers)
  (define (alt2 b a)
    (lambda (arg)
      (match (a arg)
        [(result tree tail) (result tree tail)]
        [#f (b arg)])))
  (foldl alt2 (car parsers) (cdr parsers)))

(define (seq . parsers)
  (lambda parsers
    (define (seq2 b a)
      (lambda (arg)
        (match (a arg)
          [(result tree1 tail1)
           (match (b tail1)
             [(result tree2 tail2)
              (result (append tree1 (list tree2)) tail2)]
             [#f #f])]
          [#f #f])))
    (define (init arg)
      (result (list 'seq) arg))
    (foldl seq2 init parsers)))
```

We can now write `(alt (string "foo") (string "bar") (string "baz") ...)` and `(seq (string "foo") (string "bar") (string "baz") ...)`. The `(alt . parsers)` syntax is Racket's way of specifying that the whole argument list should be stored in the variable `parsers`. The binary functions `alt2` and `seq2` are local to `alt` and `seq`, respectively. The call to `foldl` "reduces" the argument list to a single value, using the local, binary function to "sum up".

----

That is, instead of having each parser *return* a value, we'll pass in a continuation (a snippet of code) which *receives* the current parsing results, continuing as necessary. Although the parser doesn't support left recursion yet, this style is more flexible and sets the stage for implementing a general parser.

We will now develop our parser combinators in a more general direction to better accommodate recursive and ambiguous grammars.

Since

When we implemented the `alt` combinator, we only returned the first matching alternative.

Once we start defining recursive

In general terms, a grammar is simply a specification of a language. There is no guarantee that the order of the grammar will translate into a sensible order of execution.

Furthermore, grammars can be ambiguous. When we implemented the `alt` combinator, we only returned the first matching alternative. In an ambiguous grammar, several alternatives could match at the same time, giving multiple, equally valid parsings of the input.

```Scheme
(define-parser t
  (alt (seq t (string "a"))
       (string "a")))
```

So far, we have treated grammars as executable specifications.

Testing as Racket:

```Racket
(define-parser t
  (alt (seq t (string "a"))
       (string "a")))
```

Testing as Scheme:

```Scheme
(define-parser t
  (alt (seq t (string "a"))
       (string "a")))
```

Testing as Lisp:

```Lisp
(define-parser t
  (alt (seq t (string "a"))
       (string "a")))
```

Don't think we can do this inline: ```Scheme (foo bar baz)```, ``Scheme (foo bar baz``, `Scheme (foo bar baz)`.

So far, we have treated grammars in a programmatic fashion: as little

So far, we have seen grammars as

We have had a linear approach to grammars.

We have now written a simple, top-down combinator framework, implementing things in the common way. While it cannot parse all grammars, it is simple to understand and simple to extend.

2. Next, we'll rewrite this parser to use continuation-passing style. That is, instead of having each parser *return* a value, we'll pass in a continuation (a snippet of code) which *receives* the current parsing results, continuing as necessary. Although the parser doesn't support left recursion yet, this style is more flexible and sets the stage for implementing a general parser.

----

How it'll look:

```Scheme
(define article
  (string "the " "a "))

(define noun
  (string "student " "professor " "cat " "class "))

(define verb
  (string "studies " "lectures " "eats " "sleeps "))

(define noun-phrase
  (seq article noun))

(define verb-phrase
  (alt (seq verb noun-phrase)
       (seq verb sentence)))

(define sentence
  (seq NP VP))

(sentence "the student studies the cat intently "))
```

How will we deal with evaluation?

```Scheme
(define sentence
  (delay
    (seq NP VP)))
```

Or use `delay` in the macro body of ... `seq` and `alt`? Delays are memoized! Lazy Racket does this automatically, but has some issues.

```Scheme
;; verb phrase: VP -> V NP | V S

(define VP
  (alt (seq V NP)
       (seq V S)))

(define verb-phrase
  (alt (seq verb noun-phrase)
       (seq verb sentence)))
```

Although more likely:

```Scheme
(define-parser verb-phrase
  (alt (seq verb noun-phrase)
       (seq verb sentence)))
```

----

----

Other combinators that are commonly defined in this way include `many` and `many1`, which match the same parser multiple times. We will not cover their definitions here, but instead skip ahead to the *reduce combinator*, which transforms the abstract syntax tree using a given function. Among other things, the reduce operator can be used to implement a *tagging combinator* which inserts a given value directly into the syntax tree.

```Scheme
(define (red parser fn)
  (lambda (arg)
    (match (parser arg)
      [(result (cons 'seq tree) tail)
       (result (apply fn tree) tail)]
      [(result tree tail)
       (result (list fn tree) tail)
       (result (fn tree) tail)]
      [#f #f])))

(define (tag parser t)
  (red parser (lambda tree (append (list t) tree))))
```

Let us now look at a complete example. The following implements a simple linguistic grammar taken from SICP:

```Scheme
(define article
  (tag (alt (string "the ")
            (string "a "))
       'article))

(define noun
  (tag (alt (string "student ")
            (string "professor "))
       'noun))

(define verb
  (tag (alt (string "studies ")
            (string "lectures "))
       'verb))

(define noun-phrase
  (tag (seq article noun) 'noun-phrase))

(define verb-phrase
  (tag (seq verb noun-phrase) 'verb-phrase))

(define sentence
  (tag (seq noun-phrase verb-phrase) 'sentence))
```

We can parse a sentence with:

```Scheme
> (sentence "the professor lectures the student ")
(success '(sentence (noun-phrase (article "the ")
                                 (noun "professor "))
                    (verb-phrase (verb "lectures ")
                                 (noun-phrase (article "the ")
                                              (noun "student "))))
         "")
> (sentence "not a sentence")
(failure "not a sentence")
```

----

We'll use Racket, which is a dialect of Scheme. Advantages of Racket are ... blah, blah, blah. (Intermediate stages.) In the end, we'll have an implementation which can be easily ported to other languages.

----

Different versions of a parser combinator framework loosely based on the papers *Memoization in Top-Down Parsing* by Mark Johnson and *Generalized Parser Combinators* by Daniel Spiewak. Written in Racket, a beautiful dialect of Scheme.

Version 1
---------

`1/parser.rkt` contains a simple top-down parser implemented in the common way. This version does not support left recursion.

Version 2
---------

`2/parser.rkt` rewrites the parser to use continuation-passing style and memoization. This version supports left recursion, constructing the parse tree incrementally.

Version 3
---------

`3/parser.rkt` uses trampolined dispatch so that the parsing process is not constrained by the stack (or dependent on call optimization). Parse results are returned as a lazy stream.

References
----------

* [Memoization in Top-Down Parsing](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.14.3000), Mark Johnson, Brown University, 1995. Published in *Computational Linguistics*, Volume 21, Number 3.
* [Generalized Parser Combinators](http://www.cs.uwm.edu/~dspiewak/papers/generalized-parser-combinators.pdf) (draft), Daniel Spiewak, University of Wisconsin, 2010. Implemented as the [gll-combinators](https://github.com/djspiewak/gll-combinators) Scala library.

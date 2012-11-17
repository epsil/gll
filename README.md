GLL Parsers
===========

We'll show how to write a general parser combinator framework which handles left-recursive grammars.

We'll use Racket, which is a dialect of Scheme. Advantages of Racket are ... blah, blah, blah. (Intermediate stages.) In the end, we'll have an implementation which can be easily ported to other languages.

We'll write the parser in several steps:

1. First, we'll write a simple, top-down combinator framework, implementing things in the common way. This won't handle left-recursive grammars yet, but it will give us a simple syntax for piecing together parsers.

2. Next, we'll rewrite this parser to use continuation-passing style. That is, instead of having each parser *return* a value, we'll pass in a continuation (a snippet of code) which *receives* the current parsing results, continuing as necessary. Although the parser doesn't support left recursion yet, this style is more flexible and sets the stage for implementing a general parser.

3. We'll now add support for left-recursive grammars. We'll do this by memoizing the parse results, so that nothing is computed more than once. In this way we prevent the parsing from entering an infinite regress, while still allowing a left-recursive grammar to be parsed properly. This is the most important step, and we'll study it in some detail to develop our intuition.

4. Since our continuation-passing approach is prone to overflowing the stack (especially in less powerful languages), we'll add a *trampoline* to handle function calls. The trampoline, which is passed to each parser, stores both the current parse results and the work remaining to be done.

5. Now we have to wherewithal to implement a lazy parse process. The parser will return a stream of parse results, computing the results as they are requested.

Let's get started.

Step 1: Simple parser combinators
---------------------------------

How it'll look:

```Scheme
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

Let's start by defining some terms.

A *parser* is a function which takes a string as input and returns a *parse result*. A successful result contains two values: an abstract syntax tree and the remainder of the string. A failing result just contains the input string, which can be used for error reporting.

A *parser combinator* is a function that takes parsers as input and returns another parser. In other words, it's a higher-order function, taking functions as input and returning a new function as output. Using parser combinators, we can build bigger parsers out of smaller parsers.

We'll start by defining a simple string parser. First we'll define data types for parse results:

```Scheme
(struct success (tree tail) #:transparent)
(struct failure (tail) #:transparent)
```

This defines `success` and `fail` as constructor functions for parse results. For example, we can create a successful result with the expression `(success tree tail)`, and a failure with `(failure tail)`. We can also *pattern match* against these expressions, which we will see multiple examples of below. (If we didn't have pattern matching, then we would need to check for a successful result with the function `success?` and extract its parts with the functions `success-tree` and `success-tail`, which are generated automatically.) The `#:transparent` option makes the values printable.

Now we can define two trivial parsers: one which accepts any input, and one which accepts no input. The first is often known as the "empty parser", "epsilon" or "return". It can be used to define optional parsings.

```Scheme
(define (succeed str)
  (success '() str))

(define (fail str)
  (failure str))
```

Both parsers take a string as input (the parameter `str`) and return a parse result. `succeed` returns a successful result with an empty syntax tree and the whole string as remainder; that is, it consumes no part of the input string. Similarly, `fail` returns a failing result.

The next step is to define a parser which compares the beginning of the input against some string. The following parser compares against the string `"foo"`:

```Scheme
(define (foo str)
  (let* ((len (min (string-length str) 3))
         (head (substring str 0 len))
         (tail (substring str len)))
    (if (equal? head "foo")
        (success head tail)
        (failure str))))
```

If the input matches, then the parser returns a successful result where the `"foo"` part is consumed; otherwise it returns a failure. For example, the input `"foobar"` gives a syntax tree of `"foo"` and a string remainder of `"bar"`:

```Scheme
> (foo "foobar")
(success "foo" "bar")
```

Parsers of this kind are often called *terminal parsers*, because they match against a terminal expression in the grammar. Let us create a general function for constructing such parsers. The following takes a matching string as input and returns a parser for matching against that string:

```Scheme
(define (term match)
  (lambda (str)
    (let* ((len (min (string-length str) (string-length match)))
           (head (substring str 0 len))
           (tail (substring str len)))
      (if (equal? head match)
          (success head tail)
          (failure str)))))
```

For example, `(term "foo")` constructs a parser that matches against `"foo"`, while `(term "bar")` matches against `"bar"`. The function `term` could well be considered a "parser generator": it creates a parser on the basis of a given specification.

Now that we have some basic parsers, it is time to combine them. The first combinator is the *alternatives combinator* (also called the "choice" combinator), which chooses between alternative parsers. The input to the combinator is the parsers to choose from, and the output is a new parser which tries each parser in turn.

```Scheme
(define (alt a b)
  (lambda (str)
    (match (a str)
      [(success tree tail) (success tree tail)]
      [failure (b str)])))
```

Here we see Racket's matching facility in action. Initially, we invoke the first parser with `(a arg)`, matching against the return value. If we get a successful parse result `(success tree tail)`, we return it. Otherwise, we invoke the second parser with `(b arg)` and return that instead. (For simplicity, the combinator only takes two arguments: we'll write a more general version later.)

Let's create a parser that matches either `"foo"` or `"bar"`:

```Scheme
(define foo-or-bar
  (alt (term "foo")
       (term "bar")))
```

This parser will succeed in parsing both the input strings `"foobar"` and `"barfoo"` (but not `"bazfoo"`).

The next combinator is the *sequence combinator*, which chains parsers together. The output of one parser is taken as the input to another. The sequence only succeeds if each individual parser does.

```Scheme
(define (seq a b)
  (lambda (arg)
    (match (a arg)
      [(result tree1 tail1)
       (match (b tail1)
         [(result tree2 tail2)
          (result (list 'seq tree1 tree2) tail2)]
         [#f #f])]
      [#f #f])))
```

We invoke the `a` parser first, using pattern matching on its return value. If it is successful, we invoke the `b` parser, using the remainder from the first parse, `tail1`, as input. If both parsers succeed, we return a parse result with the remainder from the last parser. The resulting syntax tree combines the syntax trees from each parser in a list `(seq tree1 tree2)`. If the `a` parser fails, or if `a` succeeds and `b` fails, then the combined parser fails as well.

Using the sequence parser, we can create a parser which matches `"foo"` followed by `"bar"`:

```Scheme
(define foobar
  (seq (term "foo")
       (term "bar")))

> (foobar "foobar")
(success (seq "foo" "bar") "")
```

Next, we define a few convenience combinators. The *optional combinator* matches a parser zero or one times:

```Scheme
(define (opt parser)
  (alt parser epsilon))
```

Simple repetition is implemented by the combinators `many` and `many1`. The `many` combinator matches a parser zero or more times, while the `many1` matches one or more times. We can define the former in terms of the latter:

```Scheme
(define (many1 parser)
  (lambda (arg)
    (match ((seq parser (many parser)) arg)
      [(result (list 'seq tree1 tree2) tail)
       (match tree2
         [(cons 'seq tree3)
          (result (append (list 'seq tree1) tree3) tail)]
         [_ (result (list 'seq tree1) tail)])]
      [#f #f])))

(define (many parser)
  (opt (many1 parser)))
```

The `many1` combinator is essentially defined as the recursive sequence `(seq parser (many parser))`, but we use pattern matching to flatten the syntax tree. The resulting tree is a arbitrarily long list `(seq r1 r2 r3 ...)`.

Finally, we define the *reduce combinator*, which transforms the abstract syntax tree using a given function. Among other things, the reduce operator will be used to implement a *tagging combinator* which can be used to insert a value into the syntax tree.

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
  (tag (alt (term "the ")
            (term "a "))
       'article))

(define noun
  (tag (alt (term "student ")
            (term "professor "))
       'noun))

(define verb
  (tag (alt (term "studies ")
            (term "lectures "))
       'verb))

(define noun-phrase
  (tag (seq article noun) 'noun-phrase))

(define verb-phrase
  (tag (seq verb noun-phrase) 'verb-phrase))

(define sentence
  (tag (seq noun-phrase verb-phrase) 'sentence))
```

We can now parse a sentence with:

```Scheme
(sentence "the professor lectures the student ")
  => (result '(sentence (noun-phrase (article "the ")
                                     (noun "professor "))
                        (verb-phrase (verb "lectures ")
                                     (noun-phrase (article "the ")
                                                  (noun "student "))))
             "")
(sentence "not a sentence")
  => #f
```

Improving the code
-------------------

As defined, the `alt` and `seq` functions only take two arguments. We can use `foldl` to generalize their definitions:

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

We can now write `(alt (term "foo") (term "bar") (term "baz") ...)` and `(seq (term "foo") (term "bar") (term "baz") ...)`. The `(alt . parsers)` syntax is Racket's way of specifying that the whole argument list should be stored in the variable `parsers`. The binary functions `alt2` and `seq2` are local to `alt` and `seq`, respectively. The call to `foldl` "reduces" the argument list to a single value, using the local, binary function to "sum up".

We now turn to the issue of efficiency. Currently, none of the functions cache their results. This is very inefficient because parsing is a such a repetitious task. If each function maintains a table over its input and output values, it can avoid calculating things twice by returning the cached value instead. This is called *memoization*.

In Racket, it is easy to write a `memo` function which takes any function as input and wraps it in a memoization routine. The wrapper takes the input arguments and looks them up in a memoization table. If it finds an output value, it just returns that. If not, then it calls the original function, saves its output in the table, and returns the output. Future calls with the same arguments will return the memoized value.

```Scheme
(define (memo fn)
  (let ((alist (mlist)))
    (lambda args
      (match (massoc args alist)
        [(mcons args result)
         result]
        [#f
         (let* ((result (apply fn args))
                (entry (mcons args result)))
           (set! alist (mcons entry alist))
           result)]))))
```

This function is loosely adapted from SICP. We implement the memoization table as a mutable *association list*, using the Racket function `massoc` to access it. It is actually a list of mutable cons cells `(args . result)`. If `massoc` returns a cons cell, we match against it and return the result. Otherwise, we call the original function with `(apply fn args)`, store the result in a cons cell, insert the cons cell into the table, and then return the result.

Now we can easily memoize our combinators. For example, the `alt` combinator can be defined as:

```Scheme
(define alt
  (memo (lambda parsers
          (define (alt2 b a)
            (lambda (arg)
              (match (a arg)
                [(result tree tail) (result tree tail)]
                [#f (b arg)])))
          (memo (foldl alt2 (car parsers) (cdr parsers))))))
```

The first use of `memo` memoizes the parser combinator, so that the same parser instance is returned for the same arguments. This is prudent because the combinator may be called several times at runtime by higher-order combinators like `many` and `many1`. The other use of `memo` memoizes the parser returned by the combinator, preventing re-parsing. We memoize all the combinators in this way.

There is a final wrinkle to sort out. Because of the way Racket evaluates function arguments, it is currently troublesome to define self-referential grammars:

```Scheme
(define s
  (alt (seq (term "a") s)
       (term "a")))
```

This will give an error because `s` is evaluated as an argument to `seq` before `s` is defined. The solution is to delay the evaluation by wrapping it in a function:

```Scheme
(define s
  (lambda (arg)
    ((alt (seq (term "a") s)
          (term "a"))
     arg)))
```

To make things more convenient, we can create a `define-parser` macro which automatically delays the code for us.

```Scheme
(define-syntax-rule (define-parser parser body ...)
  (define parser
    (lambda args
      (apply (begin body ...) args))))
```

Now we can write:

```Scheme
(define-parser s
  (alt (seq (term "a") s)
       (term "a")))
```

The current parser combinators support a subset of self-referential grammars called *right-recursive grammars*. The `s` parser is right-recursive because the self-reference is "to the right" in the sequence. Therefore, `s` will always consume some part of the input string (an `"a"`) before recursing.

In a *left-recursive grammar*, on the other hand, the self-reference is "to the left" of the input-consuming part:

```Scheme
(define-parser t
  (alt (seq t (term "a"))
       (term "a")))
```

This parser will enter an infinite regress because it repeatedly calls itself before consuming any input. To handle left-recursive grammars, we need to rethink the parser combinators.

Continuation-passing style
--------------------------

So far, we have taken advantage of the fact that many grammars can be translated directly into a program. The program has a simple, hierarchical structure, with functions calling functions all the way down to the level of string matching. It returns either a single result, or no result at all.

Not all grammars are this simple, however. Once we introduce recursion, there is not guarantee that the grammar will translate into a terminating program. Furthermore, grammars can be ambiguous: with several matching alternatives, a string can parsed in multiple, equally valid ways. For simplicity, our `alt` combinator only returned a single result (the first that matched). A more complete implementation would return the *set* of results.

To address these issues, we will rewrite and express our parsers in a more flexible way: *continuation-passing style*. Instead of having our parsers return their results to the caller, they will pass them to a continuation. The continuation then carries on the parsing. All the parsers will have an additional argument for the continuation they are to pass their results to. The continuation itself is a function of one argument.

Let us start by rewriting the `success` parser. Recall the original definition:

```Scheme
(define (succeed str)
  (success '() str))
```

To transform this function to continuation-passing style, we add a second argument, `cont`. Instead of returning the parse result, we pass it to `cont`.

```Scheme
(define (succeed str cont)
  (cont (success '() str)))
```

To use this parser, we need to supply it with a continuation. Any function of one argument will do. For example, we can use `print`:

```Scheme
> (succeed "string" print)
(result '() "string")
```

Of course, this is a bit cumbersome, so in the final version we will provide an easier interface for invoking parsers. For now, let's proceed with `term`:

```Scheme
(define (term str)
  (lambda (arg)
    (let* ((len (min (string-length arg) (string-length str)))
           (head (substring arg 0 len))
           (tail (substring arg len)))
      (if (equal? head str)
          (result head tail)
          #f))))
```

That is, instead of having each parser *return* a value, we'll pass in a continuation (a snippet of code) which *receives* the current parsing results, continuing as necessary. Although the parser doesn't support left recursion yet, this style is more flexible and sets the stage for implementing a general parser.

We will now develop our parser combinators in a more general direction to better accommodate recursive and ambiguous grammars.

Since

When we implemented the `alt` combinator, we only returned the first matching alternative.

Once we start defining recursive


In general terms, a grammar is simply a specification of a language. There is no guarantee that the order of the grammar will translate into a sensible order of execution.

Furthermore, grammars can be ambiguous. When we implemented the `alt` combinator, we only returned the first matching alternative. In an ambiguous grammar, several alternatives could match at the same time, giving multiple, equally valid parsings of the input.

```Scheme
(define-parser t
  (alt (seq t (term "a"))
       (term "a")))
```

So far, we have treated grammars as executable specifications.

Testing as Racket:

```Racket
(define-parser t
  (alt (seq t (term "a"))
       (term "a")))
```

Testing as Scheme:

```Scheme
(define-parser t
  (alt (seq t (term "a"))
       (term "a")))
```

Testing as Lisp:

```Lisp
(define-parser t
  (alt (seq t (term "a"))
       (term "a")))
```

Don't think we can do this inline: ```Scheme (foo bar baz)```, ``Scheme (foo bar baz``, `Scheme (foo bar baz)`.

So far, we have treated grammars in a programmatic fashion: as little

So far, we have seen grammars as

We have had a linear approach to grammars.

We have now written a simple, top-down combinator framework, implementing things in the common way. While it cannot parse all grammars, it is simple to understand and simple to extend.

2. Next, we'll rewrite this parser to use continuation-passing style. That is, instead of having each parser *return* a value, we'll pass in a continuation (a snippet of code) which *receives* the current parsing results, continuing as necessary. Although the parser doesn't support left recursion yet, this style is more flexible and sets the stage for implementing a general parser.

----

Code cleanup:

* Rename `arg` to `str`
* Define `failure`
* Simplify `term`?

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

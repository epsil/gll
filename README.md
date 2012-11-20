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

Let's get started!

Simple parser combinators
-------------------------

Let's start by defining some terms. A *parser* is a function which takes a string as input and returns a *parse result*. A parse result is either a success or a failure.

A *parser combinator* is a function that takes parsers as input and returns another parser. In other words, it's a higher-order function, taking functions as input and returning a new function as output. Using parser combinators, we can build bigger parsers out of smaller parsers.

First we'll define data types for parse results. A successful result contains two values: an abstract syntax tree and the remainder of the string. A failing result just contains the input string, which can be used for error reporting.

```Scheme
(struct success (tree tail) #:transparent)
(struct failure (tail) #:transparent)
```

This defines `success` and `fail` as constructor functions for parse results. For example, we can create a successful result with the expression `(success tree tail)`, and a failure with `(failure tail)`. We can also *pattern match* against these expressions, which will be demonstrated below. (If we didn't have pattern matching, then we would need to check for a successful result with the function `success?` and extract its parts with the functions `success-tree` and `success-tail`, which are generated automatically.) The `#:transparent` option makes the values printable.

Now we can define two trivial parsers: one which accepts any input, and one which accepts no input. The first is often known as the "empty parser", "epsilon" or "return". It can be used to define optional parsings.

```Scheme
(define (succeed str)
  (success '() str))

(define (fail str)
  (failure str))
```

Both parsers take a string as input (the parameter `str`) and return a parse result. `succeed` returns a successful result with an empty syntax tree and the whole string as remainder; that is, it consumes no part of the input. Similarly, `fail` returns a failing result.

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
  => (success "foo" "bar")
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

Note that the function returns `(lambda (str) ...)`, that is, another function. For example, `(term "foo")` constructs a parser that matches against `"foo"`, while `(term "bar")` matches against `"bar"`. The function `term` could well be considered a "parser generator": it creates a parser on the basis of a given specification.

Now that we have some basic parsers, it is time to combine them. The first combinator is the *sequence combinator*, which chains parsers together. The output of one parser is taken as the input to another. The sequence only succeeds if each individual parser does.

```Scheme
(define (seq a b)
  (lambda (str)
    (match (a str)
      [(success tree1 tail1)
       (match (b tail1)
         [(success tree2 tail2)
          (success (list 'seq tree1 tree2) tail2)]
         [failure failure])]
      [failure failure])))
```

Here we see Racket's matching facility in action. Initially, we invoke the first parser with `(a str)`, matching against the return value. If we get a successful parse result `(success tree1 tail1)`, then we pass its remainder to the next parser with `(b tail1)`. Again matching against the return value, we check if it is successful. If it is, then we return a combined parse result. In any other case, we return failure. (For simplicity, the combinator only takes two arguments; later we'll write a general version accepting any number of arguments.)

Using the sequence parser, we can create a parser which matches `"foo"` followed by `"bar"`:

```Scheme
(define foobar
  (seq (term "foo")
       (term "bar")))
```

The result combines the syntax trees from each parser in a list `(seq tree1 tree2)`:

```Scheme
> (foobar "foobar")
  => (success (seq "foo" "bar") "")
```

The next combinator is the *alternatives combinator* (also called the "choice" combinator), which chooses between alternative parsers. Each parser is tried in turn until one of them matches.

```Scheme
(define (alt a b)
  (lambda (str)
    (match (a str)
      [(success tree tail) (success tree tail)]
      [failure (b str)])))
```

If the first parser is successful, we return its result. Otherwise, we invoke the second parser and return that.

We can define other combinators in terms of these basic combinators. For example, the *optional combinator* matches a parser zero or one times:

```Scheme
(define (opt parser)
  (alt parser epsilon))
```

Other common combinators are `many` and `many1`, which match the same parser multiple times. We will not cover their definitions here, but instead skip ahead to a complete example. The following implements a simple linguistic grammar taken from SICP:

```Scheme
(define article
  (alt (term "the ")
       (term "a ")))

(define noun
  (alt (term "student ")
       (term "professor ")))

(define verb
  (alt (term "studies ")
       (term "lectures ")))

(define noun-phrase
  (seq article noun))

(define verb-phrase
  (seq verb noun-phrase))

(define sentence
  (seq noun-phrase verb-phrase))
```

We can parse a sentence with:

```Scheme
> (sentence "the professor lectures the student ")
  => (success '(seq (seq "the " "professor ")
                    (seq "lectures "
                         (seq "the " "student ")))
              "")
> (sentence "not a sentence")
  => (failure "not a sentence")
```

Currently, the syntax tree isn't very informative, but we can see that there is parsing going on.

Memoization
-----------

We now turn to the issue of efficiency. Currently, none of the functions cache their results. That is wasteful because parsing is a such a repetitious task. If each function maintains a table over its input and output values, it can avoid calculating things twice by returning the cached value instead. This is called *memoization*.

In Racket, it is easy to write a `memo` function which takes any function as input and wraps it in a memoization routine. The wrapper takes the input arguments and looks them up in a memoization table. If it finds an output value, it just returns that. If not, then it calls the original function, saves its output in the table, and returns the output. Future calls with the same arguments will return the memoized value. The following is loosely adapted from SICP:

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

We implement the memoization table as a mutable *association list*, using the Racket function `massoc` to access it. It is actually a list of mutable cons cells `(args . result)`. If `massoc` returns a cons cell, we match against it and return the result. Otherwise, we call the original function with `(apply fn args)`, store the result in a cons cell, insert the cons cell into the table, and then return the result.

Now we can easily memoize our combinators. Here is our original definition of `alt`:

```Scheme
(define (alt a b)
  (lambda (str)
    (match (a str)
      [(success tree tail) (success tree tail)]
      [failure (b str)])))
```

First, we recall that `(define (alt a b) ...)` is syntactic sugar for `(define alt (lambda (a b) ...))`:


```Scheme
(define alt
  (lambda (a b)
    (lambda (str)
      (match (a str)
        [(success tree tail) (success tree tail)]
        [failure (b str)]))))
```

Now we can pass each `lambda` to `memo`:

```Scheme
(define alt
  (memo (lambda (a b)
          (memo (lambda (str)
                  (match (a str)
                    [(success tree tail) (success tree tail)]
                    [failure (b str)]))))))
```

The first use of `memo` memoizes the parser combinator, so that the same parser instance is returned for the same arguments. This is prudent because the combinator may be called several times at runtime due to delayed evaluation. The other use of `memo` memoizes the parser returned by the combinator, preventing re-parsing. We can memoize all the combinators in this way.

There is a final wrinkle to sort out. Because of the way Racket evaluates function arguments, it is currently troublesome to define self-referential grammars:

```Scheme
(define r
  (alt (seq (term "a") r)
       (term "a")))
```

This will give an error because `r` is evaluated as an argument to `seq` before `r` is defined. We can delay the evaluation by wrapping it in a function:

```Scheme
(define r
  (lambda (arg)
    ((alt (seq (term "a") r)
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
(define-parser r
  (alt (seq (term "a") r)
       (term "a")))
```

The current parser combinators support recursion to a limited degree. The `r` parser is right-recursive because the self-reference is "to the right" in the sequence. Therefore, `r` will always consume some part of the input string (an `"a"`) before recursing. In a left-recursive grammar, on the other hand, the self-reference comes first:

```Scheme
(define-parser s
  (alt (seq s (term "a"))
       (term "a")))
```

This parser will enter an infinite regress because it repeatedly calls itself before consuming any input. To handle left-recursive grammars, we need to reimagine the parser combinators.

Continuation-passing style
--------------------------

So far, we have taken advantage of the fact that many grammars can be translated directly into a program. Such a program will have a straightforward, hierarchical structure, with functions calling functions all the way down to the level of string matching. It will either return a single result or no result at all.

Not all grammars are this simple, however. Once we introduce recursion, there is not guarantee that the grammar will translate into a terminating program. Furthermore, grammars can be ambiguous: with several matching alternatives, a string can parsed in multiple, equally valid ways. For simplicity, our `alt` combinator only returned a single result (the first that matched). A more complete implementation would return the *set* of results.

To address these issues, we will rewrite and express our parsers in a more flexible way: *continuation-passing style*. Instead of having our parsers return their results to the caller, they will pass them to a continuation. The continuation then carries on the parsing. All the parsers will have an additional argument for the continuation they are to pass their results to. The continuation itself is a function of one argument. (Racket actually has its own continuations, but we will use functions as continuations to make the implementation more portable.)

Let us start by rewriting the `success` parser. Recall the original definition:

```Scheme
(define (succeed str)
  (success '() str))
```

To transform this function to continuation-passing style, we add a second argument, `cont`. Instead of returning the parse result, we pass it to `cont`:

```Scheme
(define (succeed str cont)
  (cont (success '() str)))
```

To use this parser, we need to supply it with a continuation. Any function of one argument will do. For example, we can use `print`, which will cause the result to be printed to standard output:

```Scheme
> (succeed "string" print)
(result '() "string")
```

Of course, this is a bit cumbersome, so in the final version we will provide a simpler interface for invoking parsers. For now, let's proceed with `fail` and `term`:

```Scheme
(define (fail str cont)
  (cont (failure str)))

(define (term match)
  (lambda (str cont)
    (let* ((len (min (string-length str) (string-length match)))
           (head (substring str 0 len))
           (tail (substring str len)))
      (if (equal? head match)
          (cont (success head tail))
          (cont (failure tail))))))
```

These definitions are quite similar to the original. The `seq` combinator, however, is more complex:

```Scheme
(define (seq a b)
  (lambda (str cont)
    (a str (lambda (result)
             (match result
               [(success tree1 tail1)
                (b tail1 (lambda (result)
                           (match result
                             [(success tree2 tail2)
                              (cont (success (list 'seq tree1 tree2)
                                             tail2))]
                             [failure (cont failure)])))]
               [failure (cont failure)])))))
```

Here we use continuations to chain the parsers together. Each parser is called with a continuation `(lambda (result) ...)` that receives the result and matches against it. If the first parser is successful, then its continuation invokes the second parser. If the second parser is also successful, then its continuation creates a combined result and passes it to `cont` (the continuation for the combined parser). In all other cases, a failure is passed to `cont`.

While expressed in a different style, all the code so far functions the same as before. For the `alt` combinator, however, we will change the semantics. It will always try all alternatives, branching out in parallel:

```Scheme
(define (alt a b)
  (lambda (str cont)
    (a str cont)
    (b str cont)))
```

The continuation will be invoked twice, once for the first parser and once for the second. If the first parser succeeds and the second fails, then the continuation will first receive a successful result, and then a failing result.

We have now rewritten our parser combinators to continuation-passing style. In itself, this doesn't solve the problems we had with recursive grammars, but it sets the stage for the solution. Observe that while the `alt` combinator produces two results, it doesn't pass them at the same time. The execution is more fine-grained: the combinator creates a separate *branch* for the calculation of each result.

In other words, there is a kind of concurrency here (even if the current implementation is sequential). The key insight is that in a recursive grammar, one branch may depend on another: the recursive branch cannot continue before the base branch has produced a result. Is there a way we can make the branches cooperate?

As it turns out, the answer is memoization! That is because when we memoize a continuation-passing style function, not only do we need to keep track of input values and output values, but also the continuations that are interested in them. Each table entry will contain a list of results (since the function may output more than one value), and a list of continuations (since the same function may be called in different places in a recursive grammar). Thus we can populate results from one branch to another, "reawakening" the interested branch. How do we awaken a branch? By invoking its continuation!

To memoize functions written in continuation-passing style, we define a separate `memo-cps` wrapper. For readability, we define a few local functions for modifying a table entry. `push-continuation!` adds a continuation to an entry, `push-result!` adds a result to the entry, and `result-subsumed?` checks if the entry already contains a given result.

```Scheme
(define (memo-cps fn)
  (define entry-continuations mcar)
  (define entry-results mcdr)
  (define (push-continuation! entry cont)
    (set-mcar! entry (mappend (entry-continuations entry) (mlist cont))))
  (define (push-result! entry result)
    (set-mcdr! entry (mcons result (entry-results entry))))
  (define (result-subsumed? entry result)
    (mmember result (entry-results entry)))
  (let ((table (mlist)))
    (lambda (str cont)
      (match (massoc str table)
        ;; first time memoized procedure has been called with str
        [#f
         (let* ((entry (mcons (mlist cont) '()))
                (pair (mcons str entry)))
           (set! table (mcons pair table))
           (fn str
               (lambda (result)
                 (unless (result-subsumed? entry result)
                   (push-result! entry result)
                   (for ((cont (entry-continuations entry)))
                        (cont result))))))]
        ;; memoized procedure has been called with str before
        [(mcons str entry)
         (push-continuation! entry cont)
         (for ((result (entry-results entry)))
              (cont result))]))))
```

There are two cases to consider: when the memoized function is called for the first time, and when it has been called before. When the function is called for the first time, we insert the original continuation, `cont`, into the table. Then we invoke the function with a custom continuation `(lambda (result) ...)` which in turn will invoke `cont`, as well as any other continuations which may have been inserted into the table in the meantime. We can think of the continuation `(lambda (result ...)` as our "man on the inside": it alone will do the work of being passed into the function and receive its results. Then it broadcasts those results to the continuations on the outside.

Thus, in the second case when the function has been called before, we just insert the continuation into the list of continuations. Our "inside man" will then notify the continuation of new results as they are produced. Meanwhile, we go through the results that have been already produced.

Now we are ready to memoize our parser combinators. We use `memo-cps` for the returned parsers and `memo` for the parser combinators, which doesn't have a continuation. Using `memo` ensures that a particular parser is created only once, so that the same instance is called in two separate branches. This is what allows the branches to cooperate.

```Scheme
(define succeed
  (memo-cps
   (lambda (str cont)
     (cont (success '() str)))))

(define fail
  (memo-cps
   (lambda (str cont)
     (cont (failure str)))))

(define term
  (memo
   (lambda (match)
     (memo-cps
      (lambda (str cont)
        ...)))))

(define seq
  (memo
   (lambda (a b)
     (memo-cps
      (lambda (str cont)
        ...)))))

(define alt
  (memo
   (lambda (a b)
     (memo-cps
      (lambda (str cont)
        ...)))))
```

Now we can define our left-recursive grammar:

```Scheme
(define-parser s
  (alt (seq s (term "a"))
       (term "a")))
```

Let's parse a string of `"a"`s:

```Scheme
> (s "aaa" print)
(success "a" "aa")
(success '(seq "a" "a") "a")
(success '(seq (seq "a" "a") "a") "")
(failure "")
```

We get three results before the parser reaches the end of the string and terminates with a failure. It is worthwhile to consider the execution in detail.

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

We can parse a sentence with:

```Scheme
> (sentence "the professor lectures the student ")
  => (success '(sentence (noun-phrase (article "the ")
                                      (noun "professor "))
                         (verb-phrase (verb "lectures ")
                                      (noun-phrase (article "the ")
                                                   (noun "student "))))
              "")
> (sentence "not a sentence")
  => (failure "not a sentence")
```

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

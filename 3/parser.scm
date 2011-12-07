#! /usr/bin/racket
#lang racket

(require racket/mpair)
(require racket/stream)

;;; Memoization

(define (memo fn)
  (let ((alist '()))
    (lambda args
      (let ((entry (assoc args alist)))
        (if entry
            (cdr entry)
            (let ((result (apply fn args)))
              (set! alist (cons (cons args result) alist))
              result))))))

;;; Trampoline

(define trampoline%
  (class object% (super-new)

    (define stack (mlist))
    (define table (mlist))

    ;; whether the call stack is empty
    (define/public (has-next)
      (not (empty? stack)))

    ;; pop a call off the call stack
    (define/public (step)
      (when (has-next)
        (let* ((call (mcar stack))
               (fn (mcar call))
               (args (mcdr call)))
          (set! stack (mcdr stack))
          (apply fn args))))

    ;; push a call onto the call stack
    (define/public (push-stack fn . args)
      (set! stack (mappend stack (mlist (mcons fn args)))))

    ;; push a parser call onto the call stack
    (define/public (push fn arg continuation)
      (let ((memo (massoc fn table))
            (entry #f))
        (unless memo
          (set! memo (mcons fn (mlist)))
          (set! table (mcons memo table)))
        (set! entry (massoc arg (mcdr memo)))
        (cond
         ((not entry)
          (set! entry (mcons arg (mcons (mlist continuation) (mlist))))
          (set-mcdr! memo (mcons entry (mcdr memo)))
          (set! entry (mcdr entry))
          (push-stack fn arg this
                      (lambda (result)
                        (unless (mmember result (mcdr entry))
                          (set-mcdr! entry (mcons result (mcdr entry)))
                          (for ((cont (mcar entry)))
                               (push-stack cont result))))))
         (else
          ;; function has been called with arg before
          (set! entry (mcdr entry))
          (set-mcar! entry (mappend (mcar entry) (mlist continuation)))
          (for ((result (mcdr entry)))
               (push-stack continuation result))))))

    ;; run through the call stack
    (define/public (run)
      (do () ((not (has-next)))
        (step)))))

;;; Parser combinators

;; seriously, racket?
(define-syntax-rule (make-stream body ...)
  (stream-rest
   (stream-cons '()
                (begin body ...))))

(define-syntax-rule (make-parser (arg trampoline continuation) body ...)
  (lambda (arg (trampoline #f) (continuation #f))
    (let* ((results (if trampoline #f (mlist)))
           (trampoline (or trampoline (new trampoline%)))
           (continuation
            (or continuation
                (lambda (result)
                  (when (string=? "" (cdr result))
                    (set! results (mcons (car result) results)))))))
      (letrec ((compute
                (lambda ()
                  (when (send trampoline has-next)
                    (do () ((or (not (empty? results))
                                (not (send trampoline has-next))))
                      (send trampoline step)))
                  (let ((stream (sequence->stream results)))
                    (set! results (mlist))
                    (if (send trampoline has-next)
                        (stream-append stream (make-stream (compute)))
                        stream)))))
        (if results
            (make-stream
             (begin body ...)
             (compute))
            (begin body ...))))))

(define-syntax-rule (define-parser parser body ...)
  (define parser
    (make-parser
     (arg trampoline continuation)
     (let ((fn (implicit-conversion (begin body ...)))
           (continuation
            (lambda (r)
              (let ((result (car r))
                    (tail (cdr r)))
                (continuation
                 (cons (cons 'parser
                             (cond
                              ((null? result)
                               result)
                              ((and (list? result)
                                    (member (car result)
                                            '(seq term)))
                               (cdr result))
                              (else
                               (list result))))
                       tail))))))
       (fn arg trampoline continuation)))))

(define term
  (memo
   (lambda (match)
     (let ((length (string-length match)))
       (lambda (arg trampoline continuation)
         (when (and (string? arg)
                    (<= length (string-length arg))
                    (string=? match (substring arg 0 length)))
           (continuation
            (cons match (substring arg length)))))))))

(define (implicit-conversion parser)
  (if (string? parser)
      (term parser)
      parser))

(define seq
  (memo
   (lambda parsers
     (make-parser
      (arg trampoline continuation)
      (let* ((parsers (map implicit-conversion parsers))
             (fn (car parsers))
             (cont
              (foldr
               (lambda (fn continuation)
                 (lambda (r)
                   (let ((result (car r)))
                     (fn (cdr r)
                         trampoline
                         (lambda (r)
                           (continuation
                            (cons (append result
                                          (list (car r)))
                                  (cdr r))))))))
               continuation
               (cdr parsers))))
        (fn arg trampoline
            (lambda (r)
              (cont (cons (list 'seq (car r))
                          (cdr r))))))))))

(define alt
  (memo
   (lambda parsers
     (make-parser
      (arg trampoline continuation)
      (let ((parsers (map implicit-conversion parsers)))
        (for ((fn parsers))
             (send trampoline push fn arg continuation)))))))

(define maybe
  (memo
   (lambda (parser)
     (alt epsilon parser))))

(define many
  (memo
   (lambda (parser)
     (alt epsilon
          (seq parser (many parser))))))

(define many1
  (memo
   (lambda (parser)
     (seq parser (many parser)))))

;;; Parsers

(define (epsilon arg trampoline continuation)
  (continuation (cons '() arg)))

;;; Grammars

;; expr ::= expr op expr
;;       |  num
;;  num ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
;;   op ::= + | -
(define-parser expr
  (alt (seq expr op expr)
       num))

(define-parser num
  (alt "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))

(define-parser op
  (alt "+" "-"))

(stream->list (expr "1+2+3"))

;; R: S ::= a S
;;       |  a
;;       |  epsilon
(define-parser R:S
  (alt (seq "a" R:S)
       "a"
       epsilon))

(R:S "aaa")

;; R*: S ::= A S a
;;        |  a
;;     A ::= epsilon
(define-parser R*:S
  (alt (seq R*:A R*:S "a")
       "a"))

(define-parser R*:A
  epsilon)

(R*:S "aaa")

;; L: S ::= S a
;;       |  a
(define-parser L:S
  (alt (seq L:S "a")
       "a"))

(L:S "aaa")

;; L0: S ::= A S d
;;        |  B s
;;        |  epsilon
;;     A ::= a
;;        |  c
;;     B ::= a
;;        |  b
(define-parser L0:S
  (alt (seq L0:A L0:S "d")
       (seq L0:B L0:S)
       epsilon))

(define-parser L0:A
  (alt "a" "c"))

(define-parser L0:B
  (alt "a" "b"))

(L0:S "aaa")

;; L1: S ::= C a
;;        |  d
;;     B ::= epsilon
;;        |  a
;;     C ::= b
;;        |  B C b
(define-parser L1:S
  (alt (seq L1:C "a")
       "d"))

(define-parser L1:B
  (alt epsilon
       "a"))

(define-parser L1:C
  (alt "b"
       (seq L1:B L1:C "b")
       (seq "b" "b")))

(L1:S "ba")

;; L2: S ::= S S S
;;        |  S S
;;        |  a
(define-parser L2:S
  (alt "b"
       (seq L2:S L2:S)
       (seq L2:S L2:S L2:S)))

(L2:S "bbb")

;; exponential grammar
(L2:S "bbbbbbb")

;; L2*: S ::= b
;;         |  S S A
;;      A ::= S
;;         |  epsilon
(define-parser L2*:S
  (alt "b"
       (seq L2*:S L2*:S L2*:A)))

(define-parser L2*:A
  (alt L2*:S
       epsilon))

(L2*:S "bbb")

(define-parser SS
  (alt SS "a"))

;; infinite grammar
(stream-ref (SS "a") 2)

(define-parser M:A
  (alt M:B
       "a"))

(define-parser M:B
  (alt M:A
       "b"))

;; infinite grammar #2
(stream-ref (M:A "b") 0)

;; CME: A ::= B a
;;      B ::= C b
;;      C ::= B
;;         |  A
;;         |  c
(define-parser CME:A
  (seq CME:B "a"))

(define-parser CME:B
  (seq CME:C "b"))

(define-parser CME:C
  (alt CME:B
       CME:A
       "c"))

(stream->list (CME:A "cba"))

;; CME*: S ::= A
;;          |  B
;;       A ::= A a
;;          |  B
;;          |  a
;;       B ::= B b
;;          |  A
;;          |  b
(define-parser CME*:S
  (alt CME*:A
       CME*:B))

(define-parser CME*:A
  (alt (seq CME*:A "a")
       CME*:B
       "a"))

(define-parser CME*:B
  (alt (seq CME*:B "b")
       CME*:A
       "b"))

;; non-terminating grammar
(stream-ref (CME*:S "ab") 0)

;; SICP
(define-parser noun
  (alt "student " "professor " "cat " "class "))

(define-parser verb
  (alt "studies " "lectures " "eats " "sleeps "))

(define-parser article
  (alt "the " "a " "an "))

(define-parser sentence
  (seq noun-phrase verb-phrase))

(define-parser verb-phrase
  (alt (seq verb-phrase prep-phrase)
       verb))

(define-parser simple-noun-phrase
  (seq article noun))

(define-parser noun-phrase
  (alt (seq noun-phrase prep-phrase)
       simple-noun-phrase))

(define-parser preposition
  (alt "for " "to " "in " "by " "with "))

(define-parser prep-phrase
  (seq preposition noun-phrase))

(sentence "the student with the cat sleeps in the class ")
(sentence "the professor lectures to the student with the cat ")
(sentence "the professor lectures to the student in the class with the cat ")

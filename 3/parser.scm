#! /usr/bin/racket
#lang racket

(require racket/mpair)
(require racket/stream)

;;; Trampoline

(define trampoline%
  (class object% (super-new)

    ;; fields
    (define table (mlist))
    (define stack (mlist))

    ;; methods
    (define/public (has-next)
      (not (empty? stack)))

    (define/public (pop)
      (let ((call (mcar stack)))
        (set! stack (mcdr stack))
        call))

    (define/public (step)
      (when (has-next)
        (let* ((call (pop))
               (fn (mcar call))
               (arg (mcdr call))
               (entry (mcdr (massoc arg (mcdr (massoc fn table))))))
          (fn arg
              this
              (lambda (result)
                (unless (mmember result (mcdr entry))
                  (set-mcdr! entry (mcons result (mcdr entry)))
                  (for ((cont (mcar entry)))
                       (cont result))))))))

    (define/public (push fn arg continuation)
      (let ((memo (massoc fn table))
            (entry #f))
        (unless memo
          (set! memo (mcons fn (mlist)))
          (set! table (mcons memo table)))
        (set! entry (massoc arg (mcdr memo)))
        (cond
         ((not entry)
          ;; first time function has been called with arg
          (set! entry (mcons arg (mcons (mlist continuation) (mlist))))
          (set-mcdr! memo (mcons entry (mcdr memo)))
          (set! stack (mcons (mcons fn arg) stack)))
         (else
          ;; function has been called with arg before
          (set! entry (mcdr entry))
          (set-mcar! entry (mcons continuation (mcar entry)))
          (for ((result (mcdr entry)))
               (continuation result))))))

    (define/public (run)
      (do () ((not (has-next)))
        (step)))))

;;; Parser combinators

;; wtf, racket
(define-syntax-rule (make-stream body ...)
  (stream-rest
   (stream-cons 'dummy
                (begin body ...))))

(define-syntax-rule (define-parser parser body ...)
  (define parser
    (lambda (arg (trampoline #f) (continuation #f))
      (cond
       ((not trampoline)
        (letrec ((results (mlist))
                 (trampoline (new trampoline%))
                 (continuation
                  (lambda (result)
                    (when (null? (cdr result))
                      (set! results (mcons result results)))))
                 (compute
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
          (make-stream
           (parser arg trampoline continuation)
           (compute))))
       (else
        ((begin body ...)
         arg
         trampoline
         (lambda (r)
           (let ((result (car r))
                 (tail (cdr r)))
             (continuation
              (cons (cons 'parser
                          (cond
                           ((null? result)
                            result)
                           ((and (pair? result)
                                 (member (car result)
                                         '(seq term)))
                            (cdr result))
                           (else
                            (list result))))
                    tail))))))))))

(define-syntax-rule (term X ...)
  (lambda (arg trampoline continuation)
    (when (and (pair? arg)
               (member (car arg) '(X ...)))
      (continuation (cons (list 'term (car arg)) (cdr arg))))))

(define-syntax-rule (seq A ...)
  (lambda (arg trampoline continuation)
    (let* ((parsers (list A ...))
           (fn (car parsers))
           (cont (foldr (lambda (fn continuation)
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
                        (cdr r))))))))

(define-syntax-rule (alt A ...)
  (lambda (arg trampoline continuation)
    (for ((fn (list A ...)))
         (send trampoline push fn arg continuation))))

(define-syntax-rule (opt A)
  (alt epsilon A))

(define-syntax-rule (k* A)
  (alt epsilon
       (seq A (k* A))))

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
  (term 0 1 2 3 4 5 6 7 8 9))

(define-parser op
  (term + -))

(expr '(1 + 2 + 3))

;; R: S ::= a S
;;       |  a
;;       |  epsilon
(define-parser R:S
  (alt (seq (term a) R:S)
       (term a)
       epsilon))

(R:S '(a a a))

;; R*: S ::= A S a
;;        |  a
;;     A ::= epsilon
(define-parser R*:S
  (alt (seq R*:A R*:S (term a))
       (term a)))

(define-parser R*:A
  epsilon)

(R*:S '(a a a))

;; L: S ::= S a
;;       |  a
(define-parser L:S
  (alt (seq L:S (term a))
       (term a)))

(L:S '(a a a))

;; L0: S ::= A S d
;;        |  B s
;;        |  epsilon
;;     A ::= a
;;        |  c
;;     B ::= a
;;        |  b
(define-parser L0:S
  (alt (seq L0:A L0:S (term d))
       (seq L0:B L0:S)
       epsilon))

(define-parser L0:A
  (alt (term a)
       (term c)))

(define-parser L0:B
  (alt (term a)
       (term b)))

(L0:S '(a a a))

;; L1: S ::= C a
;;        |  d
;;     B ::= epsilon
;;        |  a
;;     C ::= b
;;        |  B C b
(define-parser L1:S
  (alt (seq L1:C (term a))
       (term d)))

(define-parser L1:B
  (alt epsilon
       (term a)))

(define-parser L1:C
  (alt (term b)
       (seq L1:B L1:C (term b))
       (seq (term b) (term b))))

(L1:S '(b a))

;; L2: S ::= S S S
;;        |  S S
;;        |  a
(define-parser L2:S
  (alt (term b)
       (seq L2:S L2:S)
       (seq L2:S L2:S L2:S)))

(L2:S '(b b b))

;; without any semantic rules (such as string concatenation),
;; the number of matches for this grammar is exponential
(L2:S '(b b b b b b b))

;; L2*: S ::= b
;;         |  S S A
;;      A ::= S
;;         |  epsilon
(define-parser L2*:S
  (alt (term b)
       (seq L2*:S L2*:S L2*:A)))

(define-parser L2*:A
  (alt L2*:S
       epsilon))

(L2*:S '(b b b))

;; SICP
(define-parser noun
  (term student professor cat class))

(define-parser verb
  (term studies lectures eats sleeps))

(define-parser article
  (term the a an))

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
  (term for to in by with))

(define-parser prep-phrase
  (seq preposition noun-phrase))

(sentence '(the student with the cat sleeps in the class))
(sentence '(the professor lectures to the student with the cat))
(sentence '(the professor lectures to the student in the class with the cat))

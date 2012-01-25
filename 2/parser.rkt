#! /usr/bin/racket
#lang racket

(require racket/mpair)

;;; Memoization

(define (memo cps-fn)
  (let ((table (mlist)))
    (lambda (arg (continuation #f))
      (let* ((results (mlist))
             (entry (massoc arg table))
             (continuation
              (or continuation
                  (lambda (result)
                    (when (null? (cdr result))
                      (set! results (mcons result results)))))))
        (unless entry
          (set! entry (mcons arg (mcons '() '())))
          (set! table (mcons entry table)))
        (set! entry (mcdr entry))
        (cond
         ((null? (mcar entry))
          ;; first time memoized procedure has been called with arg
          (set-mcar! entry (mcons continuation (mcar entry)))
          (cps-fn arg
                  (lambda (result)
                    (unless (mmember result (mcdr entry))
                      (set-mcdr! entry (mcons result (mcdr entry)))
                      (for ((cont (mcar entry)))
                           (cont result))))))
         (else
          ;; memoized procedure has been called with arg before
          (set-mcar! entry (mcons continuation (mcar entry)))
          (for ((result (mcdr entry)))
               (continuation result))))
        (unless (null? results)
          (mlist->list (mreverse! results)))))))

;;; Parser combinators

(define-syntax-rule (define-parser parser body ...)
  (define parser
    (memo
     (lambda (in c)
       ((begin body ...)
        in
        (lambda (r)
          (let ((result (car r))
                (tail (cdr r)))
            (c (cons (cons 'parser
                           (if (and (pair? result)
                                    (member (car result)
                                            '(seq term)))
                               (cdr result)
                               (list result)))
                     tail)))))))))

(define-syntax-rule (term X ...)
  (memo
   (lambda (in c)
     (when (and (pair? in)
                (member (car in) '(X ...)))
       (c (cons (list 'term (car in)) (cdr in)))))))

(define-syntax-rule (seq A ...)
  (memo
   (lambda (in c)
     (let* ((parsers (list A ...))
            (fn (car parsers))
            (cont (foldr (lambda (fn c)
                           (lambda (r)
                             (let ((result (car r)))
                               (fn (cdr r)
                                   (lambda (r)
                                     (c (cons (append result (list (car r)))
                                              (cdr r))))))))
                         c
                         (cdr parsers))))
       (fn in (lambda (r)
                (cont (cons (list 'seq (car r))
                            (cdr r)))))))))

(define-syntax-rule (alt A ...)
  (memo
   (lambda (in c)
     (for ((fn (list A ...)))
          (fn in c)))))

(define-syntax-rule (opt A)
  (alt epsilon A))

(define-syntax-rule (k* A)
  (alt epsilon
       (seq A (k* A))))

;;; Parsers

(define (epsilon in c)
  (c (cons '() in)))

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

;;; Tests

(sentence '(the student with the cat sleeps in the class))
(sentence '(the professor lectures to the student with the cat))
(sentence '(the professor lectures to the student in the class with the cat))

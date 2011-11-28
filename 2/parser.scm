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
                                            '(seq terminal)))
                               (cdr result)
                               (list result)))
                     tail)))))))))

(define-syntax-rule (terminal X ...)
  (memo
   (lambda (in c)
     (when (and (pair? in)
                (member (car in) '(X ...)))
       (c (cons (list 'terminal (car in)) (cdr in)))))))

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

;; article
(define-parser DET (terminal the a))

;; noun
(define-parser N (terminal student professor cat class))

;; noun phrase
(define-parser NP (seq DET N))

;; verb
(define-parser V (terminal studies lectures eats sleeps))

;; sentence
(define-parser S (seq NP VP))

;; verb phrase: VP -> V NP | V S
(define-parser VP
  (alt (seq V NP)
       (seq V S)))

;;; Test

(S '(the professor lectures the student))
(S '(the student studies the cat intently))

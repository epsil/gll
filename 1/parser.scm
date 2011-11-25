#! /usr/bin/racket
#lang racket

;; Custom version of foldl.
(define (reduce fn init args)
  (if (null? args)
      init
      (reduce fn
              (fn init (car args))
              (cdr args))))

;; Custom version of set-union.
(define (union set1 set2)
  (if (null? set1)
      set2
      (if (member (car set1) set2)
          (union (cdr set1) set2)
          (cons (car set1)
                (union (cdr set1) set2)))))

(define epsilon list)

(define-syntax-rule (terminal X ...)
  (lambda (p)
    (if (and (pair? p)
             (member (car p) '(X ...)))
        (list (cdr p))
        '())))

(define-syntax-rule (seq A ...)
  (lambda (p)
    (reduce (lambda (v fn)
              (reduce union '() (map fn v)))
            (list p)
            (list A ...))))

(define-syntax-rule (alt A ...)
  (lambda (p)
    (reduce (lambda (v fn)
              (union v (fn p)))
            '()
            (list A ...))))

(define-syntax-rule (opt A)
  (alt epsilon A))

(define-syntax-rule (k* A)
  (alt epsilon
       (seq A (k* A))))

;; determiner
(define DET (terminal the a))

;; noun
(define N (terminal student professor cat class))

;; noun phrase
(define NP (seq DET N))

;; verb
(define V (terminal studies lectures eats sleeps))

;; sentence
(define S (seq NP VP))

;; verb phrase: VP -> V NP | V S
(define VP
  (alt (seq V NP)
       (seq V S)))
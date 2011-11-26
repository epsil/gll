#! /usr/bin/racket
#lang racket

;; list version of set-union
(define (union . sets)
  (reverse (set->list (apply set-union (map list->set sets)))))

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

;;; Parser combinators

(define-syntax-rule (terminal X ...)
  (memo
   (lambda (p)
     (if (and (pair? p)
              (member (car p) '(X ...)))
         (list (cdr p))
         '()))))

(define-syntax-rule (seq A ...)
  (memo
   (lambda (p)
     (foldl (lambda (fn v)
              (foldl union '() (map fn v)))
            (list p)
            (list A ...)))))

(define-syntax-rule (alt A ...)
  (memo
   (lambda (p)
     (foldl (lambda (fn v)
              (union v (fn p)))
            '()
            (list A ...)))))

(define-syntax-rule (opt A)
  (memo (alt epsilon A)))

(define-syntax-rule (k* A)
  (memo
   (alt epsilon
        (seq A (k* A)))))

;;; Parsers

(define epsilon list)

;; article
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

;;; Test

(S '(the professor lectures the student))
(S '(the student studies the cat intently))

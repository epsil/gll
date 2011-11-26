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

(define-syntax-rule (define-parser parser body ...)
  (define parser
    (memo
     (lambda (p)
       (map (lambda (r)
              (cons (cons 'parser (cdr (car r)))
                    (cdr r)))
            ((begin body ...) p))))))

(define-syntax-rule (term X ...)
  (memo
   (lambda (p)
     (if (and (pair? p)
              (member (car p) '(X ...)))
         (list (cons (list 'term (car p)) (cdr p)))
         '()))))

(define-syntax-rule (seq A ...)
  (memo
   (lambda (p)
     (foldl (lambda (fn v)
              (foldl union '()
                     (map (lambda (prev)
                            (map (lambda (next)
                                   (cons (append (car prev)
                                                 (list (car next)))
                                         (cdr next)))
                                 (fn (cdr prev))))
                          v)))
            (list (cons '(seq) p))
            (list A ...)))))

(define-syntax-rule (alt A ...)
  (memo
   (lambda (p)
     (foldl (lambda (fn v)
              (union v (fn p)))
            '()
            (list A ...)))))

(define-syntax-rule (opt A)
  (alt epsilon A))

(define-syntax-rule (k* A)
  (alt epsilon
       (seq A (k* A))))

;;; Parsers

(define epsilon list)

;; article
(define-parser DET (term the a))

;; noun
(define-parser N (term student professor cat class))

;; noun phrase
(define-parser NP (seq DET N))

;; verb
(define-parser V (term studies lectures eats sleeps))

;; sentence
(define-parser S (seq NP VP))

;; verb phrase: VP -> V NP | V S
(define-parser VP
  (alt (seq V NP)
       (seq V S)))

;;; Test

(S '(the professor lectures the student))
(S '(the student studies the cat intently))

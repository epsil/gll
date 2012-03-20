#lang racket

(define (term x)
  (lambda (str)
    (let* ((len (string-length x))
           (head (substring str 0 len))
           (tail (substring str len)))
      (if (equal? head x)
          (cons head tail)
          #f))))

(define (alt a b)
  (lambda (str)
    (or (a str)
        (b str))))

(define (seq a b)
  (lambda (str)
    (let ((result1 (a str)))
      (if result1
          (let ((result2 (b (cdr result1))))
            (if result2
                (cons (list 'seq (car result1) (car result2))
                      (cdr result2))
                #f))
          #f))))

;;; Parsers

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

;;; Test

(sentence "the professor lectures the student ")

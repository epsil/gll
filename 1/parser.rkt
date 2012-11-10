#lang racket

(require racket/mpair)

;; (define (result tree tail) (cons tree tail)) (define result-tree car) (define result-tail cdr)
(struct result (tree tail) #:transparent)

;; vacuous
(define-syntax-rule (define-parser parser body ...)
  (define parser
    (lambda args
      (apply (begin body ...) args))))

;; memoize
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

;; terminal
(define term
  (memo (lambda strs
          (define (term1 str)
            (lambda (arg)
              (let* ((len (string-length str))
                     (head (substring arg 0 len))
                     (tail (substring arg len)))
                (if (equal? head str)
                    (result head tail)
                    #f))))
          (memo (apply alt (map term1 strs))))))

;; alternatives
(define alt
  (memo (lambda parsers
          (define (alt2 b a)
            (lambda (arg)
              (or (a arg)
                  (b arg))))
          (memo (foldl alt2 (car parsers) (cdr parsers))))))

;; sequence
(define seq
  (memo (lambda parsers
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
          (memo (foldl seq2 init parsers)))))

;; reduce
(define red
  (memo (lambda (parser fn)
          (lambda (arg)
            (match (parser arg)
              [(result (cons 'seq tree) tail)
               (result (cons fn tree) tail)]
              [(result tree tail)
               (result (list fn tree) tail)]
              [#f #f])))))

;;; Parsers

(define-parser article
  (alt (term "the ")
       (term "a ")))

(define-parser noun
  (alt (term "student ")
       (term "professor ")))

(define-parser verb
  (alt (term "studies ")
       (term "lectures ")))

(define-parser noun-phrase
  (seq article noun))

(define-parser verb-phrase
  (seq verb noun-phrase))

(define-parser sentence
  (seq noun-phrase verb-phrase))

;;; Test

(print (sentence "the professor lectures the student "))
(newline)
(print (sentence "not a sentence "))
(newline)

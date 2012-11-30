#lang racket

;;; Simple parser combinators

(require racket/mpair)

(struct success (tree tail) #:transparent)
(struct failure (tail) #:transparent)

(define-syntax-rule (delay-parser parser)
  (lambda args
    (apply parser args)))

(define-syntax-rule (define-parser parser body)
  (define parser
    (delay-parser body)))

(define (memo fn)
  (let ((alist (mlist)))
    (lambda args
      (match (massoc args alist)
        [(mcons args result) result]
        [_ (let* ((result (apply fn args))
                  (entry (mcons args result)))
             (set! alist (mcons entry alist))
             result)]))))

(define succeed
  (memo
   (lambda (str)
     (success '() str))))

(define fail
  (memo
   (lambda (str)
     (failure str))))

(define string
  (memo
   (lambda (match)
     (memo
      (lambda (str)
        (let* ((len (min (string-length str) (string-length match)))
               (head (substring str 0 len))
               (tail (substring str len)))
          (if (equal? head match)
              (success head tail)
              (failure str))))))))

(define seq
  (memo
   (lambda (a b)
     (memo
      (lambda (str)
        (match (a str)
          [(success tree1 tail1)
           (match (b tail1)
             [(success tree2 tail2)
              (success (list tree1 tree2) tail2)]
             [failure failure])]
          [failure failure]))))))

(define alt
  (memo
   (lambda (a b)
     (memo
      (lambda (str)
        (let ((result (a str)))
          (match result
            [(success tree tail) result]
            [failure (b str)])))))))

(define (opt parser)
  (alt parser succeed))

(define-parser article
  (alt (string "the ")
       (string "a ")))

(define-parser noun
  (alt (string "student ")
       (string "professor ")))

(define-parser verb
  (alt (string "studies ")
       (string "lectures ")))

(define-parser noun-phrase
  (seq article noun))

(define-parser verb-phrase
  (seq verb noun-phrase))

(define-parser sentence
  (seq noun-phrase verb-phrase))

(sentence "the professor lectures the student ")
(sentence "not a sentence ")

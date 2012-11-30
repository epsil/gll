#lang racket

;;; Simple parser combinators

(require racket/mpair)

(struct success (val rest) #:transparent)
(struct failure (rest) #:transparent)

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
   (lambda (val)
     (memo
      (lambda (str)
        (success val str))))))

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

(define (bind p fn)
  (lambda (str)
    (match (p str)
      [(success val rest)
       ((fn val) rest)]
      [failure failure])))

(define seq
  (memo
   (lambda (a b)
     (memo
      (bind a (lambda (x)
                (bind b (lambda (y)
                          (succeed (list x y))))))))))

(define alt
  (memo
   (lambda (a b)
     (memo
      (lambda (str)
        (let ((result (a str)))
          (match result
            [(success val rest) result]
            [failure (b str)])))))))

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

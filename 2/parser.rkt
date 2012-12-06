#lang racket

;;; Continuation-passing style

(require racket/mpair)

(struct success (val rest) #:transparent)
(struct failure (rest) #:transparent)

(define-syntax-rule (delay-parser parser)
  (lambda args
    (apply parser args)))

(define-syntax-rule (define-parser parser body)
  (define parser
    (make-parser
     (delay-parser body))))

(define (make-parser parser)
  (lambda (str (cont #f))
    (if cont
        (parser str cont)
        (run-parser parser str))))

(define (run-parser parser str)
  (let ((results '()))
    (parser str (lambda (result)
                  (match result
                    [(success val "")
                     (set! results (cons result results))]
                    [failure failure])))
    results))

(define (memo fn)
  (let ((alist (mlist)))
    (lambda args
      (match (massoc args alist)
        [(mcons args result) result]
        [_ (let* ((result (apply fn args))
                  (entry (mcons args result)))
             (set! alist (mcons entry alist))
             result)]))))

(define (memo-cps fn)
  (let ((table (mlist)))
    (define entry-continuations mcar)
    (define entry-results mcdr)
    (define (push-continuation! entry cont)
      (set-mcar! entry (mcons cont (entry-continuations entry))))
    (define (push-result! entry result)
      (set-mcdr! entry (mcons result (entry-results entry))))
    (define (result-subsumed? entry result)
      (mmember result (entry-results entry)))
    (define (make-entry)
      (mcons (mlist) (mlist)))
    (define (table-ref str)
      (match (massoc str table)
        [(mcons str entry) entry]
        [_ (let ((entry (make-entry)))
             (set! table (mcons (mcons str entry) table))
             entry)]))
    (lambda (str cont)
      (let ((entry (table-ref str)))
        (match entry
          ;; first time memoized procedure has been called with str
          [(mcons (mlist) (mlist))
           (push-continuation! entry cont)
           (fn str (lambda (result)
                     (unless (result-subsumed? entry result)
                       (push-result! entry result)
                       (for ((cont (entry-continuations entry)))
                            (cont result)))))]
          ;; memoized procedure has been called with str before
          [_
           (push-continuation! entry cont)
           (for ((result (entry-results entry)))
                (cont result))])))))

(define succeed
  (memo
   (lambda (val)
     (memo-cps
      (lambda (str cont)
        (cont (success val str)))))))

(define string
  (memo
   (lambda (match)
     (memo-cps
      (lambda (str cont)
        (let* ((len (min (string-length str) (string-length match)))
               (head (substring str 0 len))
               (tail (substring str len)))
          (if (equal? head match)
              (cont (success head tail))
              (cont (failure tail)))))))))

(define (bind p fn)
  (lambda (str cont)
    (p str (lambda (result)
             (match result
               [(success val rest)
                ((fn val) rest cont)]
               [failure
                (cont failure)])))))

(define seq
  (memo
   (lambda (a b)
     (memo-cps
      (bind a (lambda (x)
                (bind b (lambda (y)
                          (succeed (list x y))))))))))

(define alt
  (memo
   (lambda (a b)
     (memo-cps
      (lambda (str cont)
        (a str cont)
        (b str cont))))))

(define-parser s
  (alt (seq s (string "a"))
       (string "a")))

(s "aaa")

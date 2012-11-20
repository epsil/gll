#! /usr/bin/racket
#lang racket

(require racket/mpair)

;; (struct result (tree tail) #:transparent)
(struct success (tree tail) #:transparent)
(struct failure (tail) #:transparent)

;; vacuous
(define-syntax-rule (define-parser parser body ...)
  (define parser
    (lambda args
      (apply (begin body ...) args))))

;;; Memoization

;; (let ((pair (massoc str table))
;;             entry)
;;         (match (massoc str table)
;;           [(mcons str e)
;;            (set! entry e)]
;;           [#f
;;            (set! entry (mcons '() '()))
;;            (set! pair (mcons str entry))
;;            (set! table (mcons entry table))]))

;; (define (make-table)
;;   (list '*head))
;; (define (table-ref table key)
;;   (let ((pair (assoc key (cdr table))))
;;     (if pair
;;         (cdr pair)
;;         (let ((new-entry (make-entry)))
;;           (set-cdr! table (cons (cons key new-entry)
;;                                 (cdr table)))
;;           new-entry))))
;; (define (table-ref table key)
;;   (let ((pair (massoc key table)))
;;     (match (massoc key table)
;;       [(mcons key entry) entry]
;;       [#f (let* ((entry (make-entry))
;;                  (pair (mcons key entry)))
;;             (set! table (mcons pair table))
;;             entry)])))
;;
;; (define (make-entry)
;;   (cons '() '()))
;; (define entry-continuations car)
;; (define entry-results car)
;; (define (push-continuation! entry continuation)
;;   (set-cat! entry (cons continuation (car entry))))
;; (define (push-result! entry result)
;;   (set-cdr! entry (cons result (cdr entry))))
;; (define (result-subsumed? entry result)
;;   (member result (entry-results entry)))

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

(define (memo-cps fn)
  (define entry-continuations mcar)
  (define entry-results mcdr)
  (define (push-continuation! entry cont)
    (set-mcar! entry (mappend (entry-continuations entry) (mlist cont))))
  (define (push-result! entry result)
    (set-mcdr! entry (mcons result (entry-results entry))))
  (define (result-subsumed? entry result)
    (mmember result (entry-results entry)))
  (let ((table (mlist)))
    (lambda (str cont)
      (match (massoc str table)
        ;; first time memoized procedure has been called with str
        [#f
         (let* ((entry (mcons (mlist cont) '()))
                (pair (mcons str entry)))
           (set! table (mcons pair table))
           (fn str
               (lambda (result)
                 (unless (result-subsumed? entry result)
                   (push-result! entry result)
                   (for ((cont (entry-continuations entry)))
                        (cont result))))))]
        ;; memoized procedure has been called with str before
        [(mcons str entry)
         (push-continuation! entry cont)
         (for ((result (entry-results entry)))
              (cont result))]))))

;;; Parser combinators

;; (define-syntax-rule (define-parser parser body ...)
;;   (define parser
;;     (memo-cps
;;      (lambda (in c)
;;        ((begin body ...)
;;         in
;;         (lambda (r)
;;           (let ((result (car r))
;;                 (tail (cdr r)))
;;             (c (cons (cons 'parser
;;                            (if (and (pair? result)
;;                                     (member (car result)
;;                                             '(seq term)))
;;                                (cdr result)
;;                                (list result)))
;;                      tail)))))))))
;;
;; (define-syntax-rule (term X ...)
;;   (memo-cps
;;    (lambda (in c)
;;      (when (and (pair? in)
;;                 (member (car in) '(X ...)))
;;        (c (cons (list 'term (car in)) (cdr in)))))))
;;
;; (define-syntax-rule (seq A ...)
;;   (memo-cps
;;    (lambda (in c)
;;      (let* ((parsers (list A ...))
;;             (fn (car parsers))
;;             (cont (foldr (lambda (fn c)
;;                            (lambda (r)
;;                              (let ((result (car r)))
;;                                (fn (cdr r)
;;                                    (lambda (r)
;;                                      (c (cons (append result (list (car r)))
;;                                               (cdr r))))))))
;;                          c
;;                          (cdr parsers))))
;;        (fn in (lambda (r)
;;                 (cont (cons (list 'seq (car r))
;;                             (cdr r)))))))))
;;
;; (define-syntax-rule (alt A ...)
;;   (memo-cps
;;    (lambda (in c)
;;      (for ((fn (list A ...)))
;;           (fn in c)))))
;;
;; (define-syntax-rule (opt A)
;;   (alt epsilon A))
;;
;; (define-syntax-rule (k* A)
;;   (alt epsilon
;;        (seq A (k* A))))

(define succeed
  (memo-cps
   (lambda (str cont)
     (cont (success '() str)))))

(define fail
  (memo-cps
   (lambda (str cont)
     (cont (failure str)))))

(define term
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

(define seq
  (memo
   (lambda (a b)
     (memo-cps
      (lambda (str cont)
        (a str (lambda (result)
                 (match result
                   [(success tree1 tail1)
                    (b tail1 (lambda (result)
                               (match result
                                 [(success tree2 tail2)
                                  (cont (success (list 'seq tree1 tree2)
                                                 tail2))]
                                 [failure (cont failure)])))]
                   [failure (cont failure)]))))))))

(define alt
  (memo
   (lambda (a b)
     (memo-cps
      (lambda (str cont)
        (a str cont)
        (b str cont))))))

;;; Parsers

;; (define (epsilon in c)
;;   (c (cons '() in)))
;;
;; (define (epsilon* arg cont)
;;   (cont (result '() arg)))
;;
;; (define-parser noun
;;   (term student professor cat class))
;;
;; (define-parser verb
;;   (term studies lectures eats sleeps))
;;
;; (define-parser article
;;   (term the a an))
;;
;; (define-parser sentence
;;   (seq noun-phrase verb-phrase))
;;
;; (define-parser verb-phrase
;;   (alt (seq verb-phrase prep-phrase)
;;        verb))
;;
;; (define-parser simple-noun-phrase
;;   (seq article noun))
;;
;; (define-parser noun-phrase
;;   (alt (seq noun-phrase prep-phrase)
;;        simple-noun-phrase))
;;
;; (define-parser preposition
;;   (term for to in by with))
;;
;; (define-parser prep-phrase
;;   (seq preposition noun-phrase))

;;; Tests

;; (define-parser t
;;   (alt (seq t (term a))
;;        (term a)))

(define-parser s
  (alt (seq s (term "a"))
       (term "a")))

(define (print-line arg)
  (print arg)
  (newline))

;; (sentence '(the student with the cat sleeps in the class) print-line)
;; (sentence '(the professor lectures to the student with the cat) print-line)
;; (sentence '(the professor lectures to the student in the class with the cat) print-line)

(s "aaa" print-line)

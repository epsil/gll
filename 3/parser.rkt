#! /usr/bin/racket
#lang racket

(require racket/mpair)
(require racket/stream)

(struct success (tree tail) #:transparent)
(struct failure (tail) #:transparent)

;;; Memoization

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

;;; Trampoline

(define trampoline%
  (class object% (super-new)

    (define stack (mlist))
    (define table (mlist))

    ;; whether the call stack is empty
    (define/public (has-next?)
      (not (empty? stack)))

    ;; pop a call off the call stack
    (define/public (step)
      (when (has-next?)
        (match (mcar stack)
          [(mcons fn args)
           (set! stack (mcdr stack))
           (apply fn args)])))

    ;; push a call onto the call stack
    (define/public (push-stack fn . args)
      (let ((call (mcons fn args)))
        (set! stack (mappend stack (mlist call)))))

    ;; push a parser call onto the call stack
    (define/public (push fn str cont)
      (define entry-continuations mcar)
      (define entry-results mcdr)
      (define (push-continuation! entry cont)
        (set-mcar! entry
                   (mappend (entry-continuations entry) (mlist cont))))
      (define (push-result! entry result)
        (set-mcdr! entry (mcons result (entry-results entry))))
      (define (result-subsumed? entry result)
        (mmember result (entry-results entry)))
      (define (make-entry)
        (mcons (mlist) (mlist)))
      (define (table-ref fn str)
        (let ((pair (massoc fn table)))
          (match pair
            [(mcons fn memo)
             (match (massoc str memo)
               [(mcons str entry) entry]
               [#f (let ((entry (make-entry)))
                     (set-mcdr! pair (mcons (mcons str entry) memo))
                     entry)])]
            [#f (let* ((entry (make-entry))
                       (memo (mlist (mcons str entry))))
                  (set! table (mcons (mcons fn memo) table))
                  entry)])))
      (let ((entry (table-ref fn str)))
        (match entry
          ;; first time memoized procedure has been called with str
          [(mcons (mlist) (mlist))
           (push-continuation! entry cont)
           (push-stack fn str this
                       (lambda (result)
                         (unless (result-subsumed? entry result)
                           (push-result! entry result)
                           (for ((cont (mcar entry)))
                                (cont result)))))]
          ;; memoized procedure has been called with str before
          [_
           (push-continuation! entry cont)
           (for ((result (mcdr entry)))
                (cont result))])))

    ;; run through the call stack
    (define/public (run)
      (do () ((not (has-next?)))
        (step)))))

;;; Parser combinators

;; (define parser-tag (make-parameter 'parser))

;; (define-syntax-rule (make-parser (arg trampoline continuation) body ...)
;;   (lambda (arg (trampoline #f) (continuation #f))
;;     (let* ((results (if trampoline #f (mlist)))
;;            (trampoline (or trampoline (new trampoline%)))
;;            (continuation
;;             (or continuation
;;                 (lambda (result)
;;                   (when (string=? "" (cdr result))
;;                     (set! results (mcons (car result) results)))))))
;;       (letrec ((compute
;;                 (lambda ()
;;                   (when (send trampoline has-next)
;;                     (do () ((or (not (empty? results))
;;                                 (not (send trampoline has-next))))
;;                       (send trampoline step)))
;;                   (let ((stream (sequence->stream results)))
;;                     (set! results (mlist))
;;                     (if (send trampoline has-next)
;;                         (stream-append stream (make-stream (compute)))
;;                         stream)))))
;;         (if results
;;             (make-stream
;;              (begin body ...)
;;              (compute))
;;             (begin body ...))))))

;; (define-syntax-rule (define-parser parser body ...)
;;   (define parser
;;     (make-parser
;;      (arg trampoline continuation)
;;      (parameterize ((parser-tag 'parser))
;;        ;; handle (define-parser "foo" 'string->symbol)
;;        (let ((fn (implicit-conversion (begin body ...))))
;;          (fn arg trampoline continuation))))))

(define-syntax-rule (delay-parser parser)
  (lambda args
    (apply parser args)))

(define-syntax-rule (define-parser parser body)
  (define parser
    (make-parser
     (delay-parser body))))

(define (make-parser parser)
  (lambda (str (tramp #f) (cont #f))
    (if (and tramp cont)
        (parser str tramp cont)
        (run-parser parser str))))

;; seriously, racket?
(define-syntax-rule (make-stream body ...)
  (stream-rest
   (stream-cons '() (begin body ...))))

(define (run-parser parser str)
  (let ((tramp (new trampoline%))
        (results '()))
    (define (compute)
      (when (send tramp has-next?)
        (do () ((not (and (empty? results)
                          (send tramp has-next?))))
          (send tramp step)))
      (stream))
    (define (stream)
      (let ((result (sequence->stream results)))
        (set! results (mlist))
        (if (send tramp has-next?)
            (stream-append result (make-stream (compute)))
            result)))
    (make-stream
     (parser str tramp
             (lambda (result)
               (match result
                 [(success tree "")
                  (set! results (append results (list result)))]
                 [failure failure])))
     (compute))))

;; (define terminal
;;   (memo
;;    (lambda (match)
;;      (let ((length (string-length match)))
;;        (lambda (arg trampoline continuation)
;;          (when (and (string? arg)
;;                     (<= length (string-length arg))
;;                     (string=? match (substring arg 0 length)))
;;            (continuation
;;             (cons match (substring arg length)))))))))

;; (define (implicit-conversion parser)
;;   ;; (if (string? parser)
;;   ;;     (term parser)
;;   ;;     parser)
;;   parser)

;; ;; semantic action
;; (define reduce
;;   (memo
;;    (lambda (parser func)
;;      (if (null? func)
;;          parser
;;          (make-parser
;;           (arg trampoline continuation)
;;           (parser arg trampoline
;;                   (lambda (r)
;;                     (let ((result (car r))
;;                           (tail (cdr r)))
;;                       (continuation
;;                        (cons (cond
;;                               ((null? result)
;;                                (list func))
;;                               ((and (list? result)
;;                                     (equal? (car result) 'seq))
;;                                (cons func (cdr result)))
;;                               (else
;;                                (list func result)))
;;                              tail))))))))))

;; ;; sequence
;; (define sequence
;;   (memo
;;    (lambda parsers
;;      (make-parser
;;       (arg trampoline continuation)
;;       (let* ((parsers (map implicit-conversion parsers))
;;              (fn (car parsers))
;;              (cont
;;               (foldr
;;                (lambda (fn continuation)
;;                  (lambda (r)
;;                    (let ((result (car r)))
;;                      (fn (cdr r)
;;                          trampoline
;;                          (lambda (r)
;;                            (continuation
;;                             (cons (append result
;;                                           (list (car r)))
;;                                   (cdr r))))))))
;;                continuation
;;                (cdr parsers))))
;;         (fn arg trampoline
;;             (lambda (r)
;;               (cont (cons (list 'seq (car r))
;;                           (cdr r))))))))))

;; ;; alternatives
;; (define alternatives
;;   (memo
;;    (lambda parsers
;;      (make-parser
;;       (arg trampoline continuation)
;;       (let ((parsers (map implicit-conversion parsers)))
;;         (for ((fn parsers))
;;              (send trampoline push fn arg continuation)))))))

;; (define maybe
;;   (memo
;;    (lambda (parser)
;;      (alt epsilon parser))))
;;
;; (define many
;;   (memo
;;    (lambda (parser)
;;      (alt epsilon
;;           (seq parser (many parser))))))
;;
;; (define many1
;;   (memo
;;    (lambda (parser)
;;      (seq parser (many parser)))))

(define (succeed str tramp cont)
  (cont (success '() str)))

(define (fail str tramp cont)
  (cont (failure str)))

(define string
  (memo
   (lambda (match)
     (lambda (str tramp cont)
       (let* ((len (min (string-length str) (string-length match)))
              (head (substring str 0 len))
              (tail (substring str len)))
         (if (equal? head match)
             (cont (success head tail))
             (cont (failure tail))))))))

;; (define seq
;;   (memo
;;    (lambda (a b)
;;      (lambda (str tramp cont)
;;        (a str tramp
;;           (lambda (result)
;;             (match result
;;               [(success tree1 tail1)
;;                (b tail1 tramp
;;                   (lambda (result)
;;                     (match result
;;                       [(success tree2 tail2)
;;                        (cont (success (list 'seq tree1 tree2) tail2))]
;;                       [failure (cont failure)])))]
;;               [failure (cont failure)])))))))

(define seq
  (memo
   (lambda parsers
     (define (seq2 b a)
       (lambda (str tramp cont)
         (a str tramp
            (lambda (result)
              (match result
                [(success tree1 tail1)
                 (b tail1 tramp
                    (lambda (result)
                      (match result
                        [(success tree2 tail2)
                         (cont (success (append tree1 (list tree2))
                                        tail2))]
                        [failure (cont failure)])))]
                [failure (cont failure)])))))
     (foldl seq2 succeed parsers))))

;; (define alt
;;   (memo
;;    (lambda (a b)
;;      (lambda (str tramp cont)
;;        (send tramp push a str cont)
;;        (send tramp push b str cont)))))

(define alt
  (memo
   (lambda parsers
     (lambda (str tramp cont)
       (for ((fn parsers))
            (send tramp push fn str cont))))))

(define regexp
  (memo
   (lambda (pattern)
     (lambda (str tramp cont)
       (match (regexp-match-positions (string-append "^" pattern) str)
         [(cons (cons beg end) _)
          (let* ((len (string-length str))
                 (head (substring str beg end))
                 (tail (substring str end len)))
            (cont (success head tail)))]
         [_ (cont (failure str))])))))

(define red
  (memo
   (lambda (parser fn)
     (lambda (str tramp cont)
       (parser str tramp
               (lambda (result)
                 (match result
                   [(success (list tree ...) tail)
                    (cont (success (apply fn tree) tail))]
                   [(success tree tail)
                    (cont (success (fn tree) tail))]
                   [failure
                    (cont failure)])))))))

;; (define tag
;;   (memo
;;    (lambda (parser t)
;;      (red parser (lambda tree (append (list t) tree))))))


;; (define reduce
;;   (memo
;;    (lambda (parser func)
;;      (if (null? func)
;;          parser
;;          (make-parser
;;           (arg trampoline continuation)
;;           (parser arg trampoline
;;                   (lambda (r)
;;                     (let ((result (car r))
;;                           (tail (cdr r)))
;;                       (continuation
;;                        (cons (cond
;;                               ((null? result)
;;                                (list func))
;;                               ((and (list? result)
;;                                     (equal? (car result) 'seq))
;;                                (cons func (cdr result)))
;;                               (else
;;                                (list func result)))
;;                              tail))))))))))

;; (define (tag parser t)
;;   (red parser (lambda tree (append (list t) tree))))

(define (tag parser t)
  (red parser (lambda tree
                ;; (list t tree)
                (displayln "tree is: ")
                (displayln tree)
                (cond
                 ((string? tree)
                  (list t tree))
                 ((= (length tree) 1)
                  (list t tree))
                 (else
                  (append (list t) tree)))
                ;; (match tree
                ;;   [(list tree)
                ;;    (unless (string? tree)
                ;;      (displayln (length tree)))
                ;;    (list t tree)]
                ;;   [_ (append (list t) tree)])
                )))

;; Tests

(define-parser s
  (alt (seq s (string "a") (string "b") (string "c"))
       (seq (string "a") (string "b") (string "c"))
       (seq (string "d") (string "e") (string "f"))))

(define (print-line arg)
  (print arg)
  (newline))

(define tramp (new trampoline%))
(s "abc" tramp print)
;; (send tramp run)

(s "abcabc")

(stream->list (s "defabc"))

(define-parser number
  (red (regexp "[0-9]+") string->number))

(stream->list (number "2324"))

;; (define-parser expr
;;   (alt (red (seq expr (string "+") expr)
;;             (lambda (a op b) (+ a b)))
;;        (red (seq expr (string "-") expr)
;;             (lambda (a op b) (- a b)))
;;        (red (seq "(" expr ")")
;;             (lambda (_ x __) x))
;;        num))

;; (define-parser expr
;;   (alt (red (seq expr (string "+") num)
;;             (lambda (a _ b) (+ a b)))
;;        (red (seq expr (string "-") num)
;;             (lambda (a _ b) (- a b)))
;;        (red (seq (string "(") expr (string ")"))
;;             (lambda (_ x __) x))
;;        num))

;; (define-parser num
;;   (red (regexp "[0-9]+") string->number))

;; CPTT p. 49:
;; expr   -> expr + term
;;         | expr - term
;;         | term
;; term   -> term * factor
;;         | term / factor
;;         | factor
;; factor -> num
;;         | ( expr )

;; (define-parser expr
;;   (alt (seq expr (string "+") term)
;;        (seq expr (string "-") term)
;;        term))
;; (define-parser term
;;   (alt (seq term (string "*") factor)
;;        (seq term (string "/") factor)
;;        factor))
;; (define-parser factor
;;   (alt (seq (string "(") expr (string ")"))
;;        num))
;; (define-parser num
;;   (regexp "[0-9]+"))

;; (define-parser expr
;;   (alt (red (seq expr (string "+") term)
;;             (lambda (a _ b) (+ a b)))
;;        (red (seq expr (string "-") term)
;;             (lambda (a _ b) (- a b)))
;;        term))
;;
;; (define-parser term
;;   (alt (red (seq term (string "*") factor)
;;             (lambda (a _ b) (* a b)))
;;        (red (seq term (string "/") factor)
;;             (lambda (a _ b) (/ a b)))
;;        factor))
;;
;; (define-parser factor
;;   (alt (red (seq (string "(") expr (string ")"))
;;             (lambda (_ x __) x))
;;        num))
;;
;; (define-parser num
;;   (red (regexp "[0-9]+")
;;        string->number))


;; expr -> expr "+" num
;;       | expr "-" num
;;       | num
;; num -> "0" | "1"

;; (define-parser expr
;;   (alt (seq expr (string "+") num)
;;        (seq expr (string "-") num)
;;        num))
;; (define-parser num
;;   (alt (string "0") (string "1")))

(define-parser expr
  (alt (red (seq expr (string "+") term)
            (lambda (a _ b) (+ a b)))
       (red (seq expr (string "-") term)
            (lambda (a _ b) (- a b)))
       term))

(define-parser term
  (alt (red (seq term (string "*") factor)
            (lambda (a _ b) (* a b)))
       (red (seq term (string "/") factor)
            (lambda (a _ b) (/ a b)))
       factor))

(define-parser factor
  (alt (red (seq (string "(") expr (string ")"))
            (lambda (_ x __) x))
       num))

(define-parser num
  (red (regexp "[0-9]+")
       string->number))

(displayln "Testing arithmetic interpreter ...")

(stream->list (expr "1*2+3*4"))
(stream->list (expr "9-(5+2)"))

(define-parser noun
  (tag (alt (string "student ")
            (string "professor ")
            (string "cat ")
            (string "class "))
       'noun))

(define-parser verb
  (tag (alt (string "studies ")
            (string "lectures ")
            (string "eats ")
            (string "sleeps "))
       'verb))

(define-parser article
  (tag (alt (string "the ")
            (string "a ")
            (string "an "))
       'article))

(define-parser preposition
  (tag (alt (string "for ")
            (string "to ")
            (string "in ")
            (string "by ")
            (string "with "))
       'preposition))

(define-parser simple-noun-phrase
  (tag (seq article noun)
       'simple-noun-phrase))

(define-parser noun-phrase
  (tag (alt (seq noun-phrase prep-phrase)
            simple-noun-phrase)
       'noun-phrase))

(define-parser verb-phrase
  (tag (alt (seq verb-phrase prep-phrase)
            verb)
       'verb-phrase))

(define-parser prep-phrase
  (tag (seq preposition noun-phrase)
       'prep-phrase))

(define-parser sentence
  (tag (seq noun-phrase verb-phrase)
       'sentence))

;;; Tests

;; (sentence "the student with the cat sleeps in the class ")
;; (sentence "the professor lectures to the student with the cat ")
(stream->list (sentence "the professor lectures to the student in the class with the cat "))

#lang racket

;;; Trampolined dispatch

(require racket/mpair)
(require racket/stream)

(struct success (val rest) #:transparent)
(struct failure (rest) #:transparent)

(define-syntax-rule (delay-parser parser)
  (lambda args
    (apply parser args)))

(define-syntax-rule (define-parser parser body)
  (define parser
    (make-parser
     (delay-parser body))))

(define-syntax-rule (make-stream body ...)
  (stream-rest
   (stream-cons '() (begin body ...))))

(define (make-parser parser)
  (lambda (str (tramp #f) (cont #f))
    (if (and tramp cont)
        (parser str tramp cont)
        (run-parser parser str))))

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
                 [(success val "")
                  (set! results (cons result results))]
                 [failure failure])))
     (compute))))

(define (memo fn)
  (let ((alist (mlist)))
    (lambda args
      (match (massoc args alist)
        [(mcons args result) result]
        [_ (let* ((result (apply fn args))
                  (entry (mcons args result)))
             (set! alist (mcons entry alist))
             result)]))))

(define trampoline%
  (class object% (super-new)
    (define stack (mlist))
    (define table (mlist))

    (define/public (has-next?)
      (not (empty? stack)))

    (define/public (step)
      (when (has-next?)
        (match (mcar stack)
          [(mcons fn args)
           (set! stack (mcdr stack))
           (apply fn args)])))

    (define/public (push-stack fn . args)
      (let ((call (mcons fn args)))
        (set! stack (mcons call stack))))

    (define/public (push fn str cont)
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
      (define (table-ref fn str)
        (let ((pair (massoc fn table)))
          (match pair
            [(mcons fn memo)
             (match (massoc str memo)
               ;; parser has been called with str before
               [(mcons str entry) entry]
               ;; first time parser has been called with str
               [_ (let ((entry (make-entry)))
                    (set-mcdr! pair (mcons (mcons str entry) memo))
                    entry)])]
            ;; first time parser has been called
            [_ (let* ((entry (make-entry))
                      (memo (mlist (mcons str entry))))
                 (set! table (mcons (mcons fn memo) table))
                 entry)])))
      (let ((entry (table-ref fn str)))
        (match entry
          [(mcons (mlist) (mlist))
           (push-continuation! entry cont)
           ;; push the parser on the stack
           (push-stack fn str this
                       (lambda (result)
                         (unless (result-subsumed? entry result)
                           (push-result! entry result)
                           (for ((cont (entry-continuations entry)))
                                (cont result)))))]
          [_
           (push-continuation! entry cont)
           (for ((result (entry-results entry)))
                (cont result))])))

    (define/public (run)
      (do () ((not (has-next?)))
        (step)))))

(define succeed
  (memo
   (lambda (val)
     (lambda (str tramp cont)
       (cont (success val str))))))

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

(define (bind p fn)
  (lambda (str tramp cont)
    (p str tramp
       (lambda (result)
         (match result
           [(success val rest)
            ((fn val) rest tramp cont)]
           [failure
            (cont failure)])))))

(define seq
  (memo
   (lambda parsers
     (define (seq2 b a)
       (bind a (lambda (x)
                 (bind b (lambda (y)
                           (succeed (append x (list y))))))))
     (foldl seq2 (succeed '()) parsers))))

(define alt
  (memo
   (lambda parsers
     (lambda (str tramp cont)
       (for ((fn parsers))
            (send tramp push fn str cont))))))

(define red
  (memo
   (lambda (p fn)
     (bind p (lambda (val)
               (match val
                 [(list val ...) (succeed (apply fn val))]
                 [_ (succeed (fn val))]))))))

(define-parser expr
  (alt (red (seq expr (string "+") term)
            (lambda (x _ y) (+ x y)))
       (red (seq expr (string "-") term)
            (lambda (x _ y) (- x y)))
       term))

(define-parser term
  (alt (red (seq term (string "*") factor)
            (lambda (x _ y) (* x y)))
       (red (seq term (string "/") factor)
            (lambda (x _ y) (/ x y)))
       factor))

(define-parser factor
  (alt (red (seq (string "(") expr (string ")"))
            (lambda (_ x __) x))
       num))

(define-parser num
  (red (regexp "[0-9]+")
       string->number))

(stream->list (expr "1*2+3*4"))
(stream->list (expr "9-(5+2)"))

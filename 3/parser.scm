#! /usr/bin/racket
#lang racket

(require racket/mpair)

;;; Trampoline

(define trampoline%
  (class object% (super-new)

    ;; fields
    (define table (mlist))
    (define stack (mlist))

    ;; functions
    (define (pop)
      (unless (empty? stack)
        (let* ((call (mcar stack))
               (fn (mcar call))
               (arg (mcdr call))
               (entry (mcdr (massoc arg (mcdr (massoc fn table))))))
          (set! stack (mcdr stack))
          (fn arg
              this
              (lambda (result)
                (unless (mmember result (mcdr entry))
                  (set-mcdr! entry (mcons result (mcdr entry)))
                  (for ((cont (mcar entry)))
                       (cont result))))))))

    ;; methods
    (define/public (push fn arg continuation)
      (let ((memo (massoc fn table))
            (entry #f))
        (unless memo
          (set! memo (mcons fn (mlist)))
          (set! table (mcons memo table)))
        (set! entry (massoc arg (mcdr memo)))
        (cond
         ((not entry)
          ;; first time function has been called with arg
          (set! entry (mcons arg (mcons (mlist continuation) (mlist))))
          (set-mcdr! memo (mcons entry (mcdr memo)))
          (set! stack (mcons (mcons fn arg) stack)))
         (else
          ;; function has been called with arg before
          (set! entry (mcdr entry))
          (set-mcar! entry (mcons continuation (mcar entry)))
          (for ((result (mcdr entry)))
               (continuation result))))))

    (define/public (run)
      (do () ((empty? stack))
        (pop)))))

;;; Parser combinators

(define-syntax-rule (define-parser parser body ...)
  (define parser
    (lambda (arg (trampoline #f) (continuation #f))
      (cond
       ((not trampoline)
        (let* ((results (mlist))
               (trampoline (new trampoline%))
               (continuation
                (or continuation
                    (lambda (result)
                      (when (null? (cdr result))
                        (set! results (mcons result results)))))))
          (parser arg trampoline continuation)
          (send trampoline run)
          (unless (null? results)
            (mlist->list results))))
       (else
        ((begin body ...)
         arg
         trampoline
         (lambda (r)
           (let ((result (car r))
                 (tail (cdr r)))
             (continuation
              (cons (cons 'parser
                          (cond
                           ((null? result)
                            result)
                           ((and (pair? result)
                                 (member (car result)
                                         '(seq term)))
                            (cdr result))
                           (else
                            (list result))))
                    tail))))))))))

(define-syntax-rule (term X ...)
  (lambda (arg trampoline continuation)
    (when (and (pair? arg)
               (member (car arg) '(X ...)))
      (continuation (cons (list 'term (car arg)) (cdr arg))))))

(define-syntax-rule (seq A ...)
  (lambda (arg trampoline continuation)
    (let* ((parsers (list A ...))
           (fn (car parsers))
           (cont (foldr (lambda (fn continuation)
                          (lambda (r)
                            (let ((result (car r)))
                              (fn (cdr r)
                                  trampoline
                                  (lambda (r)
                                    (continuation
                                     (cons (append result
                                                   (list (car r)))
                                           (cdr r))))))))
                        continuation
                        (cdr parsers))))
      (fn arg trampoline
          (lambda (r)
            (cont (cons (list 'seq (car r))
                        (cdr r))))))))

(define-syntax-rule (alt A ...)
  (lambda (arg trampoline continuation)
    (for ((fn (list A ...)))
         (send trampoline push fn arg continuation))))

(define-syntax-rule (opt A)
  (alt epsilon A))

(define-syntax-rule (k* A)
  (alt epsilon
       (seq A (k* A))))

;;; Parsers

(define (epsilon arg trampoline continuation)
  (continuation (cons '() arg)))

(define-parser noun
  (term student professor cat class))

(define-parser verb
  (term studies lectures eats sleeps))

(define-parser article
  (term the a an))

(define-parser sentence
  (seq noun-phrase verb-phrase))

(define-parser verb-phrase
  (alt (seq verb-phrase prep-phrase)
       verb))

(define-parser simple-noun-phrase
  (seq article noun))

(define-parser noun-phrase
  (alt (seq noun-phrase prep-phrase)
       simple-noun-phrase))

(define-parser preposition
  (term for to in by with))

(define-parser prep-phrase
  (seq preposition noun-phrase))

(define-parser S
  (alt (seq S A)
       A))

(define-parser A
  (term a))

;;; Tests

(S '(a a a))

(sentence '(the student with the cat sleeps in the class))
(sentence '(the professor lectures to the student with the cat))
(sentence '(the professor lectures to the student in the class with the cat))

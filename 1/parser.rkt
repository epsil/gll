#lang racket

(require racket/mpair)

;; (define (result tree tail) (cons tree tail)) (define result-tree car) (define result-tail cdr)
;; (struct result (tree tail) #:transparent)
(struct success (tree tail) #:transparent)
(struct failure (tail) #:transparent)

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

;; empty/epsilon/return
(define (succeed str)
  (success '() str))

(define (fail str)
  (failure str))

;; terminal
(define term
  (memo (lambda strs
          (define (term1 match)
            (lambda (str)
              (let* ((len (min (string-length str) (string-length match)))
                     (head (substring str 0 len))
                     (tail (substring str len)))
                (if (equal? head match)
                    (success head tail)
                    (failure str)))))
          (memo (apply alt (map term1 strs))))))

(define term-rewritten
  (memo (lambda strs
          (define (term1 match)
            (lambda (str)
              (let* ((len (min (string-length str) (string-length match)))
                     (head (substring str 0 len))
                     (tail (substring str len)))
                (if (equal? head match)
                    (success head tail)
                    (failure str)))))
          (memo (apply alt (map term1 strs))))))

(define (foo str)
  (let* ((len (min (string-length str) 3))
         (head (substring str 0 len))
         (tail (substring str len)))
    (if (equal? head "foo")
        (success head tail)
        (failure str))))

(define (term+ match)
  (lambda (str)
    (let* ((len (min (string-length str) (string-length match)))
           (head (substring str 0 len))
           (tail (substring str len)))
      (if (equal? head match)
          (success head tail)
          (failure str)))))

(define term*
  (memo (lambda (match)
          (memo (lambda (str)
                  (let* ((len (min (string-length str) (string-length match)))
                         (head (substring str 0 len))
                         (tail (substring str len)))
                    (if (equal? head match)
                        (success head tail)
                        (failure str))))))))

;; alternatives
(define alt
  (memo (lambda parsers
          (define (alt2 b a)
            (lambda (str)
              (match (a str)
                [(success tree tail) (success tree tail)]
                [failure (b str)])))
          (memo (foldl alt2 (car parsers) (cdr parsers))))))

(define (alt* a b)
  (lambda (str)
    (match (a str)
      [(success tree tail) (success tree tail)]
      [failure (b str)])))

(define (alt+ . parsers)
  (define (alt2 b a)
    (lambda (str)
      (match (a str)
        [(success tree tail) (success tree tail)]
        [failure (b str)])))
  (foldl alt2 (car parsers) (cdr parsers)))

;; sequence
(define seq
  (memo (lambda parsers
          (define (seq2 b a)
            (lambda (str)
              (match (a str)
                [(success tree1 tail1)
                 (match (b tail1)
                   [(success tree2 tail2)
                    (success (append tree1 (list tree2)) tail2)]
                   [failure failure])]
                [failure failure])))
          (define (init str)
            (success (list 'seq) str))
          (memo (foldl seq2 init parsers)))))

(define (seq* a b)
  (lambda (str)
    (match (a str)
      [(success tree1 tail1)
       (match (b tail1)
         [(success tree2 tail2)
          (success (list 'seq tree1 tree2) tail2)]
         [failure failure])]
      [failure failure])))

(define (seq+ . parsers)
  (define (seq2 b a)
    (lambda (str)
      (match (a str)
        [(success tree1 tail1)
         (match (b tail1)
           [(success tree2 tail2)
            (success (append tree1 (list tree2)) tail2)]
           [failure failure])]
        [failure failure])))
  (define (init str)
    (success (list 'seq) str))
  (foldl seq2 init parsers))

;; optional
(define (opt parser)
  (alt parser succeed))

;; zero or more
;; (define (many parser)
;;   (opt (many1 parser)))

;; one or more
;; (define (many1 parser)
;; (define (any a)
;;   (match (many parser)
;;     [(success (cons 'seq tree) tail)
;;      (success tail tree)]
;;     [failure failure]))
;; (seq parser (alt succeed (many1 parser))))

;; (define (many1 parser)
;;   (lambda (str)
;;     (define (iterate r)
;;       (match r
;;         [(success tree1 tail1)
;;          (match (parser tail1)
;;            [(success tree2 tail2)
;;             (iterate (success (append tree1 (list tree2)) tail2))]
;;            [(failure tail) r])]
;;         [(failure tail) (failure tail)]))
;;     (define init
;;       (seq parser))
;;     (iterate (init str))))

(define (many1 parser)
  (memo
   (lambda (str)
     (match ((seq parser (many parser)) str)
       [(success (list 'seq tree1 tree2) tail)
        (match tree2
          [(cons 'seq tree3)
           (success (append (list 'seq tree1) tree3) tail)]
          [_ (success (list 'seq tree1) tail)])]
       [failure failure]))))

(define (many parser)
  (opt (many1 parser)))

;; reduce
(define (red parser fn)
  (lambda (str)
    (match (parser str)
      [(success (cons 'seq tree) tail)
       (success (apply fn tree) tail)]
      [(success tree tail)
       (success (list fn tree) tail)
       (success (fn tree) tail)]
      [failure failure])))

;; tag
(define (tag parser t)
  (red parser (lambda tree (append (list t) tree))))

;;; Parsers

;; (define article
;;   (tag (alt (term "the ")
;;             (term "a "))
;;        'article))

;; (define noun
;;   (tag (alt (term "student ")
;;             (term "professor "))
;;        'noun))

;; (define verb
;;   (tag (alt (term "studies ")
;;             (term "lectures "))
;;        'verb))

;; (define noun-phrase
;;   (tag (seq article noun) 'noun-phrase))

;; (define verb-phrase
;;   (tag (seq verb noun-phrase) 'verb-phrase))

;; (define sentence
;;   (tag (seq noun-phrase verb-phrase) 'sentence))

;; (alt (term "foo")
;;      (term "bar"))

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

(print (sentence "the professor lectures the student "))
(newline)
(print (sentence "professor lectures the student "))
(newline)
(print (sentence "not a sentence "))
(newline)






(define test-many
  (many (term "a")))
(print (test-many "aaaaa"))
(newline)
(sentence "")

;; (define s
;;   (alt (seq s "a") "a"))

;; (define s
;;   (lambda (str)
;;     ((alt (seq s "a") "a") str)))

(define-parser s
  (alt (seq (term "a") s)
       (term "a")))

(s "aaa")

(define-parser t
  (alt (seq t (term "a"))
       (term "a")))

;; (t "aaa")

(define (succeed2 str cont)
  (cont (success '() str)))

(define (fail2 str cont)
  (cont (failure str)))

(succeed2 "string" print)

(define (term2 match)
  (lambda (str cont)
    (let* ((len (min (string-length str) (string-length match)))
           (head (substring str 0 len))
           (tail (substring str len)))
      (if (equal? head match)
          (cont (success head tail))
          (cont (failure tail))))))

;; (define alt
;;   (memo (lambda (a b)
;;           (memo (lambda (str)
;;                   (match (a str)
;;                     [(success tree tail) (success tree tail)]
;;                     [failure (b str)]))))))

(define (alt2 a b)
  (lambda (str cont)
    (a str cont)
    (b str cont)))

(define (seq2 a b)
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
               [failure (cont failure)])))))
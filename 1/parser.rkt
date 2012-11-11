#lang racket

(require racket/mpair)

;; (define (result tree tail) (cons tree tail)) (define result-tree car) (define result-tail cdr)
(struct result (tree tail) #:transparent)
;; (struct success (tree tail) #:transparent)
;; (struct failure tail #:transparent)

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

;; empty
(define (epsilon arg)
  (result '() arg))

;; terminal
(define term
  (memo (lambda strs
          (define (term1 str)
            (lambda (arg)
              (let* ((len (min (string-length arg) (string-length str)))
                     (head (substring arg 0 len))
                     (tail (substring arg len)))
                (if (equal? head str)
                    (result head tail)
                    #f))))
          (memo (apply alt (map term1 strs))))))

(define (foo arg)
  (let* ((len (min (string-length "foo") (string-length arg)))
         (head (substring arg 0 len))
         (tail (substring arg len)))
    (if (equal? head "foo")
        (result head tail)
        #f)))

(define term*
  (memo (lambda (str)
          (memo (lambda (arg)
                  (let* ((len (min (string-length arg) (string-length str)))
                         (head (substring arg 0 len))
                         (tail (substring arg len)))
                    (if (equal? head str)
                        (result head tail)
                        #f)))))))

;; alternatives
(define alt
  (memo (lambda parsers
          (define (alt2 b a)
            (lambda (arg)
              (match (a arg)
                [(result tree tail) (result tree tail)]
                [#f (b arg)])))
          (memo (foldl alt2 (car parsers) (cdr parsers))))))

(define (alt* a b)
  (lambda (arg)
    (match (a arg)
      [(result tree tail) (result tree tail)]
      [#f (b arg)])))

(define (alt+ . parsers)
  (define (alt2 b a)
    (lambda (arg)
      (match (a arg)
        [(result tree tail) (result tree tail)]
        [#f (b arg)])))
  (foldl alt2 (car parsers) (cdr parsers)))

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

(define (seq* a b)
  (lambda (arg)
    (match (a arg)
      [(result tree1 tail1)
       (match (b tail1)
         [(result tree2 tail2)
          (result (list 'seq tree1 tree2) tail2)]
         [#f #f])]
      [#f #f])))

(define (seq+ . parsers)
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
  (foldl seq2 init parsers))

;; optional
(define (opt parser)
  (alt parser epsilon))

;; zero or more
;; (define (many parser)
;;   (opt (many1 parser)))

;; one or more
;; (define (many1 parser)
;; (define (any a)
;;   (match (many parser)
;;     [(result (cons 'seq tree) tail)
;;      (result tail tree)]
;;     [result result]))
;; (seq parser (alt epsilon (many1 parser))))

;; (define (many1 parser)
;;   (lambda (arg)
;;     (define (iterate r)
;;       (match r
;;         [(result tree1 tail1)
;;          (match (parser tail1)
;;            [(result tree2 tail2)
;;             (iterate (result (append tree1 (list tree2)) tail2))]
;;            [#f r])]
;;         [#f #f]))
;;     (define init
;;       (seq parser))
;;     (iterate (init arg))))

(define (many1 parser)
  (memo
   (lambda (arg)
     (match ((seq parser (many parser)) arg)
       [(result (list 'seq tree1 tree2) tail)
        (match tree2
          [(cons 'seq tree3)
           (result (append (list 'seq tree1) tree3) tail)]
          [_ (result (list 'seq tree1) tail)])]
       [#f #f]))))

(define (many parser)
  (opt (many1 parser)))

;; reduce
(define (red parser fn)
  (lambda (arg)
    (match (parser arg)
      [(result (cons 'seq tree) tail)
       (result (apply fn tree) tail)]
      [(result tree tail)
       (result (list fn tree) tail)
       (result (fn tree) tail)]
      [#f #f])))

;; tag
(define (tag parser t)
  (red parser (lambda tree (append (list t) tree))))

;;; Parsers

(define article
  (tag (alt (term "the ")
            (term "a "))
       'article))

(define noun
  (tag (alt (term "student ")
            (term "professor "))
       'noun))

(define verb
  (tag (alt (term "studies ")
            (term "lectures "))
       'verb))

(define noun-phrase
  (tag (seq article noun) 'noun-phrase))

(define verb-phrase
  (tag (seq verb noun-phrase) 'verb-phrase))

(define sentence
  (tag (seq noun-phrase verb-phrase) 'sentence))

;; (alt (term "foo")
;;      (term "bar"))

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
;;   (lambda (arg)
;;     ((alt (seq s "a") "a") arg)))

(define-parser s
  (alt (seq s "a") "a"))

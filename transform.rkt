#lang typed/racket
(provide (all-defined-out))
(require "tuples.rkt")
(require "matrix.rkt")

(: translation (-> Float Float Float Matrix))
(define (translation x y z)
  (build-matrix 4
                4
                (lambda ([row : Exact-Nonnegative-Integer] [col : Exact-Nonnegative-Integer])
                  (cond
                    [(= row col) 1.]
                    [(and (= row 0) (= col 3)) x]
                    [(and (= row 1) (= col 3)) y]
                    [(and (= row 2) (= col 3)) z]
                    [else 0.]))))

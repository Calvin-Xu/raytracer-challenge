#lang typed/racket
(provide (all-defined-out))
(require "tuples.rkt")
(require "matrix.rkt")

(: translate (-> Float Float Float Matrix))
(define (translate x y z)
  (build-matrix 4
                4
                (lambda ([row : Exact-Nonnegative-Integer] [col : Exact-Nonnegative-Integer])
                  (cond
                    [(= row col) 1.]
                    [(and (= row 0) (= col 3)) x]
                    [(and (= row 1) (= col 3)) y]
                    [(and (= row 2) (= col 3)) z]
                    [else 0.]))))
(: scale (-> Float Float Float Matrix))
(define (scale x y z)
  (build-matrix 4
                4
                (lambda ([row : Exact-Nonnegative-Integer] [col : Exact-Nonnegative-Integer])
                  (cond
                    [(and (= row 0) (= col 0)) x]
                    [(and (= row 1) (= col 1)) y]
                    [(and (= row 2) (= col 2)) z]
                    [(and (= row 3) (= col 3)) 1.]
                    [else 0.]))))

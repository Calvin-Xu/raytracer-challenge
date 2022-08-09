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

(: rotate (-> (U 'x 'y 'z) Float Matrix))
(define (rotate axis rad)
  (cond
    [(eq? axis 'x)
     (build-matrix 4
                   4
                   (lambda ([row : Exact-Nonnegative-Integer] [col : Exact-Nonnegative-Integer])
                     (cond
                       [(and (= row 0) (= col 0)) 1.]
                       [(and (= row 1) (= col 1)) (cos rad)]
                       [(and (= row 1) (= col 2)) (- (sin rad))]
                       [(and (= row 2) (= col 1)) (sin rad)]
                       [(and (= row 2) (= col 2)) (cos rad)]
                       [(and (= row 3) (= col 3)) 1.]
                       [else 0.])))]
    [(eq? axis 'y)
     (build-matrix 4
                   4
                   (lambda ([row : Exact-Nonnegative-Integer] [col : Exact-Nonnegative-Integer])
                     (cond
                       [(and (= row 0) (= col 0)) (cos rad)]
                       [(and (= row 0) (= col 2)) (sin rad)]
                       [(and (= row 1) (= col 1)) 1.]
                       [(and (= row 2) (= col 0)) (- (sin rad))]
                       [(and (= row 2) (= col 2)) (cos rad)]
                       [(and (= row 3) (= col 3)) 1.]
                       [else 0.])))]
    [(eq? axis 'z)
     (build-matrix 4
                   4
                   (lambda ([row : Exact-Nonnegative-Integer] [col : Exact-Nonnegative-Integer])
                     (cond
                       [(and (= row 0) (= col 0)) (cos rad)]
                       [(and (= row 0) (= col 1)) (- (sin rad))]
                       [(and (= row 1) (= col 0)) (sin rad)]
                       [(and (= row 1) (= col 1)) (cos rad)]
                       [(and (= row 2) (= col 2)) 1.]
                       [(and (= row 3) (= col 3)) 1.]
                       [else 0.])))]))

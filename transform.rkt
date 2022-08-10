#lang typed/racket
(provide (all-defined-out))
(require "tuples.rkt")
(require "matrix.rkt")

(: translate (-> Float Float Float Matrix))
(define (translate x y z)
  ;; 1 0 0 x
  ;; 0 1 0 y
  ;; 0 0 1 z
  ;; 0 0 0 1
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
  ;; x 0 0 0
  ;; 0 y 0 0
  ;; 0 0 z 0
  ;; 0 0 0 1
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
     ;; 1 0      0       0
     ;; 0 cos(r) −sin(r) 0
     ;; 0 sin(r) cos(r)  0
     ;; 0 0      0       1
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
     ;; cos(r)  0 sin(r) 0
     ;; 0       1 0      0
     ;; -sin(r) 0 cos(r) 0
     ;; 0       0 0      1
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
     ;; cos(r) −sin(r) 0 0
     ;; sin(r) cos(r)  0 0
     ;; 0      0       1 0
     ;; 0      0       0 1
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

(: shear (-> Float Float Float Float Float Float Matrix))
(define (shear dx/y dx/z dy/x dy/z dz/x dz/y)
  ;; 1    dx/y dx/z 0
  ;; dy/x 1    dy/z 0
  ;; dz/x dz/y 1    0
  ;; 0    0    0    1
  (build-matrix 4
                4
                (lambda ([row : Exact-Nonnegative-Integer] [col : Exact-Nonnegative-Integer])
                  (cond
                    [(and (= row 0) (= col 0)) 1.]
                    [(and (= row 0) (= col 1)) dx/y]
                    [(and (= row 0) (= col 2)) dx/z]
                    [(and (= row 1) (= col 0)) dy/x]
                    [(and (= row 1) (= col 1)) 1.]
                    [(and (= row 1) (= col 2)) dy/z]
                    [(and (= row 2) (= col 0)) dz/x]
                    [(and (= row 2) (= col 1)) dz/y]
                    [(and (= row 2) (= col 2)) 1.]
                    [(and (= row 3) (= col 3)) 1.]
                    [else 0.]))))

(: transformation (-> (Listof Matrix) Matrix))
(define (transformation transformations)
  (: iter (-> (Listof Matrix) Matrix Matrix))
  (define (iter remaining result)
    (if (null? remaining) result (iter (cdr remaining) (mat* (car remaining) result))))
  (iter transformations id-mat-4))

(: transform (-> Point Matrix * Point))
(define (transform pt . transformations)
  (cast (mat-t* (transformation transformations) pt) Point))

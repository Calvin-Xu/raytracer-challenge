#lang typed/racket
(require "tuples.rkt")

(define-type Matrix (Immutable-Vectorof (Immutable-Vectorof Float)))

;; (struct matrix
;;   ([m : Exact-Nonnegative-Integer]
;;    [n : Exact-Nonnegative-Integer]
;;    [elements : (Immutable-Vectorof (Immutable-Vectorof Float))])
;;   #:prefab
;;   #:type-name Matrix)

(: mat
   (-> Exact-Nonnegative-Integer
       Exact-Nonnegative-Integer
       (Immutable-Vectorof (Immutable-Vectorof Float))
       Matrix))
(define (mat m n rows)
  (if (and (= m (vector-length rows))
           (andmap (lambda ([x : Integer]) (= x n)) (vector->list (vector-map vector-length rows))))
      rows
      (error "Illegal operation: input not m by n 2D immutable vector" rows)))

(: mat-m (-> Matrix Exact-Nonnegative-Integer))
(define (mat-m mat)
  (vector-length mat))

(: mat-n (-> Matrix Exact-Nonnegative-Integer))
(define (mat-n mat)
  (vector-length (vector-ref mat 0)))

;; (: mat-elems (-> Matrix (Immutable-Vectorof (Immutable-Vectorof Float))))
;; (define (mat-elems mat)
;;   (matrix-elements mat))

(: mat-entry (-> Matrix Exact-Nonnegative-Integer Exact-Nonnegative-Integer Float))
(define (mat-entry mat m n)
  (if (or (>= m (mat-m mat)) (>= n (mat-n mat)))
      (error "Illegal operation: access matrix element out of bounds")
      (vector-ref (vector-ref mat m) n)))

(: mat-row (-> Matrix Exact-Nonnegative-Integer (Immutable-Vectorof Float)))
(define (mat-row mat m)
  (vector-ref mat m))

(: mat-col (-> Matrix Exact-Nonnegative-Integer (Immutable-Vectorof Float)))
(define (mat-col mat n)
  (vector->immutable-vector (cast (for/vector #:length
                                    (mat-n mat)
                                    ([row mat])
                                    (vector-ref row n))
                                  (Mutable-Vectorof Float))))

(: mat= (-> Matrix Matrix Boolean))
(define (mat= m1 m2)
  (: flatten-mat (-> Matrix (Listof Float)))
  (define (flatten-mat mat)
    (cast (flatten (vector->list (vector-map vector->list mat))) (Listof Float)))
  (: compare (-> (Listof Float) (Listof Float) Boolean))
  (define (compare l1 l2)
    (cond
      ;; guarantee non-empty lists in 2nd case for optimization
      ;; lengths are checked equal beforehand
      [(or (null? l1) (null? l2)) #t]
      [(f= (car l1) (car l2)) (compare (cdr l1) (cdr l2))]
      [else #f]))
  (and (= (mat-m m1) (mat-m m2))
       (= (mat-n m1) (mat-n m2))
       (compare (flatten-mat m1) (flatten-mat m2))))

(provide (all-defined-out))

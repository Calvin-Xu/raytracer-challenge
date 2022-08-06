#lang typed/racket
(require "tuples.rkt")

(struct matrix
        ([m : Exact-Nonnegative-Integer]
         [n : Exact-Nonnegative-Integer]
         [elements : (Immutable-Vectorof (Immutable-Vectorof Real))])
  #:prefab
  #:type-name Matrix)

(: mat
   (-> Exact-Nonnegative-Integer
       Exact-Nonnegative-Integer
       (Immutable-Vectorof (Immutable-Vectorof Real))
       Matrix))
(define (mat m n rows)
  (define n-eq?
    (for/fold ([res : Boolean #t]) ([row : (Immutable-Vectorof Real) rows])
      (and res (= n (vector-length row)))))
  (if (and (= m (vector-length rows)) n-eq?)
      (matrix m n rows)
      (error "Illegal operation: input not m by n 2D immutable vector" rows)))

(: mat-m (-> Matrix Exact-Nonnegative-Integer))
(define (mat-m mat)
  (matrix-m mat))

(: mat-n (-> Matrix Exact-Nonnegative-Integer))
(define (mat-n mat)
  (matrix-n mat))

(: mat-elems (-> Matrix (Immutable-Vectorof (Immutable-Vectorof Real))))
(define (mat-elems mat)
  (matrix-elements mat))

(: mat-entry (-> Matrix Exact-Nonnegative-Integer Exact-Nonnegative-Integer Real))
(define (mat-entry mat m n)
  (if (or (>= m (mat-m mat)) (>= n (mat-n mat)))
      (error "Illegal operation: access matrix element out of bounds")
      (vector-ref (vector-ref (mat-elems mat) m) n)))

(provide (all-defined-out))

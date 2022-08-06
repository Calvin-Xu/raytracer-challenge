#lang typed/racket
(require "tuples.rkt")

(define-type Matrix (Immutable-Vectorof (Immutable-Vectorof Float)))

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
  (vector->immutable-vector (for/vector: :
                                         (Mutable-Vectorof Float)
                                         #:length (mat-m mat)
                                         ([row (in-vector mat)])
                                         (vector-ref row n))))

(: mat= (-> Matrix Matrix Boolean))
(define (mat= m1 m2)
  (for/and: : Boolean ([row1 (in-vector m1)] [row2 (in-vector m2)])
    (for/and: : Boolean ([col1 (in-vector row1)] [col2 (in-vector row2)])
      (f= col1 col2))))

(: mat* (-> Matrix Matrix Matrix))
(define (mat* mat1 mat2)
  (: cross* (-> (Immutable-Vectorof Float) (Immutable-Vectorof Float) Float))
  (define (cross* v1 v2)
    (for/fold ([sum 0.]) ([x (in-vector v1)] [y (in-vector v2)])
      (+ sum (* x y))))
  
  (let ([m1 : Integer (mat-m mat1)]
        [n1 : Integer (mat-n mat1)]
        [m2 : Integer (mat-m mat2)]
        [n2 : Integer (mat-n mat2)])
    (if (= n1 m2)
        ((inst vector->immutable-vector (Immutable-Vectorof Float))
         (build-vector m1
                       (lambda ([row : Exact-Nonnegative-Integer])
                         (vector->immutable-vector
                          (build-vector n2
                                        (lambda ([col : Exact-Nonnegative-Integer])
                                          (cross* (mat-row mat1 row) (mat-col mat2 col))))))))
        (error "Illegal operation: multiply matrices with incompatible sizes" mat1 mat2))))

(: mat-tuple* (-> Matrix Tuple Tuple))
(define (mat-tuple* mat1 arg)
  (: tuple->matrix (-> Tuple Matrix))
  (define (tuple->matrix t)
    (let ([rows : (Listof Float) (list (tuple-x t) (tuple-y t) (tuple-z t) (tuple-w t))])
       ((inst vector->immutable-vector (Immutable-Vectorof Float))
        (build-vector 4
                      (lambda ([i : Integer])
                        (vector->immutable-vector
                         (build-vector (ann 1 Integer)
                                       (lambda (j) ((inst list-ref Float) rows i)))))))))

  (: matrix->tuple (-> Matrix Tuple))
  (define (matrix->tuple m)
    (tuple (vector-ref (vector-ref m 0) 0)
           (vector-ref (vector-ref m 1) 0)
           (vector-ref (vector-ref m 2) 0)
           (vector-ref (vector-ref m 3) 0)))
  (matrix->tuple (mat* mat1 (tuple->matrix arg))))

(: id-mat-4 Matrix)
(define id-mat-4
  (mat 4 4 #[#[1. 0. 0. 0.] #[0. 1. 0. 0.] #[0. 0. 1. 0.] #[0. 0. 0. 1.]]))

(: mat-T (-> Matrix Matrix))
(define (mat-T mat)
  ((inst vector->immutable-vector (Immutable-Vectorof Float))
   (build-vector (mat-n mat) (lambda ([y : Exact-Nonnegative-Integer]) (mat-col mat y)))))

(provide (all-defined-out))

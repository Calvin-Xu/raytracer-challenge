#lang typed/racket
(provide (except-out (all-defined-out) det-2))
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
  (vector->immutable-vector
   (for/vector: : (Mutable-Vectorof Float)
                #:length (mat-m mat)
                ([row (in-vector mat)])
                (vector-ref row n))))

(: mat= (-> Matrix Matrix Boolean))
(define (mat= m1 m2)
  (for/and: : Boolean ([row1 (in-vector m1)] [row2 (in-vector m2)])
    (for/and: : Boolean ([col1 (in-vector row1)] [col2 (in-vector row2)])
      (f= col1 col2))))

(: build-matrix
   (-> Exact-Nonnegative-Integer
       Exact-Nonnegative-Integer
       (-> Exact-Nonnegative-Integer Exact-Nonnegative-Integer Float)
       Matrix))
(define (build-matrix m n f)
  (cast ((inst vector->immutable-vector (Immutable-Vectorof Float))
         (build-vector
          m
          (lambda ([row : Exact-Nonnegative-Integer])
            (vector->immutable-vector
             (build-vector n (lambda ([col : Exact-Nonnegative-Integer]) (f row col))))))) Matrix))

(: mat* (-> Matrix Matrix Matrix))
(define (mat* mat1 mat2)
  (: dot* (-> (Immutable-Vectorof Float) (Immutable-Vectorof Float) Float))
  (define (dot* v1 v2)
    (for/fold ([sum 0.]) ([x (in-vector v1)] [y (in-vector v2)])
      (+ sum (* x y))))
  (let ([m1 : Exact-Nonnegative-Integer (mat-m mat1)]
        [n1 : Exact-Nonnegative-Integer (mat-n mat1)]
        [m2 : Exact-Nonnegative-Integer (mat-m mat2)]
        [n2 : Exact-Nonnegative-Integer (mat-n mat2)])
    (if (= n1 m2)
        (build-matrix m1 n2
                      (lambda ([row : Exact-Nonnegative-Integer] [col : Exact-Nonnegative-Integer])
                        (dot* (mat-row mat1 row) (mat-col mat2 col))))
        (error "Illegal operation: multiply matrices with incompatible sizes" mat1 mat2))))

(: mat-t* (-> Matrix Tuple Tuple))
(define (mat-t* m t)
  (define-syntax-rule (dot* t1 t2)
    (+ (* (tuple-x t1) (tuple-x t2))
       (* (tuple-y t1) (tuple-y t2))
       (* (tuple-z t1) (tuple-z t2))
       (* (tuple-w t1) (tuple-w t2))))
  (: row->tuple (-> (Immutable-Vectorof Float) Tuple))
  (define (row->tuple row)
    (tuple (vector-ref row 0) (vector-ref row 1) (vector-ref row 2) (vector-ref row 3)))
  (tuple (dot* (row->tuple (mat-row m 0)) t)
         (dot* (row->tuple (mat-row m 1)) t)
         (dot* (row->tuple (mat-row m 2)) t)
         (dot* (row->tuple (mat-row m 3)) t)))

(: id-mat (-> Exact-Nonnegative-Integer Matrix))
(define (id-mat n)
  (build-matrix n
                n
                (lambda ([row : Exact-Nonnegative-Integer] [col : Exact-Nonnegative-Integer])
                  (if (= row col) 1. 0.))))

(: id-mat-4 Matrix)
(define id-mat-4
  (mat 4 4 #[#[1. 0. 0. 0.] #[0. 1. 0. 0.] #[0. 0. 1. 0.] #[0. 0. 0. 1.]]))

(: transpose (-> Matrix Matrix))
(define (transpose mat)
  (cast ((inst vector->immutable-vector (Immutable-Vectorof Float))
   (build-vector (mat-n mat) (lambda ([y : Exact-Nonnegative-Integer]) (mat-col mat y)))) Matrix))

(: submat (-> Matrix Exact-Nonnegative-Integer Exact-Nonnegative-Integer Matrix))
(define (submat mat row col)
  (let ([rows (vector-append (vector-take mat row) (vector-drop mat (add1 row)))])
    (cast
     (vector->immutable-vector
      (for/vector ([y (in-vector rows)])
        (vector->immutable-vector (vector-append (vector-take y col) (vector-drop y (add1 col))))))
     Matrix)))

(: det-2 (-> Matrix Float))
(define (det-2 mat)
  (- (* (mat-entry mat 0 0) (mat-entry mat 1 1)) (* (mat-entry mat 0 1) (mat-entry mat 1 0))))

(: det (-> Matrix Float))
(define (det mat)
  (cond
    [(and (= (mat-m mat) 2) (= (mat-n mat) 2)) (det-2 mat)]
    [else
     (for/fold ([sum : Float 0.] [col : Exact-Nonnegative-Integer 0] #:result sum)
               ([elem (in-vector (mat-row mat 0))])
       (values
        (+ sum (* elem ((if (even? col) identity -) (det (submat mat 0 col)))))
        (add1 col)))]))

(: cofactor (-> Matrix Exact-Nonnegative-Integer Exact-Nonnegative-Integer Float))
(define (cofactor mat row col)
  (if (or (>= row (mat-m mat)) (>= col (mat-n mat)))
      (error "Illegal operation: calculate cofactor out of bounds" mat row col)
      ((if (even? (+ row col)) identity -) (det (submat mat row col)))))

(: inverse (-> Matrix Matrix))
(define (inverse mat)
  (let ([m (mat-m mat)] [n (mat-n mat)] [determinant (det mat)])
    (if (or (not (= m n)) (= 0. determinant))
        (error "Illegal operation: matrix cannot be inverted" mat)
        (transpose (build-matrix
                    n
                    n
                    (lambda ([row : Exact-Nonnegative-Integer] [col : Exact-Nonnegative-Integer])
                      (/ (cofactor mat row col) determinant)))))))

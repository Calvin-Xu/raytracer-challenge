#lang typed/racket
(provide (except-out (all-defined-out) stripe gradient ring checker))
(require "tuples.rkt")
(require "color.rkt")
(require "matrix.rkt")

(struct _pattern ([color-at : (-> Point Color)] [transformation : Matrix] [inv-trans : Matrix])
  #:prefab
  #:type-name Pattern)

(:
 pattern
 (->* ((U 'stripe 'gradient 'ring 'checker 'plain) (Listof Color)) (#:transformation Matrix) Pattern))
(define (pattern type
          colors
          #:transformation [transformation id-mat-4])
  (cond
    [(eq? type 'stripe) (stripe (cast colors (List Color Color)) transformation)]
    [(eq? type 'gradient) (gradient (cast colors (List Color Color)) transformation)]
    [(eq? type 'ring) (ring (cast colors (List Color Color)) transformation)]
    [(eq? type 'checker) (checker (cast colors (List Color Color)) transformation)]
    [(eq? type 'plain) (_pattern (lambda (point) (car colors)) id-mat-4 id-mat-4)]
    [else (error "Illegal operation: no pattern type: " type)]))

(define pattern-color-at _pattern-color-at)

(define pattern-transformation _pattern-transformation)

(define pattern-inv-trans _pattern-inv-trans)

(: stripe (-> (List Color Color) Matrix Pattern))
(define (stripe colors transformation)
  (_pattern (lambda (point)
              (if (= 0 (remainder (exact-floor (tuple-x point)) 2)) (first colors) (second colors))) transformation (inverse transformation)))

(: gradient (-> (List Color Color) Matrix Pattern))
(define (gradient colors transformation)
  (_pattern (lambda (point)
              (let ([delta : Color
                     (color- (second colors) (first colors))]
                    [frac : Float
                          (- (tuple-x point) (floor (tuple-x point)))])
                (color+ (first colors) (color* delta frac))))
            transformation
            (inverse transformation)))

(: ring (-> (List Color Color) Matrix Pattern))
(define (ring colors transformation)
  (_pattern
   (lambda (point)
     (if (= 0 (remainder (exact-floor (sqrt (+ (sqr (tuple-x point)) (sqr (tuple-z point))))) 2))
         (first colors)
         (second colors)))
   transformation
   (inverse transformation)))

(: checker (-> (List Color Color) Matrix Pattern))
(define (checker colors transformation)
  (_pattern (lambda (point)
              (if (= 0
                     (remainder (+ (exact-floor (tuple-x point))
                                   (exact-floor (tuple-y point))
                                   (exact-floor (tuple-z point)))
                                2))
                  (first colors)
                  (second colors)))
            transformation
            (inverse transformation)))
